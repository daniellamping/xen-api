(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Generation of the cmdlets' Get-Help content, as MAML external help.

   PowerShell derives help for a binary cmdlet by reflection over its
   attributes, but only until an external help file exists: from then on the
   file replaces what reflection would have produced, rather than adding to
   it. Everything a reader sees - the parameter sets, the aliases, the
   pipeline direction, the accepted values of an enum, the inputs and outputs
   - therefore has to be written out here, for every cmdlet, or it silently
   disappears from Get-Help.

   That is why this is long: it walks the same cmdlet families as
   gen_powershell_binding (getters, constructors, setters, adders, removers,
   the Invoke cmdlets with their dynamic parameters, the HTTP actions and the
   hand-written cmdlets) and describes the parameters each of them ends up
   with. verify-help.ps1 checks the result against the compiled module in CI. *)

open Printf
open Datamodel
open Datamodel_types
open Common_functions
open CommonFunctions
module DU = Datamodel_utils

(* The product a datamodel release name refers to: "clearwater" -> "XenServer
   6.2". Numbered releases carry their own code_name ("23.24.0"), so one lookup
   serves both. Falls back to the raw name rather than inventing one. *)
let help_release_name code =
  match List.find_opt (fun r -> r.code_name = Some code) release_order_full with
  | Some r ->
      r.branding
  | None ->
      code

(* The release a reader is most likely to be pointing the SDK at: nile, which
   release_order_full brands "XenServer 8".

   The displayed name is looked up rather than written out, the way
   get_release_branding feeds it to the C# and Java generators. A hardcoded
   product version would be the only one in any SDK generator, and it would let
   this file disagree with the [Deprecated("...")] attribute gen_csharp_binding
   emits for the very same release.

   The code name itself is a literal only because datamodel_types.mli stops
   exporting the rel_* values at rel_stockholm_psr: nile, nile-preview and
   orinoco are defined in the .ml but are not in scope here. Widening a shared
   interface is not help work, so the name is checked against
   release_order_full instead, and a rename upstream fails this build rather
   than quietly mislabelling every deprecated cmdlet. *)
let rel_current = "nile"

let () =
  if
    not
      (List.exists (fun r -> r.code_name = Some rel_current) release_order_full)
  then
    failwith
      (sprintf "gen_powershell_help: %S is not a release in release_order_full"
         rel_current
      )

let current_release_name = help_release_name rel_current

(* True for a release that shipped before the current one. compare_versions
   orders code names by their position in release_order, and falls back to an
   rpm-style comparison for the numbered releases. *)
let released_before_current code = compare_versions code rel_current < 0

(* The label and sentence marking a class, message or field a reader should
   not be reaching for.

   One SDK serves every version of the product - XenServer 9, XenServer 8 and
   hosts older than either - so a binding for something withdrawn years ago is
   not a mistake to be cleaned up: a customer on an old host still needs it.
   What was missing is any way for a reader to know. VMPP went in XenServer 6.2
   and the two metrics classes in 6.1, yet their cmdlets document themselves
   exactly like live ones, so the first sign of trouble on a current host is
   MESSAGE_REMOVED from a call the help just recommended.

   Removed before the current release and deprecated both come out as
   "Deprecated", because the distinction is one about the datamodel's history
   rather than about anything the reader can act on: either way the answer is
   do not use this. The sentence that follows the label still says which of the
   two it was, and from which release, because that is what tells you whether
   your host is old enough to have it. Something removed in a release later
   than the current one is a different case - it does still work on the current
   one - and is worded as such, though nothing in the datamodel is in that
   position today.

   No generator in the SDK acts on lifecycle, which is why this says it in the
   help rather than dropping the cmdlet. Dropping it would break the users it
   still serves. *)
let help_deprecation ~noun (lc : Lifecycle.t) =
  let at change =
    List.fold_left
      (fun acc (c, release, doc) ->
        if c = change then
          Some (release, doc)
        else
          acc
      )
      None lc.Lifecycle.transitions
  in
  let because doc =
    if doc = "" then
      ""
    else
      sprintf " (%s)" doc
  in
  match lc.Lifecycle.state with
  | Lifecycle.Removed_s ->
      let sentence =
        match at Lifecycle.Removed with
        | Some (r, d) when released_before_current r ->
            sprintf
              "This %s was removed in %s%s and is not available on %s or \
               later, where a server answers MESSAGE_REMOVED. It remains in \
               the SDK so that it can still be used against an older host."
              noun (help_release_name r) (because d) current_release_name
        | Some (r, d) ->
            sprintf
              "This %s was removed in %s%s. It still works on %s and earlier; \
               a server that has removed it answers MESSAGE_REMOVED."
              noun (help_release_name r) (because d) current_release_name
        | None ->
            sprintf
              "This %s has been removed. A server that has removed it answers \
               MESSAGE_REMOVED."
              noun
      in
      Some (sprintf "DEPRECATED. %s" sentence)
  | Lifecycle.Deprecated_s ->
      let where, why =
        match at Lifecycle.Deprecated with
        | Some (r, d) ->
            (sprintf " since %s" (help_release_name r), because d)
        | None ->
            ("", "")
      in
      Some
        (sprintf
           "DEPRECATED. This %s has been deprecated%s%s. It still works on %s, \
            but should not be used in new scripts."
           noun where why current_release_name
        )
  | _ ->
      None

(* Put a deprecation note in front of a description rather than after it. A
   reader who stops at the first sentence is exactly the reader who most needs
   to be told, and Get-Help -Full prints the whole description either way. *)
let help_note_onto description = function
  | Some note ->
      sprintf "%s\n\n%s" note description
  | None ->
      description

(* A parameter as it appears in the generated Get-Help content. [hp_sets] holds
   the names of the cmdlet parameter sets the parameter belongs to; an empty
   list means it belongs to all of them, which is how the generated cmdlets
   treat a [Parameter] attribute with no ParameterSetName. [hp_values] holds the
   permitted values of an enum parameter, so that Get-Help can list them the way
   reflection does. *)
type help_parameter = {
    hp_name: string
  ; hp_type: string
  ; hp_desc: string
  ; hp_required: bool
        (* Rendered verbatim into the MAML pipelineInput attribute, so it
           carries the direction the way reflection showed it: "false",
           "true (ByValue)" or "true (ByPropertyName)". *)
  ; hp_pipeline: string
  ; hp_position: string
  ; hp_sets: string list
  ; hp_switch: bool
  ; hp_values: string list
  ; hp_aliases: string list
  ; hp_in_syntax: bool
}

(* Which curated examples were actually attached to a cmdlet. Checked at the
   end of generation: an entry that matches nothing means the cmdlet it names
   has been renamed or withdrawn, and the example is now describing something
   that does not exist. *)
let curated_used = ref []

(* Take the first [n], which is how a family tops its examples up to three
   without inventing anything: the list is already in best-first order. *)
let help_take n l = List.filteri (fun i _ -> i < n) l

(* A variable to hold an object of a class, safe to assign in a shell.
   PowerShell reserves a handful of names - $host is read-only, and assigning
   to it stops the example dead - so anything that collides takes a prefix. *)
let help_var_for cls =
  let name = ocaml_class_to_csharp_local_var cls in
  let reserved =
    [
      "host"
    ; "error"
    ; "input"
    ; "args"
    ; "this"
    ; "true"
    ; "false"
    ; "null"
    ; "matches"
    ; "pwd"
    ; "home"
    ; "pid"
    ; "profile"
    ; "sender"
    ; "switch"
    ; "foreach"
    ; "psitem"
    ]
  in
  if List.mem (String.lowercase_ascii name) reserved then
    sprintf "$xen%s" (pascal_case name)
  else
    sprintf "$%s" name

let rec gen_help () =
  let class_commands =
    List.concat_map help_for_class (List.filter generated classes)
  in
  let commands = class_commands @ help_http_actions () @ help_handwritten () in
  let json = `O [("commands", `A commands)] in
  render_file
    ("PowerShellHelp.mustache", "XenServerPowerShell.dll-Help.xml")
    json templdir destdir ;
  (* Refuse to produce help that carries an example for a cmdlet this
     generator no longer emits: the entry in curated_examples.ml has outlived
     whatever it was describing. *)
  match
    List.filter
      (fun n -> not (List.mem n !curated_used))
      Curated_examples.cmdlet_names
  with
  | [] ->
      ()
  | orphans ->
      eprintf
        "curated_examples.ml has entries for cmdlets that are not generated: %s\n"
        (String.concat ", " orphans) ;
      exit 1

(* The parameters every HTTP-action cmdlet inherits from XenServerHttpCmdlet
   and XenServerCmdlet. External help replaces reflection wholesale, so these
   have to be restated or the cmdlets end up documented with no parameters at
   all. *)
and help_http_common_params () =
  [
    help_param ~required:true "XenHost" "string"
      "The address of the server to transfer data with."
  ; help_param ~required:true "Path" "string"
      "The path of the local file to read from or write to."
  ; help_param "CancellingDelegate" "HTTP.FuncBool"
      "A delegate polled during the transfer; returning true cancels it."
  ; help_param "TimeoutMs" "int"
      "The timeout of the HTTP request, in milliseconds."
  ; help_param "Proxy" "IWebProxy" "The web proxy to use for the request."
  ; help_param "CertificateValidationCallback"
      "RemoteCertificateValidationCallback"
      "A callback used to validate the server's TLS certificate."
  ; help_param ~switch:true "NoWarnNewCertificates" "SwitchParameter"
      "If set, no warning is issued when the server presents a certificate \
       that has not been seen before."
  ; help_param ~switch:true "NoWarnCertificates" "SwitchParameter"
      "If set, no warning is issued for certificate validation failures."
  ; help_param "TaskRef" "string"
      "The opaque reference of a task to track the transfer with."
  ]

and help_http_actions () =
  http_actions
  |> List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk)
  |> List.map (fun (name, (meth, _uri, _, args, _, _)) ->
      let verb = get_http_action_verb name meth in
      let stem = get_http_action_stem name in
      let cmdlet = sprintf "%s-Xen%s" verb stem in
      let is_put = meth == Put in
      let direction =
        if is_put then
          "Uploads data to"
        else
          "Downloads data from"
      in
      let synopsis =
        sprintf "%s the server using the '%s' HTTP interface." direction name
      in
      let description =
        sprintf
          "This cmdlet wraps the '%s' HTTP interface of the server. %s the \
           server over HTTP(S)."
          name direction
      in
      let delegate =
        if is_put then
          help_param "ProgressDelegate" "HTTP.UpdateProgressDelegate"
            "A delegate called with the progress of the upload."
        else
          help_param "DataCopiedDelegate" "HTTP.DataCopiedDelegate"
            "A delegate called with the number of bytes copied so far."
      in
      let action_args =
        List.map
          (fun a ->
            (* The template marks the uuid argument, and only that one, as
               ValueFromPipelineByPropertyName. *)
            let pipeline =
              if String.lowercase_ascii (http_arg_name a) = "uuid" then
                "true (ByPropertyName)"
              else
                "false"
            in
            help_param ~pipeline (http_arg_name a) (http_arg_type a)
              (sprintf "The '%s' query argument of the '%s' interface."
                 (http_arg_name a) name
              )
          )
          args
      in
      let uuid_arg =
        if
          List.exists
            (fun a -> String.lowercase_ascii (http_arg_name a) = "uuid")
            args
        then
          " -Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff"
        else
          ""
      in
      let path =
        if is_put then
          "C:\\upload.dat"
        else
          "C:\\download.dat"
      in
      let base =
        sprintf "PS> %s -XenHost \"myserver\" -Path \"%s\"%s" cmdlet path
          uuid_arg
      in
      let examples =
        [
          help_example
            ~title:
              ( if is_put then
                  "Upload a file to the server"
                else
                  "Download a file from the server"
              )
            base
            ( if is_put then
                "Uploads the contents of the local file to the server."
              else
                "Downloads from the server into the local file."
            )
          (* These cmdlets talk to the server over HTTP rather than through the
             API, so the things worth showing after the plain call are the ones
             that differ from every other cmdlet: the transfer's own timeout,
             and running it against a session you already hold. *)
        ; help_example ~title:"Set a timeout for the transfer"
            (sprintf "%s -TimeoutMs 600000" base)
            "Gives the transfer ten minutes. The timeout covers the HTTP \
             request, not the API call that set it up."
        ; help_example ~title:"Use an existing session"
            (sprintf "%s -SessionOpaqueRef $session.opaque_ref" base)
            "Runs against a session already opened with Connect-XenServer \
             rather than the default one, which is how a script drives more \
             than one server at a time."
        ]
      in
      help_command ~name:cmdlet ~synopsis ~description
        ~parameters:((delegate :: action_args) @ help_http_common_params ())
        ~shouldprocess:is_put ~outputs:["void"] ~examples ()
  )

(* The cmdlets that are not generated from the datamodel: hand-written C# under
   autogen/src, plus ConvertTo-XenRef.

   Their parameters are written out here by hand, and that is load-bearing
   rather than lazy. MAML external help REPLACES the reflected help for a
   cmdlet it names - it does not add to it - so an entry that lists nine of a
   cmdlet's ten parameters does not leave the tenth undocumented, it removes
   the tenth from Get-Help entirely.

   These lists were taken from the [Parameter] attributes in autogen/src, and
   verify-help.ps1 is what keeps them honest: it loads the built module and
   compares every documented parameter against the compiled cmdlet - names,
   aliases, pipeline binding, enum values - and reports any parameter the
   assembly has that the help does not. That check cannot live in
   test_gen_powershell, which never sees a compiled cmdlet; it runs against the
   module, which is the only place the reflected metadata exists.

   They were previously left out of the help file altogether, for exactly that
   risk. The cost of leaving them out was worse: Connect-XenServer is the first
   command anybody runs, and it was the one cmdlet in the module with no
   examples at all. *)
and help_handwritten () =
  [
    help_command ~name:"Connect-XenServer"
      ~synopsis:"Connects to a XenServer host or pool."
      ~description:
        "Opens a session with a XenServer host and adds it to the set of \
         connections the other cmdlets can use.\n\
         Every other cmdlet needs a session. Use -SetDefaultSession so the \
         rest of the module picks this connection up implicitly; otherwise \
         pass the session to each cmdlet with -SessionOpaqueRef.\n\
         Only the pool coordinator accepts logins. Connecting to any other \
         member of a pool fails with HOST_IS_SLAVE, and the coordinator's \
         address is the second element of the error's ErrorDescription, so a \
         script that may be pointed at any host in a pool has to catch that \
         and connect again. The last example does this."
      ~common:false
      ~parameters:
        [
          help_param ~required:true ~position:"0" ~sets:["Url"] "Url" "string[]"
            "The URL of the server to connect to, for example \
             https://myserver. More than one may be given, and a session is \
             opened for each."
        ; help_param ~required:true ~position:"0" ~sets:["ServerPort"]
            ~aliases:["svr"] "Server" "string[]"
            "The address of the server to connect to, as an alternative to \
             -Url. More than one may be given."
        ; help_param ~sets:["ServerPort"] "Port" "int"
            "The port to connect on. Defaults to 443."
        ; help_param ~pipeline:"true (ByValue, ByPropertyName)"
            ~aliases:["cred"] "Creds" "PSCredential"
            "The credentials to log in with, as returned by Get-Credential. \
             Use this in preference to -UserName and -Password, which put the \
             password in the command line and so into the session history."
        ; help_param ~position:"1" ~aliases:["user"] "UserName" "string"
            "The user to log in as."
        ; help_param ~position:"2" ~aliases:["pwd"] "Password" "string"
            "The password to log in with."
        ; help_param "OpaqueRef" "string[]"
            "The reference of a session that is already open on the server, to \
             adopt instead of logging in again."
        ; help_param "Originator" "string"
            "The name this client reports to the server, which appears in the \
             server's logs and audit trail."
        ; help_param "UserAgent" "string"
            "The user agent to send with requests to the server."
        ; help_param ~switch:true "PassThru" "SwitchParameter"
            "If set, the cmdlet returns the session it opened. By default the \
             cmdlet does not generate any output."
        ; help_param ~switch:true "NoWarnNewCertificates" "SwitchParameter"
            "If set, no warning is issued when the server presents a \
             certificate that has not been seen before."
        ; help_param ~switch:true "NoWarnCertificates" "SwitchParameter"
            "If set, no warning is issued for certificate validation failures."
        ; help_param ~switch:true "SetDefaultSession" "SwitchParameter"
            "If set, this session becomes the one the other cmdlets use when \
             none is given explicitly."
        ; help_param ~switch:true "Force" "SwitchParameter"
            "If set, the cmdlet connects without prompting for confirmation."
        ]
      ~outputs:["Session"]
      ~examples:
        [
          help_example ~title:"Connect and make it the default session"
            "PS> $creds = Get-Credential\n\
             PS> Connect-XenServer -Url https://myserver -Creds $creds \
             -SetDefaultSession"
            "Opens a session and makes it the one every other cmdlet uses, so \
             nothing else has to name it. Get-Credential prompts without \
             echoing, which keeps the password out of the command line and out \
             of the session history."
        ; help_example ~title:"Connect as a domain user"
            "PS> $creds = Get-Credential MYDOMAIN\\alice\n\
             PS> Connect-XenServer -Url https://myserver -Creds $creds \
             -SetDefaultSession"
            "Prefer an Active Directory account to the local root account. \
             root is the pool's superuser and bypasses role-based access \
             control, so what it does is neither restricted nor attributable \
             to a person; a domain account gets the roles it has been granted \
             and appears as itself in the audit log. Either MYDOMAIN\\alice or \
             alice@mydomain.com is accepted."
        ; help_example ~title:"Connect from a script"
            "PS> $secret = Read-Host -AsSecureString \"Password\"\n\
             PS> $creds = New-Object \
             System.Management.Automation.PSCredential(\"MYDOMAIN\\alice\", \
             $secret)\n\
             PS> Connect-XenServer -Url https://myserver -Creds $creds \
             -SetDefaultSession"
            "Where a script cannot prompt, build the credential itself. \
             -UserName and -Password are accepted too, but they put the \
             password in the command line."
        ; help_example ~title:"Keep the session to pass explicitly"
            "PS> $creds = Get-Credential\n\
             PS> $session = Connect-XenServer -Url https://myserver -Creds \
             $creds -PassThru\n\
             PS> Get-XenVM -SessionOpaqueRef $session.opaque_ref"
            "-PassThru returns the session. Naming it on each cmdlet is how to \
             work with more than one server at a time."
        ; help_example ~title:"Connect to several servers at once"
            "PS> $creds = Get-Credential\n\
             PS> Connect-XenServer -Url https://server1, https://server2 \
             -Creds $creds"
            "A session is opened for each, and Get-XenSession lists them."
        ; help_example
            ~title:"Connect without being asked about the certificate"
            "PS> $creds = Get-Credential MYDOMAIN\\alice\n\
             PS> Connect-XenServer -Url https://myserver -Creds $creds \
             -NoWarnCertificates -NoWarnNewCertificates -SetDefaultSession"
            "A server presenting a certificate this client has not seen \
             before, or one that does not validate, prompts for confirmation - \
             which stalls an unattended script. These two switches answer the \
             prompts instead. They suppress the warning rather than fix the \
             cause, so a script that always sets them will not notice the day \
             the certificate really is wrong."
        ; help_example
            ~title:
              "Connect to a pool without knowing which host is the coordinator"
            "PS> $creds = Get-Credential MYDOMAIN\\alice\n\
             PS> try {\n\
            \      Connect-XenServer -Url https://myserver -Creds $creds \
             -SetDefaultSession -PassThru\n\
            \  }\n\
            \  catch {\n\
            \      $desc = $_.Exception.ErrorDescription\n\
            \      if ($desc[0] -ne \"HOST_IS_SLAVE\") { throw }\n\
            \      # $desc[1] is the coordinator's address\n\
            \      Connect-XenServer -Url \"https://$($desc[1])\" -Creds \
             $creds -SetDefaultSession -PassThru\n\
            \  }"
            "A pool member refuses the login rather than forwarding it, so a \
             script given the address of whichever host answers has to retry \
             against the coordinator itself. ErrorDescription[0] is the error \
             code and ErrorDescription[1] is the coordinator's address. \
             Re-throwing anything else matters: without that test this \
             swallows a wrong password as readily as a wrong host. Nothing \
             here is specific to certificates - add the switches from the \
             previous example only if the prompts are also in the way."
        ]
      ()
  ; help_command ~name:"Disconnect-XenServer"
      ~synopsis:"Closes a session with a XenServer host."
      ~description:
        "Logs out of a XenServer session and removes it from the set of \
         connections the other cmdlets can use.\n\
         Sessions do not last forever, but they do not expire the moment a \
         script ends either, so a long-running script that connects repeatedly \
         should disconnect as well."
      ~common:false
      ~parameters:
        [
          help_param ~position:"0" ~pipeline:"true (ByValue)"
            ~sets:["XenObject"] "Session" "Session"
            "The session to close, as returned by Get-XenSession or by \
             Connect-XenServer -PassThru."
        ; help_param ~position:"0" ~pipeline:"true (ByPropertyName)"
            ~sets:["Ref"] ~aliases:["opaque_ref"] "Ref" "XenRef[Session]"
            "The opaque reference of the session to close."
        ]
      ~examples:
        [
          help_example ~title:"Disconnect every open session"
            "PS> Get-XenSession | Disconnect-XenServer"
            "Closes all the sessions this module holds, which is the usual way \
             to tidy up at the end of a script."
        ; help_example ~title:"Disconnect one server"
            "PS> Get-XenSession -Url https://myserver | Disconnect-XenServer"
            "Leaves any other connections open."
        ; help_example ~title:"Disconnect a session held in a variable"
            "PS> $session = Connect-XenServer -Url https://myserver -Creds \
             (Get-Credential) -PassThru\n\
             PS> Disconnect-XenServer -Session $session"
            "The session returned by -PassThru can be closed directly."
        ]
      ()
  ; help_command ~name:"Get-XenSession"
      ~synopsis:"Gets the sessions this module has open."
      ~description:
        "Returns the XenServer sessions currently open, optionally narrowed to \
         one server or user.\n\
         This reports what this PowerShell session holds; it does not ask a \
         server which sessions exist on it."
      ~common:false
      ~parameters:
        [
          help_param ~pipeline:"true (ByPropertyName)" ~sets:["Ref"]
            ~aliases:["opaque_ref"] "Ref" "XenRef[Session]"
            "The opaque reference of the session to return."
        ; help_param ~required:true ~sets:["Url"] "Url" "string"
            "Returns the sessions open with the server at this URL."
        ; help_param ~required:true ~sets:["ServerPort"] ~aliases:["svr"]
            "Server" "string"
            "Returns the sessions open with the server at this address."
        ; help_param ~sets:["ServerPort"] "Port" "int"
            "The port the server was connected on. Defaults to 443."
        ; help_param ~required:true ~sets:["UserName"] "UserName" "string"
            "Returns the sessions opened by this user."
        ]
        (* Session[], not Session: the cmdlet declares an array OutputType, as
           every Get-Xen<Class> does, and verify-help.ps1 compares the two. *)
      ~outputs:["Session[]"]
      ~examples:
        [
          help_example ~title:"List every open session" "PS> Get-XenSession"
            "Shows what this PowerShell session is connected to."
        ; help_example ~title:"Find the session for one server"
            "PS> Get-XenSession -Url https://myserver"
            "Useful when several servers are connected at once and a cmdlet \
             needs to be told which to act on."
        ; help_example ~title:"Target a cmdlet at one connection"
            "PS> $session = Get-XenSession -Url https://myserver\n\
             PS> Get-XenVM -SessionOpaqueRef $session.opaque_ref"
            "Every cmdlet takes -SessionOpaqueRef, which overrides the default \
             session for that one call."
        ]
      ()
  ; help_command ~name:"Wait-XenTask"
      ~synopsis:"Waits for an asynchronous XenServer task to finish."
      ~description:
        "Blocks until the given task completes, then returns its result.\n\
         Cmdlets run with -Async return a task straight away instead of \
         waiting. Piping that task into this cmdlet is how to wait for the \
         operation and collect what it produced; -ShowProgress draws a \
         progress bar while it runs.\n\
         If the task fails, this cmdlet raises the error the task carries."
      ~parameters:
        [
          help_param ~switch:true "PassThru" "SwitchParameter"
            "If set, the cmdlet returns the task's result. By default the \
             cmdlet does not generate any output."
        ; help_param ~required:true ~position:"0" ~pipeline:"true (ByValue)"
            ~sets:["XenObject"] "Task" "Task" "The task to wait for."
        ; help_param ~required:true ~position:"0"
            ~pipeline:"true (ByPropertyName)" ~sets:["Ref"]
            ~aliases:["opaque_ref"] "Ref" "XenRef[Task]"
            "The opaque reference of the task to wait for."
        ; help_param ~required:true ~position:"0"
            ~pipeline:"true (ByPropertyName)" ~sets:["Uuid"] "Uuid" "Guid"
            "The uuid of the task to wait for."
        ; help_param ~required:true ~position:"0"
            ~pipeline:"true (ByPropertyName)" ~sets:["Name"]
            ~aliases:["name_label"] "Name" "string"
            "The name of the task to wait for."
        ; help_param ~switch:true "ShowProgress" "SwitchParameter"
            "If set, a progress bar is shown while the task runs."
        ; help_param "Min" "int"
            "The percentage the progress bar starts at, for a task that is one \
             step of a longer operation."
        ; help_param "Max" "int"
            "The percentage the progress bar finishes at, for a task that is \
             one step of a longer operation."
        ]
      ~examples:
        [
          help_example ~title:"Wait for an asynchronous operation"
            "PS> Get-XenVM -Name \"Demo VM\" | Invoke-XenVM -XenAction Start \
             -Async -PassThru | Wait-XenTask -ShowProgress"
            "This is the idiom for any long-running operation: -Async returns \
             a task, and Wait-XenTask follows it to completion."
        ; help_example ~title:"Collect what the task produced"
            "PS> $task = Get-XenVM -Name \"Demo VM\" | Invoke-XenVM -XenAction \
             Snapshot -Async -PassThru\n\
             PS> $snapshot = $task | Wait-XenTask -PassThru"
            "An operation that creates something returns it through the task, \
             so -PassThru is what hands back the new object."
        ; help_example ~title:"Wait for a task by reference"
            "PS> Wait-XenTask -Ref \
             OpaqueRef:f433bf7b-2b0c-5f53-7018-7d195addb3ca -ShowProgress"
            "A task can be waited for by reference, uuid or name as well as by \
             piping the object in."
        ]
      ()
  ; (* Neither this nor Send-XenOemPatchStream is reachable from the datamodel,
       so neither carries lifecycle for help_deprecation to read: both are
       hand-written cmdlets over HTTP actions that the C# SDK hardcodes in
       HTTP_actions.mustache, outside the {{#http_actions}} loop. The server
       stopped serving them long ago - /oem_patch_stream when its handler was
       deleted in 2014 (CP-9401) and /pool_patch_download when the datamodel
       definitions for both went in 2020 - so the note has to be written out
       here or these two are the only dead cmdlets in the module that document
       themselves as working. *)
    help_command ~name:"Receive-XenPoolPatch" ~deprecated:true
      ~synopsis:"Downloads a pool patch from a XenServer host."
      ~description:
        "DEPRECATED. The server no longer serves /pool_patch_download; its \
         definition was removed from the datamodel in 2020 and a current \
         server answers 404. The cmdlet remains in the SDK so that it can \
         still be used against an older host.\n\n\
         Downloads the given pool patch from the server and writes it to a \
         local file."
      ~parameters:
        ([
           help_param "DataCopiedDelegate" "HTTP.DataCopiedDelegate"
             "A delegate called as data arrives, for reporting progress."
         ; help_param ~pipeline:"true (ByPropertyName)" "Uuid" "string"
             "The uuid of the pool patch to download."
         ]
        @ help_http_common_params ()
        )
      ~outputs:["void"]
      ~examples:
        [
          help_example ~title:"Download a patch"
            "PS> Receive-XenPoolPatch -XenHost \"myserver\" -Path \
             \"C:\\download.dat\" -Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff"
            "Writes the patch into the local file."
        ; help_example ~title:"Set a timeout for the transfer"
            "PS> Receive-XenPoolPatch -XenHost \"myserver\" -Path \
             \"C:\\download.dat\" -Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff \
             -TimeoutMs 600000"
            "Gives the transfer ten minutes. The timeout covers the HTTP \
             request, not the API call that set it up."
        ; help_example ~title:"Use an existing session"
            "PS> Receive-XenPoolPatch -XenHost \"myserver\" -Path \
             \"C:\\download.dat\" -Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff \
             -SessionOpaqueRef $session.opaque_ref"
            "Runs against a session already opened with Connect-XenServer \
             rather than the default one."
        ]
      ()
  ; help_command ~name:"Send-XenOemPatchStream" ~deprecated:true
      ~synopsis:"Uploads an OEM patch stream to a XenServer host."
      ~description:
        "DEPRECATED. The server no longer serves /oem_patch_stream; its \
         handler was deleted in 2014 and a current server answers 404. The \
         cmdlet remains in the SDK so that it can still be used against an \
         older host.\n\n\
         Streams the given local file to the server as an OEM patch."
      ~shouldprocess:true
      ~parameters:
        ([
           help_param "ProgressDelegate" "HTTP.UpdateProgressDelegate"
             "A delegate called as the upload proceeds, for reporting progress."
         ]
        @ help_http_common_params ()
        )
      ~outputs:["void"]
      ~examples:
        [
          help_example ~title:"Upload a patch stream"
            "PS> Send-XenOemPatchStream -XenHost \"myserver\" -Path \
             \"C:\\upload.dat\""
            "Streams the local file to the server."
        ; help_example ~title:"See what would happen first"
            "PS> Send-XenOemPatchStream -XenHost \"myserver\" -Path \
             \"C:\\upload.dat\" -WhatIf"
            "-WhatIf reports the upload without performing it."
        ; help_example ~title:"Set a timeout for the transfer"
            "PS> Send-XenOemPatchStream -XenHost \"myserver\" -Path \
             \"C:\\upload.dat\" -TimeoutMs 600000"
            "Gives the transfer ten minutes."
        ]
      ()
  ; help_command ~name:"ConvertTo-XenRef"
      ~synopsis:"Converts a XenServer object to an object reference."
      ~description:
        "Converts a XenServer object into the corresponding opaque reference \
         (XenRef) that can be passed to other cmdlets."
      ~common:false
      ~parameters:
        [
          help_param ~required:true ~pipeline:"true (ByValue)" ~position:"0"
            "XenObject" "IXenObject"
            "The XenServer object to convert into an opaque reference."
        ]
      ~outputs:["IXenObject"]
      ~examples:
        [
          help_example ~title:"Convert an object to a reference"
            "PS> Get-XenVM -Name \"Demo VM\" | ConvertTo-XenRef"
            "Converts a VM object into the XenRef that other cmdlets accept."
        ; help_example ~title:"Convert several objects at once"
            "PS> Get-XenVM | ConvertTo-XenRef"
            "The whole collection is converted, one reference out for each \
             object in."
        ; help_example ~title:"Keep a reference for later"
            "PS> $ref = Get-XenVM -Name \"Demo VM\" | ConvertTo-XenRef\n\
             PS> Invoke-XenVM -Ref $ref -XenAction Start"
            "A reference stays valid while the object does, so it can be held \
             and passed to the cmdlets that take -Ref."
        ]
      ()
  ]

and help_paras text =
  let paras =
    String.split_on_char '\n' text
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let paras = match paras with [] -> [text] | _ -> paras in
  `A (List.map (fun p -> `O [("para", `String (escape_xml p))]) paras)

and help_param ?(required = false) ?(pipeline = "false") ?(position = "Named")
    ?(sets = []) ?(switch = false) ?(values = []) ?(aliases = [])
    ?(in_syntax = true) name typ desc =
  {
    hp_name= name
  ; hp_type= typ
  ; hp_desc=
      ( if desc = "" then
          "This parameter has no documentation."
        else
          desc
      )
  ; hp_required= required
  ; hp_pipeline= pipeline
  ; hp_position= position
  ; hp_sets= sets
  ; hp_switch= switch
  ; hp_values= values
  ; hp_aliases= aliases
  ; hp_in_syntax= in_syntax
  }

(* obj_internal_type and friends produce C# type names: namespace-qualified,
   with angle brackets for generics. Get-Help is a PowerShell surface, and
   reflection rendered the same types the PowerShell way, so present them that
   way here - XenRef<XenAPI.VM> becomes XenRef[VM]. Square brackets also spare
   the help file a pair of XML entities for every type it mentions. *)
and help_type_name t =
  let replace ~sub ~by s =
    let n = String.length sub and len = String.length s in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      if !i + n <= len && String.sub s !i n = sub then (
        Buffer.add_string buf by ;
        i := !i + n
      ) else (
        Buffer.add_char buf s.[!i] ;
        incr i
      )
    done ;
    Buffer.contents buf
  in
  t
  |> replace ~sub:"XenAPI." ~by:""
  (* PowerShell shows this one through its type accelerator, and so does the
     rest of the generated help. *)
  |> replace ~sub:"Hashtable" ~by:"hashtable"
  (* "bool?" is how C# spells Nullable<bool>; PowerShell renders the same
     parameter as "bool", and the brackets around an optional parameter
     already say that it can be left out. *)
  |> replace ~sub:"?" ~by:""
  |> String.map (function '<' -> '[' | '>' -> ']' | c -> c)

and help_param_json p =
  `O
    [
      ("name", `String (escape_xml p.hp_name))
    ; ("type", `String (escape_xml (help_type_name p.hp_type)))
    ; ("description", `String (escape_xml p.hp_desc))
    ; ( "required"
      , `String
          ( if p.hp_required then
              "true"
            else
              "false"
          )
      )
    ; ("pipeline", `String (escape_xml p.hp_pipeline))
    ; ("position", `String p.hp_position)
    ; ("switch", `Bool p.hp_switch)
    ; ("aliases", `String (escape_xml (String.concat "," p.hp_aliases)))
      (* Get-Help always prints a "Default value" row for external help, so
         give it something truthful rather than leaving it blank. Omitting an
         optional parameter on these cmdlets leaves the field alone rather than
         sending a zero, so "None" is the honest answer for everything except a
         switch, which really does default to False. *)
    ; ( "default_value"
      , `String
          ( if p.hp_switch then
              "False"
            else
              "None"
          )
      )
    ; ("has_values", `Bool (p.hp_values <> []))
    ; ( "values"
      , `A
          (List.map
             (fun v -> `O [("value", `String (escape_xml v))])
             p.hp_values
          )
      )
    ]

(* One syntaxItem per cmdlet parameter set, so that Get-Help shows the mutually
   exclusive ways of calling the cmdlet and marks the right parameters
   mandatory in each. A parameter that names no set belongs to all of them. *)
and help_syntax_items ~cmdlet all_parameters =
  let parameters = List.filter (fun p -> p.hp_in_syntax) all_parameters in
  let set_names =
    List.concat_map (fun p -> p.hp_sets) parameters
    |> List.fold_left
         (fun acc s ->
           if List.mem s acc then
             acc
           else
             acc @ [s]
         )
         []
  in
  (* Within a syntax item PowerShell lists the positional parameters first,
     then the mandatory named ones, then the optional named ones, keeping
     declaration order inside each group. Reproduce that, so the syntax reads
     the way it did when it was derived by reflection. *)
  let order params =
    let positional p = p.hp_position <> "Named" in
    List.filter positional params
    @ List.filter (fun p -> (not (positional p)) && p.hp_required) params
    @ List.filter (fun p -> (not (positional p)) && not p.hp_required) params
  in
  let item set_name params =
    `O
      [
        ("cmdlet", `String (escape_xml cmdlet))
      ; ("set_name", `String (escape_xml set_name))
      ; ("parameters", `A (List.map help_param_json (order params)))
      ]
  in
  match set_names with
  | [] ->
      [item "__AllParameterSets" parameters]
  | _ ->
      List.map
        (fun s ->
          item s
            (List.filter
               (fun p -> p.hp_sets = [] || List.mem s p.hp_sets)
               parameters
            )
        )
        set_names

(* Listed in the order XenServerCmdlet declares them, which is the order
   reflection used to present them in. *)
and help_common_params () =
  [
    help_param ~switch:true "BestEffort" "SwitchParameter"
      "If set, the cmdlet continues processing further objects instead of \
       throwing a terminating error when an operation fails for one object."
  ; help_param "SessionOpaqueRef" "string"
      "The session object on which to run the cmdlet. This overrides the \
       default session and lets you target a specific open XenServer \
       connection."
  ]

and help_passthru () =
  help_param ~switch:true "PassThru" "SwitchParameter"
    "If set, the cmdlet returns the affected object. By default the cmdlet \
     does not generate any output."

and help_async () =
  help_param ~switch:true "Async" "SwitchParameter"
    "If set, the operation runs asynchronously and returns a Task object that \
     can be used to track its progress."

(* On the Set/Add/Remove-Property cmdlets -Async is generated behind
   GenerateAsyncParam, so it only appears once a field whose operation can run
   asynchronously has been chosen. Document it, but keep it out of the syntax
   where it would read as unconditionally available. *)
and help_async_for_fields () =
  help_param ~switch:true ~in_syntax:false "Async" "SwitchParameter"
    "If set, the operation runs asynchronously and returns a Task object that \
     can be used to track its progress. Available only for those fields whose \
     underlying operation supports asynchronous invocation."

(* Cmdlets declared with SupportsShouldProcess gain -WhatIf and -Confirm.
   Reflection used to add them; external help has to declare them or they
   disappear from the syntax. *)
and help_shouldprocess_params () =
  [
    help_param ~switch:true ~aliases:["wi"] "WhatIf" "SwitchParameter"
      "Shows what would happen if the cmdlet runs. The cmdlet is not run."
  ; help_param ~switch:true ~aliases:["cf"] "Confirm" "SwitchParameter"
      "Prompts for confirmation before running the cmdlet."
  ]

(* The generated C# enums take their members from the datamodel and add an
   'unknown' member. Listing them lets Get-Help render an enum parameter as
   "{Halted | Paused | ...}" rather than as a bare type name.

   The members are the wire names with '-' replaced by '_', the same mapping
   gen_csharp_binding's enum_of_wire applies. Listing the wire names verbatim
   would advertise values the cmdlet rejects: bond_mode would read
   "balance-slb" where the accepted value is "balance_slb". *)
and enum_values_of_ty ty =
  let enum_of_wire = String.map (function '-' -> '_' | c -> c) in
  match ty with
  | Enum (_, vs) | Set (Enum (_, vs)) | Map (Enum (_, vs), _) ->
      List.map (fun (v, _) -> enum_of_wire v) vs @ ["unknown"]
  | _ ->
      []

(* Examples are numbered by help_command once the generated and the curated
   ones have been put together, so nothing here has to know its position.
   [title] says what the example shows; Get-Help puts it in the heading, the
   way a reader coming from any other module expects. *)
and help_example ?(title = "") code remarks = (title, code, remarks)

and help_example_json n (title, code, remarks) =
  let heading =
    if title = "" then
      sprintf "Example %d" n
    else
      sprintf "Example %d: %s" n title
  in
  (* Pad to a fixed total rather than a fixed number of dashes. Get-Help indents
     the heading by four and wraps at the console width, so a fixed count makes
     a titled heading spill onto a second line in an 80-column console and stop
     looking like a separator. Padding to a constant also lines the separators
     up with each other whatever the titles are. *)
  let width = 74 in
  let dashes = max 6 (width - String.length heading - 2) in
  let left = dashes / 2 in
  `O
    [
      ( "title"
      , `String
          (escape_xml
             (sprintf "%s %s %s" (String.make left '-') heading
                (String.make (dashes - left) '-')
             )
          )
      )
    ; ("code", `String (escape_xml code))
    ; ("remarks", `String (escape_xml remarks))
    ]

(* How about_XenServer.help.txt tells the reader to name the object they want:
   by name where the class has one, otherwise by uuid, otherwise by reference.
   The placeholder values match the ones used in that topic. *)
and help_selector ?(include_uuid_name = true) obj classname =
  let stem = ocaml_class_to_csharp_class classname in
  if include_uuid_name && has_name obj then
    sprintf "-Name \"Demo %s\"" stem
  else if include_uuid_name && has_uuid obj then
    "-Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff"
  else
    "-Ref OpaqueRef:f433bf7b-2b0c-5f53-7018-7d195addb3ca"

(* Only the classes with a get_all_records message get a Get-Xen<Class> cmdlet,
   so the rest cannot be shown piping from one. *)
and help_getter_call obj classname =
  if List.mem classname classes_with_records then
    let stem = ocaml_class_to_csharp_class classname in
    Some (sprintf "Get-Xen%s %s" stem (help_selector obj classname))
  else
    None

(* An example line for a cmdlet that operates on an existing object: piped from
   the getter where the class has one, naming the object directly where it does
   not. *)
and help_operate_on ?(include_uuid_name = true) obj classname rest =
  match help_getter_call obj classname with
  | Some g ->
      sprintf "PS> %s | %s" g rest
  | None ->
      sprintf "PS> %s %s" rest (help_selector ~include_uuid_name obj classname)

(* Every distinct way to name an object, best first: piped from the getter,
   then by each field the class actually has. A cmdlet that has nothing else to
   vary gets its further examples from this, which is worth a reader's time -
   which of -Ref, -Uuid, -Name and the object itself a cmdlet takes, and that
   they are separate parameter sets, is a real source of confusion. *)
and help_ways_to_name ?(include_uuid_name = true) obj classname rest =
  let stem = ocaml_class_to_csharp_class classname in
  let has_name obj = include_uuid_name && has_name obj in
  let has_uuid obj = include_uuid_name && has_uuid obj in
  let piped =
    match help_getter_call obj classname with
    | Some g ->
        [
          ( sprintf "PS> %s | %s" g rest
          , "piped from the getter"
          , "The object is piped in, which is the usual way once you already \
             have it."
          )
        ]
    | None ->
        []
  in
  let named =
    if has_name obj then
      [
        ( sprintf "PS> %s -Name \"Demo %s\"" rest stem
        , "by name"
        , "-Name matches on name_label, which is not unique, so every match is \
           operated on."
        )
      ]
    else
      []
  in
  let by_uuid =
    if has_uuid obj then
      [
        ( sprintf "PS> %s -Uuid 1871ac51-ce6b-efc3-7fd0-28bc65aa39ff" rest
        , "by uuid"
        , "-Uuid is the way to be sure of exactly one object."
        )
      ]
    else
      []
  in
  let by_ref =
    [
      ( sprintf "PS> %s -Ref OpaqueRef:f433bf7b-2b0c-5f53-7018-7d195addb3ca" rest
      , "by reference"
      , "-Ref takes the opaque reference, which is what the API itself uses \
         and what the records hold."
      )
    ]
  in
  piped @ named @ by_uuid @ by_ref

(* The value to pass for a message's parameter, and the line that obtains it.
   A reference is the case worth the trouble: "$value" tells the reader
   nothing, where naming the cmdlet that returns one tells them everything.
   Selecting the first of the collection keeps the line valid for every class,
   including those with neither a name_label nor a uuid. *)
(* Every parameter of a message, as lines that obtain the values and the
   argument list that passes them.

   A constructor whose message takes parameters rather than a record cannot be
   called with a partial set: Bond.create wants a network and its members, and
   omitting them does not fail politely. So an example for one has to supply
   all of them, which means building the whole argument list rather than
   picking a representative parameter. *)
and help_all_arguments classname m =
  let params = List.filter (fun p -> not (is_class p classname)) m.msg_params in
  let setups, args =
    List.fold_left
      (fun (setups, args) p ->
        let name = ocaml_class_to_csharp_property p.param_name in
        match p.param_type with
        | Ref cls ->
            let var = help_var_for cls in
            ( setups
              @ [
                  sprintf "PS> %s = Get-Xen%s | Select-Object -First 1" var
                    (ocaml_class_to_csharp_class cls)
                ]
            , args @ [sprintf "-%s %s" name var]
            )
        | Set (Ref cls) ->
            let var = help_var_for cls in
            ( setups
              @ [
                  sprintf "PS> %s = Get-Xen%s | Select-Object -First 1" var
                    (ocaml_class_to_csharp_class cls)
                ]
            , args @ [sprintf "-%s %s" name var]
            )
        | Enum _ as ty -> (
          match enum_values_of_ty ty with
          | v :: _ ->
              (setups, args @ [sprintf "-%s %s" name v])
          | [] ->
              (setups, args @ [sprintf "-%s %s" name "$value"])
        )
        | ty ->
            ( setups
            , args
              @ [
                  sprintf "-%s %s" name
                    (help_value_placeholder ~verb:"New" (obj_internal_type ty))
                ]
            )
      )
      ([], []) params
  in
  (* Dedupe the setup lines: two parameters of the same class share a variable. *)
  let setups =
    List.fold_left
      (fun acc s ->
        if List.mem s acc then
          acc
        else
          acc @ [s]
      )
      [] setups
  in
  (setups, String.concat " " args, List.map (fun p -> p.param_name) params)

and help_message_argument ?(verb = "Set") classname m =
  let typ = get_message_type m classname verb in
  let from_type () = (None, help_value_placeholder ~verb typ) in
  match List.find_opt (fun p -> not (is_class p classname)) m.msg_params with
  | Some {param_type= Ref cls; _} ->
      let var = help_var_for cls in
      ( Some
          (sprintf "PS> %s = Get-Xen%s | Select-Object -First 1" var
             (ocaml_class_to_csharp_class cls)
          )
      , var
      )
  | Some {param_type= Enum _ as ty; _} -> (
    (* One of the values the parameter accepts reads better than a variable,
       and Get-Help lists the rest just above. *)
    match enum_values_of_ty ty with
    | v :: _ ->
        (None, v)
    | [] ->
        from_type ()
  )
  | _ when String.starts_with ~prefix:"KeyValuePair" typ ->
      (* Adding to a map field takes one entry of it, and a hashtable does not
         bind to a KeyValuePair parameter: the reader has to construct one.
         Build it on its own line - inline it is too long to read, and the
         command is the part worth looking at. *)
      let accelerator =
        typ
        |> String.map (function '<' -> '[' | '>' -> ']' | c -> c)
        |> String.split_on_char ' '
        |> String.concat ""
      in
      ( Some
          (sprintf
             "PS> $entry = [System.Collections.Generic.%s]::new(\"region\", \
              \"emea\")"
             accelerator
          )
      , "$entry"
      )
  | _ ->
      from_type ()

and help_value_placeholder ?(verb = "Set") typ =
  let str =
    if verb = "Set" then
      "\"new value\""
    else
      (* Add and Remove take an element of the field, not a replacement. *)
      "\"value\""
  in
  match typ with
  | "string" ->
      str
  | "bool" ->
      "$true"
  | "long" | "int" ->
      "0"
  | "double" ->
      "0.0"
  | "string[]" ->
      "\"tag1\""
  | "Hashtable" | "hashtable" ->
      (* Setting a map field replaces the whole map, so the value is a
         hashtable rather than the single entry an Add takes. *)
      "@{ \"key\" = \"value\" }"
  | _ ->
      "$value"

and help_command ~name ~synopsis ~description ?(parameters = [])
    ?(common = true) ?(shouldprocess = false) ?async ?(outputs = [])
    ?(examples = []) ?(deprecated = false) () =
  (* The synopsis is the one line that follows a cmdlet everywhere: Get-Command,
     Get-Help with a wildcard, the module's own listing. A note in the
     description is only read by someone who already opened the help, which is
     not where a reader decides to use a cmdlet. *)
  let synopsis =
    if deprecated then
      "[Deprecated] " ^ synopsis
    else
      synopsis
  in
  let verb, noun =
    match String.index_opt name '-' with
    | Some i ->
        ( String.sub name 0 i
        , String.sub name (i + 1) (String.length name - i - 1)
        )
    | None ->
        (name, "")
  in
  (* Order matters only for how Get-Help prints the syntax, but matching what
     reflection produced keeps the two comparable: the cmdlet's own parameters,
     then the ones inherited from XenServerCmdlet, then the pair that
     SupportsShouldProcess adds, and last -Async, which is generated behind
     GenerateAsyncParam and so is a dynamic parameter. *)
  let parameters =
    parameters
    @ ( if common then
          help_common_params ()
        else
          []
      )
    @ ( if shouldprocess then
          help_shouldprocess_params ()
        else
          []
      )
    @ match async with Some p -> [p] | None -> []
  in
  (* Reflection derived INPUTS from the parameters that accept pipeline input;
     take them from the same place rather than restating them, so the two
     cannot drift apart. *)
  (* Where a cmdlet has curated examples, lead with the plain generated form
     and let the curated ones carry the rest, topping back up from the
     remaining generated ones only if that leaves fewer than three. A curated
     example says more than a generated one, but a cmdlet should not end up
     with fewer examples for having been given better ones. *)
  let curated = Curated_examples.for_cmdlet name in
  let all_examples =
    if curated = [] then
      examples
    else (
      curated_used := name :: !curated_used ;
      let first, rest =
        match examples with e :: r -> ([e], r) | [] -> ([], [])
      in
      let kept = first @ curated in
      kept @ help_take (3 - List.length kept) rest
    )
  in
  (* A class with neither a name_label nor a uuid, or with a single field to
     operate on, can run out of things to vary. Fall back to the session
     parameter: every cmdlet takes it, and driving more than one server from
     the same shell is worth knowing about. *)
  let all_examples =
    match all_examples with
    | (_, code, _) :: _ when List.length all_examples < 3 ->
        all_examples
        @ help_take
            (3 - List.length all_examples)
            [
              help_example ~title:"Run against a particular session"
                (code ^ " -SessionOpaqueRef $session.opaque_ref")
                "Every cmdlet takes -SessionOpaqueRef, which runs it against a \
                 session opened with Connect-XenServer rather than the default \
                 one. It is how a script drives more than one server at a \
                 time."
            ; help_example ~title:"Carry on past a failure"
                (code ^ " -BestEffort")
                "Every cmdlet takes -BestEffort, which reports a failure as a \
                 non-terminating error and moves to the next object rather \
                 than stopping the pipeline."
            ]
    | _ ->
        all_examples
  in
  let inputs =
    parameters
    |> List.filter (fun p -> p.hp_pipeline <> "false")
    |> List.map (fun p -> help_type_name p.hp_type)
    |> List.fold_left
         (fun acc t ->
           if List.mem t acc then
             acc
           else
             acc @ [t]
         )
         []
  in
  (* Reflection printed "None" for a cmdlet that takes nothing from the
     pipeline; without an inputTypes element the section renders blank. *)
  let inputs = match inputs with [] -> ["None"] | l -> l in
  `O
    [
      ("name", `String (escape_xml name))
    ; ("verb", `String (escape_xml verb))
    ; ("noun", `String (escape_xml noun))
    ; ("synopsis", `String (escape_xml synopsis))
    ; ("description", help_paras description)
    ; ("syntax", `A (help_syntax_items ~cmdlet:name parameters))
      (* The PARAMETERS section is sorted by name, the way PowerShell sorted it
         when it derived the section by reflection. The syntax items keep their
         own, declaration-based order. *)
    ; ( "parameters"
      , `A
          (List.map help_param_json
             (List.stable_sort
                (fun a b -> compare a.hp_name b.hp_name)
                parameters
             )
          )
      )
    ; ("has_inputs", `Bool (inputs <> []))
    ; ( "inputs"
      , `A (List.map (fun t -> `O [("type", `String (escape_xml t))]) inputs)
      )
    ; ("has_outputs", `Bool (outputs <> []))
    ; ( "outputs"
      , `A
          (List.map
             (fun t -> `O [("type", `String (escape_xml (help_type_name t)))])
             outputs
          )
      )
      (* The curated examples follow the generated one, so each cmdlet reads
         from the simplest form to the most involved. *)
    ; ("has_examples", `Bool (all_examples <> []))
    ; ( "examples"
      , `A (List.mapi (fun i e -> help_example_json (i + 1) e) all_examples)
      )
    ]

(* Mirrors print_xenobject_params: the identity parameters each live in their
   own parameter set, are positional, and are mandatory except for -Ref on the
   cmdlets that can also run without one. *)
and help_identity_params obj classname ~mandatory_ref ~include_xenobject
    ~include_uuid_name =
  let stem = ocaml_class_to_csharp_class classname in
  let xo =
    if include_xenobject then
      [
        help_param ~pipeline:"true (ByValue)" ~position:"0" ~required:true
          ~sets:["XenObject"]
          (ocaml_class_to_csharp_property classname)
          (qualified_class_name classname)
          (sprintf "The %s object to operate on." stem)
      ]
    else
      []
  in
  let refp =
    help_param ~pipeline:"true (ByPropertyName)" ~position:"0"
      ~required:mandatory_ref ~sets:["Ref"] ~aliases:["opaque_ref"] "Ref"
      (sprintf "XenRef<%s>" (qualified_class_name classname))
      (sprintf "The %s object to operate on, specified by its opaque reference."
         stem
      )
  in
  let uuidp =
    if include_uuid_name && has_uuid obj then
      [
        help_param ~pipeline:"true (ByPropertyName)" ~position:"0"
          ~required:true ~sets:["Uuid"] "Uuid" "guid"
          (sprintf "The UUID of the %s to operate on." stem)
      ]
    else
      []
  in
  let namep =
    if include_uuid_name && has_name obj then
      [
        help_param ~pipeline:"true (ByPropertyName)" ~position:"0"
          ~required:true ~sets:["Name"] ~aliases:["name_label"] "Name" "string"
          (sprintf "The name of the %s to operate on." stem)
      ]
    else
      []
  in
  (xo @ [refp]) @ uuidp @ namep

and help_message_params classname verb messages =
  List.map
    (fun m ->
      let cut = cut_msg_name (pascal_case m.msg_name) verb in
      let pname =
        if cut = "Host" then
          "XenHost"
        else
          cut
      in
      let typ = get_message_type m classname verb in
      let values =
        match
          List.filter (fun p -> not (is_class p classname)) m.msg_params
        with
        | p :: _ ->
            enum_values_of_ty p.param_type
        | [] ->
            []
      in
      help_param ~values pname typ m.msg_doc
    )
    messages

(* The Invoke and Get-XenFooProperty cmdlets add parameters at runtime through
   GetDynamicParameters, according to the selected action. Reflection can only
   ever surface one action's worth of them, so document them all here, each
   labelled with the actions it belongs to. They are deliberately left out of
   the syntax blocks: a cmdlet like Invoke-XenVM has dozens of them across its
   actions, and listing them all in one syntax line would be unreadable. *)
and help_dynamic_params classname verb enum_param messages =
  let entries =
    List.concat_map
      (fun m ->
        if not (is_message_with_dynamic_params classname m) then
          []
        else
          let action = cut_msg_name (pascal_case m.msg_name) verb in
          m.msg_params
          |> List.filter (fun p -> not (is_class p classname))
          |> List.map (fun p ->
              let prop = ocaml_class_to_csharp_property p.param_name in
              let pname =
                if
                  verb = "Invoke"
                  && List.mem
                       (String.lowercase_ascii p.param_name)
                       ["name"; "uuid"]
                then
                  prop ^ "Param"
                else
                  prop
              in
              ( pname
              , obj_internal_type p.param_type
              , p.param_doc
              , action
              , enum_values_of_ty p.param_type
              )
          )
      )
      messages
  in
  let names =
    List.fold_left
      (fun acc (n, _, _, _, _) ->
        if List.mem n acc then
          acc
        else
          acc @ [n]
      )
      [] entries
  in
  List.map
    (fun n ->
      let mine = List.filter (fun (m, _, _, _, _) -> m = n) entries in
      (* n was taken from entries, so mine always has a head; the empty case
         is only here to keep the match total. *)
      let typ, doc, values =
        match mine with
        | (_, typ, doc, _, values) :: _ ->
            (typ, doc, values)
        | [] ->
            ("", "", [])
      in
      let actions = List.map (fun (_, _, _, a, _) -> a) mine in
      (* Datamodel prose is used verbatim everywhere else, and plenty of it
         ends without a full stop. Here a sentence is appended to it, so a
         separator is needed or the two run together: "The name of the
         snapshotted VM Accepted when -XenAction is ...". *)
      let doc =
        if doc = "" then
          sprintf "The %s to use." n
        else if String.contains ".!?" doc.[String.length doc - 1] then
          doc
        else
          doc ^ "."
      in
      help_param ~in_syntax:false ~values n typ
        (sprintf "%s Accepted when -Xen%s is %s." doc enum_param
           (String.concat ", " actions)
        )
    )
    names

(* Mirrors gen_constructor: a real constructor takes one parameter per
   writable field of the class, anything else takes the message's own
   parameters. Looking only at the fields would leave cmdlets such as
   New-XenBond and New-XenSR with no parameters documented. *)
and help_ctor_field_params obj m =
  if is_real_constructor m then
    DU.fields_of_obj obj
    |> List.filter (fun f -> f.qualifier <> DynamicRO)
    |> List.map (fun f ->
        help_param ~sets:["Fields"] ~values:(enum_values_of_ty f.ty)
          (ocaml_class_to_csharp_property (full_name f))
          (obj_internal_type f.ty) f.field_description
    )
  else
    m.msg_params
    |> List.filter (fun p -> p.param_name <> "record")
    |> List.map (fun p ->
        help_param ~sets:["Fields"]
          ~values:(enum_values_of_ty p.param_type)
          (ocaml_class_to_csharp_property p.param_name)
          (obj_internal_type p.param_type)
          p.param_doc
    )

and help_for_class obj =
  let classname = obj.name in
  let messages = obj.messages in
  let stem = ocaml_class_to_csharp_class classname in
  (* A class-level note reaches every cmdlet of the class; a message-level one
     is added on top where a cmdlet comes from one particular message. The
     paired predicates say whether the synopsis gets the label, and have to
     agree with the notes or a cmdlet ends up flagged in one place and not the
     other. *)
  let class_note = help_deprecation ~noun:"class" obj.obj_lifecycle in
  let msg_note m = help_deprecation ~noun:"operation" m.msg_lifecycle in
  let described d = help_note_onto d class_note in
  let described_msg d m = help_note_onto (described d) (msg_note m) in
  let dep = class_note <> None in
  let dep_msg m = dep || msg_note m <> None in
  let class_desc =
    described
      ( if obj.description = "" then
          sprintf "The %s class." stem
        else
          obj.description
      )
  in
  let getter =
    if List.mem classname classes_with_records then
      let ex =
        help_example
          ~title:(sprintf "List every %s" stem)
          (sprintf "PS> Get-Xen%s" stem)
          (sprintf
             "Retrieves all %s objects from the server. With no parameters the \
              cmdlet fetches the whole collection."
             stem
          )
        :: List.map
             (fun (code, how, why) ->
               help_example
                 ~title:(sprintf "Retrieve one %s %s" stem how)
                 code
                 (sprintf "Retrieves a single %s. %s" stem why)
             )
             (* The getter is its own way of naming, so drop the piped form. *)
             (help_take 2
                (List.filter
                   (fun (_, how, _) -> how <> "piped from the getter")
                   (help_ways_to_name obj classname (sprintf "Get-Xen%s" stem))
                )
             )
      in
      [
        help_command ~name:(sprintf "Get-Xen%s" stem) ~deprecated:dep
          ~synopsis:(sprintf "Gets the %s objects present on the server." stem)
          ~description:class_desc
          ~parameters:
            (help_identity_params obj classname ~mandatory_ref:false
               ~include_xenobject:false ~include_uuid_name:true
            )
          ~outputs:[sprintf "%s[]" (qualified_class_name classname)]
          ~examples:ex ()
      ]
    else
      []
  in
  let ctor =
    match List.filter is_constructor messages with
    | m :: _ ->
        [
          help_command ~name:(sprintf "New-Xen%s" stem) ~deprecated:(dep_msg m)
            ~synopsis:(sprintf "Creates a new %s object." stem)
            ~description:
              (described_msg
                 ( if m.msg_doc = "" then
                     sprintf "Creates a new %s." stem
                   else
                     m.msg_doc
                 )
                 m
              )
            ~examples:
              ( if is_real_constructor m then
                  (* A record constructor builds the object from its writable
                     fields, so the hashtable, the field parameters and an
                     existing record are three real ways of describing it. *)
                  [
                    help_example
                      ~title:(sprintf "Create a %s from a table of fields" stem)
                      (sprintf
                         "PS> New-Xen%s -HashTable @{ name_label = \"Demo %s\" \
                          } -PassThru"
                         stem stem
                      )
                      (sprintf
                         "The keys are the API's field names, so this form \
                          reads the same for every class whatever fields it \
                          happens to have. -PassThru returns the new %s."
                         stem
                      )
                  ]
                  @ ( match help_ctor_field_params obj m with
                    | p :: _ ->
                        [
                          help_example
                            ~title:(sprintf "Create a %s from parameters" stem)
                            (sprintf "PS> New-Xen%s -%s %s -PassThru" stem
                               p.hp_name
                               (help_value_placeholder p.hp_type)
                            )
                            (sprintf
                               "The same thing with one parameter per field, \
                                which is the form that tab-completes. Get-Help \
                                New-Xen%s -Full lists them all."
                               stem
                            )
                        ]
                    | [] ->
                        []
                    )
                  @
                  if List.mem classname classes_with_records then
                    [
                      help_example
                        ~title:(sprintf "Create a %s from an existing one" stem)
                        (sprintf
                           "PS> $record = Get-Xen%s | Select-Object -First 1\n\
                            PS> New-Xen%s -Record $record -PassThru"
                           stem stem
                        )
                        (sprintf
                           "-Record takes a whole %s record, so an existing \
                            object can be used as the template for a new one."
                           stem
                        )
                    ]
                  else
                    []
                else
                  (* Anything else takes the parameters of its own create
                     message, and takes all of them: Bond.create wants a
                     network and its members, and a call without them does not
                     fail politely. The -HashTable and -Record sets of these
                     cmdlets cannot populate those parameters at all, so an
                     example must not reach for them. *)
                  let setups, args, _ = help_all_arguments classname m in
                  let call = sprintf "PS> New-Xen%s %s -PassThru" stem args in
                  let code = String.concat "\n" (setups @ [call]) in
                  [
                    help_example
                      ~title:(sprintf "Create a %s" stem)
                      code
                      (sprintf
                         "%s takes the arguments of the create call rather \
                          than a record of fields, and needs all of them. \
                          -PassThru returns the new %s."
                         (sprintf "New-Xen%s" stem) stem
                      )
                  ]
                  (* Only a message the server runs asynchronously has a task
                     to wait for; New-XenTask and New-XenMessage do not, and
                     the cmdlet has no -Async to offer. *)
                  @ ( if m.msg_async then
                        [
                          help_example
                            ~title:
                              (sprintf "Create a %s and wait for the task" stem)
                            (String.concat "\n"
                               (setups
                               @ [
                                   sprintf
                                     "PS> New-Xen%s %s -Async -PassThru |\n\
                                     \    Wait-XenTask -ShowProgress"
                                     stem args
                                 ]
                               )
                            )
                            "-Async hands the work to the server and returns \
                             the Task that represents it, which pipes into \
                             Wait-XenTask."
                        ]
                      else
                        []
                    )
                  @ [
                      help_example ~title:"See what would be created"
                        (String.concat "\n"
                           (setups
                           @ [sprintf "PS> New-Xen%s %s -WhatIf" stem args]
                           )
                        )
                        "Reports what would be created without creating it. \
                         Every cmdlet that changes the server takes -WhatIf \
                         and -Confirm."
                    ]
              )
            ~parameters:
              (help_passthru ()
              :: help_param ~required:true ~sets:["Hashtable"] "HashTable"
                   "hashtable"
                   (sprintf
                      "A hashtable of field names to values, from which to \
                       create the %s."
                      stem
                   )
              :: help_param ~required:true ~sets:["Record"] "Record"
                   (qualified_class_name classname)
                   (sprintf
                      "An existing %s record whose fields are used to create \
                       the new object."
                      stem
                   )
              :: help_ctor_field_params obj m
              )
            ~shouldprocess:true
            ~outputs:
              (qualified_class_name classname
               ::
               ( if m.msg_async then
                   ["XenAPI.Task"]
                 else
                   []
               )
              @ ["void"]
              )
            ?async:
              ( if m.msg_async then
                  Some (help_async ())
                else
                  None
              )
            ()
        ]
    | [] ->
        []
  in
  (* [void_output] mirrors the flag gen_message_family passes to the template,
     which decides whether the cmdlet declares [OutputType(typeof(void))]. *)
  let msg_family verb suffix ~void_output synopsis descr messages =
    match messages with
    | [] ->
        []
    | ms ->
        let async = List.exists (fun m -> m.msg_async) ms in
        (* about_XenServer.help.txt shows a setter piped from its getter, and
           the adders and property removers naming the object directly. *)
        (* An Add, Remove or Set message the datamodel generated for a field
           really is a field operation, and a sentence built from the field
           name describes it. One written by hand is a domain operation that
           merely starts with the same verb - Rate_limit's add_caller attaches
           a caller to a limiter, it does not add to a "Caller" field - so use
           the message's own documentation and say what it does. *)
        let field_of m = cut_msg_name (pascal_case m.msg_name) verb in
        let is_field_op m =
          match m.msg_tag with FromField _ -> true | _ -> false
        in
        let title_for m =
          let field = field_of m in
          if is_field_op m then
            sprintf "%s the %s field"
              ( match verb with
              | "Set" ->
                  "Set"
              | "Add" ->
                  "Add to"
              | _ ->
                  "Remove from"
              )
              field
          else
            sprintf "%s a %s" verb (String.lowercase_ascii field)
        in
        let remark_for m =
          if is_field_op m then
            sprintf "%s the %s field of a %s.%s"
              ( match verb with
              | "Set" ->
                  "Sets"
              | "Add" ->
                  "Adds a value to"
              | _ ->
                  "Removes a value from"
              )
              (field_of m) stem
              ( if
                  String.starts_with ~prefix:"KeyValuePair"
                    (get_message_type m classname verb)
                then
                  " The parameter takes one entry of the map; a hashtable does \
                   not bind to it."
                else
                  ""
              )
          else
            m.msg_doc
        in
        let invocation m =
          sprintf "%s-Xen%s%s -%s %s" verb stem suffix (field_of m)
            (snd (help_message_argument ~verb classname m))
        in
        let with_setup m code =
          match fst (help_message_argument ~verb classname m) with
          | Some s ->
              s ^ "\n" ^ code
          | None ->
              code
        in
        let call_for m =
          if verb = "Set" then
            help_operate_on obj classname (invocation m)
          else
            sprintf "PS> %s %s" (invocation m) (help_selector obj classname)
        in
        let example_for m =
          help_example
            (with_setup m (call_for m))
            ~title:(title_for m) (remark_for m)
        in
        (* One example per field, up to three. Where the class has only one
           field to operate on, vary how the object is named instead: which of
           -Ref, -Uuid, -Name and the piped object a cmdlet takes is worth as
           much to a reader as another field would be. *)
        let examples =
          let per_field = List.map example_for (help_take 3 ms) in
          let wanted = 3 - List.length per_field in
          if wanted <= 0 then
            per_field
          else
            per_field
            @
            match ms with
            | m :: _ ->
                let already = call_for m in
                List.map
                  (fun (code, how, why) ->
                    (* The line that builds the argument is repeated rather
                       than referred back to. It costs a line, but an example
                       that only runs if you have read the one above it is not
                       an example; running these against a pool is what showed
                       that up. *)
                    help_example (with_setup m code)
                      ~title:(sprintf "%s %s" (title_for m) how)
                      (sprintf "%s %s" (remark_for m) why)
                  )
                  (help_take wanted
                     (List.filter
                        (fun (code, _, _) -> code <> already)
                        (help_ways_to_name obj classname (invocation m))
                     )
                  )
            | [] ->
                []
        in
        [
          help_command
            ~name:(sprintf "%s-Xen%s%s" verb stem suffix)
            ~deprecated:dep ~synopsis ~description:(described descr) ~examples
            ~parameters:
              (help_identity_params obj classname ~mandatory_ref:true
                 ~include_xenobject:true ~include_uuid_name:true
              @ (help_passthru () :: help_message_params classname verb ms)
              )
            ~shouldprocess:true
            ~outputs:
              (qualified_class_name classname
               ::
               ( if async then
                   ["XenAPI.Task"]
                 else
                   []
               )
              @
              if void_output then
                ["void"]
              else
                []
              )
            ?async:
              ( if async then
                  Some (help_async_for_fields ())
                else
                  None
              )
            ()
        ]
  in
  let setter =
    msg_family "Set" "" ~void_output:true
      (sprintf "Sets fields of a %s object." stem)
      (sprintf
         "Changes writable fields of a %s. Each optional parameter listed \
          below corresponds to a field that can be set."
         stem
      )
      (List.filter is_setter messages)
  in
  let adder =
    msg_family "Add" "" ~void_output:true
      (sprintf "Adds to fields of a %s object." stem)
      (sprintf
         "Adds values to the collection-valued fields of a %s. Each optional \
          parameter listed below corresponds to a field that can be added to."
         stem
      )
      (List.filter is_adder messages)
  in
  let remover =
    msg_family "Remove" "Property" ~void_output:false
      (sprintf "Removes values from fields of a %s object." stem)
      (sprintf
         "Removes values from the collection-valued fields of a %s. Each \
          optional parameter listed below corresponds to a field that can be \
          removed from."
         stem
      )
      (List.filter is_remover messages)
  in
  let enum_family verb suffix enum_param synopsis intro messages =
    match messages with
    | [] ->
        []
    | ms ->
        (* One cmdlet covers many operations here, and they do not share a
           lifecycle - VM.snapshot_with_quiesce is gone while the rest of
           Invoke-XenVM is current - so the note goes on the operation's own
           line rather than on the cmdlet. *)
        let lines =
          List.map
            (fun m ->
              let tag =
                match m.msg_lifecycle.Lifecycle.state with
                | Lifecycle.Removed_s ->
                    " [removed]"
                | Lifecycle.Deprecated_s ->
                    " [deprecated]"
                | _ ->
                    ""
              in
              sprintf "%s: %s%s"
                (cut_msg_name (pascal_case m.msg_name) verb)
                m.msg_doc tag
            )
            ms
        in
        let description = described (String.concat "\n" (intro :: lines)) in
        let async = List.exists (fun m -> m.msg_async) ms in
        let actions =
          List.map (fun m -> cut_msg_name (pascal_case m.msg_name) verb) ms
        in
        (* Prefer an operation that takes no runtime parameters, so the example
           stands on its own rather than needing a -XenAction-specific
           parameter the reader has not met yet. Falling back to ms, which is
           non-empty in this branch, means the match is total without a
           partial head. *)
        (* The names the enum parameter accepts, best example first. The
           identity fields are the least illustrative choice - they are on the
           object already, so reading one back through the cmdlet does not show
           why the cmdlet is there - so they sort last. *)
        let ordered =
          let name m = cut_msg_name (pascal_case m.msg_name) verb in
          let plain =
            List.filter
              (fun m -> not (is_message_with_dynamic_params classname m))
              ms
          in
          let identity m =
            List.mem
              (String.lowercase_ascii (name m))
              ["uuid"; "namelabel"; "namedescription"]
          in
          List.map name
            (List.filter (fun m -> not (identity m)) plain @ plain @ ms)
          |> List.fold_left
               (fun acc n ->
                 if List.mem n acc then
                   acc
                 else
                   acc @ [n]
               )
               []
        in
        let representative = match ordered with n :: _ -> n | [] -> "" in
        (* Get-Xen<Class>Property takes neither -Name/-Uuid nor -PassThru; only
           Invoke-Xen<Class> has them, so only it gets the asynchronous
           example. *)
        let include_uuid_name = verb = "Invoke" in
        (* An Invoke cmdlet makes the call either way, but writes the result
           out only when -PassThru is given. An operation that returns
           something - a snapshot's reference, a table of usage - therefore
           looks as though it did nothing when the example omits it, so ask for
           it wherever there is something to collect. A property getter writes
           its value unconditionally and has no -PassThru to give. *)
        let returns_a_value =
          let named =
            List.map
              (fun m ->
                ( cut_msg_name (pascal_case m.msg_name) verb
                , m.msg_result <> None
                )
              )
              ms
          in
          fun n ->
            verb = "Invoke"
            && match List.assoc_opt n named with Some r -> r | None -> false
        in
        let collect name code =
          if returns_a_value name then
            code ^ " -PassThru"
          else
            code
        in
        let collected name why =
          if returns_a_value name then
            why ^ " -PassThru writes the result to the pipeline."
          else
            why
        in
        let one name =
          help_example
            ~title:
              ( if verb = "Invoke" then
                  sprintf "Run the %s operation" name
                else
                  sprintf "Get the %s property" name
              )
            (collect name
               (help_operate_on ~include_uuid_name obj classname
                  (sprintf "%s-Xen%s%s -Xen%s %s" verb stem suffix enum_param
                     name
                  )
               )
            )
            ( if verb = "Invoke" then
                collected name
                  (sprintf "Invokes the %s operation on a %s." name stem)
              else
                sprintf "Gets the %s property of a %s." name stem
            )
        in
        (* Three examples on a property getter, which is enough to show that
           the parameter chooses among the properties and that the rest work
           the same way, without restating a list that runs to a hundred
           entries on some classes. An Invoke cmdlet leads with one operation
           and follows it with the asynchronous form, which is the thing worth
           learning; a second operation would say no more than the list above
           already does. *)
        let shown =
          if verb = "Invoke" then
            help_take 1 ordered
          else
            help_take 3 ordered
        in
        (* Where the class has fewer than three properties, vary how the object
           is named instead of leaving the cmdlet with one example. *)
        let topped_up =
          let wanted =
            3
            - List.length shown
            -
            if async && verb = "Invoke" then
              1
            else
              0
          in
          if wanted <= 0 then
            []
          else
            match ordered with
            | n :: _ ->
                let rest =
                  sprintf "%s-Xen%s%s -Xen%s %s" verb stem suffix enum_param n
                in
                List.map
                  (fun (code, how, why) ->
                    help_example
                      ~title:
                        ( if verb = "Invoke" then
                            sprintf "Run the %s operation %s" n how
                          else
                            sprintf "Get the %s property %s" n how
                        )
                      (collect n code)
                      ( if verb = "Invoke" then
                          collected n
                            (sprintf "Invokes the %s operation on a %s. %s" n
                               stem why
                            )
                        else
                          sprintf "Gets the %s property of a %s. %s" n stem why
                      )
                  )
                  (* Drop whichever way the first example already used, rather
                     than assuming it was the first of the list: a property
                     getter names the object directly where an Invoke cmdlet
                     pipes it in. *)
                  (let already =
                     help_operate_on ~include_uuid_name obj classname rest
                   in
                   help_take wanted
                     (List.filter
                        (fun (code, _, _) -> code <> already)
                        (help_ways_to_name ~include_uuid_name obj classname rest)
                     )
                  )
            | [] ->
                []
        in
        let examples =
          List.map one shown
          @ topped_up
          @
          if async && verb = "Invoke" then
            [
              help_example
                ~title:(sprintf "Run %s asynchronously" representative)
                (sprintf
                   "PS> Invoke-Xen%s %s -Xen%s %s -Async -PassThru | \
                    Wait-XenTask -ShowProgress"
                   stem
                   (help_selector obj classname)
                   enum_param representative
                )
                "Runs the same operation asynchronously and follows the task \
                 it returns."
            ]
          else
            []
        in
        [
          help_command
            ~name:(sprintf "%s-Xen%s%s" verb stem suffix)
            ~deprecated:dep ~synopsis ~description ~examples
            ~parameters:
              (help_identity_params obj classname ~mandatory_ref:true
                 ~include_xenobject:true ~include_uuid_name:(verb = "Invoke")
              @ [
                  help_param ~required:true ~values:actions
                    (sprintf "Xen%s" enum_param)
                    (sprintf "Xen%s%s" stem enum_param)
                    (sprintf "Selects which %s to use."
                       (String.lowercase_ascii enum_param)
                    )
                ]
              @ help_dynamic_params classname verb enum_param ms
              @
              if verb = "Invoke" then
                [help_passthru ()]
              else
                []
              )
              (* Only Invoke-Xen<Class> declares SupportsShouldProcess;
                 Get-Xen<Class>Property does not. *)
            ~shouldprocess:(verb = "Invoke")
              (* Neither template declares an OutputType, because what comes
                 back depends on the selected action, so PowerShell falls back
                 to System.Object. Say the same thing here. *)
            ~outputs:["System.Object"]
            ?async:
              ( if async then
                  Some (help_async ())
                else
                  None
              )
            ()
        ]
  in
  let getprop =
    enum_family "Get" "Property" "Property"
      (sprintf "Gets a property of a %s object." stem)
      (sprintf
         "Gets a specified property of a %s object. Use the -XenProperty \
          parameter to select which property to retrieve."
         stem
      )
      (List.filter is_getter messages)
  in
  let invoke =
    enum_family "Invoke" "" "Action"
      (sprintf "Invokes an operation on a %s object." stem)
      (sprintf
         "Invokes an operation on a %s object. Use the -XenAction parameter to \
          select which operation to perform."
         stem
      )
      (List.filter is_invoke messages)
  in
  let destructor =
    match List.filter is_destructor messages with
    | m :: _ ->
        [
          help_command
            ~name:(sprintf "Remove-Xen%s" stem)
            ~deprecated:(dep_msg m)
            ~synopsis:(sprintf "Deletes a %s object." stem)
            ~description:
              (described_msg
                 ( if m.msg_doc = "" then
                     sprintf "Destroys the specified %s object." stem
                   else
                     m.msg_doc
                 )
                 m
              )
            ~parameters:
              (help_identity_params obj classname ~mandatory_ref:true
                 ~include_xenobject:true ~include_uuid_name:true
              @ [help_passthru ()]
              )
            ~examples:
              (let cmd = sprintf "Remove-Xen%s" stem in
               List.map
                 (fun (code, how, why) ->
                   help_example
                     ~title:(sprintf "Delete a %s %s" stem how)
                     code
                     (sprintf "Deletes a %s. %s" stem why)
                 )
                 (help_take 2 (help_ways_to_name obj classname cmd))
               (* -WhatIf earns its place on a cmdlet that deletes: it is how
                  you see what a command would take out before it does. *)
               @ [
                   help_example ~title:"See what would be deleted"
                     (sprintf "%s -WhatIf" (help_operate_on obj classname cmd))
                     (sprintf
                        "Reports the %s that would be deleted without deleting \
                         anything. Every cmdlet that changes the server takes \
                         -WhatIf and -Confirm."
                        stem
                     )
                 ]
              )
            ~shouldprocess:true
            ~outputs:
              (( if m.msg_async then
                   ["XenAPI.Task"]
                 else
                   []
               )
              @ ["void"]
              )
            ?async:
              ( if m.msg_async then
                  Some (help_async ())
                else
                  None
              )
            ()
        ]
    | [] ->
        []
  in
  getter @ ctor @ setter @ adder @ remover @ getprop @ invoke @ destructor
