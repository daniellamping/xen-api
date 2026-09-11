(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Datamodel
open Datamodel_types
open Dm_api
open Common_functions
open CommonFunctions
module DT = Datamodel_types
module DU = Datamodel_utils

module TypeSet = Set.Make (struct
  type t = DT.ty

  let compare = compare
end)

let destdir = "autogen-out"

let srcdir = "autogen-out/src"

let templdir = "templates"

type cmdlet = {cmdletname: string; content: string}

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

let cmdlets_to_export = ref []

(* Which curated examples were actually attached to a cmdlet. Checked at the
   end of generation: an entry that matches nothing means the cmdlet it names
   has been renamed or withdrawn, and the example is now describing something
   that does not exist. *)
let curated_used = ref []

let api =
  Datamodel_utils.named_self := true ;
  let field_filter field =
    (not field.internal_only) && List.mem "closed" field.release.internal
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg
    && (not msg.msg_hide_from_docs)
    && (not
          (List.mem msg.msg_name
             [
               "get_by_name_label"
             ; "get_by_uuid"
             ; "get"
             ; "get_all"
             ; "get_all_records"
             ; "get_all_records_where"
             ; "get_record"
             ]
          )
       )
    && msg.msg_tag <> FromObject GetAllRecords
    && List.mem "closed" msg.msg_release.internal
  in
  let filter api = filter_by ~field:field_filter ~message:message_filter api in
  Datamodel.all_api
  |> filter
  |> Datamodel_utils.add_implicit_messages ~document_order:false
  |> filter

let classes_with_records =
  Datamodel_utils.add_implicit_messages ~document_order:false Datamodel.all_api
  |> objects_of_api
  |> List.filter (fun x ->
      List.exists (fun y -> y.msg_name = "get_all_records") x.messages
  )
  |> List.map (fun x -> x.name)

let classes = objects_of_api api

let maps = ref TypeSet.empty

let generated x =
  not (List.mem x.name ["blob"; "session"; "debug"; "event"; "vtpm"])

let rec is_last x list =
  match list with
  | [] ->
      false
  | hd :: [] ->
      if hd = x then
        true
      else
        false
  | hd :: tl ->
      if hd = x then
        false
      else
        is_last x tl

let rec main () =
  let json =
    `O
      [
        ( "all_classes"
        , `A
            (List.map
               (fun x ->
                 `O
                   [
                     ("exposed_name", `String (exposed_class_name x.name))
                   ; ( "var_name"
                     , `String (ocaml_class_to_csharp_local_var x.name)
                     )
                   ]
               )
               classes
            )
        )
      ]
  in
  render_file
    ("ConvertTo-XenRef.mustache", "ConvertTo-XenRef.cs")
    json templdir srcdir ;

  http_actions
  |> List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk)
  |> List.iter gen_http_action ;

  let filtered_classes = List.filter generated classes in
  let cmdlets = List.concat_map gen_cmdlets filtered_classes in

  List.iter (fun x -> write_file x.cmdletname x.content) cmdlets ;

  filtered_classes |> List.iter gen_destructor ;

  cmdlets_to_export :=
    [
      "Connect-XenServer"
    ; "Disconnect-XenServer"
    ; "Get-XenSession"
    ; "Receive-XenPoolPatch"
    ; "Send-XenOemPatchStream"
    ; "Wait-XenTask"
    ; "ConvertTo-XenRef"
    ]
    @ !cmdlets_to_export ;

  cmdlets_to_export := List.sort String.compare !cmdlets_to_export ;

  let module_json =
    `O
      [
        ( "cmdlets_to_export"
        , `A
            (List.map
               (fun x ->
                 `O
                   [
                     ("cmdlet_to_export", `String x)
                   ; ("is_last", `Bool (is_last x !cmdlets_to_export))
                   ]
               )
               !cmdlets_to_export
            )
        )
      ]
  in
  render_file
    ("XenServerPSModule.mustache", "XenServerPSModule.psd1")
    module_json templdir destdir ;

  gen_help ()

(****************)
(* Http actions *)
(****************)

(* Shared by the cmdlet template and the generated help, so that the two cannot
   disagree about what an HTTP action's query arguments are called. *)
and http_arg_name = function
  | String_query_arg x | Int64_query_arg x ->
      pascal_case_rec x
  | Bool_query_arg x ->
      if String.lowercase_ascii x = "host" then
        "IsHost"
      else
        pascal_case_rec x
  | Varargs_query_arg ->
      "Args"

and http_arg_type = function
  | String_query_arg _ ->
      "string"
  | Int64_query_arg _ ->
      "long?"
  | Bool_query_arg _ ->
      "bool?"
  | Varargs_query_arg ->
      "string[]"

and gen_http_action action =
  let name, (meth, uri, _, args, _, _) = action in
  let commonVerb = get_http_action_verb name meth in
  let verbCategory = get_common_verb_category commonVerb in
  let stem = get_http_action_stem name in
  let arg_name = http_arg_name in
  let arg_type = http_arg_type in
  let json =
    `O
      [
        ("verb_category", `String verbCategory)
      ; ("common_verb", `String commonVerb)
      ; ("stem", `String stem)
      ; ("isPut", `Bool (meth == Put))
      ; ("isGet", `Bool (meth == Get))
      ; ("uri", `String uri)
      ; ("action_name", `String name)
      ; ( "args"
        , `A
            (List.map
               (fun x ->
                 `O
                   [
                     ("arg_type", `String (arg_type x))
                   ; ("arg_name", `String (arg_name x))
                   ; ( "from_pipeline"
                     , `Bool (String.lowercase_ascii (arg_name x) = "uuid")
                     )
                   ]
               )
               args
            )
        )
      ]
  in
  let cmdlet_name = sprintf "%s-Xen%s" commonVerb stem in
  render_file
    ("HttpAction.mustache", sprintf "%s.cs" cmdlet_name)
    json templdir srcdir ;
  cmdlets_to_export := cmdlet_name :: !cmdlets_to_export

(*************************)
(* Autogenerated cmdlets *)
(*************************)
and gen_cmdlets obj =
  let {name= classname; messages; _} = obj in
  let stem = ocaml_class_to_csharp_class classname in

  let cmdlets =
    [
      {cmdletname= sprintf "Get-Xen%s" stem; content= gen_class obj classname}
    ; {
        cmdletname= sprintf "New-Xen%s" stem
      ; content=
          gen_constructor obj classname (List.filter is_constructor messages)
      }
    ; {
        cmdletname= sprintf "Remove-Xen%sProperty" stem
      ; content= gen_remover obj classname (List.filter is_remover messages)
      }
    ; {
        cmdletname= sprintf "Add-Xen%s" stem
      ; content= gen_adder obj classname (List.filter is_adder messages)
      }
    ; {
        cmdletname= sprintf "Set-Xen%s" stem
      ; content= gen_setter obj classname (List.filter is_setter messages)
      }
    ; {
        cmdletname= sprintf "Get-Xen%sProperty" stem
      ; content= gen_getter obj classname (List.filter is_getter messages)
      }
    ; {
        cmdletname= sprintf "Invoke-Xen%s" stem
      ; content= gen_invoker obj classname (List.filter is_invoke messages)
      }
    ]
  in

  cmdlets |> List.filter (fun x -> x.content <> "")

and write_file cmdletname content =
  let filename = sprintf "%s.cs" cmdletname in
  let fn = Filename.concat srcdir filename in
  cmdlets_to_export := cmdletname :: !cmdlets_to_export ;
  with_output fn (fun x -> output_string x content)

(*********************************)
(* Print function for Get-XenFoo *)
(*********************************)
and render_to_string template_name json =
  let path = Filename.concat templdir template_name in
  let templ = string_of_file path |> Mustache.of_string in
  Mustache.render templ json

and gen_class obj classname =
  if List.mem classname classes_with_records then
    let json =
      `O
        [
          ("licence", `String Licence.bsd_two_clause)
        ; ("class", `String (ocaml_class_to_csharp_class classname))
        ; ("qualified_type", `String (qualified_class_name classname))
        ; ( "params"
          , `String (print_xenobject_params obj classname false false true)
          )
        ; ("has_uuid", `Bool (has_uuid obj))
        ; ("has_name", `Bool (has_name obj))
        ]
    in
    render_to_string "Get-XenObject.mustache" json
  else
    ""

(*********************************)
(* Print function for New-XenFoo *)
(*********************************)
and gen_constructor obj classname messages =
  match messages with
  | [] ->
      ""
  | [message] ->
      let fields_or_params =
        if is_real_constructor message then
          gen_fields (DU.fields_of_obj obj)
        else
          gen_constructor_params message.msg_params
      in
      let async_param_override =
        if message.msg_async then
          "\n\
          \        protected override bool GenerateAsyncParam\n\
          \        {\n\
          \            get { return true; }\n\
          \        }\n"
        else
          ""
      in
      let make =
        if is_real_constructor message then
          gen_make_record obj classname
        else
          gen_make_fields message obj
      in
      let json =
        `O
          [
            ("licence", `String Licence.bsd_two_clause)
          ; ("class", `String (ocaml_class_to_csharp_class classname))
          ; ("qualified_type", `String (qualified_class_name classname))
          ; ("async_task", `Bool message.msg_async)
          ; ("fields_or_params", `String fields_or_params)
          ; ("async_param_override", `String async_param_override)
          ; ("make", `String make)
          ; ( "shouldprocess"
            , `String (gen_shouldprocess "New" message classname)
            )
          ; ("open_brace", `String "{")
          ; ( "api_call"
            , `String (gen_csharp_api_call message classname "New" "passthru")
            )
          ]
      in
      render_to_string "New-XenObject.mustache" json
  | _ ->
      assert false

and gen_constructor_params params =
  match params with
  | [] ->
      ""
  | hd :: tl ->
      sprintf "%s\n%s"
        (gen_constructor_param hd.param_name hd.param_type ["Fields"])
        (gen_constructor_params tl)

and gen_fields fields =
  match fields with
  | [] ->
      ""
  | hd :: tl -> (
    match hd.qualifier with
    | DynamicRO ->
        gen_fields tl
    | _ ->
        sprintf "%s\n%s"
          (gen_constructor_param (full_name hd) hd.ty ["Fields"])
          (gen_fields tl)
  )

and gen_constructor_param paramName paramType paramsets =
  let publicName = ocaml_class_to_csharp_property paramName in
  (*Do not add a Record parameter; it has already been added manually as all constructors need one*)
  if paramName = "record" then
    ""
  else
    sprintf "\n        %s\n        public %s %s { get; set; }"
      (print_parameter_sets paramsets)
      (obj_internal_type paramType)
      publicName

and gen_make_record obj classname =
  sprintf
    "\n\
    \            if (Record == null && HashTable == null)\n\
    \            {\n\
    \                Record = new %s();%s\n\
    \            }\n\
    \            else if (Record == null)\n\
    \            {\n\
    \                Record = new %s(HashTable);\n\
    \            }\n"
    (qualified_class_name classname)
    (gen_record_fields (DU.fields_of_obj obj))
    (qualified_class_name classname)

and gen_record_fields fields =
  match fields with
  | [] ->
      ""
  | h :: tl -> (
    match h.qualifier with
    | DynamicRO ->
        gen_record_fields tl
    | _ ->
        sprintf "\n                %s%s" (gen_record_field h)
          (gen_record_fields tl)
  )

and gen_record_field field =
  let chk =
    sprintf
      "if (MyInvocation.BoundParameters.ContainsKey(\"%s\"))\n                "
      (ocaml_field_to_csharp_property field)
  in
  let assignment =
    match field.ty with
    | Ref _ ->
        sprintf
          "    Record.%s = new %s(%s == null ? \"OpaqueRef:NULL\" : \
           %s.opaque_ref);"
          (full_name field)
          (obj_internal_type field.ty)
          (ocaml_field_to_csharp_property field)
          (ocaml_field_to_csharp_property field)
    | Map (u, v) ->
        sprintf
          "    Record.%s = \
           CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s);"
          (full_name field) (exposed_type u) (exposed_type v)
          (pascal_case (full_name field))
    | _ ->
        sprintf "    Record.%s = %s;" (full_name field)
          (ocaml_field_to_csharp_property field)
  in
  chk ^ assignment

and gen_make_fields message obj =
  sprintf
    "\n\
    \            if (Record != null)\n\
    \            {%s\n\
    \            }\n\
    \            else if (HashTable != null)\n\
    \            {%s\n\
    \            }"
    (explode_record_fields message (DU.fields_of_obj obj))
    (explode_hashtable_fields message (DU.fields_of_obj obj))

and explode_record_fields message fields =
  let print_map tl hd =
    sprintf
      "\n\
      \                %s = \
       CommonCmdletFunctions.ConvertDictionaryToHashtable(Record.%s);%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl)
  in
  let print_record tl hd =
    sprintf "\n                %s = Record.%s;%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl)
  in
  match fields with
  | [] ->
      ""
  | hd :: tl ->
      if List.exists (fun x -> full_name hd = x.param_name) message.msg_params
      then
        match hd.ty with
        | Map (_, _) ->
            print_map tl hd
        | _ ->
            print_record tl hd
      else
        explode_record_fields message tl

and explode_hashtable_fields message fields =
  match fields with
  | [] ->
      ""
  | hd :: tl ->
      if List.exists (fun x -> full_name hd = x.param_name) message.msg_params
      then
        sprintf "\n                %s = %s;%s"
          (ocaml_class_to_csharp_property (full_name hd))
          (convert_from_hashtable (full_name hd) hd.ty)
          (explode_hashtable_fields message tl)
      else
        explode_hashtable_fields message tl

and convert_from_hashtable fname ty =
  let field = sprintf "\"%s\"" fname in
  match ty with
  | DateTime ->
      sprintf "Marshalling.ParseDateTime(HashTable, %s)" field
  | Bool ->
      sprintf "Marshalling.ParseBool(HashTable, %s)" field
  | Float ->
      sprintf "Marshalling.ParseDouble(HashTable, %s)" field
  | Int ->
      sprintf "Marshalling.ParseLong(HashTable, %s)" field
  | Ref name ->
      sprintf "Marshalling.ParseRef<%s>(HashTable, %s)"
        (exposed_class_name name) field
  | SecretString | String ->
      sprintf "Marshalling.ParseString(HashTable, %s)" field
  | Set String ->
      sprintf "Marshalling.ParseStringArray(HashTable, %s)" field
  | Set (Ref x) ->
      sprintf "Marshalling.ParseSetRef<%s>(HashTable, %s)"
        (exposed_class_name x) field
  | Set (Enum (x, _)) ->
      sprintf
        "Helper.StringArrayToEnumList<%s>(Marshalling.ParseStringArray(HashTable, \
         %s))"
        x field
  | Enum (x, _) ->
      sprintf
        "(%s)CommonCmdletFunctions.EnumParseDefault(typeof(%s), \
         Marshalling.ParseString(HashTable, %s))"
        x x field
  | Map (Ref x, Record _) ->
      sprintf "Marshalling.ParseMapRefRecord<%s, Proxy_%s>(HashTable, %s)"
        (exposed_class_name x) (exposed_class_name x) field
  | Map (_, _) as x ->
      maps := TypeSet.add x !maps ;
      sprintf "(Marshalling.ParseHashTable(HashTable, %s))" field
  | Record name ->
      sprintf "new %s((Proxy_%s)HashTable[%s])" (exposed_class_name name)
        (exposed_class_name name) field
  | Set (Record name) ->
      sprintf "Helper.Proxy_%sArrayTo%sList(Marshalling.ParseStringArray(%s))"
        (exposed_class_name name) (exposed_class_name name) field
  | _ ->
      assert false

(************************************)
(* Print function for Remove-XenFoo *)
(************************************)

and gen_destructor obj =
  let {name= classname; messages; _} = obj in
  let destructors = List.filter is_destructor messages in
  match destructors with
  | [] ->
      ()
  | [x] ->
      let json =
        `O
          [
            ("type", `String (qualified_class_name classname))
          ; ("wire_class_name", `String (exposed_class_name classname))
          ; ("class_name", `String (ocaml_class_to_csharp_class classname))
          ; ("property", `String (ocaml_class_to_csharp_property classname))
          ; ("type_local", `String (ocaml_class_to_csharp_local_var classname))
          ; ("async", `Bool x.msg_async)
          ; ("has_uuid", `Bool (has_uuid obj))
          ; ("has_name", `Bool (has_name obj))
          ]
      in
      let cmdlet_name =
        sprintf "Remove-Xen%s" (ocaml_class_to_csharp_class classname)
      in
      render_file
        ("Remove-XenObject.mustache", sprintf "%s.cs" cmdlet_name)
        json templdir srcdir ;
      cmdlets_to_export := cmdlet_name :: !cmdlets_to_export
  | _ ->
      assert false

(*****************************************)
(* Print function for Remove-XenFoo -Bar *)
(*****************************************)
and gen_message_family verb noun class_decl void_output obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let cut_message_name x = cut_msg_name (pascal_case x.msg_name) verb in
      let asyncMessages =
        List.map cut_message_name (List.filter (fun x -> x.msg_async) messages)
      in
      let json =
        `O
          [
            ("licence", `String Licence.bsd_two_clause)
          ; ("verb", `String verb)
          ; ("noun", `String noun)
          ; ("qualified_type", `String (qualified_class_name classname))
          ; ("async_task", `Bool (asyncMessages <> []))
          ; ("void_output", `Bool void_output)
          ; ("class_decl", `String class_decl)
          ; ( "params"
            , `String (print_xenobject_params obj classname true true true)
            )
          ; ("async_param", `String (print_async_param asyncMessages))
          ; ( "message_params"
            , `String (gen_message_as_param classname verb messages)
            )
          ; ("local_var", `String (ocaml_class_to_csharp_local_var classname))
          ; ("property", `String (ocaml_class_to_csharp_property classname))
          ; ( "cmdlet_methods"
            , `String (print_cmdlet_methods classname messages verb)
            )
          ; ("passthru", `String (gen_passthru classname))
          ; ( "parse_method"
            , `String (print_parse_xenobject_private_method obj classname true)
            )
          ; ( "process_methods"
            , `String
                (print_process_record_private_methods classname messages verb "")
            )
          ]
      in
      render_to_string "Set-XenObject.mustache" json

(*****************************************)
(* Print function for Remove-XenFoo -Bar *)
(*****************************************)
and gen_remover obj classname messages =
  let stem = ocaml_class_to_csharp_class classname in
  gen_message_family "Remove"
    (sprintf "%sProperty" stem)
    (sprintf "RemoveXen%sProperty" stem)
    false obj classname messages

(**************************************)
(* Print function for Set-XenFoo -Bar *)
(**************************************)
and gen_setter obj classname messages =
  let stem = ocaml_class_to_csharp_class classname in
  gen_message_family "Set" stem (sprintf "SetXen%s" stem) true obj classname
    messages

(**************************************)
(* Print function for Add-XenFoo -Bar *)
(**************************************)
and gen_adder obj classname messages =
  let stem = ocaml_class_to_csharp_class classname in
  gen_message_family "Add" stem (sprintf "AddXen%s" stem) true obj classname
    messages

(*****************************************)
(* Print function for Invoke-XenFoo -Bar *)
(*****************************************)
and gen_invoker obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let stem = ocaml_class_to_csharp_class classname in
      let messagesWithParams =
        List.filter (is_message_with_dynamic_params classname) messages
      in
      let json =
        `O
          [
            ("licence", `String Licence.bsd_two_clause)
          ; ("verb_expr", `String "VerbsLifecycle.Invoke")
          ; ("noun", `String stem)
          ; ("should_process", `String "true")
          ; ("class_decl", `String (sprintf "InvokeXen%s" stem))
          ; ("class", `String stem)
          ; ("enum_kind", `String "Action")
          ; ("open_brace", `String "{")
          ; ( "passthru_param"
            , `String
                "\n\
                \        [Parameter]\n\
                \        public SwitchParameter PassThru { get; set; }\n"
            )
          ; ( "params"
            , `String (print_xenobject_params obj classname true true true)
            )
          ; ( "dynamic_generator"
            , `String
                (print_dynamic_generator classname "Action" "Invoke"
                   messagesWithParams
                )
            )
          ; ("local_var", `String (ocaml_class_to_csharp_local_var classname))
          ; ("property", `String (ocaml_class_to_csharp_property classname))
          ; ( "cmdlet_methods_dynamic"
            , `String
                (print_cmdlet_methods_dynamic classname messages "Action"
                   "Invoke"
                )
            )
          ; ( "parse_method"
            , `String (print_parse_xenobject_private_method obj classname true)
            )
          ; ( "process_methods"
            , `String
                (print_process_record_private_methods classname messages
                   "Invoke" "passthru"
                )
            )
          ; ("messages_enum", `String (print_messages_as_enum "Invoke" messages))
          ; ( "dynamic_params"
            , `String
                (print_dynamic_params classname "Action" "Invoke"
                   messagesWithParams
                )
            )
          ]
      in
      render_to_string "Invoke-XenObject.mustache" json

(**********************************************)
(* Print function for Get-XenFooProperty -Bar *)
(**********************************************)
and gen_getter obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let stem = ocaml_class_to_csharp_class classname in
      let messagesWithParams =
        List.filter (is_message_with_dynamic_params classname) messages
      in
      let json =
        `O
          [
            ("licence", `String Licence.bsd_two_clause)
          ; ("verb_expr", `String "VerbsCommon.Get")
          ; ("noun", `String (sprintf "%sProperty" stem))
          ; ("should_process", `String "false")
          ; ("class_decl", `String (sprintf "GetXen%sProperty" stem))
          ; ("class", `String stem)
          ; ("enum_kind", `String "Property")
          ; ("open_brace", `String "{")
          ; ("passthru_param", `String "")
          ; ( "params"
            , `String (print_xenobject_params obj classname true true false)
            )
          ; ( "dynamic_generator"
            , `String
                (print_dynamic_generator classname "Property" "Get"
                   messagesWithParams
                )
            )
          ; ("local_var", `String (ocaml_class_to_csharp_local_var classname))
          ; ("property", `String (ocaml_class_to_csharp_property classname))
          ; ( "cmdlet_methods_dynamic"
            , `String
                (print_cmdlet_methods_dynamic classname messages "Property"
                   "Get"
                )
            )
          ; ( "parse_method"
            , `String (print_parse_xenobject_private_method obj classname false)
            )
          ; ( "process_methods"
            , `String
                (print_process_record_private_methods classname messages "Get"
                   "pipe"
                )
            )
          ; ("messages_enum", `String (print_messages_as_enum "Get" messages))
          ; ( "dynamic_params"
            , `String
                (print_dynamic_params classname "Property" "Get"
                   messagesWithParams
                )
            )
          ]
      in
      render_to_string "Invoke-XenObject.mustache" json

and print_cmdlet_methods_dynamic classname messages enum commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let localVar = ocaml_class_to_csharp_local_var classname in
  match messages with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \                case Xen%s%s.%s:\n\
        \                    ProcessRecord%s(%s);\n\
        \                    break;%s"
        (ocaml_class_to_csharp_class classname)
        enum (cut_message_name hd) (cut_message_name hd) localVar
        (print_cmdlet_methods_dynamic classname tl enum commonVerb)

(**************************************)
(* Common to more than one generators *)
(**************************************)
and gen_passthru classname =
  sprintf
    "if (!PassThru)\n\
    \                return;\n\n\
    \            RunApiCall(() =>\n\
    \                {\n\
    \                    var contxt = _context as \
     XenServerCmdletDynamicParameters;\n\n\
    \                    if (contxt != null && contxt.Async)\n\
    \                    {\n\
    \                        XenAPI.Task taskObj = null;\n\
    \                        if (taskRef != null && taskRef != \
     \"OpaqueRef:NULL\")\n\
    \                        {\n\
    \                            taskObj = XenAPI.Task.get_record(session, \
     taskRef.opaque_ref);\n\
    \                            taskObj.opaque_ref = taskRef.opaque_ref;\n\
    \                        }\n\n\
    \                        WriteObject(taskObj, true);\n\
    \                    }\n\
    \                    else\n\
    \                    {\n\n\
    \                        var obj = %s.get_record(session, %s);\n\
    \                        if (obj != null)\n\
    \                            obj.opaque_ref = %s;\n\
    \                        WriteObject(obj, true);\n\n\
    \                    }\n\
    \                });"
    (qualified_class_name classname)
    (ocaml_class_to_csharp_local_var classname)
    (ocaml_class_to_csharp_local_var classname)

and is_message_with_dynamic_params classname message =
  let nonClassParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  if nonClassParams <> [] || message.msg_async then
    true
  else
    false

and print_dynamic_generator classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | [] ->
      ""
  | _ ->
      sprintf
        "\n\
        \        public override object GetDynamicParameters()\n\
        \        {\n\
        \            switch (Xen%s)\n\
        \            {%s\n\
        \                default:\n\
        \                    return null;\n\
        \            }\n\
        \        }\n"
        enum
        (print_messages_with_params classname enum commonVerb messagesWithParams)

and print_messages_with_params classname enum commonVerb x =
  match x with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \                case Xen%s%s.%s:\n\
        \                    _context = new Xen%s%s%sDynamicParameters();\n\
        \                    return _context;%s"
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        (print_messages_with_params classname enum commonVerb tl)

and print_dynamic_params classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \    public class Xen%s%s%sDynamicParameters : \
         IXenServerDynamicParameter\n\
        \    {%s%s\n\
        \    }\n\
         %s"
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        ( if hd.msg_async then
            "\n\
            \        [Parameter]\n\
            \        public SwitchParameter Async { get; set; }\n"
          else
            ""
        )
        (print_dynamic_param_members classname hd.msg_params commonVerb)
        (print_dynamic_params classname enum commonVerb tl)

and print_dynamic_param_members classname params commonVerb =
  match params with
  | [] ->
      ""
  | hd :: tl ->
      if is_class hd classname then
        print_dynamic_param_members classname tl commonVerb
      else
        let publicProperty =
          if
            commonVerb = "Invoke"
            && List.mem (String.lowercase_ascii hd.param_name) ["name"; "uuid"]
          then
            ocaml_class_to_csharp_property hd.param_name ^ "Param"
          else
            ocaml_class_to_csharp_property hd.param_name
        in
        let theType = obj_internal_type hd.param_type in
        sprintf "\n        [Parameter]\n        public %s %s { get; set; }\n%s "
          theType publicProperty
          (print_dynamic_param_members classname tl commonVerb)

and print_messages_as_enum commonVerb messages =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "\n        %s" (cut_message_name x)
  | hd :: tl ->
      sprintf "\n        %s,%s" (cut_message_name hd)
        (print_messages_as_enum commonVerb tl)

and gen_message_as_param classname commonVerb messages =
  match messages with
  | [] ->
      ""
  | hd :: tl ->
      let msgType = get_message_type hd classname commonVerb in
      let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
      let msgName =
        if cutMessageName = "Host" then
          "XenHost"
        else
          cutMessageName
      in
      sprintf
        "\n\
        \        [Parameter]\n\
        \        public %s %s\n\
        \        {\n\
        \            get { return %s; }\n\
        \            set\n\
        \            {\n\
        \                %s = value;\n\
        \                %sIsSpecified = true;\n\
        \            }\n\
        \        }\n\
        \        private %s %s;\n\
        \        private bool %sIsSpecified;\n\
         %s"
        msgType msgName
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        msgType
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        (gen_message_as_param classname commonVerb tl)

and print_cmdlet_methods classname messages commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let switch_name x =
    if cut_message_name x = "Host" then
      "XenHost"
    else
      cut_message_name x
  in
  let localVar = ocaml_class_to_csharp_local_var classname in
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "if (%sIsSpecified)\n                ProcessRecord%s(%s);"
        (lower_and_underscore_first (switch_name x))
        (cut_message_name x) localVar
  | hd :: tl ->
      sprintf
        "if (%sIsSpecified)\n\
        \                ProcessRecord%s(%s);\n\
        \            %s"
        (lower_and_underscore_first (switch_name hd))
        (cut_message_name hd) localVar
        (print_cmdlet_methods classname tl commonVerb)

and print_xenobject_params obj classname mandatoryRef includeXenObject
    includeUuidAndName =
  let publicName = ocaml_class_to_csharp_property classname in
  sprintf
    "%s\n\n\
    \        [Parameter(ParameterSetName = \"Ref\"%s, \
     ValueFromPipelineByPropertyName = true, Position = 0)]\n\
    \        [Alias(\"opaque_ref\")]\n\
    \        public XenRef<%s> Ref { get; set; }\n\
     %s%s\n"
    ( if includeXenObject then
        print_param_xen_object (qualified_class_name classname) publicName
      else
        ""
    )
    ( if mandatoryRef then
        ", Mandatory = true"
      else
        ""
    )
    (qualified_class_name classname)
    (print_param_uuid (has_uuid obj && includeUuidAndName))
    (print_param_name (has_name obj && includeUuidAndName))

and print_param_xen_object qualifiedClassName publicName =
  sprintf
    "\n\
    \        [Parameter(ParameterSetName = \"XenObject\", Mandatory = true, \
     ValueFromPipeline = true, Position = 0)]\n\
    \        public %s %s { get; set; }"
    qualifiedClassName publicName

and print_param_uuid hasUuid =
  if hasUuid then
    sprintf
      "\n\
      \        [Parameter(ParameterSetName = \"Uuid\", Mandatory = true, \
       ValueFromPipelineByPropertyName = true, Position = 0)]\n\
      \        public Guid Uuid { get; set; }\n"
  else
    sprintf ""

and print_param_name hasName =
  if hasName then
    sprintf
      "\n\
      \        [Parameter(ParameterSetName = \"Name\", Mandatory = true, \
       ValueFromPipelineByPropertyName = true, Position = 0)]\n\
      \        [Alias(\"name_label\")]\n\
      \        public string Name { get; set; }\n"
  else
    sprintf ""

and print_async_param asyncMessages =
  match asyncMessages with
  | [] ->
      ""
  | _ ->
      sprintf
        "\n\
        \        protected override bool GenerateAsyncParam\n\
        \        {\n\
        \            get\n\
        \            {\n\
        \                return %s;\n\
        \            }\n\
        \        }\n"
        (condition asyncMessages)

and condition messages =
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "%sIsSpecified" (lower_and_underscore_first x)
  | hd :: tl ->
      sprintf "%sIsSpecified\n                       ^ %s"
        (lower_and_underscore_first hd)
        (condition tl)

and get_message_type message classname commonVerb =
  let messageParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  match commonVerb with
  | "Remove" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Add" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | [x; y] ->
        sprintf "KeyValuePair<%s, %s>"
          (obj_internal_type x.param_type)
          (obj_internal_type y.param_type)
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Set" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | [x; y]
      when not (obj_internal_type x.param_type = obj_internal_type y.param_type)
      ->
        sprintf "KeyValuePair<%s, %s>"
          (obj_internal_type x.param_type)
          (obj_internal_type y.param_type)
    | hd :: tl ->
        let hdtype = obj_internal_type hd.param_type in
        if List.for_all (fun x -> hdtype = obj_internal_type x.param_type) tl
        then
          sprintf "%s[]" hdtype
        else (
          Printf.eprintf "%s" message.msg_name ;
          assert false
        )
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Get" -> (
    match messageParams with
    | [] ->
        "SwitchParameter"
    | [x] ->
        obj_internal_type x.param_type
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | _ ->
      ""

and print_parameter_sets parameterSets =
  match parameterSets with
  | [] ->
      "[Parameter]"
  | [x] ->
      sprintf "[Parameter(ParameterSetName = \"%s\")]" x
  | hd :: tl ->
      sprintf "[Parameter(ParameterSetName = \"%s\")]\n        %s" hd
        (print_parameter_sets tl)

and print_parse_xenobject_private_method obj classname includeUuidAndName =
  let publicProperty = ocaml_class_to_csharp_property classname in
  let localVar = ocaml_class_to_csharp_local_var classname in
  sprintf
    "\n\
    \        private string Parse%s()\n\
    \        {\n\
    \            string %s = null;\n\n\
    \            if (%s != null)\n\
    \                %s = (new XenRef<%s>(%s)).opaque_ref;%s%s\n\
    \            else if (Ref != null)\n\
    \                %s = Ref.opaque_ref;\n\
    \            else\n\
    \            {\n\
    \                ThrowTerminatingError(new ErrorRecord(\n\
    \                    new ArgumentException(\"At least one of the \
     parameters '%s', 'Ref'%s must be set\"),\n\
    \                    string.Empty,\n\
    \                    ErrorCategory.InvalidArgument,\n\
    \                    %s));\n\
    \            }\n\n\
    \            return %s;\n\
    \        }\n"
    publicProperty localVar publicProperty localVar
    (qualified_class_name classname)
    publicProperty
    ( if has_uuid obj && includeUuidAndName then
        sprintf
          "\n\
          \            else if (Uuid != Guid.Empty)\n\
          \            {\n\
          \                var xenRef = %s.get_by_uuid(session, \
           Uuid.ToString());\n\
          \                if (xenRef != null)\n\
          \                    %s = xenRef.opaque_ref;\n\
          \            }"
          (qualified_class_name classname)
          localVar
      else
        sprintf ""
    )
    ( if has_name obj && includeUuidAndName then
        sprintf
          "\n\
          \            else if (Name != null)\n\
          \            {\n\
          \                var xenRefs = %s.get_by_name_label(session, Name);\n\
          \                if (xenRefs.Count == 1)\n\
          \                    %s = xenRefs[0].opaque_ref;\n\
          \                else if (xenRefs.Count > 1)\n\
          \                    ThrowTerminatingError(new ErrorRecord(\n\
          \                        new ArgumentException(string.Format(\"More \
           than one %s with name label {0} exist\", Name)),\n\
          \                        string.Empty,\n\
          \                        ErrorCategory.InvalidArgument,\n\
          \                        Name));\n\
          \            }"
          (qualified_class_name classname)
          localVar
          (qualified_class_name classname)
      else
        sprintf ""
    )
    localVar publicProperty
    ( if has_uuid obj then
        sprintf ", 'Uuid'"
      else
        sprintf ""
    )
    publicProperty localVar

and print_process_record_private_methods classname messages commonVerb switch =
  match messages with
  | [] ->
      sprintf ""
  | hd :: tl ->
      let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
      sprintf
        "\n\
        \        private void ProcessRecord%s(string %s)\n\
        \        {%s%s\n\
        \            RunApiCall(()=>\n\
        \            {%s\n\
        \            });\n\
        \        }\n\
         %s"
        cutMessageName
        (ocaml_class_to_csharp_local_var classname)
        ""
        (gen_shouldprocess commonVerb hd classname)
        (gen_csharp_api_call hd classname commonVerb switch)
        (print_process_record_private_methods classname tl commonVerb switch)

and gen_shouldprocess commonVerb message classname =
  match commonVerb with
  | "Get" ->
      ""
  | _ ->
      let theObj =
        if classname = "pool" || commonVerb = "New" then
          "session.Url"
        else
          ocaml_class_to_csharp_local_var classname
      in
      sprintf
        "\n\
        \            if (!ShouldProcess(%s, \"%s.%s\"))\n\
        \                return;\n"
        theObj
        (exposed_class_name classname)
        message.msg_name

and gen_csharp_api_call message classname commonVerb switch =
  let asyncPipe = gen_csharp_api_call_async_pipe in
  let passThruTask =
    if switch = "pipe" then
      asyncPipe
    else if switch = "passthru" || switch = "asyncpassthru" then
      print_pass_thru asyncPipe
    else
      ""
  in
  let syncPipe = gen_csharp_api_call_sync_pipe message classname in
  let passThruResult =
    match (switch, message.msg_result) with
    | "pipe", _ ->
        syncPipe
    | "passthru", None ->
        "\n\
        \                    if (PassThru)\n\
        \                        WriteWarning(\"-PassThru can only be used \
         with -Async for this cmdlet. Ignoring.\");"
    | "passthru", _ ->
        print_pass_thru syncPipe
    | _ ->
        ""
  in
  if message.msg_async then
    sprintf
      "\n\
      \                var contxt = _context as %s;\n\n\
      \                if (contxt != null && contxt.Async)\n\
      \                {%s%s\n\
      \                }\n\
      \                else\n\
      \                {%s%s\n\
      \                }\n"
      ( if commonVerb = "Invoke" then
          sprintf "Xen%sAction%sDynamicParameters"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Invoke")
        else if commonVerb = "Get" then
          sprintf "Xen%sProperty%sDynamicParameters"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Get")
        else
          "XenServerCmdletDynamicParameters"
      )
      (gen_csharp_api_call_async message classname commonVerb)
      passThruTask
      (gen_csharp_api_call_sync message classname commonVerb)
      passThruResult
  else
    sprintf "%s%s%s"
      ( if
          commonVerb = "Invoke"
          && is_message_with_dynamic_params classname message
        then
          sprintf
            "\n\
            \                var contxt = _context as \
             Xen%sAction%sDynamicParameters;\n\
            \                if (contxt == null)\n\
            \                    return;"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Invoke")
        else if
          commonVerb = "Get" && is_message_with_dynamic_params classname message
        then
          sprintf
            "\n\
            \                var contxt = _context as \
             Xen%sProperty%sDynamicParameters;\n\
            \                if (contxt == null)\n\
            \                    return;"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Get")
        else
          ""
      )
      (gen_csharp_api_call_sync message classname commonVerb)
      passThruResult

and print_pass_thru x =
  sprintf
    "\n\
    \                    if (PassThru)\n\
    \                    {%s\n\
    \                    }"
    x

and gen_csharp_api_call_async message classname commonVerb =
  sprintf "\n                    taskRef = %s.async_%s(%s);\n"
    (qualified_class_name classname)
    message.msg_name
    (gen_call_params classname message commonVerb)

and gen_csharp_api_call_async_pipe =
  sprintf
    "\n\
    \                        XenAPI.Task taskObj = null;\n\
    \                        if (taskRef != \"OpaqueRef:NULL\")\n\
    \                        {\n\
    \                            taskObj = XenAPI.Task.get_record(session, \
     taskRef.opaque_ref);\n\
    \                            taskObj.opaque_ref = taskRef.opaque_ref;\n\
    \                        }\n\n\
    \                        WriteObject(taskObj, true);"

and gen_csharp_api_call_sync message classname commonVerb =
  match message.msg_result with
  | None ->
      sprintf "\n                    %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Ref _, _) ->
      sprintf "\n                    string objRef = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Set (Ref _), _) ->
      sprintf "\n                    var refs = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Map (_, _), _) ->
      sprintf "\n                    var dict = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (x, _) ->
      sprintf "\n                    %s obj = %s.%s(%s);\n" (exposed_type x)
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)

and gen_csharp_api_call_sync_pipe message classname =
  match message.msg_result with
  | None ->
      sprintf
        "\n\
        \                        var obj = %s.get_record(session, %s);\n\
        \                        if (obj != null)\n\
        \                            obj.opaque_ref = %s;\n\
        \                        WriteObject(obj, true);"
        (qualified_class_name classname)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_local_var classname)
  | Some (Ref r, _) ->
      sprintf
        "\n\
        \                        %s obj = null;\n\n\
        \                        if (objRef != \"OpaqueRef:NULL\")\n\
        \                        {\n\
        \                            obj = %s.get_record(session, objRef);\n\
        \                            obj.opaque_ref = objRef;\n\
        \                        }\n\n\
        \                        WriteObject(obj, true);"
        (qualified_class_name r) (qualified_class_name r)
  | Some (Set (Ref r), _) ->
      sprintf
        "\n\
        \                        var records = new List<%s>();\n\n\
        \                        foreach (var _ref in refs)\n\
        \                        {\n\
        \                            if (_ref.opaque_ref == \"OpaqueRef:NULL\")\n\
        \                                continue;\n\n\
        \                            var record = %s.get_record(session, _ref);\n\
        \                            record.opaque_ref = _ref.opaque_ref;\n\
        \                            records.Add(record);\n\
        \                        }\n\n\
        \                        WriteObject(records, true);"
        (qualified_class_name r) (qualified_class_name r)
  | Some (Map (_, _), _) ->
      sprintf
        "\n\
        \                        Hashtable ht = \
         CommonCmdletFunctions.ConvertDictionaryToHashtable(dict);\n\
        \                        WriteObject(ht, true);"
  | Some (_, _) ->
      sprintf "\n                        WriteObject(obj, true);"

and gen_call_params classname message commonVerb =
  String.concat ", "
    ("session" :: gen_param_list classname message.msg_params message commonVerb)

and gen_param_list classname params message commonVerb =
  let cutMessageName =
    cut_msg_name (ocaml_class_to_csharp_property message.msg_name) commonVerb
  in
  let get_param_name x =
    if is_class x classname then
      ocaml_class_to_csharp_local_var classname
    else if
      commonVerb = "Invoke"
      && List.mem (String.lowercase_ascii x.param_name) ["name"; "uuid"]
    then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
      ^ "Param"
    else if commonVerb = "Invoke" then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
    else if commonVerb = "Get" then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
    else if not (commonVerb = "New") then
      cutMessageName
    else
      ocaml_class_to_csharp_property x.param_name
  in
  let valueOfPair x =
    match x.param_type with
    | Map (u, v) ->
        sprintf
          "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s.Value)"
          (exposed_type u) (exposed_type v) cutMessageName
    | _ ->
        cutMessageName ^ ".Value"
  in
  let api_call_param x =
    match x.param_type with
    | Map (u, v) ->
        sprintf "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s)"
          (exposed_type u) (exposed_type v) (get_param_name x)
    | _ ->
        get_param_name x
  in
  let messageParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  let restParams =
    List.filter (fun x -> is_class x classname) message.msg_params
  in
  let procParams = List.map get_param_name restParams in
  match commonVerb with
  | "Remove" -> (
    match messageParams with
    | [] ->
        procParams
    | [x] ->
        procParams @ [api_call_param x]
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Add" -> (
    match messageParams with
    | [x] ->
        procParams @ [api_call_param x]
    | [_; y] ->
        procParams @ [cutMessageName ^ ".Key"; valueOfPair y]
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Set" -> (
    match messageParams with
    | [x] ->
        procParams @ [api_call_param x]
    | [x; y]
      when not (obj_internal_type x.param_type = obj_internal_type y.param_type)
      ->
        procParams @ [cutMessageName ^ ".Key"; valueOfPair y]
    | hd :: tl ->
        let argList = ref [] in
        let hdtype = obj_internal_type hd.param_type in
        if List.for_all (fun x -> hdtype = obj_internal_type x.param_type) tl
        then (
          explode_array cutMessageName (List.length messageParams) argList ;
          procParams @ !argList
        ) else (
          Printf.eprintf "%s" message.msg_name ;
          assert false
        )
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | _ -> (
    match params with
    | [] ->
        []
    | h :: tl ->
        api_call_param h :: gen_param_list classname tl message commonVerb
  )

and explode_array name length result =
  for i = length - 1 downto 0 do
    result := sprintf "%s[%s]" name (string_of_int i) :: !result
  done

and is_class param classname =
  String.lowercase_ascii param.param_name = "self"
  || String.lowercase_ascii param.param_name = String.lowercase_ascii classname

(*********************************************)
(* Get-Help (MAML external help) generation  *)
(*********************************************)
and gen_help () =
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
      let example =
        help_example
          (sprintf "PS> %s -XenHost \"myserver\" -Path \"%s\"%s" cmdlet
             ( if is_put then
                 "C:\\upload.dat"
               else
                 "C:\\download.dat"
             )
             uuid_arg
          )
          ( if is_put then
              "Uploads the contents of the local file to the server."
            else
              "Downloads from the server into the local file."
          )
      in
      help_command ~name:cmdlet ~synopsis ~description
        ~parameters:((delegate :: action_args) @ help_http_common_params ())
        ~shouldprocess:is_put ~outputs:["void"] ~examples:[example] ()
  )

(* Connect-XenServer, Disconnect-XenServer, Get-XenSession, Wait-XenTask,
   Receive-XenPoolPatch and Send-XenOemPatchStream are hand-written C# under
   autogen/src, so the generator does not know their parameters. They are left
   out of the help file on purpose: an entry that documented only their
   synopsis would replace the reflected syntax with an empty one and lose every
   parameter. Leaving them out keeps Get-Help falling back to reflection, which
   still describes them accurately. ConvertTo-XenRef is generated here, so it
   can be documented in full. *)
and help_handwritten () =
  [
    help_command ~name:"ConvertTo-XenRef"
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
          help_example "PS> Get-XenVM -Name \"Demo VM\" | ConvertTo-XenRef"
            "Converts a VM object into the XenRef that other cmdlets accept."
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
   ones have been put together, so nothing here has to know its position. *)
and help_example code remarks = (code, remarks)

and help_example_json n (code, remarks) =
  `O
    [
      ( "title"
      , `String
          (escape_xml
             (sprintf
                "-------------------------- Example %d \
                 --------------------------"
                n
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

(* A plausible literal for an example, chosen from the parameter's C# type so
   that the line reads like something a caller would actually type. *)
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
  | _ ->
      "$value"

(* The first message of a family, used to name a representative field or
   operation in that family's example. *)
and help_first_message verb messages =
  match messages with
  | m :: _ ->
      Some (cut_msg_name (pascal_case m.msg_name) verb, m)
  | [] ->
      None

and help_command ~name ~synopsis ~description ?(parameters = [])
    ?(common = true) ?(shouldprocess = false) ?async ?(outputs = [])
    ?(examples = []) () =
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
  (* Where a cmdlet has curated examples, keep only the first generated one as
     the plain form and let the curated ones carry the rest; the generated
     asynchronous example would otherwise repeat one of them. *)
  let curated = Curated_examples.for_cmdlet name in
  let all_examples =
    if curated = [] then
      examples
    else (
      curated_used := name :: !curated_used ;
      (match examples with e :: _ -> [e] | [] -> []) @ curated
    )
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
  let class_desc =
    if obj.description = "" then
      sprintf "The %s class." stem
    else
      obj.description
  in
  let getter =
    if List.mem classname classes_with_records then
      let ex =
        help_example
          (sprintf "PS> Get-Xen%s" stem)
          (sprintf "Retrieves all %s objects from the server." stem)
        ::
        ( if has_name obj || has_uuid obj then
            [
              help_example
                (sprintf "PS> Get-Xen%s %s" stem (help_selector obj classname))
                (sprintf "Retrieves a single %s." stem)
            ]
          else
            []
        )
      in
      [
        help_command ~name:(sprintf "Get-Xen%s" stem)
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
          help_command ~name:(sprintf "New-Xen%s" stem)
            ~synopsis:(sprintf "Creates a new %s object." stem)
            ~description:
              ( if m.msg_doc = "" then
                  sprintf "Creates a new %s." stem
                else
                  m.msg_doc
              )
              (* The Hashtable set is shown rather than the field set: it is
                 the one form that reads the same for every class, whatever
                 fields the class happens to have. *)
            ~examples:
              [
                help_example
                  (sprintf
                     "PS> New-Xen%s -HashTable @{ name_label = \"Demo %s\" } \
                      -PassThru"
                     stem stem
                  )
                  (sprintf
                     "Creates a %s from a hashtable of field names to values. \
                      Use Get-Help New-Xen%s -Full to see the field parameters \
                      that can be given instead."
                     stem stem
                  )
              ]
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
        let examples =
          match help_first_message verb ms with
          | None ->
              []
          | Some (field, m) ->
              let value =
                help_value_placeholder ~verb (get_message_type m classname verb)
              in
              let one =
                if verb = "Set" then
                  help_operate_on obj classname
                    (sprintf "Set-Xen%s -%s %s" stem field value)
                else
                  sprintf "PS> %s-Xen%s%s %s -%s %s" verb stem suffix
                    (help_selector obj classname)
                    field value
              in
              [
                help_example one
                  (sprintf "%s the %s field of a %s."
                     ( match verb with
                     | "Set" ->
                         "Sets"
                     | "Add" ->
                         "Adds a value to"
                     | _ ->
                         "Removes a value from"
                     )
                     field stem
                  )
              ]
        in
        [
          help_command
            ~name:(sprintf "%s-Xen%s%s" verb stem suffix)
            ~synopsis ~description:descr ~examples
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
        let lines =
          List.map
            (fun m ->
              sprintf "%s: %s"
                (cut_msg_name (pascal_case m.msg_name) verb)
                m.msg_doc
            )
            ms
        in
        let description = String.concat "\n" (intro :: lines) in
        let async = List.exists (fun m -> m.msg_async) ms in
        let actions =
          List.map (fun m -> cut_msg_name (pascal_case m.msg_name) verb) ms
        in
        (* Prefer an operation that takes no runtime parameters, so the example
           stands on its own rather than needing a -XenAction-specific
           parameter the reader has not met yet. Falling back to ms, which is
           non-empty in this branch, means the match is total without a
           partial head. *)
        let representative =
          let plain =
            List.filter
              (fun m -> not (is_message_with_dynamic_params classname m))
              ms
          in
          match plain @ ms with
          | m :: _ ->
              cut_msg_name (pascal_case m.msg_name) verb
          | [] ->
              ""
        in
        (* Get-Xen<Class>Property takes neither -Name/-Uuid nor -PassThru; only
           Invoke-Xen<Class> has them, so only it gets the asynchronous
           example. *)
        let include_uuid_name = verb = "Invoke" in
        let examples =
          [
            help_example
              (help_operate_on ~include_uuid_name obj classname
                 (sprintf "%s-Xen%s%s -Xen%s %s" verb stem suffix enum_param
                    representative
                 )
              )
              ( if verb = "Invoke" then
                  sprintf "Invokes the %s operation on a %s." representative
                    stem
                else
                  sprintf "Gets the %s property of a %s." representative stem
              )
          ]
          @
          if async && verb = "Invoke" then
            [
              help_example
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
            ~synopsis ~description ~examples
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
            ~synopsis:(sprintf "Deletes a %s object." stem)
            ~description:
              ( if m.msg_doc = "" then
                  sprintf "Destroys the specified %s object." stem
                else
                  m.msg_doc
              )
            ~parameters:
              (help_identity_params obj classname ~mandatory_ref:true
                 ~include_xenobject:true ~include_uuid_name:true
              @ [help_passthru ()]
              )
            ~examples:
              [
                help_example
                  (help_operate_on obj classname (sprintf "Remove-Xen%s" stem))
                  (sprintf "Deletes a %s." stem)
              ]
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

let _ = main ()
