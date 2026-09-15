(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Worked examples for the handful of cmdlets where the generated one-liner
   does not teach enough.

   gen_powershell_binding gives every cmdlet an example built from its family's
   idiom, which is right for the bulk of the SDK but cannot show why you would
   reach for a thing. The entries here are deliberately a small selection: the
   ones that carry an idiom a reader would otherwise have to work out - the
   asynchronous form, a dynamic parameter taking an object, a field that is
   written from one side and read from the other.

   These are appended after the generated example, so each cmdlet reads from
   the simplest form to the most involved.

   KEEPING THEM HONEST

   Hand-written examples rot quietly as the datamodel moves, so nothing here is
   taken on trust:

     - test_gen_powershell checks every entry in this file: that it names a
       cmdlet once, that the snippet is written at a prompt and invokes the
       cmdlet it is filed under, and that it carries an explanation;
     - gen_powershell_binding fails to build if an entry names a cmdlet it does
       not generate, so a renamed or withdrawn cmdlet cannot leave a stale
       entry behind;
     - verify-help.ps1 parses every example against the built module and fails
       CI if a cmdlet, a parameter or an enum value it names has gone.

   So the way to find out that an example here is out of date is that the build
   stops, not that a reader is misled. Adding one is just a matter of appending
   to the list; the checks apply to it automatically. *)

(* cmdlet name, then (code, explanation) pairs. *)
let examples : (string * (string * string) list) list =
  [
    ( "Invoke-XenVM"
    , [
        ( "PS> $template | Invoke-XenVM -XenAction Clone -NewName \"my-vm\" \
           -PassThru"
        , "Pipes a template straight into the Clone action. -PassThru returns \
           the new VM object, so it can be piped onwards."
        )
      ; ( "PS> $vm | Invoke-XenVM -XenAction Start -Async -PassThru | \
           Wait-XenTask -ShowProgress"
        , "Starts the VM asynchronously. -Async -PassThru emits a Task, which \
           pipes straight into Wait-XenTask; -ShowProgress reports progress \
           rather than blocking silently. This is the idiom for any \
           long-running operation."
        )
      ; ( "PS> $vm | Invoke-XenVM -XenAction Revert -Snapshot $snapshot"
        , "Reverts a VM to a snapshot. The VM is piped in and the snapshot to \
           restore is given by -Snapshot, one of the parameters this cmdlet \
           adds for the Revert action."
        )
      ]
    )
  ; ( "Get-XenVM"
    , [
        ( "PS> Get-XenVM | Where-Object { $_.is_a_template } | Select-Object \
           -First 1"
        , "Templates are VM objects with is_a_template set, so they are found \
           by filtering the VMs rather than by a separate cmdlet."
        )
      ; ( "PS> Get-XenVM | Where-Object { $_.power_state -eq \"Running\" }"
        , "Filters the returned objects on any field of the XenAPI record."
        )
      ]
    )
  ; ( "Set-XenVM"
    , [
        ( "PS> $vm | Set-XenVM -NameLabel \"new name\" -NameDescription \"new \
           description\""
        , "Sets more than one field in a single synchronous call. Only one \
           field at a time can be set with -Async."
        )
      ]
    )
  ; ( "Set-XenNetwork"
    , [
        ( "PS> $network | Set-XenNetwork -MTU 9000"
        , "Raises the MTU to use jumbo frames. Every NIC and switch along the \
           path has to agree, which is why this belongs to the network rather \
           than to any one interface."
        )
      ]
    )
  ; ( "Invoke-XenVBD"
    , [
        ( "PS> Invoke-XenVBD -Ref $vbd.opaque_ref -XenAction Unplug"
        , "Deactivates a disk by unplugging its VBD. The disk stays attached \
           to the VM; it is only no longer live."
        )
      ]
    )
  ; ( "Invoke-XenVDI"
    , [
        ( "PS> Invoke-XenVDI -Ref $vdi.opaque_ref -XenAction ResizeOnline \
           -Size $newSize"
        , "Grows a disk that is attached to a running VM. Resizing a live disk \
           is a different action from resizing an offline one, not a flag on \
           the same one."
        )
      ]
    )
  ; ( "Invoke-XenPool"
    , [
        ( "PS> Invoke-XenPool -Ref $pool.opaque_ref -XenAction RotateSecret"
        , "Rotates the pool secret. Existing sessions survive it, so a running \
           script carries on without reconnecting."
        )
      ]
    )
  ; ( "Get-XenVMGroup"
    , [
        ( "PS> (Get-XenVMGroup -Ref $group.opaque_ref).VMs"
        , "Group membership is written from the VM side with Set-XenVM \
           -Groups, and read back from the group side like this - the half \
           that is easy to get wrong."
        )
      ]
    )
  ]

(* Looked up by cmdlet name; [] when the generated example is enough. *)
let for_cmdlet name =
  match List.assoc_opt name examples with Some l -> l | None -> []

let cmdlet_names = List.map fst examples
