(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)

(* Ocamlbuild plugin to create version.ml file *)

open Ocamlbuild_plugin

(* We want to make sure version.ml is rebuilt every time. For that we need a
 * command which will be different every time when we run ocamlbuild. Let's use
 * something with current time for that. *)

open Unix

let time_cmd = Printf.sprintf "echo \"Building at %f seconds since 01.01.1970.\" &&" (Unix.time ())

let create_version_file _ _ = Cmd (S [ Sh time_cmd ; A "sh" ; P "../scripts/find-version.sh" ; Sh ">" ; P "version.ml" ])

let () = dispatch begin function
  | After_rules ->
    rule "version.ml" ~prod: "version.ml" create_version_file ;
    dispatch_default After_rules
  | x -> dispatch_default x
  end
;;

(* See https://github.com/ocaml/ocamlbuild/issues/236 *)
begin
  let ccflag ~lang ~phase ~flag =
    pflag [lang; phase] flag (fun param -> S [A ("-"^flag); A param])
  in
  ["c"; "ocaml"] |> List.iter (fun lang ->
      ["compile"; "link"] |> List.iter (fun phase ->
          ["cc"; "ccopt"; "cclib"] |> List.iter (fun flag ->
              ccflag ~lang ~phase ~flag)))
end;;
