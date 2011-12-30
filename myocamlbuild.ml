(* Ocamlbuild plugin to create version.ml file *)

open Ocamlbuild_plugin

(* We want to make sure version.ml is rebuilt every time. For that we need a
 * command which will be different every time when we run ocamlbuild. Let's use
 * something with current time for that. *)

open Unix

let time_cmd = Printf.sprintf "echo \"Building at %f seconds since 01.01.1970.\" &&" (Unix.time ())

(* let get_version = "if [ -z \"$(git diff-index --name-only HEAD)\" ] ; then
  * echo \"let version = \\\"$(git rev-parse HEAD)\\\"\" ; else echo \"let
  * version = \\\"$(git rev-parse HEAD)-modified\\\"\"; fi >" *)

let create_version_file _ _ = Cmd (S [ Sh time_cmd ; A "sh" ; P "../scripts/find-version.sh" ; Sh ">" ; P "version.ml" ])

let () = dispatch begin function
  | After_rules ->
      rule "version.ml" ~prod: "version.ml" create_version_file
  | _ -> ()
end

