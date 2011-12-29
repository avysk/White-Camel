(* Ocamlbuild plugin to create version.ml file *)

open Ocamlbuild_plugin

let get_version = "if [ -z \"$(git diff-index --name-only HEAD)\" ] ; then echo \"let version = \\\"$(git rev-parse HEAD)\\\"\" ; else echo \"let version = \\\"$(git rev-parse HEAD)-modified\\\"\"; fi >"

let create_version_file _ _ = Cmd (S [ Sh get_version ; P "version.ml" ])

let () = dispatch begin function
  | After_rules ->
      rule "version.ml" ~prod: "version.ml" create_version_file
  | _ -> ()
end

