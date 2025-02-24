open Base
open! Stdio
open Common

let format ~path ~contents =
  Profile.record "dyn_fmast" (fun () ->
      with_str_in_file contents (fun contents_path ->
          run_process
            [ "ocamlformat"
            ; "--name=" ^ Cwdpath.to_string path
            ; no_dash (Abspath.to_string contents_path)
            ]))
