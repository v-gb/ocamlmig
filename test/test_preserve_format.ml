[@@@ocamlformat "disable"]

(* comments should be preserved *)
  let _ =   0   + Fun.id
      ((* comment 2 *) 1
      +
      (* comment 3 *)
      2 (* comment 4 *))
(* comment 5 *)

(* parens should be inserted where necessary, not currently implemented *)

let _ = ignore "hello"

(* Result:

[@@@ocamlformat "disable"]

(* comments should be preserved *)
  let _ =   0   + (* comment 2 *) 1
                  +
                  (* comment 3 *)
                  2 (* comment 4 *)
(* comment 5 *)

(* parens should be inserted where necessary *)

let _ = ignore ();
               "hello"

*)
