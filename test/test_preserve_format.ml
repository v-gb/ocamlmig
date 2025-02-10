[@@@ocamlformat "disable"]

(* comment 1 *)
  let _ =   0   + Fun.id
      ((* comment 2 *) 1
      +
      (* comment 3 *)
      2 (* comment 4 *))
(* comment 5 *)

(* Result:

[@@@ocamlformat "disable"]

(* comment 1 *)
  let _ =   0   + (* comment 2 *) 1
                  +
                  (* comment 3 *)
                  2 (* comment 4 *)
(* comment 5 *)

*)
