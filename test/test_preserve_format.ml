(* comment 1 *)
let _ =
  Fun.id
    ((* comment 2 *) 1
    +
    (* comment 3 *)
    2 (* comment 4 *))
(* comment 5 *)

(* Result:

(* comment 1 *)
let _ =
  1 + 2
(* comment 5 *)

*)
