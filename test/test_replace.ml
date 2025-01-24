(* Comment to check that comments don't randomly move. *)

open Base

let _ = List.map ~f:(fun x -> x + 1) [ 2 ]
[@@migrate_test.replace let _ = List.map' ~f:(fun x -> x + 1) [ 2 ]]

let _ = List.filter_map ~f:(fun x -> Some (x + 1)) [ 2 ]
[@@migrate_test.replace let _ = List.filter_map' [ 2 ] ~f:(fun x -> Some (x + 1))]

let _ =
  let open Stdlib in
  List.memq (ref 3) [ ref 1; ref 2 ]
[@@migrate_test.replace
  let _ =
    let open Stdlib in
    List.mem ~eq:Stdlib.( == ) (ref 3) [ ref 1; ref 2 ]]

(* Replace List.mapi by List.map to observe that the comment is moved. I suspect the
   problem is that as we replace List.map ~f:__f __l by List.map __l ~f:__f, the position
   for the function application is not transferred to the new function application. *)
let _ =
  (* comment *)
  List.mapi ~f:(fun _ _ -> ()) []
