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

type record =
  { field1 : int
  ; field2 : string
  }

let _ =
  let field1 = 1 in
  ignore { field1; field2 = "a" }
[@@migrate_test.replace
  let _ =
    let field1 = 1 in
    ignore { field3 = field1; field2 = "a" }]

module _ = struct
  (* [%move_def] *)

  module M = struct
    let _x = 1
  end

  open M

  (* [%move_def] handling of scoping changes *)
  module T1 = struct
    let _y = 1
    let def = 1 (* a *) + _x + _y
    let _x, _y = (2, 2)
    let _ = def
  end
  [@@migrate_test.replace
    module T1 = struct
      let _y = 1
      let _x, _y = (2, 2)

      let _ =
        1
        + M._x
        +
        ("MIG: variable _y is bound to different value";
         _y)
    end]

  (* [%move_def] inlining ghost expressions *)
  module T2 = struct
    let def () = 1
    let _ = def
  end
  [@@migrate_test.replace
    module T2 = struct
      let _ = fun () -> 1
    end]

  (* Nested [%move_defs] don't work. *)
  module T3 = struct
    let def = 1
    let def2 = def
    let _ = def2
  end
  [@@migrate_test.replace
    module T3 = struct
      let _ = def
    end]
end

(* Replace List.mapi by List.map to observe that the comment is moved. I suspect the
   problem is that as we replace List.map ~f:__f __l by List.map __l ~f:__f, the position
   for the function application is not transferred to the new function application. *)
let _ =
  (* comment *)
  List.mapi ~f:(fun _ _ -> ()) []
