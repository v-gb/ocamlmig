(* Comment to check that comments don't randomly move. *)

open Base

let _ = List.map ~f:(fun x -> x + 1) [ 2 ]
[@@migrate_test.replace_expr {| List.map ~f:__f __l /// List.map' __l ~f:__f |}]
[@@migrate_test.replace let _ = List.map' ~f:(fun x -> x + 1) [ 2 ]]

let _ = List.map ~f:(fun x -> Some (x + 1)) [ 2 ]
[@@migrate_test.replace_expr
  {| List.map ~f:__f __l /// List.map' __l ~f:__f [@reorder] |}]
[@@migrate_test.replace let _ = List.map' [ 2 ] ~f:(fun x -> Some (x + 1))]

let _ : _ = List.map ~f:(fun x -> Some (x + 1))
[@@migrate_test.replace_expr
  {| List.map ~f:__f __l /// List.map' __l ~f:__f [@reorder] |}]
[@@migrate_test.replace let _ : _ = List.map ~f:(fun x -> Some (x + 1))]

let _ =
  let open Stdlib in
  List.memq (ref 3) [ ref 1; ref 2 ]
[@@migrate_test.replace_expr {| List.memq __etc /// List.mem ~eq:Stdlib.(==) __etc |}]
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
[@@migrate_test.replace_expr
  {| { field1 = __field1; __etc } /// { __etc; field3 = __field1 } |}]
[@@migrate_test.replace
  let _ =
    let field1 = 1 in
    ignore { field3 = field1; field2 = "a" }]

module _ = struct
  (* type coercions *)
  let coercion = None
  let _ = coercion
  let _ = (coercion : int option)
  let _ = (coercion : string option)
end
[@@migrate_test.replace_expr {| (coercion : Stdlib.Int.t option) /// Some 1 |}]
[@@migrate_test.replace
  module _ = struct
    let coercion = None
    let _ = coercion
    let _ = (Some 1 : int option)
    let _ = (coercion : string option)
  end]

module _ = struct
  (* [%move_def] *)

  module M = struct
    let _x = 1
  end

  open M

  (* [%move_def] handling of scoping changes *)
  module _ = struct
    let _y = 1
    let def = 1 (* a *) + _x + _y
    let _x, _y = (2, 2)
    let _ = def
  end
  [@@migrate_test.replace_expr {| def & [%move_def __def] /// __def |}]
  [@@migrate_test.replace
    module _ = struct
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
  module _ = struct
    let def () = 1
    let _ = def
  end
  [@@migrate_test.replace_expr {| def & [%move_def __def] /// __def |}]
  [@@migrate_test.replace
    module _ = struct
      let _ = fun () -> 1
    end]

  (* [%move_def] handling of scoping changes, even when destructing the inside of
     the %move_def. *)
  module _ = struct
    let _y = 1
    let def z = 1 (* a *) + _x + _y + z
    let _x, _y = (2, 2)
    let _ = def
  end
  [@@migrate_test.replace_expr
    {| def & [%move_def (fun __p -> __e)] /// (fun bla -> let __p = bla in __e) |}]
  [@@migrate_test.replace
    module _ = struct
      let _y = 1
      let _x, _y = (2, 2)

      let _ =
       fun bla ->
        let z = bla in
        1
        + M._x
        + ("MIG: variable _y is bound to different value";
           _y)
        + z
    end]

  (* Nested [%move_defs] don't work. *)
  module _ = struct
    let def = 1
    let def2 = def
    let _ = def2
  end
  [@@migrate_test.replace_expr {| [%move_def __def] /// __def |}]
  [@@migrate_test.replace
    module _ = struct
      let _ = def
    end]

  (* [%move_def] from a different module. *)
  module _ = struct
    module M = struct
      let def = 1
    end

    let _ = M.def
  end
  [@@migrate_test.replace_expr {| [%move_def __def] /// __def |}]
  [@@migrate_test.replace
    module _ = struct
      module M = struct end

      let _ = 1
    end]
end

module _ = struct
  (* n-ary replaces *)
  let nary _ = ()

  let _ = fun a b c d e -> (a, b, c, d, e)
  [@@migrate_test.replace_expr {| (fun __p1 __p2 __p3 -> __body) /// __body |}]
  [@@migrate_test.replace let _ = (a, b, c, d, e)]

  let const _ = ()
  let ( $ ) _ _ = ()

  let _ = nary (const 1 $ 2 $ 3 $ 5 $ 5 $ 6 $ 7)
  [@@migrate_test.replace_expr {| nary (const __f $ __e1 $ __e2 $ __e3) /// __f |}]
  [@@migrate_test.replace let _ = 1]

  module _ = struct
    let _ = nary (const (fun a b c d e -> (a, b, c, d, e)) $ 2 $ 3 $ 5 $ 5 $ 6 $ 7)
    let _ = nary (const (fun a b c d e -> (a, b, c, d, e)) $ 2 $ 3 $ 5 $ 5 $ 6)
  end
  [@@migrate_test.replace_expr
    {| nary (const (fun __p1 __p2 __p3 -> __body) $ __e1 $ __e2 $ __e3) /// __body |}]
  [@@migrate_test.replace
    module _ = struct
      let _ = nary (const (fun a b c d e -> (a, b, c, d, e)) $ 2 $ 3 $ 5 $ 5 $ 6 $ 7)
      let _ = (a, b, c, d, e)
    end]

  let _ = nary (const (fun a b c d e -> (a, b, c, d, e)) $ 2 $ 3 $ 5 $ 5 $ 6)
  [@@migrate_test.replace_expr
    {| nary (const (fun __p1 __p2 __p3 -> __body) $ __e1 $ __e2 $ __e3) /// let+ __p1 = __e1 and+ __p2 = __e2 and+ __p3 = __e3 in __body |}]
  [@@migrate_test.replace
    let _ =
      let+ a = 2 and+ b = 3 and+ c = 5 and+ d = 5 and+ e = 6 in
      (a, b, c, d, e)]

  let _ = nary (((((), Fn.id 1), Fn.id 2), Fn.id 3), Fn.id 4)
  [@@migrate_test.replace_expr
    {| nary ((((), Fn.id __e1), Fn.id __e2), Fn.id __e3) /// let+ _ = __e1 and+ _ = __e2 and+ _ = __e3 in () |}]
  [@@migrate_test.replace let _ = let+ _ = 1 and+ _ = 2 and+ _ = 3 and+ _ = 4 in

                                  ()]
end

(* Replace List.mapi by List.map to observe that the comment is moved. I suspect the
   problem is that as we replace List.map ~f:__f __l by List.map __l ~f:__f, the position
   for the function application is not transferred to the new function application. *)
let _ =
  (* comment *)
  List.mapi ~f:(fun _ _ -> ()) []
