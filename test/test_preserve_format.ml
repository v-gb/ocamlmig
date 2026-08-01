[@@@ocamlformat "disable"]

(* comments should be preserved *)
  let _ =   0   + Fun.id
      ((* comment 2 *) 1
      +
      (* comment 3 *)
      2 (* comment 4 *))
(* comment 5 *)

(* parens should be inserted where necessary *)

let _ = ignore "hello"
let () =
  let e = 1 in
  let ( .!{} ) _ _ = () in
  (fun _ _ _ _ -> ()) 1.(Fun.id 1)1(Fun.id None).!{0};
  (fun _ _ _ _ _ -> ()) 1.(Fun.id e)1(Fun.id None)(Fun.id e);
  ignore (match 1 with 1 -> 1 + Fun.id (match () with () -> 2) | _ -> 3);
  ignore (1 + Fun.id (match () with () -> 2));
  let module type S = sig type t val x : t end in
  let module M = struct let () = Fun.id () module type S = S end in
  let f (module X : M.S) : X.t = Fun.id X.x in
  let () = f (module struct type t = unit let x = () end) in
  ()

(* Result:

[@@@ocamlformat "disable"]

(* comments should be preserved *)
  let _ =   0   + ((* comment 2 *) 1
                  +
                  (* comment 3 *)
                  2 (* comment 4 *))
(* comment 5 *)

(* parens should be inserted where necessary *)

let _ = ignore (();
               "hello")
let () =
  let e = 1 in
  let ( .!{} ) _ _ = () in
  (fun _ _ _ _ -> ()) 1. 1 1(None).!{0};
  (fun _ _ _ _ _ -> ()) 1. e 1 None e;
  ignore (match 1 with 1 -> 1 + (match () with () -> 2) | _ -> 3);
  ignore (1 + match () with () -> 2);
  let module type S = sig type t val x : t end in
  let module M = struct let () = () module type S = S end in
  let f (module X : M.S) : X.t = X.x in
  let () = f (module struct type t = unit let x = () end) in
  ()

*)
