(* comment *)
[@@@ocamlformat "disable"]

open Format

let  _  = print_string

(* A couple of tricky cases involving parsing changing behavior when removing
   a structure item. The first case actually causing an internal exception, so
   it has to be commented out. *)
(* include struct let () = (); open! Format let () = () end *)
include struct let () = ();open! Format;;1 end

(* When a constructor is changed without updating its payload, we reprint only the
   constructor, not the payload. We do this by treating "Foo a" as two expressions
   Foo and a. But Foo is unlike a proper expression, as it cannot be parenthesized
   (doing so would cause a parse error in patterns, or a type error in expressions).
   This expression tests that we don't choke in this case. *)
let _ = function Output_string _ -> () | _ -> ()

(* Result:

(* comment *)
[@@@ocamlformat "disable"]


let  _  = Format.print_string

(* A couple of tricky cases involving parsing changing behavior when removing
   a structure item. The first case actually causing an internal exception, so
   it has to be commented out. *)
(* include struct let () = (); open! Format let () = () end *)
include struct let () = (); ;;1 end

(* When a constructor is changed without updating its payload, we reprint only the
   constructor, not the payload. We do this by treating "Foo a" as two expressions
   Foo and a. But Foo is unlike a proper expression, as it cannot be parenthesized
   (doing so would cause a parse error in patterns, or a type error in expressions).
   This expression tests that we don't choke in this case. *)
let _ = function Format.Output_string _ -> () | _ -> ()

*)
