(* comment *)
[@@@ocamlformat "disable"]

open Format

let  _  = print_string

(* A couple of tricky cases involving parsing changing behavior when removing
   a structure item. The first case actually causing an internal exception, so
   it has to be commented out. *)
(* include struct let () = (); open! Format let () = () end *)
include struct let () = ();open! Format;;1 end

(* Result:

(* comment *)
[@@@ocamlformat "disable"]


let  _  = Format.print_string

(* A couple of tricky cases involving parsing changing behavior when removing
   a structure item. The first case actually causing an internal exception, so
   it has to be commented out. *)
(* include struct let () = (); open! Format let () = () end *)
include struct let () = (); ;;1 end

*)
