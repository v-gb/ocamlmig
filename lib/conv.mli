(** Various functions that copy one type in the upstream ast to the same type in the
    ocamlformat ast, and vice versa. *)

module Fmast : sig
  include module type of Ocamlformat_ocaml_common
  include module type of Ocamlformat_parser_extended
end

val location : Location.t -> Fmast.Location.t
val location' : Fmast.Location.t -> Location.t
val located : ('a -> 'b) -> 'a Location.loc -> 'b Fmast.Location.loc
val located' : ('a -> 'b) -> 'a Fmast.Location.loc -> 'b Location.loc
val longident' : Fmast.Longident.t -> Longident.t
val longident : Longident.t -> Fmast.Longident.t
val arg_label : Asttypes.arg_label -> Fmast.Asttypes.arg_label
