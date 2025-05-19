(** Various functions that copy one type in the upstream ast to the same type in the
    ocamlformat ast, and vice versa. *)

module P = Parsetree

module Fmast : sig
  include module type of Ocamlformat_ocaml_common
  include module type of Ocamlformat_parser_extended
  module P = Parsetree
end

val location : Location.t -> Fmast.Location.t
val location' : Fmast.Location.t -> Location.t
val located : ('a -> 'b) -> 'a Location.loc -> 'b Fmast.Location.loc
val located' : ('a -> 'b) -> 'a Fmast.Location.loc -> 'b Location.loc
val longident' : Fmast.Longident.t -> Longident.t
val longident : Longident.t -> Fmast.Longident.t
val arg_label : Asttypes.arg_label -> Fmast.Asttypes.arg_label

(** These ast converting functions used to be implemented as simply print/parse, which is
    much easier. The problem is that locations are lost when doing this, which is not easy
    to improve. And we need locations to match between asts when we lookup fmast locations
    in a Build.Type_index.t, which is built from a typedtree, which itself is built from
    an uast, which may either come from parsing the same string as for creating the fmast
    (in which case, everything is good either method), or by conversion of the uast into
    fmast (as happens when typing repl expressions on the fly, in which case the loss of
    location means types are unfindable). *)

val expr : P.expression -> Fmast.P.expression
val expr' : Fmast.P.expression -> P.expression
