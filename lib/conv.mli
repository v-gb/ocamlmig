(** Various functions that copy one type in the upstream ast to the same type in the
    ocamlformat ast, and vice versa. *)

module Fmast : sig
  include module type of Ocamlformat_ocaml_common
  include module type of Ocamlformat_parser_extended
  module P = Parsetree
end

module Uast : sig
  module Location = Location
  module Longident = Longident
  module Asttypes = Asttypes
  module P = Parsetree
end

(** The ast converting functions used to be implemented as simply print/parse, which is
    much easier. The problem is that locations are lost when doing this, which is not easy
    to improve. And we need locations to match between asts when we lookup fmast locations
    in a Build.Type_index.t, which is built from a typedtree, which itself is built from
    an uast, which may either come from parsing the same string as for creating the fmast
    (in which case, everything is good either method), or by conversion of the uast into
    fmast (as happens when typing repl expressions on the fly, in which case the loss of
    location means types are unfindable). *)

module Fmu : sig
  module From = Uast
  module To = Fmast

  val location : From.Location.t -> To.Location.t
  val located : ('a -> 'b) -> 'a From.Location.loc -> 'b To.Location.loc
  val longident : From.Longident.t -> To.Longident.t
  val arg_label : From.Asttypes.arg_label -> To.Asttypes.arg_label
  val expr : From.P.expression -> To.P.expression
end

module Ufm : sig
  module From = Fmast
  module To = Uast

  val location : From.Location.t -> To.Location.t
  val located : ('a -> 'b) -> 'a From.Location.loc -> 'b To.Location.loc
  val longident : From.Longident.t -> To.Longident.t
  val arg_label : From.Asttypes.arg_label -> To.Asttypes.arg_label
  val expr : From.P.expression -> To.P.expression
end
