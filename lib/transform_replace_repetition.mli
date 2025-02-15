(** A submodule of Transform_replace. It is focused on inferring the repetition expressed
    implicitly in a motif (if any), and expressing explicitly.

    Given a motif like [const (fun __p1 __p2 __p3 -> __body) $ __e1 $ __e2 $ __e3], this
    code is what's responsible for expression the repetition explicitly as:
    [[%repeat [%acc const [%repeat2 fun [%X __p] -> __body] $ __e1] $ [%X __e]]]

    We try this repetition inference rather than asking the user to specify the pattern in
    an arity-generic way, because the syntax for specifying repeting fragments of AST is
    likely to get unwieldy and obscure, making it hard to learn. On the other hand,
    writing the size-3 version of a motif is easy, and probably still more readable than
    the generic version.

    Only a single repetition is supported, that is all the variables match series of the
    same length. *)

module P := Fmast.Parsetree

val x_var : [< `Expr of P.expression | `Pat of P.pattern ] -> string option
val infer : P.expression -> P.expression

val repeat2_template :
     P.expression
  -> Ocamlformat_parser_extended.Ast_mapper.mapper
  -> count:int
  -> env_pat:(string -> P.pattern)
  -> env_exp:(string -> P.expression)
  -> P.expression
