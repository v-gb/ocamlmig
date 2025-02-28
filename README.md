ocamlmig is a command line tool for rewriting ocaml source code with access to scope
and type information.

As a simple example of what it can do, let's say an opam-installed library A provides
this interface:

```ocaml
val new_name : int -> int

val old_name : int -> int
[@@migrate { repl = Rel.new_name }]
```

and your repository contains a file b.ml:

```
let _ = A.old_name 1
```

then you could do:

```shell
$ git diff b.ml
$ ocamlmig migrate -w
$ git diff b.ml
-let _ = A.old_name 1
+let _ = A.new_name 1
```

Examples of real rewrites:

- [Renaming operators](https://github.com/v-gb/Gillian/commit/e15ac20a5fac0849dae51523d1b73f1612f976e5) (not trivial because the operators change precedence)
- [Switching code using both Stdlib and Core to mostly Core](https://github.com/v-gb/ortografe/commit/b0b6a0c323edb67c03ae938d122e73b4f6a8affc)
- [Switching code using Cmdliner's `$` to `let+`](https://github.com/tarides/dune-release/pull/503), in three parts: [switch to let+ proper](https://github.com/tarides/dune-release/pull/503/commits/17352d037304fe8df9ada390130d91add9165d49), [removing tags](https://github.com/tarides/dune-release/pull/503/commits/67dbebf9ffed4022debf0482dda7063cd42c68c4), and [inlining flag terms](https://github.com/tarides/dune-release/pull/503/commits/ca2ce09432d7572be45bc8b07491324661bbffc2). ocamlmig also checks that variables in moved code keep pointing to the same value.

If that piqued your interest, you can find more information about [what ocamlmig
does](doc/what.md), and [using it](doc/using.md).

This is work in progress, so many things are only partially implemented, but the
existing functionality as is should still be interesting.
