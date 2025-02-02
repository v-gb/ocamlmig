This page describes at a high level what problem ocamlmig is intended to solve, and
how. For explanations about using the tool, see [this page](using.md) instead.

# The main type of problem

ocamlmig is a command line tool created for reducing the friction caused by changes of
library interfaces.  An example will probably illustrate this best. Let's say I had
this code:

```ocaml
let () = Gc.eventlog_pause ()
```

and after upgrading the ocaml compiler, I now get:

```ocaml
 1 | let () = Gc.eventlog_pause ()
              ^^^^^^^^^^^^^^^^^
Error (alert deprecated): Stdlib.Gc.eventlog_pause
Use Runtime_events.pause instead.
```

Ok, let's use the new function then:

```ocaml
let () = Runtime_events.pause ()
```

```ocaml
 1 | let () = Runtime_events.pause ()
              ^^^^^^^^^^^^^^^^^^^^
Error: Unbound module Runtime_events
```

Ah ok. Hm. Maybe the deprecation message meant this instead?

```ocaml
let () = Gc.Runtime_events.pause ()
```

```ocaml
 1 | let () = Gc.Runtime_events.pause ()
              ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound module Gc.Runtime_events
```

Still no. Hm... Maybe the module was renamed, say to singular `Runtime_event`, without
updating the deprecation warning? It's not clear how to figure this out.

While this example is a bit artificial (there were probably very few users of
`Gc.eventlog_pause ()`), deprecation warnings do sometimes require digging into what's
changing due to unclear wording (`Base.Uchar.utf_8_byte_length` was deprecated with the
message "use Utf8.byte_length". What do you think is the correct upgrade? I thought
`Base.Utf8.byte_length`, but no). Even when the updates are clear, it can be a chore
to apply them all to the entire codebase.

In many cases, this friction is not inherent, it is caused by a lack of tooling: the
author of the deprecation warning knew exactly how callers should update their code,
and that update is very mechanical, but the only mechanism they have to express it is
textual instructions, which lack specificity.

Had ocamlmig existed when `Gc.eventlog_pause` was deprecated, the author could have
written an attribute that unambiguously specifies where the new functions now lives:

```ocaml
(* If the new function lives in a Runtime_events module of the Stdlib *)
val eventlog_pause : unit -> unit
[@@deprecated "..."]
[@@migrate { repl = Runtime_events.pause }]


(* If Gc.Runtime_events.pause is where the new function lives *)
val eventlog_pause : unit -> unit
[@@deprecated "..."]
[@@migrate { repl = Rel.Runtime_events.pause }]


(* If the new function lives in a Runtime_events library, whether that library
   is shipped with the compiler or not. *)
val eventlog_pause : unit -> unit
[@@deprecated "..."]
[@@migrate { repl = Runtime_events.pause; libraries = [ "runtime_events" ] }]
```

In this case, the author would have written the last attribute. As a user, this would
let me run `ocamlmig migrate`, which would have given me the following correctly
compiling code change, skipping the "where did the function go" side quest:

```diff
--- bin/main.ml
+++ bin/main.ml
-let () = Gc.eventlog_pause ()
+let () = Runtime_events.pause ()
--- bin/dune
+++ bin/dune
@@ -1,4 +1,5 @@
 (executable
  (public_name zz)
  (name main)
- (libraries zz))
+ (libraries runtime_events
+            zz))
--- dune-project
+++ dune-project
@@ -19,7 +19,7 @@
  (name zz)
  (synopsis "A short synopsis")
  (description "A longer description")
- (depends ocaml dune))
+ (depends ocaml dune runtime_events))
```

This workflow splits the work caused by interface changes between the library author
and the library user, each working on the code they know best, i.e. their own code. The
library author makes the interface change as usual, and now also adds a `@migrate`
attribute. The library user simply run `ocamlmig migrate`, and potentially cleans up
their code further, whether immediately or next time they touch that file. The library
author could even be a user of their own library in this workflow, if they wanted to
use ocamlmig to update their test suite for instance.

# Generalizing

ocamlmig is not limited to replacing identifiers by identifiers, like in the example
above. If I had:

```ocaml
let create ?(x = 0) () = do_whatever x
```

and thought "this optional argument should really be mandatory", I could do this:

```ocaml
let create_explicit x = do_whatever x
let create ?(x = 0) () = create_explicit x
[@@migrate { repl = (fun ?(x = 0) () -> Rel.create_explicit x) }]
```

`ocamlmig migrate` would then modify callers like this:

```diff
-let _ = Foo.create (), Foo.create ~x:2 ()
+let _ = Foo.create_explicit 0, Foo.create_explicit 2

(* original code:
     let _ = Foo.create (), Foo.create ~x:2 ()
   after inlining the replacement expression and substituting Rel:
     let _ = (fun ?(x = 0) () -> Foo.create_explicit x) (),
             (fun ?(x = 0) () -> Foo.create_explicit x) ~x:2 () 
   after simplifying the function calls:
     let _ = (let x = 0 in Foo.create_explicit x),
             (let x = 2 in Foo.create_explicit x)
   after simplifying the let-bindings:
     let _ = Foo.create_explicit 0, Foo.create_explicit 2
*)
```

As you may have noticed, the `repl` expression and the definition of `create` are
almost identical. This is no coincidence: when evolving an interface, it is common for
an old value to be a shim calling newer values, as done above.  Inlining that shim into
a caller migrates uses of the old interface into uses of the new interface,
i.e. exactly what we're trying to do.

Now, not every interface change can be handled mechanically, and not every mechanical
upgrade can be expressed with ocamlmig, but the hope is that "replacing value
identifier by expression" is a useful tool nonetheless.

# Stepping back

As shown above, ocamlmig provides a workflow where:

- when library authors want to evolve their library interface and phase out bits of the
  current one, they can annotate value identifiers with the description of an
  executable migration. Any ocaml programmer should be able to write migrations: no one
  needs to learn syntax trees, and there should be very little ocamlmig-specific
  knowledge to relearn every time the need arises.
- library users can apply such migrations

A migration doesn't have to be bundled with a deprecation warning as above.

ocamlmig also makes it possible to set `@migrate` attribute to an identifier without
being the author of the corresponding interface.  Thus anyone could provide a migration
turning, say, [`open_in` into `In_channel.open_text` without modifying the standard
library](../examples/stdlib_to_stdlib/stdlib_to_stdlib.ml).

Aside from the migration workflow above, ocamlmig also provides refactor tools that
benefit from having access to types or syntax trees: removing `open A` and adding `A.`
where appropriate in the remainder of the code, or inversely adding `open A` and
potentially qualifying any module identifier that would otherwise get shadowed.

# Status

ocamlmig should be useful as is, but with presumably a fair amount of rough edges. Here are examples of what has been done with it:

- [Renaming operators](https://github.com/v-gb/Gillian/commit/e15ac20a5fac0849dae51523d1b73f1612f976e5) (not trivial because the operators change precedence)
- [Switching code using both Stdlib and Core to mostly Core](https://github.com/v-gb/ortografe/commit/b0b6a0c323edb67c03ae938d122e73b4f6a8affc), using [these attributes](../examples/stdlib_to_base/stdlib_to_base.ml).

Aside from making what exists work better (nicer resulting code, more correct code in
corner cases, better performance, etc), we could imagine improving ocamlmig along a
number of axes:

- it may be possible to handle migrations for "in-place" changes, meaning cases like
  `let f x = ...`  in version 1 of the library becoming `let f ~x = ...` in version 2.
- in addition to updating expression, it may be possible to manage a form of update
  types and modules, say turning `type t = A | B` into `type t`.
- we could take `@version` tags in documentation into account to either adjust the
  version bounds of dependencies automatically, or to refrain from applying migrations
  that would cause such updates
- for sufficiently involved changes, using ocamlmig as a library is probably
  necessary to more precisely control the changes. This could be better supported.
- we could provide a command for sed-like transformation, but working on syntax trees
  with access to types. Rewrites like `ocamlmig replace '(Array.get : float array ->
  _)' Float.Array.get` could be useful. Or [this
  example](https://github.com/v-gb/Gillian/commit/e15ac20a5fac0849dae51523d1b73f1612f976e5),
  which is not easily done with sed due to precedence changes.

# What ocamlmig doesn't do

The ocamlmig executable supports a specific form of rewrites that can't possibly cover
everything possible.

But one could rewrite code in arbitrary ways by using ocamlmig as a library
instead. Then, only the limitations of the library itself would potentially be
limiting: only builds that use dune and ocamlformat are supported (except for very
simple changes), and of course the library may be missing functionality (like getting
build feedback while rewriting files).

# Using ocamlmig

See [this page](using.md)!
