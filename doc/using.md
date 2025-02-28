This page explains how to use ocamlmig. For an overview of what ocamlmig does,
see [this](./what.md) instead.

# Installation and requirements

To rewrite your code with ocamlmig, you need to:

- build the ocamlmig command line executable
- use dune to build your own code
- preferably use ocamlformat, though this is not strictly required

You can set up a test repository this way:

```shell
cd /tmp &&
git init trymig &&
cd trymig &&
(echo _build; echo _opam) > .gitignore &&
opam switch create . --packages ocaml.5.2.1,dune,ocamlformat,ocamlmig &&
eval "$(opam env)" &&
dune init project trymig . &&
{ cat > bin/main.ml <<'EOF'
let () = Printf.printf "should be 1: %f\n" ((cos 0.3)**2. +. (sin 0.3)**2.)

let f1 x = x + 1 [@@migrate { repl = fun x -> Rel.f2 ~x }]
let f2 ~x = x + 1
let _ = f1 1
let _ = f2
EOF
} &&
touch .ocamlformat &&
{ dune fmt 2> /dev/null || true; } &&
dune build @check &&
git add . &&
git commit -m init &&
cat <<EOF
*** All good! ***
Now look at bin/main.ml, and try these:
$ ocamlmig mig
$ ocamlmig mig -extra-migration ocamlmig.stdlib_to_stdlib
EOF
```

In an existing project, you can either `opam install ocamlmig` in that project's
switch, or use the ocamlmig binary installed in a different switch (in which case,
ocamlmig and the project must be built with the same compiler version).

If you merely want to add attributes to a project, you can do so without dependencies
or build system tweaks. You may still want to install ocamlmig to check that your
attributes are working as intended though.

# How to rewrite code

## Migrations
<!-- Give concrete executable above. Make an example directory? -->

If someone else has written `@migrate` attributes for you, all you need to do is:

- build the part of the repository you want to upgrade. `dune build @check` is the
  recommended way to do that, as a mere `dune build` doesn't quite build everything
  that's necessary. If the build fails due to warnings, you should build with `dune
  build --profile release` so the build succeeds.
- run `ocamlmig migrate` to see the proposed diff. By default, the command runs on all
  .ml files in the repository, but you can specify fewer files with e.g.  `ocamlmig
  migrate lib/*.ml`.
- run `ocamlmig migrate -w` to actually update the source files, if the diff looks good

If something is not working right, the usual way to debug is by adding a
`[@migrate.log]` around the expression where you expected a different result, then
rerunning `ocamlmig migrate`. `[@migrate.log]` should generally wrap as small an
expression as possible, as the amount of debug output can be overwhelming otherwise.

If your repo isn't configured to use ocamlformat, ocamlmig will print code in a way
that tries to preserve the initial formatting. This is more experimental, so if you
observe issues, you may want to try `ocamlmig migrate -format ocamlformat` to see if it
makes a difference.

## Builtin transformations

`ocamlmig transform` provides a few refactorings described below. As you'll notice, it
wouldn't be possible to implement similar things with sed invocations and the
like. You can chain such refactorings sequentially, so long as the code successfully
builds between steps, to build larger and more interesting changes.

### Removing opens

After compiling the following program with dune:

```ocaml
open Format
let () = print_string "a"
let () = prerr_string "a"
```

ocamlmig would refactor it this way:

```shell
$ ocamlmig transform rescope -unopen Format
-open Format
-let () = print_string "a"
+let () = Format.print_string "a"
 let () = prerr_string "a"
$ ocamlmig transform rescope -unopen Format -w # update the file if the diff is good
```

Notice that `print_string` was turned to `Format.print_string`, but `prerr_string`
is unchanged (since `prerr_string` is not defined by `Format`).

### Adding opens

Inversely, `ocamlmig transform rescope -open Format` applied to
a file:

```ocaml
let () = print_string "a"
let () = Format.print_string "a"
```

would generate this diff:

```diff
-let () = print_string "a"
+open Format
+
+let () = Stdlib.print_string "a"
 let () = Format.print_string "a"
```

`print_string` was requalified, otherwise the extra `open Format` would make it refer
to `Format.print_string`. As you can see `Format.print_string` wasn't shortened to
`print_string`, although it could have been. The next transformation does exactly that.

### Shortening identifiers

`ocamlmig transform rescope -unqualify Format` on:

```ocaml
open Format

let () = Stdlib.print_string "a"
let () = Format.print_string "a"
```

would update the code like so:

```diff
 open Format
 let () = Stdlib.print_string "a"
-let () = Format.print_string "a"
+let () = print_string "a"
```

# How to write attributes

Here, we will assume you have read through the [high level picture](./what.md) first.

For library authors, here is how to describe a migration:

```ocaml
(* This is library Foo *)

val starts_with2 : string -> prefix:string -> bool

val starts_with : string -> string -> bool
[@@migrate { repl = (fun str prefix -> Rel.starts_with2 str ~prefix) }]
```

What this means is: during `ocamlmig migrate`, any reference `Foo.starts_with` to the
value above will be replaced by `(fun str prefix -> Foo.starts_with2 str ~prefix)`.
`Rel` is special syntax to refer to the module path to the old value (mostly meant so
that if the module of the value has been aliased in the calling code, the updated code
keeps using the alias).  ocamlmig then cleans up the code if possible:

```ocaml
(* original code *)
let _ = Foo.starts_with "a" "b"

(* after "inlining" the migrate annotation and replacing Rel *)
let _ = (fun str prefix -> Foo.starts_with2 str ~prefix) "a" "b"

(* after clean up *)
let _ = Foo.starts_with2 "a" ~prefix:b"
```

As you may have noticed, the `repl` field contains an expression that, aside from the
use of `Rel`, is probably exactly how `starts_with` is implemented in the library.

### Attribute placement

In general, the `migrate` attribute is supported:

- on `val` signature items, `val x : unit -> unit [@@migrate ...]`
- on let bindings defining a single variable, `let x () = () [@@migrate ...]`
- on an arbitrary identifier, to define a migration without modifying the definition
  of the identifier:

    ```ocaml
    let _ = List.map [@migrate { repl = fun f l -> ListLabels.map ~f l }]`
    ```

    Note the single `@` sign.
    
    Such attributes differ from inline ones in a couple of ways:

    - They must be defined in a single-module library. Let's say the library is called
      `use_labels`, then the code above would live in `use_labels.ml`.
    - `ocamlmig migrate` will not apply such transform automatically, this must be
      requested explicitly with a `-extra-migration use_labels` flag.

    Currently, the following syntax is also supported:
    
    ```ocaml
    let _ = [ List.map; (fun f l -> ListLabels.map ~f l) ] [@migrate]`
    ```

    The advantage of this version is that the replacement expression is typechecked,
    and is checked to have a type that's unifiable with the original identifier. The
    downside is that the annotated code maybe need more dependencies to compile.

- (experimental) on module declaration, `module M : ... [@@migrate { repl = Mnew }]`.
  The replacement must be a path, not a general expression, and these migrations
  attributes are only searched for with `ocamlmig mig -module-migration`.

### Attribute syntax

The general form of the `migrate` attribute is:

```ocaml
[@@migrate
  { repl (* a required expression, e.g. *) = (fun f l -> ListLabels.map l ~f)
  ; libraries (* an optional list of libraries names, e.g. *) = [ "core.unix" ]
  }]
```

### `repl` field

There are a few things to know about the `repl` expression:

- Since the expression is specified in an attribute, it will be parsed but not
  typechecked, so you may want to manually check (by applying the migration) that your
  migration generates working code.
- Since the expression is meant to be inserted into the caller's source code, although
  any arbitrary ocaml expression can be inserted, not every ocaml bit of the language
  is a good idea to use. Here are known things to be wary about:

    - The scope at the attribute definition and at the call site is different. In
      particular sum type constructors and record fields may need to be qualified. The
      intention is that replacement expression should assume that Stdlib is in scope
      and use fully qualified paths (or `Rel`) to refer to other names.
    - Type variables `(... : 'a list)` should be avoided, because they can collide
      with type variables of the same name in the caller, and cause typing errors even if
      they are fresh.
    - Extension nodes (say `[%compare: int]`) should be avoided. If the replacement code
      contains an extension node, then the caller would either get the macro-expansion
      or the extension node (depending on how the ppx works), and neither is ideal.

Finally, the code supports applying slightly different rewrites depending on the
context of the original identifier. Concretely, it looks like this:

```ocaml
let _ = compare
  [@migrate { repl = function [%context: int -> _] -> Int.compare
                            | [%context: string -> _] -> String.compare }
                            | _ -> Compare.Poly.compare
  ]

let z1 x = compare x 1 (* compare would be replaced by Int.compare *)
let z2 (x : string) y = compare x y (* compare would be replaced by String.compare *)
let z3 x y = compare x y (* compare would be replaced by Compare.Poly.compare *)
```

The `function` syntax maps conditions about the context to a replacement expression in
that context. `[%context: type]` accepts only call sites compatible with the specified
type, while `_` accepts all call sites. A final `_` is not required: if no contexts
match, a call site won't be rewritten.

### `libraries` field

The `libraries` contains a list of libraries names as specified in dune files. This
should be used when the replacement expression refers to libraries that the caller code
may not have in scope. Whenever that migrate attribute is used, ocamlmig will add the
dependency to the relevant dune files.

