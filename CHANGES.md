# next

- Added support for rewriting .mli files, just not .ml files.
- First stab at supporting migrating modules: `module Old_name : sig end
  .... [@@migrate { repl = New_name }]`. These annotations are only looked for when
  passing `-module-migration`, and the replacement is limited to a module path.
- given an annotation `val x : ... [@@migrate { repl = e }]`, when inserting `e` at a
  use-site of `x`, in some case, we now modify `e` to avoid shadowing-related
  issues, and shorten module paths according to the modules opened in that scope.
- added an experimental printer that ocamlformats only the sections of the code
  that are modified, instead of the entire file. It is used by default when a repository
  does not use ocamlformat.
- `ocamlmig transform rescope -unopen` now know to rename all kinds of identifiers
  (module identifiers, type constructors, class identifiers, etc), instead of only
  values.

# 5.2-20250202

Correct a failure to use some opam-installed files.

# 5.2-20250129

Initial release.
