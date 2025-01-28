(lang dune 3.15)

(formatting
 (enabled_for ocaml))

(name ocamlmig)
(generate_opam_files true)
(source (github v-gb/ocamlmig))
(authors "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(maintainers "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(documentation https://github.com/v-gb/ocamlmig/tree/main/doc)

;; manually duplicated in ocamlmig.opam.template, because for some reason
;; dune doesn't propagate it there
(pin
 (url "git+https://github.com/v-gb/ocamlformat#r-49fc0660ec931943a97b13f10ac9eadbe0cd63f3")
 (package (name ocamlformat)))
(pin
 (url "git+https://github.com/v-gb/ocamlformat#r-49fc0660ec931943a97b13f10ac9eadbe0cd63f3")
 (package (name ocamlformat-lib)))

(package
 (name ocamlmig)
 (synopsis "Ocaml source code rewriting tool")
 (description "Ocamlmig is a command line tool to rewrite ocaml source code, especially to make updating to newer interfaces easier")
 (depends
  (ocaml (and (>= 5.2) (< 5.3))) ;; no Shape_reduce in 5.1, incompatible changes in 5.3
   dune
   ;;
   base
   core
   core_unix
   csexp
   ocamlformat
   ocamlformat-lib
   ppx_partial
 ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
