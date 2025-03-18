(lang dune 3.15)

(formatting
 (enabled_for ocaml))

(using menhir 2.1)

(name ocamlmig)
(generate_opam_files true)
(source (github v-gb/ocamlmig))
(authors "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(maintainers "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(documentation https://github.com/v-gb/ocamlmig/blob/main/README.md)

; (pin
;  (url "git+https://github.com/v-gb/ocamlformat#r-49fc0660ec931943a97b13f10ac9eadbe0cd63f3")
;  (package (name ocamlformat)))
; (pin
;  (url "git+https://github.com/v-gb/ocamlformat#r-49fc0660ec931943a97b13f10ac9eadbe0cd63f3")
;  (package (name ocamlformat-lib)))

(package
 (name ocamlmig)
 (synopsis "OCaml source code rewriting tool")
 (description "Ocamlmig is a command line tool to rewrite ocaml source code, especially to make updating to newer interfaces easier.")
 (depends
  (ocaml (and (>= 5.3) (< 5.4)))
   dune
   ;;
   base
   core
   core_unix
   csexp
   ppx_partial
   ;; MARKER ocamlformat
   (ocaml
    (>= 4.08))
   (alcotest
    (and
     (= 1 0)
     (>= 1.3.0)))
   (base
    (>= v0.12.0))
   (cmdliner
    (>= 1.1.0))
   dune
   dune-build-info
   either
   fix
   (fpath
    (>= 0.7.3))
   (menhir
    (>= 20201216))
   (menhirLib
    (>= 20201216))
   (menhirSdk
    (>= 20201216))
   (ocaml-version
    (>= 3.5.0))
   (ocamlformat-rpc-lib
    (and
     (= 1 0)
     (= :version)))
   (ocp-indent
    (or
     (and
      
      (>= 1.8.0))
     (and
      (= 1 0)
      (>= 1.8.1))))
   stdio
   (uuseg
    (>= 10.0.0))
   (uutf
    (>= 1.0.1))
   (csexp
    (>= 1.4.0))
   ; for vendored odoc-parser
   astring
   camlp-streams
   (re (>= 1.10.3))
   ;; ENDMARKER ocamlformat
 ))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
