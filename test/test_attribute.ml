(* This is not run automatically, need to figure out how to update run, or how to
   update the code itself to make this possible. *)
let f () = ()
[@@migrate
  { repl = f
  ; libraries =
      [ "new-no-version"
      ; ("new-with-version", ~min_version:"1.1")
      ; "astring" (* existing no version *)
      ; ("dune", ~min_version:"3.5" (* existing with newer version *))
      ; ("re", ~min_version:"1.12" (* existing with older version *))
      ]
  }]

let _ = f
(* Result:

--- ./dune
+++ ./dune
@@ -X,X +X,X @@
 (library
  (name test_ocamlmig)
- (libraries base)
+ (libraries astring
+            base
+            dune
+            new-no-version
+            new-with-version
+            re)
  (preprocess (pps ppx_partial))
 )
 
--- dune-project
+++ dune-project
@@ -X,X +X,X @@
    ; for vendored odoc-parser
    astring
    camlp-streams
-   (re (>= 1.10.3))
+   new-no-version
+   (new-with-version (>= 1.1))
+   (re (>= 1.12))
    ;; ENDMARKER ocamlformat
  ))
 (warnings (duplicate_deps disabled))
*)
