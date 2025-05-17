module M_abs : sig
  val x1 : int [@@migrate { repl = Test_ocamlmig.Test_migration_check.M_rel.y }]
  val y : float
end
[@@migrate_test
  "File \"test/test_migration_check.mli\", line 2, characters 0-42:\n\
   Error: The value Test_ocamlmig.Test_migration_check.M_rel.y has type \n\
  \       float but an expression was expected of type int"]

module M_rel : sig
  val x1 : int [@@migrate { repl = Rel.y }]
  val y : float
end
[@@migrate_test
  "File \"test/test_migration_check.mli\", line 11, characters 0-5:\n\
   Error: The value Rel.y has type float but an expression was expected of type\n\
  \         int"]
