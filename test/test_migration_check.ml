let _ = ( + ) [@migrate { repl = Base.( + ) }] [@@migrate_test "types"]

let _ = ( + ) [@migrate { repl = Rel.( + ) }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 3, characters 33-36:\n\
   Error: Unbound module Rel"]

let _ = ( + ) [@migrate { repl = unbound }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 8, characters 33-40:\n\
   Error: Unbound value unbound"]

let _ = ( + ) [@migrate { repl = Base.( = ) }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 13, characters 8-13:\n\
   Error: The value Base.(=) has type int -> int -> bool\n\
  \       but an expression was expected of type int -> int -> int\n\
  \       Type bool is not compatible with type int"]

let _ = [ ( - ); Base.( - ) ] [@migrate] [@@migrate_test "types"]

module M_abs : sig
  val x1 : int [@@migrate { repl = Test_ocamlmig.Test_migration_check.M_rel.y }]
  val y : float
end = struct
  let x1 = 1
  let y = 1.
end
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 23, characters 35-77:\n\
   Error: The value Test_ocamlmig.Test_migration_check.M_rel.y has type \n\
  \       float but an expression was expected of type int"]

module M_rel : sig
  val x1 : int [@@migrate { repl = Rel.y }]
  val y : float
end = struct
  let x1 = 1
  let y = 1.
end
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 35, characters 35-40:\n\
   Error: The value Rel.y has type float but an expression was expected of type\n\
  \         int"]
