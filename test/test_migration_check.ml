let _ = ( + ) [@migrate { repl = Base.( + ) }] [@@migrate_test "types"]

let _ = ( + ) [@migrate { repl = Rel.( + ) }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 3, characters 0-9:\n\
   Error: Unbound module Rel"]

let _ = ( + ) [@migrate { repl = unbound }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 8, characters 0-7:\n\
   Error: Unbound value unbound"]

let _ = ( + ) [@migrate { repl = Base.( = ) }]
[@@migrate_test
  "File \"test/test_migration_check.ml\", line 13, characters 0-10:\n\
   Error: The value Base.(=) has type int -> int -> bool\n\
  \       but an expression was expected of type int -> int -> int\n\
  \       Type bool is not compatible with type int"]

let _ = [ ( - ); Base.( - ) ] [@migrate] [@@migrate_test "types"]
