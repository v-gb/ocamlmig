(* Silly example of rewriting a .mli *)

include module type of struct
  let _ = Z.Migrate_on_val.x [@@migrate_test let _ = 3]
end
