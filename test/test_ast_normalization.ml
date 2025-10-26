let _ =
  let open[@foo] String in
  compare

(* so just replace actually does something on the file *)
let () = () [@@migrate_test.replace_expr {| z///z |}] [@@migrate_test.replace let () = ()]
