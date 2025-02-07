let test = Test_migration.test

let () =
  test "open"
    (module struct
      let _ = (open_in, open_out, output_string)

      exception A

      let _ = (Not_found, A)
      let _ = ((function Not_found -> () | _ -> ()), function A -> () | _ -> ())
    end)
[@@migrate_test.open
  let () =
    test "open"
      (module struct
        let _ = (open_in, Stdlib.open_out, Stdlib.output_string)

        exception A

        let _ = (Stdlib.Not_found, A)

        let _ =
          ((function Stdlib.Not_found -> () | _ -> ()), function A -> () | _ -> ())
      end)]
