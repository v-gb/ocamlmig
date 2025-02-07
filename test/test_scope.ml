let test = Test_migration.test

let () =
  test "unqualify"
    (module struct
      let _ = (Z.z1, Z.Nested.n1) [@@migrate_test let _ = (Z.z1, Z.Nested.n1)]

      open! Z

      let _ = (Z.z1, Z.Nested.n1, Z.( !! ) 2)
      [@@migrate_test let _ = (z1, Nested.n1, !!2)]

      open struct
        let z1 = 2
        let _ = z1
      end

      let _ = (Z.z1, Z.Nested.n1) [@@migrate_test let _ = (Z.z1, Nested.n1)]
    end)

let () =
  test "unopen"
    (module struct
      type t = int

      let z2 = 1

      open Z

      let _ = (z2, z1, Nested.n1, (1 : z), (1 : t))
    end)
[@@migrate_test.unopen
  let () =
    test "unopen"
      (module struct
        type t = int

        let z2 = 1
        let _ = (z2, Z.z1, Z.Nested.n1, (1 : Z.z), (1 : t))
      end)]

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
