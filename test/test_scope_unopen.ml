let test = Test_migration.test

let () =
  test "value"
    (module struct
      let z2 = 1

      open Z

      let _ = (z2, z1, Nested.n1)
    end)
[@@migrate_test.unopen
  let () =
    test "value"
      (module struct
        let z2 = 1
        let _ = (z2, Z.z1, Z.Nested.n1)
      end)]

let () =
  test "type"
    (module struct
      type t = int

      open Z

      let _ = ((1 : z), (1 : t))
    end)
[@@migrate_test.unopen
  let () =
    test "type"
      (module struct
        type t = int

        let _ = ((1 : Z.z), (1 : t))
      end)]
