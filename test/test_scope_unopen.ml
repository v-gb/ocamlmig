let test = Test_migration.test

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
