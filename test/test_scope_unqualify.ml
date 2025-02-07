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
