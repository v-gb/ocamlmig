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

let () =
  test "class"
    (module struct
      class c = object end

      open Z

      let _ = ((new c : #c), (new zc : #zc))

      class _zc2 =
        object
          inherit zc
        end

      let _ =
        object
          inherit zc
        end

      class _c2 =
        object
          inherit c
        end

      let _ =
        object
          inherit c
        end
    end)
[@@migrate_test.unopen
  let () =
    test "class"
      (module struct
        class c = object end

        let _ = ((new c : #c), (Z.zc : #Z.zc))

        class _zc2 =
          object
            inherit Z.zc
          end

        let _ =
          object
            inherit Z.zc
          end

        class _c2 =
          object
            inherit c
          end

        let _ =
          object
            inherit c
          end
      end)]
