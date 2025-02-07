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
  test "constructor"
    (module struct
      type sum = CS

      exception CE

      open Z

      let _ = (ZS, ZE, CS, CE)
      let _ = match () with () | (exception ZE) | (exception CE) -> ()
      let __ v = match v with Some ZS, Some CS -> () | _ -> ()

      exception ZE2 = ZE (* TODO *)
      exception CE2 = CE

      let _ = (ZE2, CE2)
    end)
[@@migrate_test.unopen
  let () =
    test "constructor"
      (module struct
        type sum = CS

        exception CE

        let _ = (Z.ZS, Z.ZE, CS, CE)
        let _ = match () with () | (exception Z.ZE) | (exception CE) -> ()
        let __ v = match v with Some Z.ZS, Some CS -> () | _ -> ()

        exception ZE2 = ZE
        exception CE2 = CE

        let _ = (ZE2, CE2)
      end)]

let () =
  test "class & class type"
    (module struct
      class c = object end

      class type ct = object end

      open Z

      let _ = ((new c : #c), (new zc : #zc))

      class _zc2 : zct =
        object
          inherit zc
        end

      let _ =
        object
          inherit zc
        end

      class type _zct2 = object
        inherit zct
      end

      class _c2 : ct =
        object
          inherit c
        end

      let _ =
        object
          inherit c
        end

      class type _ct2 = object
        inherit ct
      end
    end)
[@@migrate_test.unopen
  let () =
    test "class & class type"
      (module struct
        class c = object end

        class type ct = object end

        let _ = ((new c : #c), (Z.zc : #Z.zc))

        class _zc2 : Z.zct =
          object
            inherit Z.zc
          end

        let _ =
          object
            inherit Z.zc
          end

        class type _zct2 = object
          inherit Z.zct
        end

        class _c2 : ct =
          object
            inherit c
          end

        let _ =
          object
            inherit c
          end

        class type _ct2 = object
          inherit ct
        end
      end)]
