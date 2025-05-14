(* Comment for the purpose of checking that comments aren't moved randomly *)

let test = Test_migration.test

module M : sig
  val x : int

  type t = int
  type tr = { mutable a : int }
  type tc = A of int

  module Sub : sig end

  module type S = sig end

  class c : object end

  class type ct = object end
end = struct
  let x = 1

  type t = int
  type tr = { mutable a : int }
  type tc = A of int

  module Sub = struct end

  module type S = sig end

  class c = object end

  class type ct = object end
end
[@@migrate { repl = M2 }]

let () =
  test "migration on module"
    (module struct
      let __ r = ((M.x : M.t), r.M.a, (r.M.a <- 1), { M.a = 1 }, M.A 1)
      [@@migrate_test
        let __ r = ((M2.x : M2.t), r.M2.a, (r.M2.a <- 1), { M2.a = 1 }, M2.A 1)]

      let __ = function { M.a = _ }, M.A 1 -> () | _ -> ()
      [@@migrate_test let __ = function { M2.a = _ }, M2.A 1 -> () | _ -> ()]

      module A = M.Sub [@@migrate_test module A = M2.Sub]

      module type S = M.S [@@migrate_test module type S = M2.S]

      class _c = M.c [@@migrate_test class _c = M2.c]

      let _ : #M.c = new M.c [@@migrate_test let _ : #M2.c = new M2.c]

      class type _ct = M.ct [@@migrate_test class type _ct = M2.ct]
    end)
