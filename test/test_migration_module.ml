(* Comment for the purpose of checking that comments aren't moved randomly *)

let test = Test_migration.test

module M : sig
  val x : int

  type t = int
  type tr = { a : int }
  type tc = A of int

  module Sub : sig end

  module type S = sig end

  class c : object end

  class type ct = object end
end = struct
  let x = 1

  type t = int
  type tr = { a : int }
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
      (* Not all namespaces are implemented here. *)

      let __ r = ((M.x : M.t), r.M.a, M.A 1)
      [@@migrate_test let __ r = ((M2.x : M2.t), r.M.a, M2.A 1)]

      module A = M.Sub [@@migrate_test module A = M2.Sub]

      module type S = M.S [@@migrate_test module type S = M.S]

      class _c = M.c [@@migrate_test class _c = M.c]

      class type _ct = M.ct [@@migrate_test class type _ct = M.ct]
    end)
