(* used by other tests *)
type z = int

class zc = object end

class type zct = object end

type zsum = ZS

exception ZE

type zr =
  { zr1 : int
  ; zr2 : int
  }

module ZM = struct end

module type ZS = sig end

let z1 = 1
let open_out = 2
let ( !! ) x = x + 1

exception Not_found = Not_found [@deprecated ""]

include (
  struct
    let output_string = output_string
  end :
    sig
      val output_string : out_channel -> string -> unit [@@deprecated "don't!"]
    end)

module Nested = struct
  let n1 = 1
end

module Migrate_on_val : sig
  val x : int [@@migrate { repl = 3 }]
end = struct
  let x = 3
end

module Module = struct
  let b = 1
  let a1 = b
  let a2 = b [@@migrate { repl = Rel.b }]
  let a3 = b [@@migrate { repl = Z.Module.b }]
end
