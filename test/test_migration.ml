(* Comment for the purpose of checking that comments aren't moved randomly *)

module type Empty = sig end

let test _name (_ : (module Empty)) = ()

let () =
  test "simple name replacement"
    (module struct
      module A = struct
        let name = "2" [@@migrate { repl = name2 }]
      end

      let _ = A.name [@@migrate_test let _ = name2]

      module B = struct
        let name = "2" [@@migrate { repl = Rel.name2 }]
      end

      let _ = B.name [@@migrate_test let _ = B.name2]
    end)

let () =
  test "replacement on signature"
    (module struct
      module A : sig
        val name : string [@@migrate { repl = name2 }]
      end = struct
        let name = "2"
      end

      let _ = A.name [@@migrate_test let _ = name2]
      let _ = Z.Migrate_on_val.x [@@migrate_test let _ = 3]
    end)

let () =
  test "replacement by function literal, maybe reordering"
    (module struct
      let f a b = a + b [@@migrate { repl = (fun a b -> f2 ~a ~b) }]
      let _ = f 1 2 [@@migrate_test let _ = f2 ~a:1 ~b:2]
      let g a b = a + b [@@migrate { repl = (fun a b -> g2 ~b ~a) }]
      let _ = g 1 2 [@@migrate_test let _ = g2 ~a:1 ~b:2]
      let g' a b = a + b [@@migrate { repl = (fun a b -> (g'2 ~b ~a [@reorder])) }]
      let _ = g' 1 2 [@@migrate_test let _ = g'2 ~b:2 ~a:1]
      let h a b = a + b [@@migrate { repl = (fun a b -> h2 b a) }]
      let _ = h 1 2 [@@migrate_test let _ = h2 2 1]
      let i ~a ~b = a + b [@@migrate { repl = (fun ~a ~b -> i2 b a) }]
      let _ = i ~b:2 ~a:1 [@@migrate_test let _ = i2 2 1]
    end)

let () =
  test "replacement by function literal, underapplied, maybe reordering"
    (module struct
      let f a b = a + b [@@migrate { repl = (fun a b -> f2 ~a ~b) }]
      let _ : _ = f 1 [@@migrate_test let _ : _ = f2 ~a:1]
      let g a b = a + b [@@migrate { repl = (fun a b -> g2 ~b ~a) }]
      let _ : _ = g 1 [@@migrate_test let _ : _ = g2 ~a:1]
      let g' a b = a + b [@@migrate { repl = (fun a b -> (g'2 ~b ~a [@reorder])) }]
      let _ : _ = g' [@@migrate_test let _ : _ = fun a b -> g'2 ~b ~a]
      let h a b = a + b [@@migrate { repl = (fun a b -> h2 b a) }]
      let _ : _ = h 1 [@@migrate_test let _ : _ = h2 __ 1]

      let _ : _ = h 1
      [@@migrate.no_ppx_partial]
      [@@migrate_test let _ : _ = (fun a b -> h2 b a) 1 [@@migrate.no_ppx_partial]]
      (* QOI: should at least apply what we can *)

      let i a b c d = a + b + c + d [@@migrate { repl = (fun a b c d -> i2 d c b a) }]
      let _ : _ = i 1 [@@migrate_test let _ : _ = (fun a b c d -> i2 d c b a) 1]
      (* QOI: should at least apply what we can *)

      let j ~a ~b ~c = a + b + c [@@migrate { repl = (fun ~a ~b ~c -> j2 a b c) }]
      let _ : _ = j ~b:1 [@@migrate_test let _ : _ = (fun ~a ~b ~c -> j2 a b c) ~b:1]
      (* QOI: should at least apply what we can *)
    end)

let () =
  test "replacement by function literal, overapplied"
    (module struct
      let f a b = a + b [@@migrate { repl = (fun a -> f2 ~a) }]
      let _ = f 1 2 [@@migrate_test let _ = (f2 ~a:1) 2]
    end)

let () =
  test "replacement by function literal, optional arguments"
    (module struct
      let f ?a ?b ?c ?d () = ignore (a, b, c, d)
      [@@migrate { repl = (fun ?a ?(b = 1) ?c:c2 ?d:(d2 = 1) () -> f2 ~a ~b ~c2 ~d2) }]

      let _ = f () [@@migrate_test let _ = f2 ~a:None ~b:1 ~c2:None ~d2:1]

      let __ a b c d = f ?a ?b ?c ?d ()
      [@@migrate_test
        let __ a b c d =
          f2 ~a
            ~b:(match b with Some x -> x | None -> 1)
            ~c2:c
            ~d2:(match d with Some x -> x | None -> 1)]

      let __ a b c d = f ~a ~b ~c ~d ()
      [@@migrate_test let __ a b c d = f2 ~a:(Some a) ~b ~c2:(Some c) ~d2:d]

      let __ a d = f ~d ~a () (* we try to preserve the user argument order *)
      [@@migrate_test let __ a d = f2 ~b:1 ~c2:None ~d2:d ~a:(Some a)]

      let f ?a ?b () = (a, b) [@@migrate { repl = (fun ?a ?b () -> f2 ?a2:a ?b2:b ()) }]
      let __ b = f ~b () [@@migrate_test let __ b = f2 ~b2:b ()]
    end)

let () =
  test "replacement by function literal, duplicated arguments"
    (module struct
      let f a b _ = a + b [@@migrate { repl = (fun a b c -> f2 a a b b c c) }]

      let _ = f 1 (Random.int 100) None
      [@@migrate_test
        let _ =
          let a = 1 in
          let b = Random.int 100 in
          f2 a a b b None None]
    end)

let () =
  test "replacement by function literal, type based disambiguation"
    (module struct
      module M = struct
        type t = A of int
      end

      let f (A a : M.t) = a + a [@@migrate { repl = (fun a -> f2 a a) }]

      let _ = f (A 1)
      [@@migrate_test
        let _ =
          let a : M.t = A 1 in
          f2 a a]
    end)

let () =
  test "replacement by function literal, capture avoidance"
    (module struct
      (* inlining an argument with free var b into a scope where b is bound *)
      let f a b = a + b
      [@@migrate
        { repl =
            (fun a ->
              ();
              fun b -> f2 (a + 1) (b + 1))
        }]

      let __ b = f b
      [@@migrate_test
        let __ b =
          ();
          fun b2 -> f2 (b + 1) (b2 + 1)]

      (* inlining a function that closes over b into a scope where b is bound *)
      let f g b = g () + b + b [@@migrate { repl = (fun g b -> g () + b + b) }]

      let __ b = f (fun () -> b) (b + 1)
      [@@migrate_test
        let __ b =
          let g () = b in
          let b = b + 1 in
          g () + b + b]

      (* idem, but trying to trick the code with a use of b2 *)
      let f g b b2 = g () + b + b + b2 + b2
      [@@migrate { repl = (fun g b b2 -> g () + b + b + b2 + b2) }]

      let __ b b2 = f (fun () -> b) (b + 1) (b2 + 1)
      [@@migrate_test
        let __ b b2 =
          let g () = b in
          let b = b + 1 in
          let b2 = b2 + 1 in
          g () + b + b + b2 + b2]

      (* idem, but trying to trick the code with another use of b2 *)
      let f g b = g () + b + b
      [@@migrate
        { repl =
            (fun g b ->
              let b2 = () in
              g () + b + b)
        }]

      let __ b = f (fun () -> b) (b + 1)
      [@@migrate_test
        let __ b =
          let g () = b in
          let b = b + 1 in
          let b2 = () in
          g () + b + b]

      let f a b = (a, b) [@@migrate { repl = (fun a b -> (b, a)) }]

      let __ a b = f b a
      [@@migrate_test
        let __ a b =
          let a = b and b = a in
          (b, a)]
      (* QOI: we should be able to inline, we're too conservative here *)

      (* A difficult case where an open is ambiguous, and we don't even have type
         information about it because it comes from the replacement code. *)
      let f a = a
      [@@migrate
        { repl =
            (fun a ->
              let b = 1 in
              (b, M.(b), a))
        }]

      let __ b = f b
      [@@migrate_test
        let __ b =
          let a = b in
          let b = 1 in
          ( b
          , (let open M in
             b)
          , a )]
    end)

let () =
  test "replacement by function literal, capture avoidance in other scopes"
    (module struct
      (* type vars *)
      let f x = x [@@migrate { repl = (fun (x : 'a) -> x) }]

      let __ (a : 'a) =
        f ();
        a
      [@@migrate_test
        let __ (a : 'a) =
          (let (x : 'a) = () in
           x);

          a]
      (* QOI. 'a should not escape the code of the inlined code. Even if the original
         code didn't mention 'a, this would be wrong because the simple act of saying
         (_ : 'a) for a fresh 'a forces the 'a to be ungeneralized and in scope (so it
         can't reference a variable from a local unpacked module type or gadt) until
         the innermost containing structure item. *)

      (* module *)
      let f x = x
      [@@migrate
        { repl =
            (fun x ->
              let module M = struct
                let x = 2
              end in
              (x, M.x))
        }]

      let _ =
        let module M = struct
          let x = 1
        end in
        f M.x
      [@@migrate_test
        let _ =
          let module M = struct
            let x = 1
          end in
          let module M = struct
            let x = 2
          end in
          (M.x, M.x)]
      (* QOI. Computes (2, 2) instead of (1, 2) *)

      (* We can show the same bugs for any other scope: types, module types, classes,
         etc. But these cases are probably not the most relevant considering the kind of
         use cases we target. *)
    end)

let () =
  test "replacement by function literal, effect preservation"
    (module struct
      (* Arguments are executed before the body. *)
      let f _ = () [@@migrate { repl = (fun x -> f2 (print_string "a") x) }]

      let _ = f (print_string "b")
      [@@migrate_test
        let _ =
          let x = print_string "b" in
          f2 (print_string "a") x]

      (* Arguments are executed eagerly, no matter the control of the body. *)
      let f _ _ = () [@@migrate { repl = (fun x y -> if true then x else y) }]

      let _ = f (print_string "a") (print_string "b")
      [@@migrate_test
        let _ =
          let x = print_string "a" in
          let y = print_string "b" in
          if true then x else y]

      (* Can't move arguments into functions... *)
      let f _ = () [@@migrate { repl = (fun x -> loop (fun () -> x)) }]

      let _ = f (print_string "a")
      [@@migrate_test
        let _ =
          let x = print_string "a" in
          loop (fun () -> x)]

      (* or loops. *)
      let f _ = ()
      [@@migrate
        { repl =
            (fun x ->
              while true do
                x
              done)
        }]

      let _ = f (print_string "a")
      [@@migrate_test
        let _ =
          let x = print_string "a" in
          while true do
            x
          done]

      let f _ = () [@@migrate { repl = (fun x -> try x with _ -> ()) }]

      (* Moving arguments into try-with might be possible, but it seems delicate. *)
      let _ = f (print_string "a")
      [@@migrate_test
        let _ =
          let x = print_string "a" in
          try x with _ -> ()]
    end)

let () =
  test "simplification: match/if"
    (module struct
      let _untouched () = if true then 1 else 2
      [@@migrate_test let _untouched () = if true then 1 else 2]

      let not_ b = if b then false else true
      [@@migrate
        { repl =
            (fun b -> match not2 (if b then `T else `F) with `T -> true | `F -> false)
        }]

      let __ b = ((if not_ b then "t" else "f"), if not_ true then "t" else "f")
      [@@migrate_test
        let __ b =
          ( (match not2 (if b then `T else `F) with `T -> "t" | `F -> "f")
          , match not2 `T with `T -> "t" | `F -> "f" )]

      let f _ = `A
      [@@migrate
        { repl =
            (fun () ->
              let a = 1 in
              `A)
        }]

      let __ =
        let a = "A" in
        match f () with `A -> a | `B -> "B"
      [@@migrate_test
        let __ =
          let a = "A" in
          let a = 1 in
          a]
      (* bug: capture *)

      let partition_map _ _ = []
      [@@migrate
        { repl =
            (fun l f ->
              List.partition_map l ~f:(fun a ->
                  match f a with `Left l -> First l | `Right r -> Second r))
        }]

      let __ l =
        partition_map l (fun a ->
            if a mod 3 = 0 then `Left 3 else if a mod 5 = 0 then `Left 5 else `Right ())
      [@@migrate_test
        let __ l =
          List.partition_map l ~f:(fun a ->
              if a mod 3 = 0 then First 3 else if a mod 5 = 0 then First 5 else Second ())]
    end)

let () =
  test "simplification: beta redex"
    (module struct
      let _untouched () = (fun x -> x) ()
      [@@migrate_test let _untouched () = (fun x -> x) ()]

      let fold f init l =
        ignore (f, l);
        init
      [@@migrate { repl = (fun f init l -> foldl l init (fun elt acc -> f acc elt)) }]

      (* We should preserve user-names, here sum/prod/i, rather than use acc/elt *)
      let __ l = fold (fun (sum, prod) i -> (sum + i, prod * i)) (0, 1) l
      [@@migrate_test
        let __ l =
          foldl l (0, 1) (fun i acc ->
              let sum, prod = acc in
              (sum + i, prod * i))]
      (* bug: we should really handle this better, this creates very annoying code *)

      (* Same thing, but more tricky due to another use of acc *)
      let __ l =
        let acc = 1 in
        fold (fun sum i -> acc + sum + i) 0 l
      [@@migrate_test
        let __ l =
          let acc = 1 in
          foldl l 0 (fun i sum -> acc + sum + i)]

      let __ l =
        let f sum i = sum + i in
        fold f 0 l
      [@@migrate_test
        let __ l =
          let f sum i = sum + i in
          foldl l 0 (fun elt acc -> f acc elt)]
      (* QOI: we could conceivably move the wrapping of f into the definition of f,
         considering the argument is a local function that's used only here. *)

      (* Sometimes, we can't use the preferred name, because it would cause a capture
         elsewhere. *)
      let fold _ = 0
      [@@migrate
        { repl =
            (fun f ->
              let aa = 0 in
              ( f aa
              , let bb = 0 in
                aa + bb ))
        }]

      let __ () = fold (fun bb -> bb)
      [@@migrate_test
        let __ () =
          let bb = 0 in
          ( bb
          , let bb = 0 in
            bb + bb )]
      (* bug: capture *)

      (* It is possible to have several preferred names, although probably not likely
         in practice. One gets picked when that happens. *)
      let fold _ _ = 0
      [@@migrate
        { repl =
            (fun f g ->
              let aa = 0 in
              (f aa, g aa))
        }]

      let __ () = fold (fun bb -> bb) (fun cc -> cc)
      [@@migrate_test
        let __ () =
          let bb = 0 in
          (bb, bb)]
    end)

let () =
  test "simplification: let"
    (module struct
      type stringable = E : 'a * ('a -> string) -> stringable

      let _untouched () =
        let (E (x, to_string)) = E (1, string_of_int) in
        to_string x
      [@@migrate_test
        let _untouched () =
          let (E (x, to_string)) = E (1, string_of_int) in
          to_string x]

      let f i = E (i + 1, string_of_int)
      [@@migrate { repl = (fun i -> E (f2 i, string_of_int)) }]

      let __ =
        let (E (x, to_string)) = f 12 in
        to_string x
      [@@migrate_test
        let __ =
          let (E (x, to_string)) = E (f2 12, string_of_int) in
          to_string x]
      (* QOI: we could simplify the constructor, and the resulting
         bindings such that we end up with [string_of_int (f2 12)]. Removing
         the GADT removes the local types they manipulate, but that seems ok?
         At least in this case, maybe GADT with constraints instead of only
         existential wouldn't work.
         let Eq = (Eq : ('a, int) eq) in
         can't be removed for instance, but it wouldn't be anyway, because
         it's not purely matching a constructor against itself, there's the
         type annotation as well. *)
    end)

let () =
  test "let operators"
    (module struct
      let ( let+ ) a f = Option.map f a
      let ( and+ ) = Base.Option.both

      (* let+ in replacement code *)
      let f x y g =
        let+ x = x and+ y = y in
        g x y
      [@@migrate
        { repl =
            (fun x y g ->
              let+ x = x and+ y = y in
              g x y)
        }]

      let __ = f None (Some 1) (fun x y -> Some (x + y))
      [@@migrate_test let __ = let+ x = None and+ y = Some 1 in

                               Some (x + y)]

      (* A more realistic scenario, seems complicated to get good code out of that
         by regular simplification rules. *)
      let const a = Some a

      let app f a = match (f, a) with Some f, Some a -> Some (f a) | _ -> None
      [@@migrate
        { repl =
            (* (function
               | [%context it (Some f) a] ->
                   let+ a = a in
                   f a
               | _ -> *)
            (fun f a ->
              let+ f = f and+ a = a in
              f a)
        }]

      let __ = app (app (const (fun a b -> a + b)) None) (Some 1)
      [@@migrate_test
        let __ =
          let f =
            let f = const (fun a b -> a + b) in
            let+ f = f and+ a = None in

            f a
          in
          let+ f = f and+ a = Some 1 in

          f a]
      (* QOI: simplify this somehow?? *)

      let f g =
        let x = 1 in
        g (Some (x * 1)) 2
      [@@migrate
        { repl =
            (fun g ->
              let x = 1 in
              g (Some (x * 1)) 2)
        }]

      (* simplification across let ops *)
      let _ =
        f (fun x y ->
            let+ x = x in
            x + y)
      [@@migrate_test
        let _ =
          let x = 1 in
          let x = Some (x * 1) in
          let+ x = x in
          x + 2]
      (* QOI: the `let x = Some (x * 1) in` should be inlined *)
    end)

let () =
  (* bug: extra migrations should also be sourced from cmt files *)
  test "extra migration"
    (module struct
      let _ = List.map [@migrate { repl = (fun f l -> Base.List.map l ~f) }]

      let __ = (List.map Fun.id [], Stdlib.List.map Fun.id [])
      [@@migrate_test let __ = (Base.List.map ~f:Fun.id [], Base.List.map ~f:Fun.id [])]

      (* Extra migrations in list syntax. *)
      let f ?(a = 0) ~b ~c () = a + b + c
      let f2 = f
      let _ = [ f; (fun ?a:_ ~b ~c () -> f2 ~c ~b ()) ] [@migrate]
      let __ = f ~c:1 ~b:2 () [@@migrate_test let __ = f2 ~c:1 ~b:2 ()]

      (* When we have type information (in list syntax, currently), we avoid
         captures in the replacement expression. *)
      let _ = [ infinity; Float.infinity ] [@migrate]
      let _ = [ max_int; Int.max_int ] [@migrate]

      module Float = struct end

      let _ = (infinity, max_int)
      [@@migrate_test let _ = (Stdlib.Float.infinity, Int.max_int)]
    end)

let () =
  test "reference by module signature"
    (module struct
      module A : sig
        val x : int
        val y : int (* [@@deprecated ""] *)
      end = struct
        let x = 1
        let y = 2 [@@migrate { repl = Rel.y2 }]
      end

      let () =
        (* Matching a module against its own signature generates no deprecation
           warning, so it seems alright that do not rewrite the implied [let y = A.y]
           and instead rewrite the instance of B.y. The rule seems to be such implied
           aliases only trigger deprecation warnings when coerced with a [val ...]
           that doesn't have a deprecation warning. *)
        let module _ = struct
          module B : sig
            include module type of A

            val z : int
          end = struct
            include A

            let z = 3
          end

          let _ = (B.y, B.z)
        end in
        ()
      [@@migrate_test
        let () =
          let module _ = struct
            module B : sig
              include module type of A

              val z : int
            end = struct
              include A

              let z = 3
            end

            let _ = (B.y2, B.z)
          end in
          ()]

      let () =
        (* bug: This would generate a deprecation alert at the coercion, not at the
           mention of C.y, so we should rewrite A into [struct include A let y =
           replacement end]. *)
        let module _ = struct
          module C : sig
            val y : int
          end =
            A

          let _ = C.y
        end in
        ()
      [@@migrate_test
        let () =
          let module _ = struct
            module C : sig
              val y : int
            end =
              A

            let _ = C.y2
          end in
          ()]
    end)

let () =
  test "migrations in functors"
    (module struct
      module type S = sig
        type t

        val compare : t -> t -> int
      end

      module Make (X : S) = struct
        let mem _ _ =
          let _ = X.compare in
          false
        [@@migrate { repl = (fun set key -> Set2.mem Rel.compare_key set key) }]
      end

      module M = Make (String)

      let __ set key = M.mem set key
      [@@migrate_test let __ set key = Set2.mem M.compare_key set key]
      (* For the purpose of migrating one function inside the functor to another, this
         is good enough. For the purpose of migrating from one functor to either a
         defunctorized interface, or another functor, it's less clear whether this
         is enough. Maybe the ability to change the functor application is all that's
         needed to make everything possible, as that would allow us to give ourselves
         access to the functor argument.

         I wonder if the ability to match on the context would be enough to provide
         a clean switch to base. For instance:
         module Make(X) = ...
         [@@migrate { repl = function
                        | [%context It(X).find x map] -> Base.Map.find map x
                        | [%context It(X).empty] -> Base.Map.empty (module X)
                        | _ -> Base.Map.Make_from_stdlib }]
      *)
    end)

let () =
  test "matching on context"
    (module struct
      let ( = ) = Stdlib.( = )
      [@@migrate
        { repl =
            (function
            | [%context: char -> _] -> Char.equal
            | [%context: int -> _] -> ( = )
            | [%context: string -> _] -> String.equal
            | [%context: _ list -> _] -> fun a b -> List.equal Poly.equal a b
            | _ -> Stdlib.( = ))
        }]

      let _ : int -> int -> bool = ( = )
      [@@migrate_test let _ : int -> int -> bool = ( = )]

      let _ : Stdlib.String.t -> string -> bool = ( = )
      [@@migrate_test let _ : Stdlib.String.t -> string -> bool = String.equal]

      let _ : _ = ( = ) 'c' [@@migrate_test let _ : _ = Char.equal 'c']

      (* make sure we can simplify the code after checking which context applies *)
      let __ x = [] = x [@@migrate_test let __ x = List.equal Poly.equal [] x]
      let __ x = ( = ) x [@@migrate_test let __ x = Stdlib.( = ) x]

      (* TODO:
         - the scope of the user-provided types is probably wrong
         - the -I search path is probably wrong too
      *)
    end)

let () =
  test "approximated shapes"
    (module struct
      module type S = sig
        val dirname : string -> string
      end

      (* This is a simplified version of how Filename.dirname is defined, and we'd like
         to support attaching a migration to it. *)
      module Filename2 =
        (val if true
             then
               (module struct
                 let dirname x = x
               end : S)
             else assert false)

      let _ = Filename.dirname [@migrate { repl = Core.Filename.basename }]
      let __ = Filename.dirname [@@migrate_test let __ = Core.Filename.basename]
      let _ = Filename2.dirname [@migrate { repl = Core.Filename.basename }]
      let __ = Filename2.dirname [@@migrate_test let __ = Filename2.dirname]
      (* BUG: this should be rewritable, although this is more of a compiler bug:
         the shape is approximated and I think we can do nothing about it, short
         of changing the compiler. *)
    end)

let () =
  test "comment positions"
    (module struct
      let f x y = x + y [@@migrate_manual { repl = (fun x y -> f2 y x) }]

      (* ocamlformat's interface makes it difficult to test comments, in particular:
         - if we use [@@migrate_test], we end up duplicating the AST, but since the
           AST doesn't contain the comments, the comments are not duplicated along
           with it, so the ASTs that get printed earlier get the comments and the
           other ASTs don't.
         - even if we use let[@migrate_test] to have the migrated AST first, the
           result is not the same as making a change in place. In particulier, some
           comments are dropped when doing this that aren't dropped when changing
           the AST "in place"
         So this will have to tested by hand, at least until either we figure out
         a better testing story, ocamlformat improves, or we drop ocamlformat. *)
      let _ =
        (* 1 *)
        let (* 2b *) a (* 2a *) =
          (* 3b *) 1
          (* 3a *)
        in
        (* 4 *)
        f (* 5b *) a
          (* 5a *)
          ((* 6b *) a
          (* 6a *)
          +
          (* 7b *)
          1 (* 7a *))
      (* 8 *)
      (* result: only 5b moves, along with a, so that's reasonable. *)

      let _ =
        (* 4 *)
        f (* 5b *) 3
          (* 5a *)
          ((* 6b *) 4
          (* 6a *)
          +
          (* 7b *)
          1 (* 7a *))

      (* 8 *)
      (* result: only 5b moves to follow the 3, which is ok. This is because we in fact
         ensures that replacing [f x y] by [f2 y z] moves the position of [f] onto [f2],
         so that a comment next to f moves next to f2. We don't need to do anything
         special for argument, because they get inlined into place, replacing the
         inserted parameter, but the opposite happens for the function itself. *)

      (* Reduced test case from dict-gen/common/rules.ml. The A comment should stay
         where it is, not move to the first list_iter. *)
      let list_iter = List.iter [@@migrate_manual { repl = (fun f l -> list_iter ~f l) }]

      let __ () =
        list_iter ignore [ ""; "c" ];
        ignore "for spacing";
        (* A *)
        list_iter ignore [];
        ()

      let f b f = if b then f () else 0
      [@@migrate_manual { repl = (fun b f -> if b then f () else 0) }]

      let _ = f (* 1b *) false (* 1a *) (fun (* 2b *) () -> failwith "no" (* 2a *))
      (* result: let _ = (* all the comments *) 0, which is ok. *)
    end)
