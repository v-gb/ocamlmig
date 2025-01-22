let _ = ( + ) [@migrate { repl = Base.( + ) }]
[@@migrate_test.inverse let _ = Base.( + ) [@migrate { repl = ( + ) }]]

let _ = [ ( - ); Base.( - ) ] [@migrate]
[@@migrate_test.inverse let _ = Base.( - ) [@migrate { repl = ( - ) }]]

let _ = List.mapi [@migrate { repl = (fun f l -> Base.List.mapi l ~f) }]
[@@migrate_test.inverse
  let _ = Base.List.mapi [@migrate { repl = (fun l ~f -> List.mapi f l) }]]

let _ = List.map [@migrate { repl = (fun f l -> (Base.List.map l ~f [@reorder])) }]
[@@migrate_test.inverse
  let _ = Base.List.map [@migrate { repl = (fun l ~f -> List.map f l) }]]

let _ =
  List.partition_map
  [@migrate
    { repl =
        (fun f l ->
          Base.List.partition_map l ~f:(fun x ->
              match f x with Either.Left a -> Base.First.First a | Right b -> Second b))
    }]
[@@migrate_test.inverse let _ = "can't inverse List.partition_map"]
(* this is not supported, but it could be *)

let _ =
  List.fold_left
  [@migrate { repl = (fun f init l -> foldl l init (fun elt acc -> f acc elt)) }]
[@@migrate_test.inverse let _ = "can't inverse List.fold_left"]
(* this is not supported, but it could be *)
