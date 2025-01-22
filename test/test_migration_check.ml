let _ = ( + ) [@migrate { repl = Base.( + ) }]
[@@migrate_test let _ = [ ( + ); Base.( + ) ]]

let _ = [ ( - ); Base.( - ) ] [@migrate] [@@migrate_test let _ = [ ( - ); Base.( - ) ]]
