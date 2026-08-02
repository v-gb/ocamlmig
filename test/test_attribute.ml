(* This is not run automatically, need to figure out how to update run, or how to
   update the code itself to make this possible. *)
let f () = ()
[@@migrate
  { repl = (fun () -> ())
  ; libraries = [ "whatever"; ("something-else", ~min_version:"1.1") ]
  }]

let _ = f
