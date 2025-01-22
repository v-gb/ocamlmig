(** A command to show dune's build graph. I could find no existing tool for this, which
    has to be wrong, but dune build --dump-memo-graph is not it, and dune-deps isn't
    either.

    That's been useful to look at why cmt files are built or not.
 *)

val cmd : string * Core.Command.t
