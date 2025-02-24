(** Minimal support for building traces that can be viewed in browers in
    https://ui.perfetto.dev, or in chrome chrome://tracing. *)

val record : string -> (unit -> 'a) -> 'a
val with_profile : string -> (unit -> 'a) -> 'a
