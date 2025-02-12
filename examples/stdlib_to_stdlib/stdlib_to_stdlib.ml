(** Migration attributes to move from Stdlib names to other preferred Stdlib names, like
    using [Float.acos] instead of [acos].

    This is intended to br used this way:
    {v ocamlmig migrate -extra-migration ocamlmig.stdlib_to_stdlib v} *)

(*
let _ = [ ( == ); phys_equal ] [@@migrate]
let _ = [ ( != ); (fun a b -> not (phys_equal a b)) ] [@@migrate]
 *)

let _ = [ succ; Int.succ ] [@migrate]
let _ = [ pred; Int.pred ] [@migrate]
let _ = [ abs; Int.abs ] [@migrate]
let _ = [ max_int; Int.max_int ] [@migrate]
let _ = [ min_int; Int.min_int ] [@migrate]
let _ = [ exp; Float.exp ] [@migrate]
let _ = [ expm1; Float.expm1 ] [@migrate]
let _ = [ acos; Float.acos ] [@migrate]
let _ = [ asin; Float.asin ] [@migrate]
let _ = [ atan; Float.atan ] [@migrate]
let _ = [ atan2; Float.atan2 ] [@migrate]
let _ = [ hypot; Float.hypot ] [@migrate]
let _ = [ cos; Float.cos ] [@migrate]
let _ = [ cosh; Float.cosh ] [@migrate]
let _ = [ acosh; Float.acosh ] [@migrate]
let _ = [ log; Float.log ] [@migrate]
let _ = [ log10; Float.log10 ] [@migrate]
let _ = [ log1p; Float.log1p ] [@migrate]
let _ = [ sin; Float.sin ] [@migrate]
let _ = [ sinh; Float.sinh ] [@migrate]
let _ = [ asinh; Float.asinh ] [@migrate]
let _ = [ sqrt; Float.sqrt ] [@migrate]
let _ = [ tan; Float.tan ] [@migrate]
let _ = [ tanh; Float.tanh ] [@migrate]
let _ = [ atanh; Float.atanh ] [@migrate]
let _ = [ ceil; Float.ceil ] [@migrate]
let _ = [ floor; Float.floor ] [@migrate]
let _ = [ abs_float; Float.abs ] [@migrate]
let _ = [ copysign; Float.copy_sign ] [@migrate]
let _ = [ mod_float; Float.rem ] [@migrate]
let _ = [ frexp; Float.frexp ] [@migrate]
let _ = [ ldexp; Float.ldexp ] [@migrate]
let _ = [ modf; Float.modf ] [@migrate]
let _ = [ float; Float.of_int ] [@migrate]
let _ = [ float_of_int; Float.of_int ] [@migrate]
let _ = [ truncate; Float.to_int ] [@migrate]
let _ = [ int_of_float; Float.to_int ] [@migrate]
let _ = [ infinity; Float.infinity ] [@migrate]
let _ = [ neg_infinity; Float.neg_infinity ] [@migrate]
let _ = [ nan; Float.nan ] [@migrate]
let _ = [ max_float; Float.max_float ] [@migrate]
let _ = [ min_float; Float.min_float ] [@migrate]
let _ = [ epsilon_float; Float.epsilon ] [@migrate]
let _ = [ classify_float; Float.classify_float ] [@migrate]
let _ = [ int_of_char; Char.code ] [@migrate]
let _ = [ char_of_int; Char.chr ] [@migrate]
let _ = [ string_of_float; Float.to_string ] [@migrate]
let _ = [ float_of_string; Float.of_string ] [@migrate]
let _ = [ float_of_string_opt; Float.of_string_opt ] [@migrate]

(* We could migrate the following values, but this is not clearly better.

let _ = [ stdin; In_channel.stdin ] [@migrate]
let _ = [ stdout; Out_channel.stdout ] [@migrate]
let _ = [ stderr; Out_channel.stderr ] [@migrate]
 *)

let _ = [ print_char; (fun c -> Out_channel.output_char stdout c) ] [@migrate]

(* let _ = [ print_string; (fun c -> Out_channel.output_string stdout c) ] [@migrate] *)

let _ = [ print_bytes; (fun c -> Out_channel.output_bytes stdout c) ] [@migrate]

let _ =
  [ print_int; (fun n -> Out_channel.output_string stdout (Int.to_string n)) ] [@migrate]

let _ =
  [ print_float; (fun f -> Out_channel.output_string stdout (Float.to_string f)) ]
  [@migrate]

(* no equivalent of print_endline/print_newline in Out_channel, weirdly *)

let _ = [ prerr_char; (fun c -> Out_channel.output_char stderr c) ] [@migrate]

(* let _ = [ prerr_string; (fun c -> Out_channel.output_string stderr c) ] [@migrate] *)

let _ = [ prerr_bytes; (fun c -> Out_channel.output_bytes stderr c) ] [@migrate]

let _ =
  [ prerr_int; (fun n -> Out_channel.output_string stderr (Int.to_string n)) ] [@migrate]

let _ =
  [ prerr_float; (fun f -> Out_channel.output_string stderr (Float.to_string f)) ]
  [@migrate]

(* no equivalent of prerr_endline/prerr_newline in Out_channel *)

(* read_{line,int,int_opt,float,float_opt} seems very specific functions. Not sure
   if it is of any help of migrate read_float to Float.of_string (read_line ()) *)

let _ = [ open_out; Out_channel.open_text ] [@migrate]
let _ = [ open_out_bin; Out_channel.open_bin ] [@migrate]
let _ = [ open_out_gen; Out_channel.open_gen ] [@migrate]
let _ = [ flush; Out_channel.flush ] [@migrate]
let _ = [ flush_all; Out_channel.flush_all ] [@migrate]
let _ = [ output_char; Out_channel.output_char ] [@migrate]
let _ = [ output_string; Out_channel.output_string ] [@migrate]
let _ = [ output_bytes; Out_channel.output_bytes ] [@migrate]
let _ = [ output; Out_channel.output ] [@migrate]
let _ = [ output_substring; Out_channel.output_substring ] [@migrate]
let _ = [ output_value; (fun ch a -> Marshal.to_channel ch a []) ] [@migrate]

(* We could map seek_out to (fun ch n -> Out_channel.seek ch (Int64.of_int n)), and
   similarly for the other functions manipulating file offsets, but:

   - seek_out works perfectly fine on 4+GB files on 64bit
   - 64bit are required in native code with ocaml 5
   - using Int64 makes code heavier and slower

   So we'd be making code less nice purely so bytecode can work. Meh. It won't matter
   in browsers either. It might matter if you had wasm outside of the browser. *)

let _ = [ LargeFile.seek_out; Out_channel.seek ] [@migrate]
let _ = [ LargeFile.pos_out; Out_channel.pos ] [@migrate]
let _ = [ LargeFile.out_channel_length; Out_channel.length ] [@migrate]
let _ = [ close_out; Out_channel.close ] [@migrate]
let _ = [ close_out_noerr; Out_channel.close_noerr ] [@migrate]
let _ = [ set_binary_mode_out; Out_channel.set_binary_mode ] [@migrate]
let _ = [ open_in_bin; In_channel.open_bin ] [@migrate]
let _ = [ open_in; In_channel.open_text ] [@migrate]
let _ = [ open_in_gen; In_channel.open_gen ] [@migrate]
let _ = [ input; In_channel.input ] [@migrate]
let _ = [ input_value; Marshal.from_channel ] [@migrate]
let _ = [ LargeFile.seek_in; In_channel.seek ] [@migrate]
let _ = [ LargeFile.pos_in; In_channel.pos ] [@migrate]
let _ = [ LargeFile.in_channel_length; In_channel.length ] [@migrate]
let _ = [ close_in; In_channel.close ] [@migrate]
let _ = [ close_in_noerr; In_channel.close_noerr ] [@migrate]
let _ = [ set_binary_mode_in; In_channel.set_binary_mode ] [@migrate]
