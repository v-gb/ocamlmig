(* A set of annotations to convert Stdlib code to Base, to be used with ocamlmig run
   -side-migrations Stdlib_to_base. This should have decent coverage, but note that it
   only works for functions operating on the same datatypes in both libs. So for
   instance, String functions can be converted, but Queue functions cannot, because the
   queue types are not identical. *)

open struct
  (* toplevel of Stdlib should be fully converted *)
  let _ = raise [@migrate { repl = Base.raise }]
  let _ = raise_notrace [@migrate { repl = Base.Exn.raise_without_backtrace }]
  let _ = invalid_arg [@migrate { repl = Base.invalid_arg }]
  let _ = failwith [@migrate { repl = Base.failwith }]

  (* can't do anything about the exception Exit, Match_failure, etc *)

  let _ =
    ( = )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( = )
          | [%context: string -> _] -> Base.String.( = )
          | [%context: char -> _] -> Base.Char.( = )
          | _ -> Base.Poly.( = ))
      }]

  let _ =
    ( <> )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( <> )
          | [%context: string -> _] -> Base.String.( <> )
          | [%context: char -> _] -> Base.Char.( <> )
          | _ -> Base.Poly.( <> ))
      }]

  let _ =
    ( > )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( > )
          | [%context: string -> _] -> Base.String.( > )
          | [%context: char -> _] -> Base.Char.( > )
          | _ -> Base.Poly.( > ))
      }]

  let _ =
    ( >= )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( >= )
          | [%context: string -> _] -> Base.String.( >= )
          | [%context: char -> _] -> Base.Char.( >= )
          | _ -> Base.Poly.( >= ))
      }]

  let _ =
    ( < )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( < )
          | [%context: string -> _] -> Base.String.( < )
          | [%context: char -> _] -> Base.Char.( < )
          | _ -> Base.Poly.( < ))
      }]

  let _ =
    ( <= )
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.( <= )
          | [%context: string -> _] -> Base.String.( <= )
          | [%context: char -> _] -> Base.Char.( <= )
          | _ -> Base.Poly.( <= ))
      }]

  let _ =
    compare
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.compare
          | [%context: string -> _] -> Base.String.compare
          | [%context: char -> _] -> Base.Char.compare
          | _ -> Base.Poly.compare)
      }]

  let _ =
    min
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.min
          | [%context: string -> _] -> Base.String.min
          | [%context: char -> _] -> Base.Char.min
          | _ -> Base.Poly.min)
      }]

  let _ =
    max
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> Base.max
          | [%context: string -> _] -> Base.String.max
          | [%context: char -> _] -> Base.Char.max
          | _ -> Base.Poly.max)
      }]

  let _ = ( == ) [@migrate { repl = Base.phys_equal }]
  let _ = ( != ) [@migrate { repl = (fun a b -> Base.not (Base.phys_equal a b)) }]
  let _ = not [@migrate { repl = Base.not }]
  let _ = ( && ) [@migrate { repl = Base.( && ) }]
  let _ = ( || ) [@migrate { repl = Base.( || ) }]

  (* no equivalent for __LOC__ etc *)

  let _ = ( |> ) [@migrate { repl = Base.( |> ) }]
  let _ = ( @@ ) [@migrate { repl = Base.( @@ ) }]
  let _ = ( ~- ) [@migrate { repl = Base.( ~- ) }]
  let _ = succ [@migrate { repl = Base.Int.succ }]
  let _ = pred [@migrate { repl = Base.Int.pred }]
  let _ = ( + ) [@migrate { repl = Base.( + ) }]
  let _ = ( - ) [@migrate { repl = Base.( - ) }]
  let _ = ( * ) [@migrate { repl = Base.( * ) }]
  let _ = ( / ) [@migrate { repl = Base.( / ) }]
  let _ = ( mod ) [@migrate { repl = Base.Int.rem }]

  (* This is exposed as Core.(mod) but not Base.(mod). Base.(%) is slighly different *)
  let _ = abs [@migrate { repl = Base.abs }]
  let _ = max_int [@migrate { repl = Base.Int.max_value }]
  let _ = min_int [@migrate { repl = Base.Int.min_value }]
  let _ = ( land ) [@migrate { repl = Base.( land ) }]
  let _ = ( lor ) [@migrate { repl = Base.( lor ) }]
  let _ = ( lxor ) [@migrate { repl = Base.( lxor ) }]
  let _ = lnot [@migrate { repl = Base.lnot }]
  let _ = ( lsl ) [@migrate { repl = Base.( lsl ) }]
  let _ = ( lsr ) [@migrate { repl = Base.( lsr ) }]
  let _ = ( asr ) [@migrate { repl = Base.( asr ) }]
  let _ = ( ~-. ) [@migrate { repl = Base.( ~-. ) }]
  let _ = ( +. ) [@migrate { repl = Base.( +. ) }]
  let _ = ( -. ) [@migrate { repl = Base.( -. ) }]
  let _ = ( *. ) [@migrate { repl = Base.( *. ) }]
  let _ = ( /. ) [@migrate { repl = Base.( /. ) }]
  let _ = ( ** ) [@migrate { repl = Base.( **. ) }]
  let _ = sqrt [@migrate { repl = Base.Float.sqrt }]
  let _ = exp [@migrate { repl = Base.Float.exp }]
  let _ = log [@migrate { repl = Base.Float.log }]
  let _ = log10 [@migrate { repl = Base.Float.log10 }]
  let _ = expm1 [@migrate { repl = Base.Float.expm1 }]
  let _ = log1p [@migrate { repl = Base.Float.log1p }]
  let _ = cos [@migrate { repl = Base.Float.cos }]
  let _ = sin [@migrate { repl = Base.Float.sin }]
  let _ = tan [@migrate { repl = Base.Float.tan }]
  let _ = acos [@migrate { repl = Base.Float.acos }]
  let _ = asin [@migrate { repl = Base.Float.asin }]
  let _ = atan [@migrate { repl = Base.Float.atan }]
  let _ = atan2 [@migrate { repl = Base.Float.atan2 }]
  let _ = hypot [@migrate { repl = Base.Float.hypot }]
  let _ = cosh [@migrate { repl = Base.Float.cosh }]
  let _ = sinh [@migrate { repl = Base.Float.sinh }]
  let _ = tanh [@migrate { repl = Base.Float.tanh }]
  let _ = acosh [@migrate { repl = Base.Float.acosh }]
  let _ = asinh [@migrate { repl = Base.Float.asinh }]
  let _ = atanh [@migrate { repl = Base.Float.atanh }]
  let _ = ceil [@migrate { repl = Base.Float.round_up }]
  let _ = floor [@migrate { repl = Base.Float.round_down }]
  let _ = abs_float [@migrate { repl = Base.Float.abs }]
  let _ = copysign [@migrate { repl = Base.Float.copysign }]
  let _ = mod_float [@migrate { repl = Base.Float.mod_float }]
  let _ = frexp [@migrate { repl = Base.Float.frexp }]
  let _ = ldexp [@migrate { repl = Base.Float.ldexp }]

  (* let _ = modf [@migrate { repl = Base.Float.modf }] involves a different type *)

  let _ = float [@migrate { repl = Base.Float.of_int }]
  let _ = float_of_int [@migrate { repl = Base.Float.of_int }]
  let _ = truncate [@migrate { repl = Base.Int.of_float }]
  let _ = int_of_float [@migrate { repl = Base.Int.of_float }]
  let _ = infinity [@migrate { repl = Base.Float.infinity }]
  let _ = neg_infinity [@migrate { repl = Base.Float.neg_infinity }]
  let _ = nan [@migrate { repl = Base.Float.nan }]
  let _ = max_float [@migrate { repl = Base.Float.max_finite_value }]
  let _ = min_float [@migrate { repl = Base.Float.min_positive_normal_value }]
  let _ = epsilon_float [@migrate { repl = Base.Float.epsilon_float }]

  (* let _ = classify_float [@migrate { repl = Base.Float.classify }] involves a different type *)

  let _ = ( ^ ) [@migrate { repl = Base.( ^ ) }]
  let _ = int_of_char [@migrate { repl = Base.Char.to_int }]
  let _ = char_of_int [@migrate { repl = Base.Char.of_int_exn }]
  let _ = ignore [@migrate { repl = Base.ignore }]
  let _ = string_of_bool [@migrate { repl = Base.Bool.to_string }]
  let _ = bool_of_string [@migrate { repl = Base.Bool.of_string }]

  (* no equivalent of bool_of_string_opt *)

  let _ = string_of_int [@migrate { repl = Base.Int.to_string }]
  let _ = int_of_string [@migrate { repl = Base.Int.of_string }]

  (* no equivalent of int_of_string_opt *)

  let _ = string_of_float [@migrate { repl = Base.Float.to_string }]
  let _ = float_of_string [@migrate { repl = Base.Float.of_string }]

  (* no equivalent of float_of_string_opt *)

  let _ = fst [@migrate { repl = Base.fst }]
  let _ = snd [@migrate { repl = Base.snd }]
  let _ = ( @ ) [@migrate { repl = Base.( @ ) }]

  (* Stdio exposes stdin at toplevel, but Core deprecates it :/ *)
  let _ = stdin [@migrate { repl = Stdio.In_channel.stdin; libraries = [ "stdio" ] }]
  let _ = stdout [@migrate { repl = Stdio.Out_channel.stdout; libraries = [ "stdio" ] }]
  let _ = stderr [@migrate { repl = Stdio.Out_channel.stderr; libraries = [ "stdio" ] }]

  let _ =
    print_char
    [@migrate
      { repl = (fun c -> Stdio.Out_channel.output_char Stdio.Out_channel.stdout c)
      ; libraries = [ "stdio" ]
      }]

  let _ = print_string [@migrate { repl = Stdio.print_string; libraries = [ "stdio" ] }]

  let _ =
    print_bytes
    [@migrate
      { repl = (fun b -> Stdio.Out_channel.output_bytes Stdio.Out_channel.stdout b)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    print_int
    [@migrate
      { repl = (fun n -> Stdio.print_string (Base.Int.to_string n))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    print_float
    [@migrate
      { repl = (fun f -> Stdio.print_string (Base.Float.to_string f))
      ; libraries = [ "stdio" ]
      }]

  let _ = print_endline [@migrate { repl = Stdio.print_endline; libraries = [ "stdio" ] }]

  let _ =
    print_newline
    [@migrate { repl = (fun () -> Stdio.print_endline ""); libraries = [ "stdio" ] }]

  let _ =
    prerr_char
    [@migrate
      { repl = (fun c -> Stdio.Out_channel.output_char Stdio.Out_channel.stderr c)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    prerr_string
    [@migrate
      { repl = (fun s -> Stdio.Out_channel.output_string Stdio.Out_channel.stderr s)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    prerr_bytes
    [@migrate
      { repl = (fun b -> Stdio.Out_channel.output_bytes Stdio.Out_channel.stderr b)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    prerr_int
    [@migrate
      { repl =
          (fun n ->
            Stdio.Out_channel.output_string Stdio.Out_channel.stderr
              (Base.Int.to_string n))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    prerr_float
    [@migrate
      { repl =
          (fun f ->
            Stdio.Out_channel.output_string Stdio.Out_channel.stderr
              (Base.Float.to_string f))
      ; libraries = [ "stdio" ]
      }]

  let _ = prerr_endline [@migrate { repl = Stdio.prerr_endline; libraries = [ "stdio" ] }]

  let _ =
    prerr_newline
    [@migrate { repl = (fun () -> Stdio.prerr_endline ""); libraries = [ "stdio" ] }]

  let _ =
    read_line
    [@migrate
      { repl =
          (fun () ->
            Stdio.In_channel.input_line_exn Stdio.In_channel.stdin ~fix_win_eol:false)
      ; libraries = [ "stdio" ]
      }]

  (* Ignoring read_int_opt read_int read_float_opt read_float, which seem like
     exceedingly rare functions *)

  let _ = open_out [@migrate { repl = Stdio.Out_channel.create; libraries = [ "stdio" ] }]

  let _ =
    open_out_bin
    [@migrate { repl = Stdio.Out_channel.create ~binary:false; libraries = [ "stdio" ] }]

  (* no equivalent of open_out_gen *)

  let _ = flush [@migrate { repl = Stdio.Out_channel.flush; libraries = [ "stdio" ] }]

  (* probably no equivalent for flush_all *)

  let _ =
    output_char
    [@migrate { repl = Stdio.Out_channel.output_char; libraries = [ "stdio" ] }]

  let _ =
    output_string
    [@migrate { repl = Stdio.Out_channel.output_string; libraries = [ "stdio" ] }]

  let _ =
    output_bytes
    [@migrate { repl = Stdio.Out_channel.output_bytes; libraries = [ "stdio" ] }]

  let _ =
    output
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.Out_channel.output ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    output_substring
    [@migrate
      { repl =
          (fun ch buf pos len -> Stdio.Out_channel.output_substring ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    output_byte
    [@migrate { repl = Stdio.Out_channel.output_byte; libraries = [ "stdio" ] }]

  let _ =
    output_binary_int
    [@migrate { repl = Stdio.Out_channel.output_binary_int; libraries = [ "stdio" ] }]

  let _ =
    output_value
    [@migrate { repl = Stdio.Out_channel.output_value; libraries = [ "stdio" ] }]

  let _ =
    seek_out
    [@migrate
      { repl = (fun ch n -> Stdio.Out_channel.seek ch (Base.Int64.of_int n))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    pos_out
    [@migrate
      { repl = (fun ch -> Base.Int64.to_int_exn (Stdio.Out_channel.pos ch))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    out_channel_length
    [@migrate
      { repl = (fun ch -> Base.Int64.to_int_exn (Stdio.Out_channel.length ch))
      ; libraries = [ "stdio" ]
      }]

  let _ = close_out [@migrate { repl = Stdio.Out_channel.close; libraries = [ "stdio" ] }]

  let _ =
    close_out_noerr
    [@migrate { repl = Stdio.Out_channel.close_no_err; libraries = [ "stdio" ] }]

  let _ =
    set_binary_mode_out
    [@migrate { repl = Stdio.Out_channel.set_binary_mode; libraries = [ "stdio" ] }]

  let _ = open_in [@migrate { repl = Stdio.In_channel.create; libraries = [ "stdio" ] }]

  let _ =
    open_in_bin
    [@migrate { repl = Stdio.In_channel.create ~binary:false; libraries = [ "stdio" ] }]

  (* no equivalent of open_in_gen *)

  (*
  let _ =
    input_char
    [@migrate
      { repl = Stdio.In_channel.input_char; libraries = [ "stdio" ] }]
   *)
  (* would need _exn of this, and same thing with input_byte, input_binary_int,
     input_value *)

  let _ =
    input_line
    [@migrate
      { repl = (fun ch -> Stdio.In_channel.input_line_exn ~fix_win_eol:false ch)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    input
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.In_channel.input ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    really_input
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.In_channel.really_input_exn ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  (* no equivalent of really_input_string *)

  let _ =
    seek_in
    [@migrate
      { repl = (fun ch n -> Stdio.In_channel.seek ch (Base.Int64.of_int n))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    pos_in
    [@migrate
      { repl = (fun ch -> Base.Int64.to_int_exn (Stdio.In_channel.pos ch))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    in_channel_length
    [@migrate
      { repl = (fun ch -> Base.Int64.to_int_exn (Stdio.In_channel.length ch))
      ; libraries = [ "stdio" ]
      }]

  let _ =
    set_binary_mode_in
    [@migrate { repl = Stdio.In_channel.set_binary_mode; libraries = [ "stdio" ] }]

  let _ = close_in [@migrate { repl = Stdio.In_channel.close; libraries = [ "stdio" ] }]

  (*
  let _ =
    close_in_noerr
    [@migrate { repl = Stdio.In_channel.close_no_err; libraries = [ "stdio" ] }]
   *)
  (* turns out this function is not provided, unlike in out_channel *)

  let _ =
    LargeFile.seek_out
    [@migrate { repl = Stdio.Out_channel.seek; libraries = [ "stdio" ] }]

  let _ =
    LargeFile.pos_out [@migrate { repl = Stdio.Out_channel.pos; libraries = [ "stdio" ] }]

  let _ =
    LargeFile.out_channel_length
    [@migrate { repl = Stdio.Out_channel.length; libraries = [ "stdio" ] }]

  let _ =
    LargeFile.seek_in [@migrate { repl = Stdio.In_channel.seek; libraries = [ "stdio" ] }]

  let _ =
    LargeFile.pos_in [@migrate { repl = Stdio.In_channel.pos; libraries = [ "stdio" ] }]

  let _ =
    LargeFile.in_channel_length
    [@migrate { repl = Stdio.In_channel.length; libraries = [ "stdio" ] }]

  let _ = ref [@migrate { repl = Base.ref }]
  let _ = ( ! ) [@migrate { repl = Base.( ! ) }]
  let _ = incr [@migrate { repl = Base.Int.incr }]
  let _ = decr [@migrate { repl = Base.Int.decr }]

  (* no equivalent of string_of_format, format_of_string, (^^) *)

  (* not sure where the replacements are for exit, at_exit *)
end

open struct
  open Buffer (* should be fully converted *)

  let _ = create [@migrate { repl = Base.Buffer.create }]
  let _ = contents [@migrate { repl = Base.Buffer.contents }]
  let _ = to_bytes [@migrate { repl = Base.Buffer.contents_bytes }]

  let _ =
    sub [@migrate { repl = (fun buf pos len -> Base.Buffer.To_string.sub buf ~pos ~len) }]

  let _ =
    blit
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Buffer.blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  let _ = nth [@migrate { repl = Base.Buffer.nth }]
  let _ = length [@migrate { repl = Base.Buffer.length }]
  let _ = clear [@migrate { repl = Base.Buffer.clear }]
  let _ = reset [@migrate { repl = Base.Buffer.reset }]

  let _ =
    output_buffer
    [@migrate { repl = Stdio.Out_channel.output_buffer; libraries = [ "stdio" ] }]

  (* no equivalent of truncate *)

  let _ = add_char [@migrate { repl = Base.Buffer.add_char }]

  (* no equivalent of add_utf_8_uchar, etc *)

  let _ = add_string [@migrate { repl = Base.Buffer.add_string }]
  let _ = add_bytes [@migrate { repl = Base.Buffer.add_bytes }]

  let _ =
    add_substring
    [@migrate { repl = (fun buf s pos len -> Base.Buffer.add_substring buf s ~pos ~len) }]

  let _ =
    add_subbytes
    [@migrate { repl = (fun buf b pos len -> Base.Buffer.add_subbytes buf b ~pos ~len) }]

  (* no equivalent of add_substitute *)

  let _ = add_buffer [@migrate { repl = Base.Buffer.add_buffer }]

  (* I don't see an equivalent of Buffer.add_channel in In_channel *)
  (* no equivalent of seq functions *)
  (* don't know where the binary int functions are *)
end

open struct
  open Bytes (* should be fully converted *)

  let _ = length [@migrate { repl = Base.Bytes.length }]
  let _ = get [@migrate { repl = Base.Bytes.get }]
  let _ = set [@migrate { repl = Base.Bytes.set }]
  let _ = create [@migrate { repl = Base.Bytes.create }]
  let _ = make [@migrate { repl = Base.Bytes.make }]
  let _ = init [@migrate { repl = (fun n f -> Base.Bytes.init n ~f) }]

  (* no equivalent for empty *)

  let _ = copy [@migrate { repl = Base.Bytes.copy }]
  let _ = of_string [@migrate { repl = Base.Bytes.of_string }]
  let _ = to_string [@migrate { repl = Base.Bytes.to_string }]
  let _ = sub [@migrate { repl = (fun str pos len -> Base.Bytes.sub str ~pos ~len) }]

  let _ =
    sub_string
    [@migrate { repl = (fun str pos len -> Base.Bytes.To_string.sub str ~pos ~len) }]

  (* no equivalent of extend that I can see *)

  let _ = fill [@migrate { repl = (fun b pos len c -> Base.Bytes.fill b ~pos ~len c) }]

  let _ =
    blit
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  let _ =
    blit_string
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.From_string.blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  (* no equivalent of concat, cat, iter, iteri *)

  let _ = map [@migrate { repl = (fun f s -> Base.Bytes.map s ~f) }]
  let _ = mapi [@migrate { repl = (fun f s -> Base.Bytes.mapi s ~f) }]

  (* no equivalent of fold_left, fold_right, for_all, exists, trim, escaped, {,r}index{,_from}{,_opt} *)

  let _ = contains [@migrate { repl = (fun b c -> Base.Bytes.contains b c) }]

  let _ =
    contains_from [@migrate { repl = (fun s pos c -> Base.Bytes.contains s c ~pos) }]

  (* no equivalent to rcontains_from, {uppercase,lowercase,capitalize,uncapitalize}_ascii *)

  let _ = equal [@migrate { repl = Base.Bytes.equal }]
  let _ = compare [@migrate { repl = Base.Bytes.compare }]

  (* no equivalent to starts_with, ends_with *)

  let _ =
    unsafe_to_string
    [@migrate
      { repl =
          (fun b -> Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b)
      }]

  let _ =
    unsafe_of_string [@migrate { repl = Base.Bytes.unsafe_of_string_promise_no_mutation }]

  (* no equivalent to split_on_char, to_seq, to_seqi, of_seq *)
  (* no equivalent to get_utf_8_uchar, but there is set_utf_8_uchar etc? weird *)

  let _ = set_utf_8_uchar [@migrate { repl = Base.Bytes.Utf8.set }]

  (* not sure where the equivalent of set_uint8 are. *)

  let _ = unsafe_get [@migrate { repl = Base.Bytes.unsafe_get }]
  let _ = unsafe_set [@migrate { repl = Base.Bytes.unsafe_set }]

  let _ =
    unsafe_blit
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  let _ =
    unsafe_blit_string
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.From_string.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  (* no equivalent of unsafe_fill *)
  (* no equivalent of unsafe_escape *)
end

open struct
  open Char (* should be fully converted *)

  let _ = code [@migrate { repl = Base.Char.to_int }]
  let _ = chr [@migrate { repl = Base.Char.of_int_exn }]
  let _ = unsafe_chr [@migrate { repl = Base.Char.unsafe_of_int }]
  let _ = escaped [@migrate { repl = Base.Char.escaped }]
  let _ = lowercase_ascii [@migrate { repl = Base.Char.lowercase }]
  let _ = uppercase_ascii [@migrate { repl = Base.Char.uppercase }]
  let _ = compare [@migrate { repl = Base.Char.compare }]
  let _ = equal [@migrate { repl = Base.Char.equal }]
  (* leaving the hash functions unconverted *)
end

open struct
  open Filename

  let _ =
    chop_extension
    [@migrate
      { repl = Filename_base.chop_extension; libraries = [ "core.filename_base" ] }]
  (* no equivalent for remove_extension! *)

  let _ =
    dirname
    [@migrate { repl = Filename_base.dirname; libraries = [ "core.filename_base" ] }]

  let _ =
    basename
    [@migrate { repl = Filename_base.basename; libraries = [ "core.filename_base" ] }]
  (* Reducing the shapes for these identifiers doesn't work, which prevents our the
     rewriting. We should fix this to simply require a normal form, instead of a
     definition. And probably drop all this shape stuff. *)

  let _ =
    concat
    [@migrate { repl = Filename_base.concat; libraries = [ "core.filename_base" ] }]
  (* not exactly equivalent for two reasons:
     - [concat "" "/a"] is an error with Core, but returns "/a" with Stdlib, which is
       unlikely to ever be correct. Given even open("", O_PATH, 0) rejects the empty
       path, the behavior of Core is correct.
     - some "." get cleaned up with Core. *)

  let _ =
    temp_file
    [@migrate
      { repl =
          (fun ?temp_dir prefix suffix ->
            Filename_unix.temp_file ?in_dir:temp_dir prefix suffix)
      ; libraries = [ "core_unix.filename_unix" ]
      }]

  let _ =
    temp_dir
    [@migrate
      { repl =
          (fun ?temp_dir ?perms prefix suffix ->
            Filename_unix.temp_dir ?in_dir:temp_dir ?perm:perms prefix suffix)
      ; libraries = [ "core_unix.filename_unix" ]
      }]
end

open struct
  open Float

  let _ = of_int [@migrate { repl = Base.Float.of_int }]
end

open struct
  open In_channel (* should be fully converted *)

  let _ = stdin [@migrate { repl = Stdio.In_channel.stdin; libraries = [ "stdio" ] }]
  let _ = open_bin [@migrate { repl = Stdio.In_channel.create; libraries = [ "stdio" ] }]

  let _ =
    open_text
    [@migrate { repl = Stdio.In_channel.create ~binary:false; libraries = [ "stdio" ] }]

  (* no clear equivalent to open_gen *)

  let _ =
    with_open_bin
    [@migrate
      { repl = (fun path f -> Stdio.In_channel.with_file path ~f)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    with_open_text
    [@migrate
      { repl = (fun path f -> Stdio.In_channel.with_file ~binary:false path ~f)
      ; libraries = [ "stdio" ]
      }]

  (* no clear equivalent to with_open_gen *)

  let _ = close [@migrate { repl = Stdio.In_channel.close; libraries = [ "stdio" ] }]

  (*
  let _ =
    close_noerr
    [@migrate { repl = Stdio.In_channel.close_no_err; libraries = [ "stdio" ] }]
         *)
  (* no equivalent, weirdly *)

  (*
  let _ =
    input_char
    [@migrate { repl = Stdio.In_channel.input_char; libraries = [ "stdio" ] }]

  let _ =
    input_byte
    [@migrate { repl = Stdio.In_channel.input_byte; libraries = [ "stdio" ] }]
   *)
  (* would need _exn versions of these *)

  let _ =
    input_line
    [@migrate
      { repl = (fun ch -> Stdio.In_channel.input_line ch ~fix_win_eol:false)
      ; libraries = [ "stdio" ]
      }]

  (* no equivalent of really_input_string, I think *)

  let _ =
    input_all [@migrate { repl = Stdio.In_channel.input_all; libraries = [ "stdio" ] }]

  let _ =
    input_lines
    [@migrate
      { repl = (fun ch -> Stdio.In_channel.input_lines ch ~fix_win_eol:false)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    input
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.In_channel.input ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  (* no equivalent of input_bigarray *)

  let _ =
    really_input
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.In_channel.really_input ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  (* no equivalent of really_input_bigarray *)
  let _ =
    fold_lines
    [@migrate
      { repl =
          (fun f init ch -> Stdio.In_channel.fold_lines ~fix_win_eol:false ch ~init ~f)
      ; libraries = [ "stdio" ]
      }]

  let _ = seek [@migrate { repl = Stdio.In_channel.seek; libraries = [ "stdio" ] }]
  let _ = pos [@migrate { repl = Stdio.In_channel.pos; libraries = [ "stdio" ] }]
  let _ = length [@migrate { repl = Stdio.In_channel.length; libraries = [ "stdio" ] }]

  let _ =
    set_binary_mode
    [@migrate { repl = Stdio.In_channel.set_binary_mode; libraries = [ "stdio" ] }]
  (* no equivalent of is_binary_mode, isatty *)
end

open struct
  open Int (* should be fully converted *)

  let _ = zero [@migrate { repl = Base.Int.zero }]
  let _ = one [@migrate { repl = Base.Int.one }]
  let _ = minus_one [@migrate { repl = Base.Int.zero }]
  let _ = neg [@migrate { repl = Base.Int.neg }]
  let _ = add [@migrate { repl = Base.Int.( + ) }]
  let _ = sub [@migrate { repl = Base.Int.( - ) }]
  let _ = div [@migrate { repl = Base.Int.( / ) }]
  let _ = rem [@migrate { repl = Base.Int.( / ) }]
  let _ = to_string [@migrate { repl = Base.Int.to_string }]
  let _ = rem [@migrate { repl = Base.Int.rem }]
  let _ = succ [@migrate { repl = Base.Int.succ }]
  let _ = pred [@migrate { repl = Base.Int.pred }]
  let _ = abs [@migrate { repl = Base.Int.abs }]
  let _ = max_int [@migrate { repl = Base.Int.max_value }]
  let _ = min_int [@migrate { repl = Base.Int.min_value }]
  let _ = logand [@migrate { repl = Base.Int.bit_and }]
  let _ = logor [@migrate { repl = Base.Int.bit_or }]
  let _ = logxor [@migrate { repl = Base.Int.bit_xor }]
  let _ = lognot [@migrate { repl = Base.Int.bit_not }]
  let _ = shift_left [@migrate { repl = Base.Int.shift_left }]
  let _ = shift_right [@migrate { repl = Base.Int.shift_right }]
  let _ = shift_right_logical [@migrate { repl = Base.Int.shift_right_logical }]
  let _ = equal [@migrate { repl = Base.Int.equal }]
  let _ = compare [@migrate { repl = Base.Int.compare }]
  let _ = min [@migrate { repl = Base.Int.min }]
  let _ = max [@migrate { repl = Base.Int.max }]
  let _ = to_float [@migrate { repl = Base.Int.to_float }]
  let _ = of_float [@migrate { repl = Base.Int.of_float_unchecked }]
  let _ = to_string [@migrate { repl = Base.Int.to_string }]
  (* leaving hash functions alone *)
end

open struct
  open Lazy (* should be fully converted *)

  let _ = force [@migrate { repl = Base.force }]
  let _ = map [@migrate { repl = (fun f l -> Base.Lazy.map l ~f) }]
  let _ = is_val [@migrate { repl = Base.Lazy.is_val }]
  let _ = from_val [@migrate { repl = Base.Lazy.from_val }]

  (* no equivalent of map_val *)
  let _ = from_fun [@migrate { repl = Base.Lazy.from_fun }]
  let _ = force_val [@migrate { repl = Base.Lazy.force_val }]
end

open struct
  open List (* should be fully converted *)

  let _ = length [@migrate { repl = Base.List.length }]
  let _ = is_empty [@migrate { repl = Base.List.is_empty }]

  (* no equivalent of compare_lengths, compare_length_with *)

  let _ = cons [@migrate { repl = Base.List.cons }]
  let _ = hd [@migrate { repl = Base.List.hd_exn }]
  let _ = tl [@migrate { repl = Base.List.tl_exn }]
  let _ = nth [@migrate { repl = Base.List.nth_exn }]
  let _ = nth_opt [@migrate { repl = Base.List.nth }]
  let _ = rev [@migrate { repl = Base.List.rev }]
  let _ = init [@migrate { repl = (fun n f -> Base.List.init n ~f) }]
  let _ = append [@migrate { repl = Base.List.append }]
  let _ = rev_append [@migrate { repl = Base.List.rev_append }]
  let _ = concat [@migrate { repl = Base.List.concat }]
  let _ = flatten [@migrate { repl = Base.List.concat }]
  let _ = equal [@migrate { repl = Base.List.equal }]
  let _ = compare [@migrate { repl = Base.List.compare }]
  let _ = iter [@migrate { repl = (fun f l -> Base.List.iter l ~f) }]
  let _ = iteri [@migrate { repl = (fun f l -> Base.List.iteri l ~f) }]
  let _ = map [@migrate { repl = (fun f l -> (Base.List.map l ~f [@reorder])) }]
  let _ = mapi [@migrate { repl = (fun f l -> Base.List.mapi l ~f) }]
  let _ = rev_map [@migrate { repl = (fun f l -> Base.List.rev_map l ~f) }]
  let _ = filter_map [@migrate { repl = (fun f l -> Base.List.filter_map l ~f) }]
  let _ = concat_map [@migrate { repl = (fun f l -> Base.List.concat_map l ~f) }]

  let _ =
    fold_left_map [@migrate { repl = (fun f init l -> Base.List.fold_map l ~init ~f) }]

  let _ = fold_left [@migrate { repl = (fun f init l -> Base.List.fold_left l ~init ~f) }]

  let _ =
    fold_right [@migrate { repl = (fun f l init -> Base.List.fold_right l ~init ~f) }]

  let _ = iter2 [@migrate { repl = (fun f l1 l2 -> Base.List.iter2_exn l1 l2 ~f) }]
  let _ = map2 [@migrate { repl = (fun f l1 l2 -> Base.List.map2_exn l1 l2 ~f) }]
  let _ = rev_map2 [@migrate { repl = (fun f l1 l2 -> Base.List.rev_map2_exn l1 l2 ~f) }]

  let _ =
    fold_left2
    [@migrate { repl = (fun f init l1 l2 -> Base.List.fold2_exn l1 l2 ~init ~f) }]

  let _ =
    fold_right2
    [@migrate { repl = (fun f l1 l2 init -> Base.List.fold_right2_exn l1 l2 ~init ~f) }]

  let _ = for_all [@migrate { repl = (fun f l -> Base.List.for_all l ~f) }]
  let _ = exists [@migrate { repl = (fun f l -> Base.List.exists l ~f) }]
  let _ = for_all2 [@migrate { repl = (fun f l -> Base.List.for_all2_exn l ~f) }]
  let _ = exists2 [@migrate { repl = (fun f l -> Base.List.exists2_exn l ~f) }]

  let _ =
    mem
    [@migrate
      { repl =
          (function
          | [%context: int -> _] -> fun a l -> Base.List.mem ~equal:Base.( = ) l a
          | [%context: string -> _] ->
              fun a l -> Base.List.mem ~equal:Base.String.( = ) l a
          | [%context: char -> _] -> fun a l -> Base.List.mem ~equal:Base.Char.( = ) l a
          | _ -> fun a l -> Base.List.mem ~equal:Base.Poly.( = ) l a)
      }]

  let _ = memq [@migrate { repl = (fun a l -> Base.List.mem ~equal:Base.phys_equal l a) }]

  (* let _ = find [@migrate { repl = (fun f l -> Base.List.find_exn l ~f) }] *)
  let _ = find_opt [@migrate { repl = (fun f l -> Base.List.find l ~f) }]

  let _ =
    find_index
    [@migrate
      { repl =
          (fun f l ->
            match Base.List.findi l ~f:(fun _ a -> f a) with
            | None -> None
            | Some (i, _) -> Some i)
      }]

  let _ = find_map [@migrate { repl = (fun f l -> Base.List.find_map l ~f) }]
  let _ = find_mapi [@migrate { repl = (fun f l -> Base.List.find_mapi l ~f) }]
  let _ = filter [@migrate { repl = (fun f l -> Base.List.filter l ~f) }]
  let _ = find_all [@migrate { repl = (fun f l -> Base.List.filter l ~f) }]
  let _ = filteri [@migrate { repl = (fun f l -> Base.List.filteri l ~f) }]
  let _ = partition [@migrate { repl = (fun f l -> Base.List.partition_tf l ~f) }]

  let _ =
    partition_map
    [@migrate
      { repl =
          (fun f l ->
            Base.List.partition_map l ~f:(fun a ->
                match f a with Stdlib.Either.Left l -> First l | Right r -> Second r))
      }]

  (* let _ =
   *   assoc
   *   [@migrate
   *     { repl = (fun a l -> Base.List.Assoc.find_exn ~equal:Base.Poly.( = ) l a) }] *)

  let _ =
    assoc_opt
    [@migrate { repl = (fun a l -> Base.List.Assoc.find ~equal:Base.Poly.( = ) l a) }]

  (* let _ =
   *   assq
   *   [@migrate
   *     { repl = (fun a l -> Base.List.Assoc.find_exn ~equal:Base.phys_equal l a) }] *)

  let _ =
    assq_opt
    [@migrate { repl = (fun a l -> Base.List.Assoc.find ~equal:Base.phys_equal l a) }]

  let _ =
    mem_assoc
    [@migrate { repl = (fun a l -> Base.List.Assoc.mem ~equal:Base.Poly.( = ) l a) }]

  let _ =
    mem_assq
    [@migrate { repl = (fun a l -> Base.List.Assoc.mem ~equal:Base.phys_equal l a) }]

  let _ =
    remove_assoc
    [@migrate { repl = (fun a l -> Base.List.Assoc.remove ~equal:Base.Poly.( = ) l a) }]

  let _ =
    remove_assq
    [@migrate { repl = (fun a l -> Base.List.Assoc.remove ~equal:Base.phys_equal l a) }]

  let _ = split [@migrate { repl = Base.List.unzip }]
  let _ = combine [@migrate { repl = Base.List.zip_exn }]
  let _ = sort [@migrate { repl = (fun compare l -> Base.List.sort ~compare l) }]

  let _ =
    stable_sort [@migrate { repl = (fun compare l -> Base.List.stable_sort ~compare l) }]

  let _ = fast_sort [@migrate { repl = (fun compare l -> Base.List.sort ~compare l) }]

  let _ =
    sort_uniq [@migrate { repl = (fun compare l -> Base.List.dedup_and_sort ~compare l) }]

  let _ =
    merge [@migrate { repl = (fun compare l1 l2 -> Base.List.merge l1 l2 ~compare) }]

  (* no equivalent of to_seq, of_seq *)
end

open struct
  open Option (* should be fully converted *)

  let _ = none [@migrate { repl = None }]
  let _ = some [@migrate { repl = Base.Option.some }]
  let _ = value [@migrate { repl = Base.Option.value }]
  let _ = get [@migrate { repl = (fun o -> Base.Option.value_exn o) }]
  let _ = bind [@migrate { repl = (fun o f -> Base.Option.bind o ~f) }]
  let _ = join [@migrate { repl = Base.Option.join }]
  let _ = map [@migrate { repl = (fun f o -> Base.Option.map o ~f) }]

  let _ =
    fold
    [@migrate
      { repl = (fun ~none ~some o -> Base.Option.value_map o ~default:none ~f:some) }]

  let _ = iter [@migrate { repl = (fun f o -> Base.Option.iter o ~f) }]
  let _ = is_none [@migrate { repl = Base.Option.is_none }]
  let _ = is_some [@migrate { repl = Base.Option.is_some }]
  let _ = equal [@migrate { repl = Base.Option.equal }]
  let _ = compare [@migrate { repl = Base.Option.compare }]

  let _ =
    to_result [@migrate { repl = (fun ~none o -> Base.Result.of_option ~error:none o) }]

  let _ = to_list [@migrate { repl = Base.Option.to_list }]
  (* no equivalent for to_seq *)
end

open struct
  open Out_channel (* should be fully converted *)

  let _ = stdout [@migrate { repl = Stdio.Out_channel.stdout; libraries = [ "stdio" ] }]
  let _ = stderr [@migrate { repl = Stdio.Out_channel.stderr; libraries = [ "stdio" ] }]
  let _ = open_bin [@migrate { repl = Stdio.Out_channel.create; libraries = [ "stdio" ] }]

  let _ =
    open_text
    [@migrate { repl = Stdio.Out_channel.create ~binary:false; libraries = [ "stdio" ] }]

  (* no clear equivalent to open_gen *)

  let _ =
    with_open_bin
    [@migrate
      { repl = (fun path f -> Stdio.Out_channel.with_file path ~f)
      ; libraries = [ "stdio" ]
      }]
  (* here it'd good to have context matching to turn
     Out_channel.with_open_bin old_path (fun oc ->
       Out_channel.output_string oc src)
     into
       Stdio.Out_channel.write_all old_path ~data:src
  *)

  let _ =
    with_open_text
    [@migrate
      { repl = (fun path f -> Stdio.Out_channel.with_file ~binary:false path ~f)
      ; libraries = [ "stdio" ]
      }]

  (* no clear equivalent to with_open_gen *)

  let _ = close [@migrate { repl = Stdio.Out_channel.close; libraries = [ "stdio" ] }]

  let _ =
    close_noerr
    [@migrate { repl = Stdio.Out_channel.close_no_err; libraries = [ "stdio" ] }]

  let _ =
    output_char
    [@migrate { repl = Stdio.Out_channel.output_char; libraries = [ "stdio" ] }]

  let _ =
    output_byte
    [@migrate { repl = Stdio.Out_channel.output_byte; libraries = [ "stdio" ] }]

  let _ =
    output_string
    [@migrate { repl = Stdio.Out_channel.output_string; libraries = [ "stdio" ] }]

  let _ =
    output_bytes
    [@migrate { repl = Stdio.Out_channel.output_bytes; libraries = [ "stdio" ] }]

  let _ =
    output
    [@migrate
      { repl = (fun ch buf pos len -> Stdio.Out_channel.output ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  let _ =
    output_substring
    [@migrate
      { repl =
          (fun ch buf pos len -> Stdio.Out_channel.output_substring ch ~buf ~pos ~len)
      ; libraries = [ "stdio" ]
      }]

  (* no idea where output_bigarray is *)

  let _ = flush [@migrate { repl = Stdio.Out_channel.flush; libraries = [ "stdio" ] }]

  (* probably no equivalent for flush_all *)

  let _ = seek [@migrate { repl = Stdio.Out_channel.seek; libraries = [ "stdio" ] }]
  let _ = pos [@migrate { repl = Stdio.Out_channel.pos; libraries = [ "stdio" ] }]
  let _ = length [@migrate { repl = Stdio.Out_channel.length; libraries = [ "stdio" ] }]

  let _ =
    set_binary_mode
    [@migrate { repl = Stdio.Out_channel.set_binary_mode; libraries = [ "stdio" ] }]

  (* no equivalent of is_binary_mode, set_buffered, is_buffered, isatty *)
end

open struct
  open Bool (* should be fully converted *)

  let _ = not [@migrate { repl = Base.not }]
  let _ = ( && ) [@migrate { repl = Base.( && ) }]
  let _ = ( || ) [@migrate { repl = Base.( || ) }]
  let _ = equal [@migrate { repl = Base.Bool.equal }]
  let _ = compare [@migrate { repl = Base.Bool.compare }]
  let _ = to_int [@migrate { repl = Base.Bool.to_int }]
  let _ = to_string [@migrate { repl = Base.Bool.to_string }]

  (* leaving the hash functions, and to float unconverted *)
end

open struct
  open String (* should be fully converted *)

  let _ = make [@migrate { repl = Base.String.make }]
  let _ = init [@migrate { repl = (fun n f -> Base.String.init n ~f) }]
  let _ = empty [@migrate { repl = "" }]
  let _ = length [@migrate { repl = Base.String.length }]
  let _ = get [@migrate { repl = Base.String.get }]
  let _ = of_bytes [@migrate { repl = Base.Bytes.to_string }]
  let _ = to_bytes [@migrate { repl = Base.Bytes.of_string }]

  let _ =
    blit
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.From_string.blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]

  let _ = concat [@migrate { repl = (fun sep l -> Base.String.concat l ~sep) }]
  let _ = cat [@migrate { repl = Base.( ^ ) }]
  let _ = equal [@migrate { repl = Base.String.equal }]
  let _ = compare [@migrate { repl = Base.String.compare }]

  let _ =
    starts_with [@migrate { repl = (fun ~prefix s -> Base.String.is_prefix s ~prefix) }]

  let _ =
    ends_with [@migrate { repl = (fun ~suffix s -> Base.String.is_suffix s ~suffix) }]

  let _ =
    contains_from [@migrate { repl = (fun s pos c -> Base.String.contains s c ~pos) }]

  (* not sure there's an equivalent to rcontains_from *)

  let _ = contains [@migrate { repl = Base.String.contains }]
  let _ = sub [@migrate { repl = (fun str pos len -> Base.String.sub str ~pos ~len) }]
  let _ = split_on_char [@migrate { repl = (fun sep l -> Base.String.split l ~on:sep) }]
  let _ = map [@migrate { repl = (fun f s -> Base.String.map s ~f) }]
  let _ = mapi [@migrate { repl = (fun f s -> Base.String.mapi s ~f) }]
  let _ = fold_left [@migrate { repl = (fun f init s -> Base.String.fold s ~init ~f) }]
  (* no equivalent of fold_right *)

  let _ = for_all [@migrate { repl = (fun f s -> Base.String.for_all s ~f) }]
  let _ = exists [@migrate { repl = (fun f s -> Base.String.exists s ~f) }]
  let _ = trim [@migrate { repl = Base.String.strip }]
  (*
  This is not 100% equivalent, because Stdlib.String.trim strips \011 whereas
  Core.String.strip doesn't. It's hard to imagine when it would ever matter. Ideally,
  we'd use String.strip ~drop:Stdlib.Char.is_space but since the is-a-space criteria is
  not provided as a separate function, we can't even do that. Maybe we should have either
  a way to say "you could migrate to this, but it's not exactly the same", or perhaps if
  we should emit a warning (or annotation in the comment) saying "there is a slighly
  corner case here".
   *)

  let _ = escaped [@migrate { repl = Base.String.escaped }]
  let _ = lowercase_ascii [@migrate { repl = Base.String.lowercase }]
  let _ = uppercase_ascii [@migrate { repl = Base.String.uppercase }]
  let _ = capitalize_ascii [@migrate { repl = Base.String.capitalize }]
  let _ = uncapitalize_ascii [@migrate { repl = Base.String.capitalize }]
  let _ = iter [@migrate { repl = (fun f s -> Base.String.iter s ~f) }]
  let _ = iteri [@migrate { repl = (fun f s -> Base.String.iteri s ~f) }]

  (* let _ = index_from [@migrate { repl = Base.String.index_from_exn }] *)
  let _ = index_from_opt [@migrate { repl = Base.String.index_from }]

  (* let _ = rindex_from [@migrate { repl = Base.String.rindex_from_exn }] *)
  let _ = rindex_from_opt [@migrate { repl = Base.String.rindex_from }]

  (* let _ = index [@migrate { repl = Base.String.index_exn }]
   * let _ = rindex [@migrate { repl = Base.String.rindex_exn }] *)
  let _ = index_opt [@migrate { repl = Base.String.index }]
  let _ = rindex_opt [@migrate { repl = Base.String.rindex }]

  (* no equivalent of to_seq, to_seqi, of_seq *)

  (* get_utf_8_uchar and the other ones don't seem to have direct equivalent.
     There is Base.String.Utf8.get, but it works on a type of known-to-be-valid-utf8
     string, which is is not necessarily what one wants, so I don't think we should
     translate to that. There's the same split as Bstr vs String in rust. *)

  (* seems simpler to leave hash, seeded_hash alone, and let people switch to hash,
     hash_fold_t if they want to. These functions are probably rarely used, considering
     the lack of support for hand written hash functions in the stdlib. *)

  (* no equivalent for get_int{32,64}_{le,be}. *)
  let _ = unsafe_get [@migrate { repl = Base.String.unsafe_get }]

  let _ =
    unsafe_blit
    [@migrate
      { repl =
          (fun src src_pos dst dst_pos len ->
            Base.Bytes.From_string.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
      }]
end

open struct
  open Sys (* should be as fully converted as is possible *)

  let _ = argv [@migrate { repl = Base.Sys.get_argv () }]

  let _ =
    executable_name
    [@migrate { repl = Sys_unix.executable_name; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    file_exists
    [@migrate { repl = Sys_unix.file_exists_exn; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    is_directory
    [@migrate { repl = Sys_unix.is_directory_exn; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    is_regular_file
    [@migrate { repl = Sys_unix.is_file_exn; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    remove [@migrate { repl = Sys_unix.remove; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    rename [@migrate { repl = Sys_unix.rename; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    getenv
    [@migrate
      { repl =
          (fun var ->
            match Base.Sys.getenv var with
            | None -> Base.raise Stdlib.Not_found
            | Some v -> v)
      }]

  let _ = getenv_opt [@migrate { repl = Base.Sys.getenv }]

  let _ =
    command [@migrate { repl = Sys_unix.command; libraries = [ "core_unix.sys_unix" ] }]

  (* not sure whether we should migrate Sys.time, since it's very heavy to switch from
     Sys.time () to Core.Time_float.Span.to_seconds_since_epoch
                      (Core.Time_float.to_span_since_epoch
                        (Core.Time_float.now ()))
     though it'd make for easier switching by hand, since that would indicate where the
     replacement is for the functionality. Perhaps we could have functionality to add a
     comment to the callsite, to indicate where to look for equivalent functionality, to
     get the discovery benefit without adding too much mess to the code.

     Stupidly, we couldn't even use Time_ns, because even though Core.Time_float.now () is
     implemented on top of Time_now.nanoseconds_since_unix_epoch (), which is exactly what
     Core.Time_ns.now () should be, Core.Time_ns.now is not provided, and points to
     Time_ns_unix instead. *)

  let _ = chdir [@migrate { repl = Sys_unix.chdir; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    (* mkdir and rmdir don't seem to be in Sys_unix, weirdly *)
    mkdir
    [@migrate
      { repl = (fun dir perm -> Core_unix.mkdir dir ~perm); libraries = [ "core_unix" ] }]

  let _ = rmdir [@migrate { repl = Core_unix.rmdir; libraries = [ "core_unix" ] }]

  let _ =
    getcwd [@migrate { repl = Sys_unix.getcwd; libraries = [ "core_unix.sys_unix" ] }]

  let _ =
    readdir [@migrate { repl = Sys_unix.readdir; libraries = [ "core_unix.sys_unix" ] }]

  let _ = interactive [@migrate { repl = Base.Sys.interactive }]
  let _ = os_type [@migrate { repl = Base.Sys.os_type }]
  let _ = backend_type [@migrate { repl = Base.Sys.backend_type }]
  let _ = unix [@migrate { repl = Base.Sys.unix }]
  let _ = win32 [@migrate { repl = Base.Sys.win32 }]
  let _ = cygwin [@migrate { repl = Base.Sys.cygwin }]
  let _ = word_size [@migrate { repl = Base.Sys.word_size_in_bits }]
  let _ = int_size [@migrate { repl = Base.Sys.int_size_in_bits }]
  let _ = big_endian [@migrate { repl = Base.Sys.big_endian }]
  let _ = max_string_length [@migrate { repl = Base.Sys.max_string_length }]
  let _ = max_array_length [@migrate { repl = Base.Sys.max_array_length }]

  (* no equivalent for max_floatarray_length *)

  let _ = runtime_variant [@migrate { repl = Base.Sys.runtime_variant }]
  let _ = runtime_parameters [@migrate { repl = Base.Sys.runtime_parameters }]

  (* all the signal stuff should be replaced by Core.Signal, but Core.Signal.t is opaque from Stdlib.Sys uses int, so not clear what to do. Maybe we should insert sigabrt by Core.Signal.to_caml_int Core.Signal.sigabrt? *)

  let _ =
    (* ideally, we'd do the same thing with the Break exception, but ocamlmig doesn't
       support that *)
    catch_break
    [@migrate { repl = Sys_unix.catch_break; libraries = [ "core_unix.sys_unix" ] }]

  let _ = ocaml_version [@migrate { repl = Base.Sys.ocaml_version }]

  (* no equivalent of development_version, ocaml_release *)

  let _ = enable_runtime_warnings [@migrate { repl = Base.Sys.enable_runtime_warnings }]
  let _ = runtime_warnings_enabled [@migrate { repl = Base.Sys.runtime_warnings_enabled }]
  let _ = opaque_identity [@migrate { repl = Base.Sys.opaque_identity }]

  (* no equivalent of Immediate64, but int63 is provided instead *)
end

open struct
  open Uchar

  let _ = utf_8_byte_length [@migrate { repl = Base.Uchar.Utf8.byte_length }]
  let _ = to_char [@migrate { repl = Base.Uchar.to_char_exn }]

  let _ =
    utf_decode_uchar
    [@migrate { repl = Base.Uchar.Decode_result.uchar_or_replacement_char }]

  let _ = utf_decode_length [@migrate { repl = Base.Uchar.Decode_result.bytes_consumed }]
end

open struct
  open Unit (* should be fully converted *)

  let _ = to_string [@migrate { repl = Base.Unit.to_string }]
  let _ = compare [@migrate { repl = Base.Unit.compare }]
  let _ = equal [@migrate { repl = Base.Unit.equal }]
  (* leaving the hash functions unconverted *)
end
