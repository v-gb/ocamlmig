open Base
open Stdio
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format

let () = Ocamlformat_lib.Conf.disable_version_check := true
let ( =: ) = String.( = )
let ( <>: ) = String.( <> )
let ( >: ) = String.( > )
let ( <: ) = String.( < )
let ( >=: ) = String.( >= )
let ( <=: ) = String.( <= )
let transient_line str = Printf.eprintf "%s%!\r[K" str
let flush_transient () = Stdio.Out_channel.flush stderr
let in_test = Option.is_some (Base.Sys.getenv "OCAMLMIG_TEST")

type 'a staged = { next : 'a } [@@unboxed]

type debug =
  { all : bool
  ; build_artifacts : bool
  ; ocamlformat : bool
  ; extra_migrations : bool
  ; preserve_format_parens : bool
  ; replace_repetitions : bool
  }

let debug =
  let all f b =
    { all = f "all" b
    ; build_artifacts = f "build_artifacts" b
    ; ocamlformat = f "ocamlformat" b
    ; extra_migrations = f "extra_migrations" b
    ; preserve_format_parens = f "preserve_format_parens" b
    ; replace_repetitions = f "replace_repetitions" b
    }
  in
  let l = ref [] in
  let no_debug =
    all
      (fun name b ->
        l := name :: !l;
        b)
      false
  in
  match Base.Sys.getenv "OCAMLMIG_DEBUG" with
  | None -> no_debug
  | Some "help" ->
      print_string (String.concat_lines (List.rev !l));
      Stdlib.exit 0
  | Some str ->
      List.fold_left (String.split str ~on:',') ~init:no_debug ~f:(fun d -> function
        | "all" -> all (fun _ b -> b) true
        | "build_artifacts" -> { d with build_artifacts = true }
        | "ocamlformat" -> { d with ocamlformat = true }
        | "extra_migrations" -> { d with extra_migrations = true }
        | "preserve_format_parens" -> { d with preserve_format_parens = true }
        | "replace_repetitions" -> { d with replace_repetitions = true }
        | s ->
            eprintf "OCAMLMIG_DEBUG: unknown field %S (see OCAMLMIG_DEBUG=help)\n" s;
            d)

let log = ref false
let not_tc x = x
let _ = not_tc

module Process_res = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Detailed : ('a, ('a, Unix.process_status * Sexp.t) Result.t) t
end

let with_process_full ?cwd (type a res) (process_res : (a, res) Process_res.t) argv f :
    res =
  let argv =
    match cwd with
    | None -> argv
    | Some cwd ->
        (* We should probably use spawn in general. *)
        [ "bash"
        ; "-c"
        ; "cd " ^ Core.Sys.quote cwd ^ " && " ^ Core.Sys.concat_quoted argv
        ]
  in
  let prog = Option.value (List.hd argv) ~default:"" in
  let res = ref (Unix.WEXITED 0) in
  let (fres : a), stderr =
    Exn.protectx
      ~finally:(fun channels -> res := Unix.close_process_full channels)
      (Unix.open_process_args_full prog (Array.of_list argv) (Unix.environment ()))
      ~f:(fun (stdout, stdin, stderr) ->
        let res = f (stdout, stdin) in
        Out_channel.close stdin;
        In_channel.close stdout;
        (res, In_channel.input_all stderr))
  in
  match !res with
  | WEXITED 0 -> ( match process_res with Raise -> fres | Detailed -> Ok fres)
  | _ -> (
      let explain =
        match !res with
        | WEXITED n -> "code " ^ Int.to_string n
        | WSIGNALED n -> "signal " ^ Int.to_string n
        | WSTOPPED _ -> assert false
      in
      let sexp =
        [%sexp
          ("process exited with " ^ explain : string)
        , (argv : string list)
        , ~~(stderr : string)]
      in
      match process_res with Raise -> raise_s sexp | Detailed -> Error (!res, sexp))

let run_process ?cwd process_res argv =
  with_process_full ?cwd process_res argv (fun (stdout, stdin) ->
      Out_channel.close stdin;
      (* we should use eio or async, as this can deadlock if stderr is big enough *)
      In_channel.input_all stdout)

let print_s s = print_endline (Sexp.to_string_hum s)
let eprint_s s = prerr_endline (Sexp.to_string_hum s)

let list_from_iter f =
  let r = ref [] in
  f (fun v -> r := v :: !r);
  List.rev !r

let set_from_iter c f =
  let r = ref (Set.empty c) in
  f (fun v -> r := Set.add !r v);
  !r

let exists_from_iter f =
  let exception E in
  try
    f (fun () -> raise E);
    false
  with E -> true

let no_dash path = if String.is_prefix path ~prefix:"-" then "./" ^ path else path
let quote_no_dash path = Filename.quote (no_dash path)

let with_tmpfile f =
  let tmp_fname, tmp_oc = Filename.open_temp_file "ocamlmig" "" in
  Exn.protect ~finally:(fun () -> Sys.remove tmp_fname) ~f:(fun () -> f tmp_oc tmp_fname)

module Cwdpath = struct
  (** A path that can be either absolute or relative, with the assumption that relative
      paths are meant to be relative to the current cwd (which is assumed to be fixed
      throughout the process's lifetime).

      The reason for this to exist, rather than passing a string around, is that we get
      strings from a number of sources (build artifacts, command output, command line) and
      give strings to various bits of code (processes, libraries), and it's too easy to
      pass strings without thinking about what they represent when all paths are
      stringly-typed, thus resulting in code that doesn't work properly when
      [not (cwd = dune root = repo root)].

      With this type, you have to use Cwdpath.create and Cwdpath.to_string, which is a
      small hurdle that forces one to consider the question of what the string paths are
      supposed to be relative to. *)

  type t = { p : string } [@@unboxed]

  let hash t = String.hash t.p
  let compare t1 t2 = String.compare t1.p t2.p
  let sexp_of_t t = sexp_of_string t.p

  include (val Comparator.make ~sexp_of_t ~compare)

  let to_string { p } = p
  let create p = { p }

  let create_abs_exn p =
    assert ((not (Filename.is_relative p)) && not (Filename.is_implicit p));
    { p }

  let create_list ps = List.map ps ~f:create
  let concat p s = { p = Filename.concat p.p s }
  let dirname t = { p = Filename.dirname t.p }
end

module Abspath = struct
  type t = { p : string } [@@unboxed]

  let compare t1 t2 = String.compare t1.p t2.p
  let sexp_of_t t = sexp_of_string t.p

  include (val Comparator.make ~sexp_of_t ~compare)

  let to_string { p } = p

  let create_exn p =
    assert ((not (Filename.is_relative p)) && not (Filename.is_implicit p));
    { p }

  let cwd () = create_exn (Sys.getcwd ())
  let concat p s = { p = Filename.concat p.p s }
  let dirname t = { p = Filename.dirname t.p }
  let to_cwdpath { p } = Cwdpath.create p
end

let with_str_in_file str f =
  with_tmpfile (fun tmp_oc tmp_fname ->
      Exn.protect
        ~finally:(fun () -> Out_channel.close tmp_oc)
        ~f:(fun () -> Out_channel.output_string tmp_oc str);
      f (Abspath.create_exn tmp_fname))

let lexing_from_string str ~file_path =
  let lexbuf = Lexing.from_string str in
  Lexing.set_filename lexbuf file_path;
  lexbuf

let string_sub str ~pos1 ~pos2 = String.sub str ~pos:pos1 ~len:(pos2 - pos1)
