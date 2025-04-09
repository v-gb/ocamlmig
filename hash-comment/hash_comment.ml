let _f x =
  x#a;
  x # a;
  x # a
[@@ocamlformat "disable"]

let subi str pos1 pos2 = String.sub str pos1 (pos2 - pos1)
let read_file fname = In_channel.with_open_bin fname In_channel.input_all

let rindex_default str pos char =
  String.rindex_from_opt str pos char |> Option.value ~default:0

let index_default str pos char =
  String.index_from_opt str pos char |> Option.value ~default:(String.length str)

let () =
  let fnames =
    if Array.length Sys.argv > 1
    then Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
    else
      let tmp_fname = Filename.temp_file "check-lexing" "" in
      Fun.protect
        ~finally:(fun () -> Sys.remove tmp_fname)
        (fun () ->
          match
            Sys.command
              ("find -name _build -prune -or -type f '(' -name '*.ml' -or -name '*.mli' \
                ')' -print > "
              ^ Filename.quote tmp_fname)
          with
          | 0 ->
              read_file tmp_fname
              |> String.split_on_char '\n'
              |> List.filter (function "" -> false | _ -> true)
              |> List.map (fun s ->
                     Option.value (Base.String.chop_prefix ~prefix:"./" s) ~default:s)
          | n -> exit n)
  in
  let stats_found = ref 0 in
  let stats_parsed = ref 0 in
  let stats_has_hash = ref 0 in
  let stats_would_break = ref 0 in
  let first = ref true in
  List.iter
    (fun fname ->
      Ocamlformat_parser_extended.Lexer.init ();
      let input = read_file fname in
      let lexbuf = Lexing.from_string input in
      let has_hash = ref false in
      let rec z () =
        match Ocamlformat_parser_extended.Lexer.token lexbuf with
        | exception e -> Error e
        | EOF -> Ok ()
        | HASH ->
            has_hash := true;
            z ()
        | _ -> z ()
      in
      incr stats_found;
      match z () with
      | Error _ -> () (* people using custom syntaxes, like cppo *)
      | Ok () -> (
          incr stats_parsed;
          if !has_hash then incr stats_has_hash;
          match List.rev !Ocamlformat_parser_extended.Lexer.bad with
          | [] -> ()
          | bads ->
              incr stats_would_break;
              ( ListLabels.map bads
                  ~f:(fun ((bstart : Lexing.position), (bend : Lexing.position)) ->
                    let start_of_line =
                      let i = rindex_default input bstart.pos_cnum '\n' in
                      rindex_default input (max 0 (i - 1)) '\n'
                    in
                    let start_of_line =
                      start_of_line + if start_of_line = 0 then 0 else 1
                    in
                    let end_of_line =
                      let i = index_default input bstart.pos_cnum '\n' in
                      index_default input (min (String.length input) (i + 1)) '\n'
                    in
                    (start_of_line, bstart.pos_cnum, bend.pos_cnum, end_of_line))
              |> fun l ->
                let rec join l1 (cs, matches, ce) l2 =
                  match l2 with
                  | (ns, nm1, nm2, ne) :: tl2 ->
                      if ce >= ns
                      then join l1 (cs, (nm1, nm2) :: matches, ne) tl2
                      else
                        join
                          ((cs, List.rev matches, ce) :: l1)
                          (ns, [ (nm1, nm2) ], ne)
                          tl2
                  | [] -> List.rev ((cs, List.rev matches, ce) :: l1)
                in
                match l with
                | [] -> []
                | (ns, nm1, nm2, ne) :: tl -> join [] (ns, [ (nm1, nm2) ], ne) tl )
              |> fun l ->
              List.iter
                (fun (cs, matches, ce) ->
                  if not !first then print_string "---\n";
                  let b = Buffer.create 14 in
                  let from = ref cs in
                  List.iter
                    (fun (m1, m2) ->
                      Buffer.add_string b (subi input !from m1);
                      Buffer.add_string b "[31;1m";
                      Buffer.add_string b (subi input m1 m2);
                      Buffer.add_string b "[39;22m";
                      from := m2)
                    matches;
                  Buffer.add_string b (subi input !from ce);
                  let s = Buffer.contents b in
                  Base.String.split_lines s
                  |> List.map (fun x -> fname ^ ":" ^ x)
                  |> Base.String.concat_lines
                  |> print_string;
                  first := false)
                l))
    fnames;
  Printf.printf "parsed=%d has_HASH=%d would_break=%d\n" !stats_parsed !stats_has_hash
    !stats_would_break
