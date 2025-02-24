open Base
open Stdio

let create_now ~name ~ph =
  (* Doc about the json format: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview?tab=t.0#heading=h.yr4qxyxotyw *)
  let string = Printf.sprintf "%S" in
  let obj l =
    List.map l ~f:(fun (k, v) -> string k ^ ":" ^ v)
    |> String.concat ~sep:", "
    |> Printf.sprintf "{%s}"
  in
  let int = Int.to_string in
  obj
    [ ("name", string name)
    ; ("ph", string (match ph with `B -> "B" | `E -> "E"))
    ; ("ts", int (Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) / 1000))
    ; ("pid", int 0)
    ; ("tid", int 0)
    ]
  ^ "\n"

let dst = ref None

let record name f =
  match !dst with
  | None -> f ()
  | Some (out_ch, prefix) ->
      Out_channel.output_string out_ch (!prefix ^ create_now ~name ~ph:`B);
      prefix := ",";
      Exn.protect ~f ~finally:(fun () ->
          Out_channel.output_string out_ch (!prefix ^ create_now ~name ~ph:`E))

let with_profile fname f =
  Out_channel.with_file fname ~f:(fun out_ch ->
      Out_channel.output_string out_ch "[";
      Exn.protect
        ~finally:(fun () -> Out_channel.output_string out_ch "]")
        ~f:(fun () ->
          Ref.set_temporarily dst (Some (out_ch, ref "")) ~f:(fun () -> record "start" f)))
