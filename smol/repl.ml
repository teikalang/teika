open Smol

let () = Printexc.record_backtrace true
let debug = false
let max_line_size = 1024 * 1024
let write_welcome buf = Eio.Buf_write.string buf "Welcome to Smol"
let write_newline buf = Eio.Buf_write.char buf '\n'
let write_pending buf = Eio.Buf_write.string buf "> "

let write_exception buf exn =
  match debug with
  | true -> Eio.Buf_write.string buf (Printexc.get_backtrace ())
  | false -> Eio.Buf_write.string buf (Printexc.to_string exn)

let write_term buf term =
  Eio.Buf_write.string buf (Format.asprintf "%a" Tprinter.pp_term term)

let type_of_string string =
  match Slexer.from_string Sparser.term_opt string with
  | Some term ->
      let term =
        let loc = Location.none in
        Lparser.parse_term ~loc term
      in
      let term =
        let open Ttyper in
        let ctx = Translate.Context.initial in
        Translate.translate_term ctx term
      in
      let ctx = Ttyper.Context.initial in
      Some (Ttyper.infer_term ctx term)
  | None -> None

let check buf line =
  match type_of_string line with
  | Some type_ -> write_term buf type_
  | None -> ()

type state = Check

let run state buf line = match state with Check -> check buf line

let run state buf line =
  try run state buf line with exn -> write_exception buf exn

let command state buf line =
  let state =
    match line with
    | ".check" -> Check
    | _ ->
        run state buf line;
        write_newline buf;
        state
  in
  write_pending buf;
  state

let repl read_buf write_buf =
  write_welcome write_buf;
  write_newline write_buf;
  write_pending write_buf;
  let lines = Eio.Buf_read.lines read_buf in
  let initial = Check in
  let _state =
    Seq.fold_left (fun state line -> command state write_buf line) initial lines
  in
  ()

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let read_buf = Eio.Buf_read.of_flow ~max_size:max_line_size stdin in
  Eio.Buf_write.with_flow stdout @@ fun write_buf -> repl read_buf write_buf

let () = main ()
