(* open Syntax
open Teika

type context

let parse : string -> Ctree.term = assert false
let solve : Ctree.term -> (Ttree.pat * Ttree.term) list = assert false
let eval : Ttree.term -> (Ttree.pat * Ttree.term) list = assert false

let jsgen code =
  let ctree =
    match Clexer.from_string Cparser.term_opt code with
    | Some ctree -> ctree
    | None -> failwith "failed to parse"
  in
  let ttree =
    match Solve.(solve_term initial ctree) with
    | Ok ttree -> ttree
    | Error exn ->
        failwith
        @@ Format.asprintf "failed to infer types: %s" (Printexc.to_string exn)
  in
  let ftree = Flatten.(flatten initial ttree) in
  let jtree = Jsgen.(gen_program initial ftree) in
  Format.printf "%a\n%!" Jprinter.pp_program jtree

open Eio

(* TODO: is it UTF-8? *)
let read_char_stream ~sw flow =
  (* TODO: this should not be created by this function probably *)
  let stream = Stream.create 0 in
  let () =
    Fiber.fork ~sw @@ fun () ->
    let rec dump buf off len =
      match off < len with
      | true ->
          let char = Cstruct.get_char buf off in
          let () = Stream.add stream (Some char) in
          dump buf (off + 1) len
      | false -> ()
    in
    (* TODO: buf size? *)
    let buf = Cstruct.create 1024 in
    let rec loop () =
      let got = Flow.single_read flow buf in
      let () = dump buf 0 got in
      loop ()
    in
    loop ()
  in
  stream

let read_term_stream ~sw flow =
  (* TODO: may raise End_of_file *)
  let char_stream = read_char_stream ~sw flow in
  let lex_buf =
    (* TODO: string buffer to avoid Eio? *)
    Sedlexing.Utf8.from_gen (fun () -> Eio.Stream.take char_stream)
  in
  let provider = Clexer.provider lex_buf in
  let term_stream = Eio.Stream.create 0 in
  let () =
    Fiber.fork ~sw @@ fun () ->
    let rec loop () =
      let term =
        MenhirLib.Convert.Simplified.traditional2revised Cparser.term_or_let
          provider
      in
      Stream.add term_stream term;
      loop ()
    in
    loop ()
  in
  term_stream

let read_until : sw:Switch.t -> Ctree.term = assert false

type env
type value

let eval : env -> Ttree.pat -> Ttree.term -> env * value = assert false
let show : value -> string = assert false

let () = jsgen {|
  true = x => y => x;
  true
|} *)
