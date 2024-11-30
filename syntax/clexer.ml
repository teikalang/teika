open Cparser
open Sedlexing.Utf8

exception Lexer_error of { loc : Location.t }
exception Parser_error of { loc : Location.t }

let () =
  Printexc.register_printer @@ function
  | Lexer_error { loc = _ } -> Some "lexer: syntax error"
  | Parser_error { loc = _ } -> Some "parser: syntax error"
  | _ -> None

let loc buf =
  let loc_start, loc_end = Sedlexing.lexing_positions buf in
  Location.{ loc_start; loc_end; loc_ghost = false }

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let variable = [%sedlex.regexp? (alphabet | '_'), Star (alphabet | digit | '_')]
let extension = [%sedlex.regexp? '%', variable]
let string = [%sedlex.regexp? '"', Star (Sub (any, '"')), '"']
let number = [%sedlex.regexp? Plus '0' .. '9']

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | variable -> VAR (lexeme buf)
  | extension -> EXTENSION (lexeme buf)
  | ":" -> COLON
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "=" -> EQUAL
  | "," -> COMMA
  | "&" -> AMPERSAND
  | ";" -> SEMICOLON
  | string ->
      (* TODO: this should probably be somewhere else *)
      let literal = lexeme buf in
      (* TODO: this is dangerous *)
      let literal = String.sub literal 1 (String.length literal - 2) in
      STRING literal
  | number ->
      (* TODO: this should probably be somewhere else *)
      let literal = lexeme buf in
      (* TODO: this is dangerous *)
      let literal = Z.of_string literal in
      NUMBER literal
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | "{" -> LEFT_BRACE
  | "}" -> RIGHT_BRACE
  | eof -> EOF
  | _ ->
      let loc = loc buf in
      raise @@ Lexer_error { loc }

let next buf =
  let token = tokenizer buf in
  let start, end_ = Sedlexing.lexing_positions buf in
  (token, start, end_)

open Cparser.MenhirInterpreter

let rec loop buf state =
  match state with
  | InputNeeded _env ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         checkpoint. Then, repeat. *)
      let token, start, end_ = next buf in
      let state = offer state (token, start, end_) in
      loop buf state
  | Shifting _ | AboutToReduce _ ->
      let state = resume state in
      loop buf state
  | HandlingError _env ->
      let loc = loc buf in
      raise (Parser_error { loc })
  | Accepted value -> value
  | Rejected -> failwith "cdriver.loop: rejected reached"

let buf_from_string string =
  (* TODO: from string seems to not trigger new line, likely a bug in sedlex *)
  let index = ref 0 in
  let length = String.length string in
  from_gen (fun () ->
      match !index < length with
      | true ->
          let char = String.get string !index in
          incr index;
          Some char
      | false -> None)

let term_opt_from_string string =
  let buf = buf_from_string string in
  (* TODO: allow to change this *)
  let start, _end = Sedlexing.lexing_positions buf in
  loop buf @@ Cparser.Incremental.term_opt start
