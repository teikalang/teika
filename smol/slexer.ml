open Sparser
open Sedlexing.Utf8

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let variable = [%sedlex.regexp? (alphabet | '_'), Star (alphabet | digit | '_')]

(* TODO: escape, proper strings *)
let string =
  [%sedlex.regexp? '"', Plus (alphabet | digit | '_' | whitespace), '"']

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | variable -> VAR (lexeme buf)
  | string ->
      let buf = lexeme buf in
      let string = String.sub buf 1 (String.length buf - 2) in
      STRING string
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "=" -> EQUAL
  | "," -> COMMA
  | ":" -> COLON
  | ";" -> SEMICOLON
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | eof -> EOF
  | _ -> failwith "unknown token"

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider
