open Token
open Sedlexing.Utf8

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']

let identifier =
  [%sedlex.regexp? (alphabet | '_'), Star (alphabet | digit | '_')]

let number = [%sedlex.regexp? Plus digit]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  (* TODO: emojis?*)
  | identifier -> IDENT (lexeme buf)
  | number -> NUMBER (lexeme buf)
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "=" -> EQUAL
  | ":" -> COLON
  | ";" -> SEMICOLON
  | "." -> DOT
  | "*" -> ASTERISK
  | "|" -> PIPE
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | "{" -> LEFT_BRACE
  | "}" -> RIGHT_BRACE
  | eof -> EOF
  | _ ->
      (* TODO: better error message *)
      failwith "unknown token"

(* TODO: better name? *)
let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

(* TODO: alternatives? *)
let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider
