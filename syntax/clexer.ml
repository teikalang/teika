open Cparser
open Sedlexing.Utf8

(* TODO: more information *)
exception Lexer_unknown_token

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
  | "$" -> GRADE
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "@->" -> SELF_ARROW
  | "@=>" -> FIX_ARROW
  | "@" -> UNROLL
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
      let literal = int_of_string literal in
      NUMBER literal
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | "{" -> LEFT_BRACE
  | "}" -> RIGHT_BRACE
  | eof -> EOF
  | _ -> raise Lexer_unknown_token

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  (token, start, stop)

let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider
