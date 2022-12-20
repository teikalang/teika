open Sparser
open Sedlexing.Utf8

(* TODO: more information *)
exception Lexer_unknown_token

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let variable = [%sedlex.regexp? (alphabet | '_'), Star (alphabet | digit | '_')]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | variable -> VAR (lexeme buf)
  | ":" -> COLON
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "=>" -> FAT_ARROW
  | "=" -> EQUAL
  | "," -> COMMA
  | "&" -> AMPERSAND
  | ";" -> SEMICOLON
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

(* TODO: expose better APIs *)
let from_string ~filename parser string =
  let buf = from_string string in
  (* TODO: Sedlexing.set_position, ppx for tests *)
  Sedlexing.set_filename buf filename;
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider
