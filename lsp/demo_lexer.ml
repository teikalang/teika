open Cparser
open Sedlexing.Utf8

exception Lexer_error of { location : Location.t; lexeme : string }
exception Parser_error of { location : Location.t }

let () =
  Printexc.register_printer (function
    | Lexer_error { lexeme; _ } ->
        Some (Format.asprintf "unexpected token %S" lexeme)
    | Parser_error _ -> Some "syntax error"
    | _ -> None)

let location buffer =
  let loc_start, loc_end = Sedlexing.lexing_bytes_positions buffer in
  Location.{ loc_start; loc_end; loc_ghost = false }

let horizontal_space = [%sedlex.regexp? Plus (' ' | '\t' | '\r')]
let newline = [%sedlex.regexp? '\n']
let line_comment = [%sedlex.regexp? "//", Star (Sub (any, '\n'))]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let variable = [%sedlex.regexp? alphabet, Star (alphabet | digit | '_')]
let string_literal = [%sedlex.regexp? '"', Star (Sub (any, '"')), '"']
let number = [%sedlex.regexp? Plus digit]

let rec token buffer =
  match%sedlex buffer with
  | horizontal_space | newline | line_comment -> token buffer
  | "_" -> UNDERSCORE
  | "self" -> SELF
  | "fix" -> FIX
  | variable -> VAR (lexeme buffer)
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | ".@" -> DOT_UNFOLD
  | ".0" -> DOT_FST
  | ".1" -> DOT_SND
  | ":" -> COLON
  | "=" -> EQUAL
  | "," -> COMMA
  | "&" -> AMPERSAND
  | "|" -> PIPE
  | ";" -> SEMICOLON
  | string_literal ->
      let value = lexeme buffer in
      STRING (String.sub value 1 (String.length value - 2))
  | number -> NUMBER (Z.of_string (lexeme buffer))
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | "[" -> LEFT_BRACKET
  | "]" -> RIGHT_BRACKET
  | "{" -> LEFT_BRACE
  | "}" -> RIGHT_BRACE
  | eof -> EOF
  | _ ->
      raise (Lexer_error { location = location buffer; lexeme = lexeme buffer })

let next buffer =
  let token = token buffer in
  let start, end_ = Sedlexing.lexing_bytes_positions buffer in
  (token, start, end_)

let term_opt_from_string text =
  let buffer = Sedlexing.Utf8.from_string text in
  let provider () = next buffer in
  try MenhirLib.Convert.Simplified.traditional2revised Cparser.term_opt provider
  with Cparser.Error -> raise (Parser_error { location = location buffer })

let tokens_from_string text =
  let buffer = Sedlexing.Utf8.from_string text in
  let rec collect tokens =
    match token buffer with
    | EOF -> List.rev (EOF :: tokens)
    | next -> collect (next :: tokens)
  in
  collect []
