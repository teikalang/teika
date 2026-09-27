open Cparser

exception Error of Location.t * string

let location buffer =
  let loc_start, loc_end = Sedlexing.lexing_bytes_positions buffer in
  Location.{ loc_start; loc_end; loc_ghost = false }

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let identifier = [%sedlex.regexp? letter, Star (letter | digit | '_')]

let rec token buffer =
  let text () = Sedlexing.Utf8.lexeme buffer in
  match%sedlex buffer with
  | Plus (' ' | '\t' | '\r' | '\n') -> token buffer
  | "//", Star (Sub (any, '\n')) -> token buffer
  | '_' -> UNDERSCORE
  | identifier -> VAR (text ())
  | ':' -> COLON
  | '=' -> EQUAL
  | ',' -> COMMA
  | '&' -> AMPERSAND
  | '|' -> PIPE
  | ';' -> SEMICOLON
  | '(' -> LEFT_PARENS
  | ')' -> RIGHT_PARENS
  | '[' -> LEFT_BRACKET
  | ']' -> RIGHT_BRACKET
  | '{' -> LEFT_BRACE
  | '}' -> RIGHT_BRACE
  (* The concrete syntax recognizes literals; the language's logical parser
     deliberately rejects them because typer.ml has no literal types. *)
  | Plus digit -> NUMBER (Z.of_string (text ()))
  | '"', Star (Sub (any, '"')), '"' ->
      let literal = text () in
      STRING (String.sub literal 1 (String.length literal - 2))
  | eof -> EOF
  | _ -> raise (Error (location buffer, "unexpected token " ^ text ()))

let parse_with ?(trailing_semicolon = false) parser text =
  let buffer = Sedlexing.Utf8.from_string text in
  let next () =
    let token = token buffer in
    let start, finish = Sedlexing.lexing_bytes_positions buffer in
    (token, start, finish)
  in
  let rec drive checkpoint pending loc =
    let open Cparser.MenhirInterpreter in
    match checkpoint with
    | InputNeeded _ ->
        let token =
          match pending with Some token -> token | None -> next ()
        in
        let ((_, start, finish) as token), pending =
          match token with
          | SEMICOLON, _, _ when trailing_semicolon -> (
              match next () with
              | (EOF, _, _) as last -> (last, None)
              | following -> (token, Some following))
          | _ -> (token, None)
        in
        let loc =
          Location.{ loc_start = start; loc_end = finish; loc_ghost = false }
        in
        drive (offer checkpoint token) pending loc
    | Shifting _ | AboutToReduce _ -> drive (resume checkpoint) pending loc
    | Accepted term -> term
    | HandlingError _ | Rejected -> raise (Error (loc, "syntax error"))
  in
  let start, _ = Sedlexing.lexing_bytes_positions buffer in
  drive (parser start) None (location buffer)

let parse text =
  Option.map Lparser.parse_term (parse_with Cparser.Incremental.term_opt text)

let parse_line text =
  parse_with ~trailing_semicolon:true Cparser.Incremental.term_opt text
