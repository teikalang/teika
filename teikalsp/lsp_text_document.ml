(* TODO: proper types for text and version *)
open Syntax
open Teika

type document =
  | Solved of { version : int; term : Ttree.term }
  | Parsed of { version : int; term : Ctree.term; loc : Location.t; exn : exn }
  | Unparsed of { version : int; text : string; loc : Location.t; exn : exn }

type t = document
type diagnostic = { loc : Location.t; message : string }

let diagnostics document =
  match document with
  | Solved { version = _; term = _ } -> []
  | Parsed { version = _; term = _; loc; exn } ->
      let message = Printexc.to_string exn in
      [ { loc; message } ]
  | Unparsed { version = _; text = _; loc; exn } ->
      let message = Printexc.to_string exn in
      [ { loc; message } ]

let teika ~version term =
  match Solve.(solve_term initial term) with
  | Ok term -> Solved { version; term }
  | Error exn -> raise exn

let teika ~version term =
  let term = Option.get @@ Clexer.term_opt_from_string term in
  try teika ~version term with
  | Solve.Solve_error { loc; exn } -> Parsed { version; term; loc; exn }
  | exn ->
      (* TODO: use this to report bad syntax *)
      (* TODO: this is bad *)
      let loc = Location.none in
      Parsed { version; term; loc; exn }

let teika ~version ~text =
  try teika ~version text with
  | (Clexer.Lexer_error { loc } | Clexer.Parser_error { loc }) as exn ->
      Unparsed { version; text; loc; exn }
  | exn ->
      let loc = Location.none in
      (* TODO: use this to report bad syntax *)
      Unparsed { version; text; loc; exn }

let with_change ~version ~text _document =
  (* TODO: use the version for something? *)
  teika ~version ~text
