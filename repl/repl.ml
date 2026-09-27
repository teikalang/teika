open Repl_core

let process session source =
  let session, output = Session.submit session source in
  match output with
  | None -> session
  | Some { type_; value } ->
      let type_ = Format.asprintf "%a" Printer.pp type_ in
      let value = Format.asprintf "%a" Printer.pp value in
      Format.printf "type:  %s@.value: %s@." type_ value;
      session

let report = function
  | Lexer.Error (loc, message)
  | Lparser.Error (loc, message)
  | Typer.Error (loc, message) ->
      let column = loc.loc_start.pos_cnum - loc.loc_start.pos_bol + 1 in
      Format.eprintf "error at column %d: %s@." column message
  | Failure message -> Format.eprintf "error: %s@." message
  | exn -> Format.eprintf "error: %s@." (Printexc.to_string exn)

let () =
  let interactive = Unix.isatty Unix.stdin in
  if interactive then Format.printf "teika REPL — one term or let per line@.";
  let rec loop session =
    if interactive then Format.printf "> %!";
    match input_line stdin with
    | line ->
        let session =
          try process session line
          with exn ->
            report exn;
            session
        in
        loop session
    | exception End_of_file -> ()
  in
  loop Session.empty
