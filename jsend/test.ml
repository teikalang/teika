open Teika
open Jsend

let compile code =
  let term = Option.get @@ Slexer.from_string Sparser.term_opt code in
  let term = Lparser.from_stree term in
  let term =
    match Context.run @@ fun () -> Typer.infer_term term with
    | Ok ttree -> ttree
    | Error error ->
        Format.eprintf "%a\n%!" Terror.pp error;
        failwith "infer"
  in

  let term = Untype.untype_term term in
  let term = Emit.emit_term term in
  Format.printf "%a" Jprinter.pp_expression term

let () =
  compile
    {|
      Bool = (A : Type) -> (t : A) -> (f : A) -> A;
      @native("debug")(A => t => f => t : Bool)
    |}
