open Teika
open Jsend

let compile code =
  let term = Option.get @@ Slexer.from_string Sparser.term_opt code in
  let term = Lparser.from_stree term in
  let term =
    match Context.Typer_context.run @@ fun () -> Typer.infer_term term with
    | Ok ttree -> ttree
    | Error error ->
        Format.eprintf "%a\n%!" Terror.pp error;
        failwith "infer"
  in

  let term = Untype.untype_term term in
  let term = Emit.emit_term term in
  Format.printf "%a\n\n%!" Jprinter.pp_expression term

let () =
  compile
    {|
      Unit = (A : Type) -> (x : A) -> A;
      (noop : (u : Unit) -> Unit) = u => u Unit u;
      noop
    |}

let () =
  compile
    {|
      Bool = (A : Type) -> (t : A) -> (f : A) -> A;
      (true : Bool) = A => x => y => x;
      (false : Bool) = A => x => y => y;
      f = (bool : Bool) => @native("debug")(bool String "!!true" "!!false");
      f false
    |}

let () =
  compile
    {|
        Nat = (A : Type) -> (z : A) ->
          (s : (x : A) -> A) -> (k : (x : A) -> A) -> A;
        (zero : Nat) = A => z => s => k => k z;
        (succ : (n : Nat) -> Nat) =
          n => A => z => s => k => n A z s (x => k (s x));
        (add : (n : Nat) -> (m : Nat) -> Nat) =
          n => m => n Nat m succ (x => x);
        (mul : (n : Nat) -> (m : Nat) -> Nat) =
          n => m => n Nat zero (add m) (x => x);
        one = succ zero;
        two = succ one;
        four = mul two two;
        eight = mul two four;
        sixteen = mul two eight;
        byte = mul sixteen sixteen;
        short = mul byte byte;
        short String "zero" (_ => @native("debug")("hello")) (x => x)
      |}