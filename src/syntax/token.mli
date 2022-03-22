type t =
  (* x1 *)
  | VARIABLE of string
  (* -> *)
  | ARROW
  (* = *)
  | EQUAL
  (* : *)
  | COLON
  (* ; *)
  | SEMICOLON
  (* . *)
  | DOT
  (* ( *)
  | LEFT_PARENS
  (* ) *)
  | RIGHT_PARENS
  (* { *)
  | LEFT_BRACE
  (* } *)
  | RIGHT_BRACE
  | EOF

type token = t
