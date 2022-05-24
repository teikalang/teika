type t =
  (* x1 *)
  | IDENT of string
  (* 123 *)
  | NUMBER of string
  (* -> *)
  | ARROW
  (* => *)
  | FAT_ARROW
  (* = *)
  | EQUAL
  (* : *)
  | COLON
  (* ; *)
  | SEMICOLON
  (* . *)
  | DOT
  (* * *)
  | ASTERISK
  (* | *)
  | PIPE
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
