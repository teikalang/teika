include Tree

let value_from_string string = Lexer.from_string Parser.term_opt string
