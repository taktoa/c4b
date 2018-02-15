type token =
  | ASSERT
  | WHILE
  | LOOP
  | IF
  | ELSE
  | BREAK
  | SEMI
  | COMMA
  | PLUS
  | MINUS
  | STAR
  | LPAREN
  | RPAREN
  | LT
  | GT
  | LE
  | GE
  | EQ
  | FUNC
  | LOCAL
  | RETURN
  | IDNT of (string)
  | NUM of (int)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit Ast.func list * unit Ast.prog
