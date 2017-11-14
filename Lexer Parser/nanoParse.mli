type token =
  | Num of (int)
  | Id of (string)
  | EOF
  | LET
  | REC
  | EQ
  | IN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LPAREN
  | RPAREN
  | SEMI
  | COLONCOLON
  | TRUE
  | FALSE
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | LT
  | LE
  | NE
  | LBRAC
  | RBRAC

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
