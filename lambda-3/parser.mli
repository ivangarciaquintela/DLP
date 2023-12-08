type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | CONCAT
  | STRLEN
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | STRING
  | LBRACK
  | RBRACK
  | COMMA
  | LBRACE
  | RBRACE
  | LIST
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | SEMICOLON
  | EOF
  | LT
  | GT
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command list
