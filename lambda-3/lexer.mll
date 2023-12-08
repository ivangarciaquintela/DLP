
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "concat"    { CONCAT }
  | "strlen"    { STRLEN }
  | "let"       { LET }
  | "letrec"       { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  
  | '['         { LBRACK }
  | ']'         { RBRACK }
  | ','         { COMMA }

  | '{'         { LBRACE }
  | '}'         { RBRACE }

  | "list"      { LIST }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }

  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ";;"        { SEMICOLON }
  | ">"         { GT }
  | "<"         { LT }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*  
                { IDV (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"' 
                {let s = Lexing.lexeme lexbuf in 
                STRINGV (String.sub s 1 (String.length s - 2)) }
  | eof         { EOF }
  | _           { raise Lexical_error }

