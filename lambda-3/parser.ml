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
  | LET
  | IN
  | BOOL
  | NAT
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | SEMICOLON
  | EOF
  | INTV of (int)
  | IDV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 31 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* IN *);
  268 (* BOOL *);
  269 (* NAT *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* DOT *);
  273 (* EQ *);
  274 (* COLON *);
  275 (* ARROW *);
  276 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  277 (* INTV *);
  278 (* IDV *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\006\000\006\000\006\000\001\000\002\000\002\000\
\002\000\002\000\003\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\014\000\021\000\000\000\000\000\
\006\000\000\000\000\000\007\000\008\000\009\000\000\000\000\000\
\001\000\010\000\000\000\000\000\000\000\011\000\019\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\004\000\017\000\003\000\005\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\034\000\017\000\035\000"

let yysindex = "\015\000\
\005\255\000\000\235\254\000\000\000\000\005\255\018\255\018\255\
\018\255\252\254\005\255\000\000\000\000\000\000\010\255\018\255\
\000\000\013\255\036\255\000\000\000\000\000\000\020\255\027\255\
\000\000\000\000\032\255\005\255\005\255\000\000\000\000\000\000\
\032\255\031\255\029\255\043\255\039\255\037\255\005\255\032\255\
\005\255\005\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\233\255\251\255\000\000"

let yytablesize = 52
let yytable = "\019\000\
\018\000\020\000\021\000\022\000\024\000\003\000\004\000\005\000\
\006\000\038\000\026\000\007\000\008\000\009\000\010\000\001\000\
\045\000\023\000\011\000\004\000\005\000\036\000\037\000\016\000\
\016\000\012\000\013\000\002\000\002\000\025\000\027\000\011\000\
\044\000\002\000\046\000\047\000\029\000\002\000\012\000\013\000\
\028\000\030\000\002\000\031\000\032\000\033\000\039\000\040\000\
\041\000\042\000\000\000\043\000"

let yycheck = "\006\000\
\022\001\007\000\008\000\009\000\011\000\001\001\002\001\003\001\
\004\001\033\000\016\000\007\001\008\001\009\001\010\001\001\000\
\040\000\022\001\014\001\002\001\003\001\028\000\029\000\015\001\
\016\001\021\001\022\001\005\001\006\001\020\001\018\001\014\001\
\039\000\011\001\041\000\042\000\017\001\015\001\021\001\022\001\
\005\001\015\001\020\001\012\001\013\001\014\001\016\001\019\001\
\006\001\011\001\255\255\015\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  IN\000\
  BOOL\000\
  NAT\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 39 "parser.mly"
      ( _1 )
# 156 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 43 "parser.mly"
      ( _1 )
# 163 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 172 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 47 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 181 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 49 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 190 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 53 "parser.mly"
      ( _1 )
# 197 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 55 "parser.mly"
      ( TmSucc _2 )
# 204 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 57 "parser.mly"
      ( TmPred _2 )
# 211 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 59 "parser.mly"
      ( TmIsZero _2 )
# 218 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( TmApp (_1, _2) )
# 226 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 65 "parser.mly"
      ( _2 )
# 233 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
      ( TmTrue )
# 239 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
      ( TmFalse )
# 245 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
      ( TmVar _1 )
# 252 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 262 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 80 "parser.mly"
      ( _1 )
# 269 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 82 "parser.mly"
      ( TyArr (_1, _3) )
# 277 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 86 "parser.mly"
      ( _2 )
# 284 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
      ( TyBool )
# 290 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
      ( TyNat )
# 296 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.term)
