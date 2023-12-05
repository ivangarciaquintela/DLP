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
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Lambda;;
    open Hashtbl;;
    let table = create 1024;;
# 43 "parser.ml"
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
  266 (* CONCAT *);
  267 (* STRLEN *);
  268 (* LET *);
  269 (* LETREC *);
  270 (* IN *);
  271 (* BOOL *);
  272 (* NAT *);
  273 (* STRING *);
  274 (* LBRACK *);
  275 (* RBRACK *);
  276 (* COMMA *);
  277 (* LBRACE *);
  278 (* RBRACE *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* DOT *);
  282 (* EQ *);
  283 (* COLON *);
  284 (* ARROW *);
  285 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  286 (* INTV *);
  287 (* IDV *);
  288 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\007\000\007\000\
\007\000\006\000\006\000\006\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\008\000\008\000\008\000\008\000\008\000\
\008\000\005\000\005\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\006\000\006\000\006\000\
\008\000\003\000\003\000\003\000\003\000\003\000\000\000\003\000\
\005\000\000\000\001\000\003\000\001\000\002\000\002\000\002\000\
\003\000\002\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\032\000\000\000\033\000\040\000\000\000\000\000\000\000\
\021\000\000\000\031\000\000\000\022\000\023\000\024\000\000\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\025\000\
\000\000\000\000\000\000\014\000\000\000\010\000\012\000\028\000\
\000\000\002\000\011\000\013\000\037\000\038\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\000\000\
\035\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000"

let yydgoto = "\002\000\
\021\000\022\000\036\000\024\000\065\000\037\000\040\000\025\000\
\066\000"

let yysindex = "\009\000\
\001\000\000\000\239\254\000\000\000\000\080\255\037\255\037\255\
\037\255\037\255\037\255\027\255\033\255\080\255\112\255\080\255\
\000\000\000\000\039\255\000\000\000\000\048\255\054\255\037\255\
\000\000\046\255\000\000\004\255\000\000\000\000\000\000\037\255\
\000\000\059\255\053\255\248\254\078\255\073\255\082\255\083\255\
\251\254\080\255\001\000\019\255\000\000\028\255\080\255\000\000\
\080\255\028\255\080\255\000\000\080\255\000\000\000\000\000\000\
\054\255\000\000\000\000\000\000\000\000\000\000\000\000\028\255\
\075\255\074\255\000\255\253\254\081\255\000\000\249\254\084\255\
\080\255\028\255\080\255\080\255\080\255\086\255\000\000\054\255\
\000\000\054\255\054\255\002\255\073\255\000\000\080\255\054\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\087\255\096\255\000\000\
\000\000\000\000\005\255\000\000\000\000\000\000\097\255\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\255\000\000\001\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\255\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\070\255\000\000\000\000\000\000\000\000\106\255\000\000\
\000\000\000\000\000\000\000\000\000\000\096\255\000\000\147\255\
\000\000\158\255\169\255\000\000\000\000\000\000\000\000\180\255"

let yygindex = "\000\000\
\066\000\000\000\255\255\000\000\253\255\243\255\051\000\046\000\
\000\000"

let yytablesize = 289
let yytable = "\023\000\
\017\000\039\000\031\000\031\000\028\000\075\000\031\000\031\000\
\047\000\001\000\076\000\051\000\078\000\026\000\041\000\087\000\
\044\000\044\000\056\000\044\000\031\000\044\000\031\000\031\000\
\044\000\031\000\044\000\031\000\044\000\031\000\031\000\031\000\
\031\000\031\000\031\000\031\000\031\000\070\000\004\000\005\000\
\057\000\023\000\061\000\062\000\063\000\067\000\069\000\068\000\
\059\000\060\000\064\000\071\000\029\000\030\000\031\000\032\000\
\033\000\034\000\019\000\016\000\072\000\019\000\018\000\035\000\
\042\000\018\000\018\000\027\000\020\000\045\000\081\000\080\000\
\046\000\082\000\083\000\084\000\043\000\048\000\044\000\050\000\
\003\000\004\000\005\000\006\000\049\000\088\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\034\000\034\000\034\000\
\052\000\014\000\053\000\073\000\015\000\074\000\016\000\054\000\
\055\000\018\000\077\000\079\000\058\000\018\000\027\000\020\000\
\003\000\004\000\005\000\006\000\085\000\015\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\003\000\004\000\016\000\
\086\000\014\000\005\000\005\000\015\000\000\000\016\000\000\000\
\000\000\000\000\000\000\005\000\000\000\018\000\038\000\020\000\
\005\000\005\000\000\000\005\000\000\000\005\000\005\000\007\000\
\007\000\000\000\005\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\006\000\006\000\000\000\007\000\007\000\000\000\
\007\000\000\000\007\000\006\000\000\000\008\000\008\000\007\000\
\006\000\006\000\000\000\006\000\000\000\006\000\008\000\000\000\
\009\000\009\000\006\000\008\000\008\000\000\000\008\000\000\000\
\008\000\009\000\000\000\000\000\000\000\008\000\009\000\009\000\
\000\000\009\000\000\000\009\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\015\000\000\000\016\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\019\000\
\020\000"

let yycheck = "\001\000\
\000\000\015\000\002\001\003\001\006\000\006\001\002\001\003\001\
\005\001\001\000\014\001\020\001\020\001\031\001\016\000\014\001\
\025\001\025\001\024\001\025\001\020\001\025\001\022\001\023\001\
\025\001\025\001\025\001\023\001\025\001\025\001\030\001\031\001\
\032\001\029\001\030\001\031\001\032\001\051\000\002\001\003\001\
\042\000\043\000\015\001\016\001\017\001\047\000\050\000\049\000\
\030\001\031\001\023\001\053\000\007\000\008\000\009\000\010\000\
\011\000\031\001\019\001\023\001\064\000\022\001\019\001\031\001\
\026\001\022\001\030\001\031\001\032\001\024\000\074\000\073\000\
\027\001\075\000\076\000\077\000\029\001\032\000\025\001\027\001\
\001\001\002\001\003\001\004\001\026\001\087\000\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\024\001\025\001\026\001\
\019\001\018\001\026\001\025\001\021\001\028\001\023\001\022\001\
\022\001\019\001\026\001\024\001\043\000\030\001\031\001\032\001\
\001\001\002\001\003\001\004\001\031\001\022\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\029\001\029\001\022\001\
\078\000\018\001\005\001\006\001\021\001\255\255\023\001\255\255\
\255\255\255\255\255\255\014\001\255\255\030\001\031\001\032\001\
\019\001\020\001\255\255\022\001\255\255\024\001\025\001\005\001\
\006\001\255\255\029\001\255\255\255\255\255\255\255\255\255\255\
\014\001\255\255\005\001\006\001\255\255\019\001\020\001\255\255\
\022\001\255\255\024\001\014\001\255\255\005\001\006\001\029\001\
\019\001\020\001\255\255\022\001\255\255\024\001\014\001\255\255\
\005\001\006\001\029\001\019\001\020\001\255\255\022\001\255\255\
\024\001\014\001\255\255\255\255\255\255\029\001\019\001\020\001\
\255\255\022\001\255\255\024\001\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\255\255\023\001\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\031\001\
\032\001"

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
  CONCAT\000\
  STRLEN\000\
  LET\000\
  LETREC\000\
  IN\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
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
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
        ( [] )
# 268 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.command list) in
    Obj.repr(
# 55 "parser.mly"
        ( _1::_3 )
# 276 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
    (Eval (_1))
# 283 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
        ( add table _1 _3; Bind (_1, _3) )
# 291 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 66 "parser.mly"
        ( _1 )
# 298 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 68 "parser.mly"
        ( TmIf (_2, _4, _6) )
# 307 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 70 "parser.mly"
        ( TmAbs (_2, _4, _6) )
# 316 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 72 "parser.mly"
        ( TmLetIn (_2, _4, _6) )
# 325 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 74 "parser.mly"
        ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 335 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 76 "parser.mly"
        (TmTuple (_2))
# 342 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
        (TmProj (_1, _3))
# 350 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'record) in
    Obj.repr(
# 80 "parser.mly"
        (TmRecord (_2))
# 357 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
        (TmProjR (_1, _3))
# 365 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 85 "parser.mly"
        (TmList (_2))
# 372 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
        ( [] )
# 378 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 93 "parser.mly"
        ( [(_1, _3)] )
# 386 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 96 "parser.mly"
        ( (_1, _3) :: _5 )
# 395 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
        ( [] )
# 401 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 104 "parser.mly"
        ( [_1] )
# 408 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 107 "parser.mly"
        ( _1 :: _3 )
# 416 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 111 "parser.mly"
        ( _1 )
# 423 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 113 "parser.mly"
        ( TmSucc _2 )
# 430 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 115 "parser.mly"
        ( TmPred _2 )
# 437 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 117 "parser.mly"
        ( TmIsZero _2 )
# 444 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 119 "parser.mly"
        ( TmConcat (_2, _3) )
# 452 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 121 "parser.mly"
        ( TmStrlen _2 )
# 459 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 123 "parser.mly"
        ( TmApp (_1, _2) )
# 467 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 127 "parser.mly"
        ( _2 )
# 474 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
        ( TmTrue )
# 480 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
        ( TmFalse )
# 486 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
        ( try find table _1 with Not_found -> TmVar (_1) )
# 493 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 136 "parser.mly"
        ( let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f _1 )
# 503 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 141 "parser.mly"
        ( TmString _1 )
# 510 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 144 "parser.mly"
        ( _1 )
# 517 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 146 "parser.mly"
        ( TyArr (_1, _3) )
# 525 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 150 "parser.mly"
        ( _2 )
# 532 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
        ( TyBool )
# 538 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
        ( TyNat )
# 544 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
        ( TyString )
# 550 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command list)
