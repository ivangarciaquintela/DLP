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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Lambda;;
    open Hashtbl;;
    let table = create 1024;;
# 51 "parser.ml"
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
  279 (* LIST *);
  280 (* NIL *);
  281 (* CONS *);
  282 (* ISNIL *);
  283 (* HEAD *);
  284 (* TAIL *);
  285 (* LPAREN *);
  286 (* RPAREN *);
  287 (* DOT *);
  288 (* EQ *);
  289 (* COLON *);
  290 (* ARROW *);
  291 (* SEMICOLON *);
    0 (* EOF *);
  292 (* LT *);
  293 (* GT *);
    0|]

let yytransl_block = [|
  294 (* INTV *);
  295 (* IDV *);
  296 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\007\000\007\000\007\000\006\000\006\000\006\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\008\000\
\008\000\008\000\008\000\008\000\008\000\005\000\005\000\005\000\
\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\006\000\006\000\006\000\
\008\000\003\000\003\000\003\000\003\000\003\000\005\000\004\000\
\004\000\004\000\000\000\003\000\005\000\000\000\001\000\003\000\
\001\000\002\000\002\000\002\000\003\000\002\000\002\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\003\000\002\000\
\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\033\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\043\000\044\000\
\000\000\000\000\001\000\036\000\000\000\037\000\045\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\035\000\000\000\
\000\000\026\000\027\000\028\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\000\000\040\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\010\000\012\000\032\000\041\000\
\000\000\002\000\011\000\013\000\014\000\000\000\000\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\016\000\017\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000"

let yydgoto = "\002\000\
\023\000\024\000\025\000\026\000\027\000\043\000\044\000\028\000\
\029\000"

let yysindex = "\014\000\
\001\000\000\000\234\254\000\000\000\000\087\255\072\255\072\255\
\072\255\072\255\072\255\237\254\239\254\000\000\000\000\000\000\
\127\255\087\255\000\000\000\000\249\254\000\000\000\000\013\255\
\003\255\072\255\024\255\000\000\251\254\029\255\000\000\005\255\
\087\255\000\000\000\000\000\000\072\255\000\000\025\255\031\255\
\044\255\255\254\064\255\065\255\009\255\075\255\087\255\001\000\
\076\255\000\000\093\255\000\000\056\255\056\255\087\255\000\000\
\087\255\056\255\087\255\087\255\000\000\000\000\000\000\000\000\
\003\255\000\000\000\000\000\000\000\000\072\255\072\255\072\255\
\072\255\056\255\000\000\039\255\007\255\000\255\060\255\038\255\
\000\000\072\255\000\000\000\000\000\000\077\255\087\255\087\255\
\087\255\087\255\070\255\000\000\003\255\003\255\003\255\002\255\
\044\255\000\000\087\255\003\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\255\000\000\000\000\000\000\021\255\000\000\000\000\000\000\
\088\255\140\255\000\000\000\000\048\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\255\100\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\102\255\000\000\000\000\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\111\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\091\255\000\000\158\255\176\255\180\255\000\000\
\000\000\000\000\000\000\198\255"

let yygindex = "\000\000\
\093\000\000\000\250\255\000\000\245\255\087\000\058\000\251\255\
\000\000"

let yytablesize = 297
let yytable = "\032\000\
\019\000\034\000\035\000\036\000\037\000\038\000\046\000\035\000\
\035\000\055\000\042\000\045\000\088\000\089\000\001\000\099\000\
\030\000\052\000\060\000\039\000\050\000\040\000\035\000\035\000\
\047\000\035\000\045\000\035\000\053\000\049\000\049\000\056\000\
\049\000\049\000\035\000\049\000\035\000\049\000\063\000\049\000\
\065\000\075\000\076\000\035\000\035\000\035\000\079\000\048\000\
\077\000\035\000\078\000\035\000\080\000\042\000\051\000\035\000\
\057\000\091\000\035\000\035\000\035\000\054\000\086\000\058\000\
\082\000\083\000\084\000\085\000\049\000\087\000\014\000\015\000\
\016\000\004\000\005\000\059\000\092\000\038\000\038\000\038\000\
\093\000\094\000\095\000\096\000\074\000\061\000\062\000\003\000\
\004\000\005\000\006\000\090\000\100\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\033\000\014\000\015\000\016\000\
\064\000\051\000\064\000\017\000\097\000\020\000\031\000\022\000\
\019\000\067\000\068\000\018\000\069\000\070\000\071\000\072\000\
\073\000\023\000\003\000\022\000\020\000\031\000\022\000\003\000\
\004\000\005\000\006\000\004\000\020\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\066\000\014\000\015\000\016\000\
\005\000\005\000\081\000\017\000\098\000\000\000\000\000\000\000\
\000\000\005\000\000\000\018\000\000\000\000\000\000\000\005\000\
\000\000\005\000\007\000\007\000\020\000\041\000\022\000\000\000\
\000\000\005\000\005\000\007\000\000\000\000\000\005\000\000\000\
\000\000\007\000\000\000\007\000\006\000\006\000\000\000\000\000\
\008\000\008\000\000\000\007\000\000\000\006\000\000\000\000\000\
\007\000\008\000\000\000\006\000\000\000\006\000\000\000\008\000\
\000\000\008\000\009\000\009\000\000\000\006\000\000\000\000\000\
\000\000\008\000\006\000\009\000\000\000\000\000\008\000\000\000\
\000\000\009\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\014\000\
\015\000\016\000\000\000\000\000\000\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\021\000\
\022\000"

let yycheck = "\006\000\
\000\000\007\000\008\000\009\000\010\000\011\000\018\000\002\001\
\003\001\005\001\017\000\018\000\006\001\014\001\001\000\014\001\
\039\001\023\001\020\001\039\001\026\000\039\001\002\001\003\001\
\032\001\020\001\033\000\022\001\034\001\031\001\031\001\037\000\
\031\001\031\001\029\001\031\001\031\001\031\001\030\001\031\001\
\047\000\053\000\054\000\038\001\039\001\040\001\058\000\035\001\
\055\000\029\001\057\000\031\001\059\000\060\000\031\001\035\001\
\032\001\020\001\038\001\039\001\040\001\033\001\074\000\033\001\
\070\000\071\000\072\000\073\000\031\001\031\001\015\001\016\001\
\017\001\002\001\003\001\032\001\082\000\030\001\031\001\032\001\
\087\000\088\000\089\000\090\000\029\001\022\001\022\001\001\001\
\002\001\003\001\004\001\032\001\099\000\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\029\001\015\001\016\001\017\001\
\030\001\031\001\030\001\021\001\039\001\038\001\039\001\040\001\
\022\001\038\001\039\001\029\001\024\001\025\001\026\001\027\001\
\028\001\022\001\035\001\022\001\038\001\039\001\040\001\001\001\
\002\001\003\001\004\001\035\001\022\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\048\000\015\001\016\001\017\001\
\005\001\006\001\060\000\021\001\091\000\255\255\255\255\255\255\
\255\255\014\001\255\255\029\001\255\255\255\255\255\255\020\001\
\255\255\022\001\005\001\006\001\038\001\039\001\040\001\255\255\
\255\255\030\001\031\001\014\001\255\255\255\255\035\001\255\255\
\255\255\020\001\255\255\022\001\005\001\006\001\255\255\255\255\
\005\001\006\001\255\255\030\001\255\255\014\001\255\255\255\255\
\035\001\014\001\255\255\020\001\255\255\022\001\255\255\020\001\
\255\255\022\001\005\001\006\001\255\255\030\001\255\255\255\255\
\255\255\030\001\035\001\014\001\255\255\255\255\035\001\255\255\
\255\255\020\001\255\255\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\030\001\255\255\255\255\255\255\255\255\
\035\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\255\255\255\255\255\255\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\038\001\039\001\
\040\001"

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
  LIST\000\
  NIL\000\
  CONS\000\
  ISNIL\000\
  HEAD\000\
  TAIL\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  SEMICOLON\000\
  EOF\000\
  LT\000\
  GT\000\
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
# 64 "parser.mly"
        ( [] )
# 302 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.command list) in
    Obj.repr(
# 66 "parser.mly"
        ( _1::_3 )
# 310 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 70 "parser.mly"
    (Eval (_1))
# 317 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 72 "parser.mly"
        ( add table _1 _3; Bind (_1, _3) )
# 325 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 77 "parser.mly"
        ( _1 )
# 332 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
        ( TmIf (_2, _4, _6) )
# 341 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 81 "parser.mly"
        ( TmAbs (_2, _4, _6) )
# 350 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 83 "parser.mly"
        ( TmLetIn (_2, _4, _6) )
# 359 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 85 "parser.mly"
        ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 369 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 87 "parser.mly"
        (TmTuple (_2))
# 376 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 89 "parser.mly"
        (TmProj (_1, _3))
# 384 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'record) in
    Obj.repr(
# 91 "parser.mly"
        (TmRecord (_2))
# 391 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
        (TmProjR (_1, _3))
# 399 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    Obj.repr(
# 96 "parser.mly"
        ( TmNil _1 )
# 406 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 98 "parser.mly"
        ( TmCons (_1, _4, _5) )
# 415 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 100 "parser.mly"
        ( TmIsNil (_1, _4))
# 423 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 102 "parser.mly"
        ( TmHead (_1, _4))
# 431 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 104 "parser.mly"
        ( TmTail (_1, _4))
# 439 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
        ( [] )
# 445 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 110 "parser.mly"
        ( [(_1, _3)] )
# 453 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 112 "parser.mly"
        ( (_1, _3) :: _5 )
# 462 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
        ( [] )
# 468 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 118 "parser.mly"
        ( [_1] )
# 475 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 120 "parser.mly"
        ( _1 :: _3 )
# 483 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 124 "parser.mly"
        ( _1 )
# 490 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 126 "parser.mly"
        ( TmSucc _2 )
# 497 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 128 "parser.mly"
        ( TmPred _2 )
# 504 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 130 "parser.mly"
        ( TmIsZero _2 )
# 511 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 132 "parser.mly"
        ( TmConcat (_2, _3) )
# 519 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 134 "parser.mly"
        ( TmStrlen _2 )
# 526 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 136 "parser.mly"
        ( TmApp (_1, _2) )
# 534 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 140 "parser.mly"
        ( _2 )
# 541 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
        ( TmTrue )
# 547 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
        ( TmFalse )
# 553 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 146 "parser.mly"
        ( try find table _1 with Not_found -> TmVar (_1) )
# 560 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 148 "parser.mly"
        ( let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f _1 )
# 570 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
        ( TmString _1 )
# 577 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 156 "parser.mly"
        ( _1 )
# 584 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 158 "parser.mly"
        ( TyArr (_1, _3) )
# 592 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 160 "parser.mly"
        (TyList _1)
# 599 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 164 "parser.mly"
        ( _2 )
# 606 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
        ( TyBool )
# 612 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parser.mly"
        ( TyNat )
# 618 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
        ( TyString )
# 624 "parser.ml"
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
