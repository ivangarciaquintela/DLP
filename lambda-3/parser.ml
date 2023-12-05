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
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
    open Lambda;;
    open Hashtbl;;
    let table = create 1024;;
# 49 "parser.ml"
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
    0|]

let yytransl_block = [|
  292 (* INTV *);
  293 (* IDV *);
  294 (* STRINGV *);
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

let yysindex = "\030\000\
\001\000\000\000\001\255\000\000\000\000\087\255\083\255\083\255\
\083\255\083\255\083\255\009\255\011\255\000\000\000\000\000\000\
\125\255\087\255\000\000\000\000\023\255\000\000\000\000\005\255\
\041\255\083\255\043\255\000\000\247\254\054\255\000\000\003\255\
\087\255\000\000\000\000\000\000\083\255\000\000\044\255\059\255\
\069\255\002\255\053\255\084\255\006\255\030\255\087\255\001\000\
\033\255\000\000\188\255\000\000\000\255\000\255\087\255\000\000\
\087\255\000\255\087\255\087\255\000\000\000\000\000\000\000\000\
\041\255\000\000\000\000\000\000\000\000\083\255\083\255\083\255\
\083\255\000\255\000\000\074\255\004\255\251\254\075\255\008\255\
\000\000\083\255\000\000\000\000\000\000\079\255\087\255\087\255\
\087\255\087\255\073\255\000\000\041\255\041\255\041\255\255\254\
\069\255\000\000\087\255\041\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\089\255\000\000\000\000\000\000\021\255\000\000\000\000\000\000\
\078\255\138\255\000\000\000\000\244\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\255\092\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\093\255\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\096\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\089\255\000\000\145\255\165\255\172\255\000\000\
\000\000\000\000\000\000\176\255"

let yygindex = "\000\000\
\074\000\000\000\250\255\000\000\245\255\070\000\040\000\251\255\
\000\000"

let yytablesize = 295
let yytable = "\032\000\
\019\000\034\000\035\000\036\000\037\000\038\000\046\000\055\000\
\089\000\088\000\042\000\045\000\099\000\052\000\014\000\015\000\
\016\000\038\000\038\000\038\000\050\000\060\000\035\000\035\000\
\053\000\049\000\045\000\091\000\074\000\049\000\001\000\056\000\
\049\000\049\000\049\000\063\000\049\000\030\000\049\000\048\000\
\065\000\075\000\076\000\035\000\035\000\039\000\079\000\040\000\
\077\000\035\000\078\000\035\000\080\000\042\000\047\000\035\000\
\035\000\035\000\035\000\064\000\051\000\035\000\086\000\035\000\
\082\000\083\000\084\000\085\000\067\000\068\000\035\000\049\000\
\035\000\051\000\061\000\057\000\092\000\035\000\035\000\035\000\
\093\000\094\000\095\000\096\000\004\000\005\000\054\000\003\000\
\004\000\005\000\006\000\058\000\100\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\059\000\014\000\015\000\016\000\
\087\000\062\000\090\000\017\000\064\000\097\000\019\000\033\000\
\003\000\023\000\022\000\018\000\004\000\020\000\020\000\031\000\
\022\000\066\000\020\000\031\000\022\000\003\000\004\000\005\000\
\006\000\081\000\098\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\000\000\014\000\015\000\016\000\005\000\005\000\
\000\000\017\000\000\000\000\000\000\000\007\000\007\000\005\000\
\000\000\018\000\000\000\000\000\000\000\005\000\007\000\005\000\
\020\000\041\000\022\000\000\000\007\000\000\000\007\000\005\000\
\005\000\006\000\006\000\000\000\005\000\000\000\007\000\000\000\
\008\000\008\000\006\000\007\000\009\000\009\000\000\000\000\000\
\006\000\008\000\006\000\000\000\000\000\009\000\000\000\008\000\
\000\000\008\000\006\000\009\000\000\000\009\000\000\000\006\000\
\000\000\008\000\000\000\000\000\000\000\009\000\008\000\000\000\
\000\000\000\000\009\000\069\000\070\000\071\000\072\000\073\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\014\000\
\015\000\016\000\000\000\000\000\000\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\021\000\022\000"

let yycheck = "\006\000\
\000\000\007\000\008\000\009\000\010\000\011\000\018\000\005\001\
\014\001\006\001\017\000\018\000\014\001\023\001\015\001\016\001\
\017\001\030\001\031\001\032\001\026\000\020\001\002\001\003\001\
\034\001\031\001\033\000\020\001\029\001\031\001\001\000\037\000\
\031\001\031\001\031\001\030\001\031\001\037\001\031\001\035\001\
\047\000\053\000\054\000\002\001\003\001\037\001\058\000\037\001\
\055\000\029\001\057\000\031\001\059\000\060\000\032\001\035\001\
\036\001\037\001\038\001\030\001\031\001\020\001\074\000\022\001\
\070\000\071\000\072\000\073\000\036\001\037\001\029\001\031\001\
\031\001\031\001\022\001\032\001\082\000\036\001\037\001\038\001\
\087\000\088\000\089\000\090\000\002\001\003\001\033\001\001\001\
\002\001\003\001\004\001\033\001\099\000\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\032\001\015\001\016\001\017\001\
\031\001\022\001\032\001\021\001\030\001\037\001\022\001\029\001\
\035\001\022\001\022\001\029\001\035\001\022\001\036\001\037\001\
\038\001\048\000\036\001\037\001\038\001\001\001\002\001\003\001\
\004\001\060\000\091\000\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\005\001\006\001\
\255\255\021\001\255\255\255\255\255\255\005\001\006\001\014\001\
\255\255\029\001\255\255\255\255\255\255\020\001\014\001\022\001\
\036\001\037\001\038\001\255\255\020\001\255\255\022\001\030\001\
\031\001\005\001\006\001\255\255\035\001\255\255\030\001\255\255\
\005\001\006\001\014\001\035\001\005\001\006\001\255\255\255\255\
\020\001\014\001\022\001\255\255\255\255\014\001\255\255\020\001\
\255\255\022\001\030\001\020\001\255\255\022\001\255\255\035\001\
\255\255\030\001\255\255\255\255\255\255\030\001\035\001\255\255\
\255\255\255\255\035\001\024\001\025\001\026\001\027\001\028\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\255\255\255\255\255\255\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\036\001\037\001\038\001"

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
# 61 "parser.mly"
        ( [] )
# 294 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.command list) in
    Obj.repr(
# 63 "parser.mly"
        ( _1::_3 )
# 302 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 67 "parser.mly"
    (Eval (_1))
# 309 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 69 "parser.mly"
        ( add table _1 _3; Bind (_1, _3) )
# 317 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 74 "parser.mly"
        ( _1 )
# 324 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 76 "parser.mly"
        ( TmIf (_2, _4, _6) )
# 333 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 78 "parser.mly"
        ( TmAbs (_2, _4, _6) )
# 342 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
        ( TmLetIn (_2, _4, _6) )
# 351 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 82 "parser.mly"
        ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 361 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 84 "parser.mly"
        (TmTuple (_2))
# 368 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
        (TmProj (_1, _3))
# 376 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'record) in
    Obj.repr(
# 88 "parser.mly"
        (TmRecord (_2))
# 383 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
        (TmProjR (_1, _3))
# 391 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    Obj.repr(
# 93 "parser.mly"
      ( TmNil _1 )
# 398 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 95 "parser.mly"
      ( TmCons (_1, _4, _5) )
# 407 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 97 "parser.mly"
      ( TmIsNil (_1, _4))
# 415 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 99 "parser.mly"
      ( TmHead (_1, _4))
# 423 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 101 "parser.mly"
      ( TmTail (_1, _4))
# 431 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
        ( [] )
# 437 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 109 "parser.mly"
        ( [(_1, _3)] )
# 445 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 112 "parser.mly"
        ( (_1, _3) :: _5 )
# 454 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
        ( [] )
# 460 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 120 "parser.mly"
        ( [_1] )
# 467 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 123 "parser.mly"
        ( _1 :: _3 )
# 475 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 127 "parser.mly"
        ( _1 )
# 482 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 129 "parser.mly"
        ( TmSucc _2 )
# 489 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 131 "parser.mly"
        ( TmPred _2 )
# 496 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 133 "parser.mly"
        ( TmIsZero _2 )
# 503 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 135 "parser.mly"
        ( TmConcat (_2, _3) )
# 511 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 137 "parser.mly"
        ( TmStrlen _2 )
# 518 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 139 "parser.mly"
        ( TmApp (_1, _2) )
# 526 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 143 "parser.mly"
        ( _2 )
# 533 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "parser.mly"
        ( TmTrue )
# 539 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
        ( TmFalse )
# 545 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 150 "parser.mly"
        ( try find table _1 with Not_found -> TmVar (_1) )
# 552 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 152 "parser.mly"
        ( let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f _1 )
# 562 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
        ( TmString _1 )
# 569 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 160 "parser.mly"
        ( _1 )
# 576 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 162 "parser.mly"
        ( TyArr (_1, _3) )
# 584 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 164 "parser.mly"
        (TyList _1)
# 591 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 168 "parser.mly"
        ( _2 )
# 598 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
        ( TyBool )
# 604 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 172 "parser.mly"
        ( TyNat )
# 610 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "parser.mly"
        ( TyString )
# 616 "parser.ml"
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
