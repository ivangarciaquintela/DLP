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
\003\000\003\000\003\000\003\000\007\000\007\000\007\000\006\000\
\006\000\006\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\008\000\008\000\008\000\008\000\008\000\008\000\005\000\
\005\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\006\000\006\000\006\000\
\008\000\003\000\003\000\003\000\000\000\003\000\005\000\000\000\
\001\000\003\000\001\000\002\000\002\000\002\000\003\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\030\000\000\000\031\000\038\000\000\000\000\000\000\000\019\000\
\000\000\029\000\000\000\020\000\021\000\022\000\000\000\024\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\023\000\000\000\000\000\
\000\000\000\000\010\000\012\000\026\000\000\000\002\000\011\000\
\035\000\036\000\037\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\033\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000"

let yydgoto = "\002\000\
\020\000\021\000\022\000\023\000\061\000\037\000\038\000\024\000\
\062\000"

let yysindex = "\004\000\
\001\000\000\000\238\254\000\000\000\000\015\255\083\255\083\255\
\083\255\083\255\083\255\254\254\000\255\048\255\015\255\000\000\
\000\000\007\255\000\000\000\000\006\255\012\255\083\255\000\000\
\013\255\000\000\005\255\000\000\000\000\000\000\083\255\000\000\
\022\255\035\255\038\255\242\254\046\255\059\255\029\255\015\255\
\001\000\040\255\000\000\081\255\015\255\000\000\015\255\081\255\
\015\255\015\255\000\000\000\000\000\000\012\255\000\000\000\000\
\000\000\000\000\000\000\081\255\062\255\056\255\252\254\245\254\
\063\255\243\254\000\000\064\255\015\255\081\255\015\255\015\255\
\015\255\060\255\000\000\012\255\000\000\012\255\012\255\246\254\
\038\255\000\000\015\255\012\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\072\255\000\000\000\000\
\000\000\080\255\000\000\000\000\000\000\088\255\102\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\070\255\077\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\096\255\000\000\000\000\000\000\092\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\255\000\000\000\000\
\000\000\101\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\255\000\000\114\255\000\000\127\255\139\255\000\000\
\000\000\000\000\000\000\152\255"

let yygindex = "\000\000\
\084\000\000\000\250\255\000\000\228\255\079\000\056\000\175\000\
\000\000"

let yytablesize = 289
let yytable = "\027\000\
\016\000\071\000\072\000\083\000\001\000\050\000\074\000\036\000\
\039\000\045\000\042\000\042\000\025\000\042\000\042\000\003\000\
\004\000\005\000\006\000\065\000\042\000\007\000\008\000\009\000\
\010\000\011\000\012\000\013\000\033\000\042\000\034\000\068\000\
\040\000\054\000\041\000\014\000\042\000\015\000\063\000\044\000\
\064\000\077\000\066\000\036\000\017\000\026\000\019\000\047\000\
\003\000\004\000\005\000\006\000\053\000\042\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\048\000\076\000\049\000\
\078\000\079\000\080\000\051\000\014\000\056\000\015\000\029\000\
\029\000\032\000\032\000\032\000\084\000\017\000\035\000\019\000\
\052\000\029\000\029\000\070\000\004\000\005\000\069\000\075\000\
\073\000\029\000\081\000\029\000\029\000\013\000\029\000\057\000\
\058\000\059\000\017\000\029\000\029\000\029\000\029\000\060\000\
\029\000\015\000\005\000\005\000\029\000\029\000\029\000\029\000\
\017\000\026\000\019\000\005\000\003\000\016\000\007\000\007\000\
\004\000\005\000\014\000\005\000\055\000\005\000\005\000\007\000\
\067\000\082\000\005\000\006\000\006\000\007\000\000\000\007\000\
\000\000\007\000\000\000\000\000\006\000\000\000\007\000\008\000\
\008\000\000\000\006\000\000\000\006\000\000\000\006\000\000\000\
\008\000\000\000\000\000\006\000\009\000\009\000\008\000\000\000\
\008\000\000\000\008\000\000\000\000\000\009\000\000\000\008\000\
\000\000\000\000\000\000\009\000\000\000\009\000\000\000\009\000\
\000\000\000\000\000\000\000\000\009\000\028\000\029\000\030\000\
\031\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\018\000\
\019\000"

let yycheck = "\006\000\
\000\000\006\001\014\001\014\001\001\000\020\001\020\001\014\000\
\015\000\005\001\025\001\025\001\031\001\025\001\025\001\001\001\
\002\001\003\001\004\001\048\000\025\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\031\001\025\001\031\001\060\000\
\026\001\040\000\029\001\021\001\025\001\023\001\045\000\027\001\
\047\000\070\000\049\000\050\000\030\001\031\001\032\001\026\001\
\001\001\002\001\003\001\004\001\024\001\025\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\027\001\069\000\026\001\
\071\000\072\000\073\000\022\001\021\001\030\001\023\001\002\001\
\003\001\024\001\025\001\026\001\083\000\030\001\031\001\032\001\
\022\001\002\001\003\001\028\001\002\001\003\001\025\001\024\001\
\026\001\020\001\031\001\022\001\023\001\022\001\025\001\015\001\
\016\001\017\001\022\001\030\001\031\001\032\001\023\001\023\001\
\025\001\023\001\005\001\006\001\029\001\030\001\031\001\032\001\
\030\001\031\001\032\001\014\001\029\001\022\001\005\001\006\001\
\029\001\020\001\022\001\022\001\041\000\024\001\025\001\014\001\
\050\000\074\000\029\001\005\001\006\001\020\001\255\255\022\001\
\255\255\024\001\255\255\255\255\014\001\255\255\029\001\005\001\
\006\001\255\255\020\001\255\255\022\001\255\255\024\001\255\255\
\014\001\255\255\255\255\029\001\005\001\006\001\020\001\255\255\
\022\001\255\255\024\001\255\255\255\255\014\001\255\255\029\001\
\255\255\255\255\255\255\020\001\255\255\022\001\255\255\024\001\
\255\255\255\255\255\255\255\255\029\001\007\000\008\000\009\000\
\010\000\011\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\023\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\031\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\021\001\255\255\023\001\
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
    Obj.repr(
# 85 "parser.mly"
        ( [] )
# 363 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 88 "parser.mly"
        ( [(_1, _3)] )
# 371 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 91 "parser.mly"
        ( (_1, _3) :: _5 )
# 380 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
        ( [] )
# 386 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 99 "parser.mly"
        ( [_1] )
# 393 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 102 "parser.mly"
        ( _1 :: _3 )
# 401 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 106 "parser.mly"
        ( _1 )
# 408 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 108 "parser.mly"
        ( TmSucc _2 )
# 415 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 110 "parser.mly"
        ( TmPred _2 )
# 422 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 112 "parser.mly"
        ( TmIsZero _2 )
# 429 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 114 "parser.mly"
        ( TmConcat (_2, _3) )
# 437 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 116 "parser.mly"
        ( TmStrlen _2 )
# 444 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 118 "parser.mly"
        ( TmApp (_1, _2) )
# 452 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 122 "parser.mly"
        ( _2 )
# 459 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
        ( TmTrue )
# 465 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
        ( TmFalse )
# 471 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
        ( try find table _1 with Not_found -> TmVar (_1) )
# 478 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 131 "parser.mly"
        ( let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f _1 )
# 488 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
        ( TmString _1 )
# 495 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 139 "parser.mly"
        ( _1 )
# 502 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 141 "parser.mly"
        ( TyArr (_1, _3) )
# 510 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 145 "parser.mly"
        ( _2 )
# 517 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
        ( TyBool )
# 523 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
        ( TyNat )
# 529 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
        ( TyString )
# 535 "parser.ml"
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
