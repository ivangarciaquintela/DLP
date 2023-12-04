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
# 41 "parser.ml"
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
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* DOT *);
  280 (* EQ *);
  281 (* COLON *);
  282 (* ARROW *);
  283 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTV *);
  285 (* IDV *);
  286 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\005\000\
\005\000\005\000\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\001\000\006\000\006\000\006\000\
\008\000\001\000\002\000\002\000\002\000\003\000\002\000\002\000\
\003\000\005\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\005\000\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\019\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\022\000\000\000\023\000\031\000\000\000\003\000\000\000\010\000\
\000\000\021\000\000\000\011\000\012\000\013\000\000\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\014\000\000\000\000\000\000\000\017\000\004\000\002\000\
\028\000\029\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\000\000\027\000\007\000\025\000\006\000\
\008\000\000\000\000\000\000\000\026\000\009\000"

let yydgoto = "\002\000\
\020\000\021\000\022\000\023\000\054\000\024\000\055\000"

let yysindex = "\006\000\
\001\000\000\000\003\255\000\000\000\000\012\255\045\255\045\255\
\045\255\045\255\045\255\005\255\007\255\012\255\012\255\000\000\
\000\000\249\254\000\000\000\000\011\255\000\000\045\255\000\000\
\019\255\000\000\044\255\000\000\000\000\000\000\045\255\000\000\
\026\255\032\255\031\255\039\255\012\255\001\000\000\000\075\255\
\012\255\000\000\012\255\075\255\012\255\000\000\000\000\000\000\
\000\000\000\000\000\000\082\255\075\255\041\255\036\255\059\255\
\057\255\048\255\058\255\060\255\061\255\012\255\075\255\012\255\
\012\255\012\255\000\000\082\255\000\000\000\000\000\000\000\000\
\000\000\065\255\066\255\012\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255\000\000\000\000\000\000\000\000\062\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\048\000\000\000\250\255\000\000\025\000\251\255\233\255"

let yytablesize = 287
let yytable = "\027\000\
\016\000\028\000\029\000\030\000\031\000\032\000\001\000\035\000\
\036\000\024\000\024\000\024\000\003\000\004\000\005\000\006\000\
\037\000\039\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\042\000\021\000\021\000\060\000\014\000\047\000\025\000\
\015\000\033\000\056\000\034\000\057\000\038\000\059\000\017\000\
\026\000\019\000\021\000\040\000\075\000\021\000\004\000\005\000\
\041\000\043\000\045\000\021\000\021\000\021\000\021\000\070\000\
\044\000\072\000\073\000\074\000\046\000\063\000\014\000\062\000\
\064\000\015\000\005\000\005\000\058\000\078\000\065\000\066\000\
\017\000\026\000\019\000\005\000\067\000\061\000\076\000\068\000\
\005\000\005\000\069\000\005\000\077\000\048\000\000\000\071\000\
\005\000\049\000\050\000\051\000\052\000\000\000\000\000\053\000\
\049\000\050\000\051\000\000\000\000\000\000\000\053\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\012\000\013\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\018\000\019\000"

let yycheck = "\006\000\
\000\000\007\000\008\000\009\000\010\000\011\000\001\000\014\000\
\015\000\022\001\023\001\024\001\001\001\002\001\003\001\004\001\
\024\001\023\000\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\031\000\002\001\003\001\052\000\018\001\037\000\029\001\
\021\001\029\001\041\000\029\001\043\000\027\001\045\000\028\001\
\029\001\030\001\018\001\025\001\068\000\021\001\002\001\003\001\
\005\001\024\001\020\001\027\001\028\001\029\001\030\001\062\000\
\025\001\064\000\065\000\066\000\022\001\026\001\018\001\023\001\
\006\001\021\001\005\001\006\001\044\000\076\000\014\001\024\001\
\028\001\029\001\030\001\014\001\019\001\053\000\014\001\020\001\
\019\001\020\001\022\001\022\001\019\001\038\000\255\255\063\000\
\027\001\015\001\016\001\017\001\018\001\255\255\255\255\021\001\
\015\001\016\001\017\001\255\255\255\255\255\255\021\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001"

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
# 50 "parser.mly"
        ( [] )
# 253 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'command) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.command list) in
    Obj.repr(
# 52 "parser.mly"
        ( _1::_3 )
# 261 "parser.ml"
               : Lambda.command list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
    (Eval (_1))
# 268 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
        ( add table _1 _3; Bind (_1, _3) )
# 276 "parser.ml"
               : 'command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 63 "parser.mly"
        ( _1 )
# 283 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 65 "parser.mly"
        ( TmIf (_2, _4, _6) )
# 292 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 67 "parser.mly"
        ( TmAbs (_2, _4, _6) )
# 301 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 69 "parser.mly"
        ( TmLetIn (_2, _4, _6) )
# 310 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 71 "parser.mly"
        ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 320 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 74 "parser.mly"
        ( _1 )
# 327 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 76 "parser.mly"
        ( TmSucc _2 )
# 334 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 78 "parser.mly"
        ( TmPred _2 )
# 341 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 80 "parser.mly"
        ( TmIsZero _2 )
# 348 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 82 "parser.mly"
        ( TmConcat (_2, _3) )
# 356 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 84 "parser.mly"
        ( TmStrlen _2 )
# 363 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 86 "parser.mly"
        ( TmApp (_1, _2) )
# 371 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 90 "parser.mly"
        ( _2 )
# 378 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 92 "parser.mly"
        ( TmTuple(_2, _4))
# 386 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
        ( TmTrue )
# 392 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
        ( TmFalse )
# 398 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
        ( try find table _1 with Not_found -> TmVar (_1) )
# 405 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 101 "parser.mly"
        ( let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f _1 )
# 415 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
        ( TmString _1 )
# 422 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 109 "parser.mly"
        ( _1 )
# 429 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 111 "parser.mly"
        ( TyArr (_1, _3) )
# 437 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'atomicTy) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 113 "parser.mly"
        (TyTuple (_2, _4))
# 445 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 117 "parser.mly"
        ( _2 )
# 452 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
        ( TyBool )
# 458 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
        ( TyNat )
# 464 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
        ( TyString )
# 470 "parser.ml"
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
