
%{
    open Lambda;;
    open Hashtbl;;
    let table = create 1024;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token CONCAT
%token STRLEN
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token STRING

%token LBRACK
%token RBRACK
%token COMMA

%token LBRACE
%token RBRACE

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token SEMICOLON
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command list> s

%%

s :
    EOF 
        { [] }
    | command SEMICOLON s
        { $1::$3 }

command:
    term
    {Eval ($1)}
    | IDV EQ term
        { add table $1 $3; Bind ($1, $3) }


term :
    appTerm
        { $1 }
    | IF term THEN term ELSE term
        { TmIf ($2, $4, $6) }
    | LAMBDA IDV COLON ty DOT term
        { TmAbs ($2, $4, $6) }
    | LET IDV EQ term IN term
        { TmLetIn ($2, $4, $6) }
    | LETREC IDV COLON ty EQ term IN term
        { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
    | LBRACE list RBRACE
        {TmTuple ($2)}
    | term DOT INTV
        {TmProj ($1, $3)}
    | LBRACE record RBRACE
        {TmRecord ($2)}
    | term DOT IDV
        {TmProjR ($1, $3)}
        
    | LBRACK list RBRACK
        {TmList ($2)}

record :
    /* Manejar el caso de una lista vacía */
    | 
        { [] }
    /* Manejar un solo campo en el registro */
    | IDV EQ term
        { [($1, $3)] }
    /* Manejar múltiples campos en el registro */
    | IDV EQ term COMMA record
        { ($1, $3) :: $5 }
        
list :
    /* Manejar el caso de una lista vacía */
    | 
        { [] }
    /* Manejar una lista con un solo término */
    | term
        { [$1] }
    /* Manejar una lista con múltiples términos separados por comas */
    | term COMMA list
        { $1 :: $3 }

appTerm :
    atomicTerm
        { $1 }
    | SUCC atomicTerm
        { TmSucc $2 }
    | PRED atomicTerm
        { TmPred $2 }
    | ISZERO atomicTerm
        { TmIsZero $2 }
    | CONCAT atomicTerm atomicTerm
        { TmConcat ($2, $3) }
    | STRLEN atomicTerm
        { TmStrlen $2 }
    | appTerm atomicTerm
        { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
        { $2 }
    | TRUE
        { TmTrue }
    | FALSE
        { TmFalse }
    | IDV
      //{ TmVar $1 }
        { try find table $1 with Not_found -> TmVar ($1) }      
    | INTV
        { let rec f = function
            0 -> TmZero
            | n -> TmSucc (f (n-1))
        in f $1 }
    | STRINGV
        { TmString $1 }
ty :
    atomicTy
        { $1 }
    | atomicTy ARROW ty
        { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
        { $2 }
    | BOOL
        { TyBool }
    | NAT
        { TyNat }
    | STRING
        { TyString }


