
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
%token LET
%token IN
%token BOOL
%token NAT

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

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
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

