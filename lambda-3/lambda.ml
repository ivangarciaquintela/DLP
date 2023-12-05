
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty

;;

type context =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmString of string
  | TmConcat of term * term
  | TmStrlen of term
  | TmFix of term
  | TmTuple of term list
  | TmProj of term * int
  | TmRecord of (string * term) list
  | TmProjR of term * string

  (* Lists *)
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
;;

type command =
  | Eval of term
  | Bind of string * term

;;


(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ty1 ^ " -> " ^  string_of_ty ty2 
  | TyString ->
      "String"
  | TyTuple tyL ->
        let sFdL = List.map string_of_ty tyL in
        "(" ^ (String.concat ", " sFdL) ^ ")"
  | TyRecord t -> 
      let sfdL = List.map (fun (label, field_ty) -> label ^ ": " ^ string_of_ty field_ty) t 
          in  
          "(" ^ (String.concat ", " sfdL) ^ ")"
  | TyList ty ->
          "List " ^ (string_of_ty ty)

;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
      _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT2 = tyT11 then tyT12
            else raise (Type_error "parameter type mismatch")
        | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
  
  | TmString _ ->
      TyString

  |TmStrlen (t1) ->
      if typeof ctx t1 = TyString then TyNat
      else raise (Type_error "argument of strlen is not a string")


  | TmConcat (t1, t2) ->
      if typeof ctx t1= TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "arguments of concat are not strings")

  | TmFix t1 ->
    let tyT1 = typeof ctx t1 in
    (match tyT1 with
    TyArr (tyT11, tyT12) ->
      if tyT11 = tyT12 then tyT12
      else raise(Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected")
      )
  | TmTuple t -> TyTuple (List.map (typeof ctx) t)

  | TmProj (t, n) ->
    let tyT = typeof ctx t in
    (match tyT with
    | TyTuple tyL ->
        (match List.nth_opt tyL n with
        | Some ty -> ty  
        | None -> raise (Type_error "Projection position out of bounds"))
        | _ ->
          raise (Type_error "Cant apply projection to non-tuple type"))
  
  | TmRecord l -> TyRecord (List.map (fun (s,t) -> (s, typeof ctx t)) l)
  
  | TmProjR (t, label) ->
    (match typeof ctx t with
     | TyRecord fields ->
       (try List.assoc label fields with
        | Not_found -> raise (Type_error ("Label '" ^ label ^ "' not found in record")))
     | _ -> raise (Type_error "Projection can only be applied to record types"))

  (* T-Nil *)
  | TmNil ty -> TyList ty
  
  (* T-Cons *)
  | TmCons (ty,h,t) ->
      let tyTh = typeof ctx h in
        let tyTt = typeof ctx t in
          if (tyTh = ty) && (tyTt = TyList(ty)) then TyList(ty)
          else raise (Type_error "elements of list have different types")
  
  (* T-IsNil *)
  | TmIsNil (ty,t) -> 
    if typeof ctx t = TyList(ty) then TyBool
    else raise (Type_error ("argument of isempty is not a " ^ (string_of_ty ty) ^ " list"))
 
  (* T-Head *)    
  | TmHead (ty,t) ->     
    if typeof ctx t = TyList(ty) then ty
    else raise (Type_error ("argument of head is not a " ^ (string_of_ty ty) ^ " list"))
    
  (* T-Tail *)    
  | TmTail (ty,t) -> 
    if typeof ctx t = TyList(ty) then TyList(ty)
    else raise (Type_error ("argument of tail is not a " ^ (string_of_ty ty) ^ " list"))

;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ string_of_term t1 ^ "\n" ^
      " then " ^ string_of_term t2 ^ "\n" ^
      " else " ^ string_of_term t3 
  | TmZero ->
      "0"
  | TmSucc t ->
    let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ string_of_term t 
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
    "lambda " ^ s ^ " : " ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ "\n"
  | TmApp (t1, t2) ->
      string_of_term t1 ^ " " ^ string_of_term t2 
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmStrlen t ->
      "strlen " ^ string_of_term t 
  | TmConcat (t1, t2) ->
      string_of_term t1 ^ " ^ " ^ string_of_term t2
  | TmFix t -> 
      "fix" ^ string_of_term t

  | TmTuple t -> 
      let values_string = String.concat ", " (List.map string_of_term t) in
    "tuple {" ^ values_string ^ "}"
    
  | TmProj (t,pos) ->
    "proj (" ^ string_of_term t ^ ", " ^ string_of_int pos ^ ")"

  | TmRecord t -> 
      let values_string = String.concat ", " (List.map (fun (label, field_ty) -> label ^ ": " ^ string_of_term field_ty) t) in
    "record {" ^ values_string ^ "}"
  
  | TmProjR (t, label) ->
      string_of_term t ^ "." ^ label


  | TmNil ty -> 
      "nil[" ^ string_of_ty ty ^ "]" 
| TmCons (ty,h,t) -> 
      "cons[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term h ^ ") (" ^ (string_of_term t) ^ ")"
| TmIsNil (ty,t) -> 
      "isnil[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
| TmHead (ty,t) -> 
      "head[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
| TmTail (ty,t) -> 
      "tail[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmString _ ->
      []
  | TmStrlen t ->
      free_vars t
      (*alomejor mal*)
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  |TmFix t -> free_vars t
  
  | TmTuple t -> 
    List.fold_left (fun acc term -> lunion acc (free_vars term)) [] t
  
  | TmProj (t, _) ->
      free_vars t
  
  | TmRecord fields ->
        let free_in_field (label, term) =
          free_vars term
        in
        List.flatten (List.map free_in_field (fields))
  
  | TmProjR (t, _) ->
      free_vars t

  
  | TmNil ty -> 
      []
  | TmCons (ty,t1,t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmIsNil (ty,t) ->
      free_vars t
  | TmHead (ty,t) ->
      free_vars t
  | TmTail (ty,t) ->
      free_vars t
  
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
          if not (List.mem y fvs)
          then TmAbs (y, tyY, subst x s t)
          else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
          if not (List.mem y fvs)
          then TmLetIn (y, subst x s t1, subst x s t2)
          else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmString st ->
      TmString st
  | TmStrlen t ->
      TmStrlen (subst x s t)
            (*alomejor mal*)
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmFix t -> TmFix (subst x s t)

  | TmTuple t -> TmTuple (List.map (subst x s) t)

  | TmProj (t, n) ->
    TmProj (subst x s t, n)
  
  | TmRecord fields ->
      let subst_field (label, term) =
        (label, subst x s term)
      in
      TmRecord (List.map subst_field fields)
  
  | TmProjR (t, label) ->
      TmProjR (subst x s t, label)
      | TmNil ty -> 
        tm
    | TmCons (ty,t1,t2) -> 
        TmCons (ty, (subst  x s t1), (subst x s t2))
    | TmIsNil (ty,t) ->
        TmIsNil (ty, (subst  x s t))
    | TmHead (ty,t) ->
        TmHead (ty, (subst  x s t))
    | TmTail (ty,t) ->
        TmTail (ty, (subst x s t))
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple t when List.for_all isval t -> true
  | TmRecord fields ->
    let is_val_field (_, term) = isval term in
    List.for_all is_val_field fields
  | TmProjR (t, _) when isval t -> true
  | TmNil _ -> true
  | TmCons(_,h,t) -> (&&) (isval h) (isval t)
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 t1 in
      TmLetIn (x, t1', t2)

    (* E-Strlen *)
    | TmStrlen (TmString s) ->
      let len = String.length s in
      let rec make_succ n acc = 
        if n = 0 then acc else make_succ (n - 1) (TmSucc acc) 
      in
      make_succ len TmZero

    (* E-Strlen1 *)
  | TmStrlen (s) ->
      let s' = eval1 s in
      TmStrlen (s')

    (* E-String *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2) 

    (* E-String1 *)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 t2 in
      TmConcat (TmString s1, t2')
  
    (* E-String2 *)
  | TmConcat (t1, t2) ->
      let t1' = eval1 t1 in
      TmConcat (t1', t2)
    

    (* E-FixBeta *)
  |TmFix (TmAbs (x, _, t12)) ->
    subst x tm t12

  (* E-Fix *)
  | TmFix t1 ->
    let t1' = eval1 t1 in
    TmFix t1'

  (* E-Tuple *)
  |TmTuple t ->
    let eval_t = List.map eval1 t in
    TmTuple eval_t

  | TmProj (TmTuple t, pos) when List.length t > pos ->
      List.nth t pos
  
  | TmRecord fields ->
        let eval_field (label, term) =
          let term' = eval1 term in
          (label, term')
        in
        let eval_fields = List.map eval_field fields in
        TmRecord eval_fields
  | TmProjR (TmRecord fields, label) ->
    (try List.assoc label fields with
      | Not_found -> raise NoRuleApplies)
  | TmProjR (t, label) when isval t ->
      let t' = eval1 t in
        TmProjR (t', label)

    (* E-Cons2 *)
    | TmCons(ty,h,t) when isval h -> 
      TmCons(ty,h,(eval1 t)) 
  
  (* E-Cons1 *)
  | TmCons(ty,h,t) -> 
      TmCons(ty,(eval1 h),t)
  
  (* E-IsNilNil *)
  | TmIsNil(ty,TmNil(_)) -> 
      TmTrue  
  
  (* E-IsNilCons *)
  | TmIsNil(ty,TmCons(_,_,_)) -> 
      TmFalse
  
  (* E-IsNil *)
  | TmIsNil(ty,t) -> 
      TmIsNil(ty,eval1 t)
  
  (* E-HeadCons *)
  | TmHead(ty,TmCons(_,h,_)) -> 
      h
  
  (* E-Head *)
  | TmHead(ty,t) -> 
      TmHead(ty,eval1 t)
  
  (* E-TailCons *)
  | TmTail(ty,TmCons(_,_,t)) -> 
      t
  
  (* E-Tail *)
  | TmTail(ty,t) -> 
      TmTail(ty,eval1 t)
  
  | _ ->
      raise NoRuleApplies
;;

let rec eval tm =
  try
    let tm' = eval1 tm in
    eval tm'
  with
    NoRuleApplies -> tm
;;

