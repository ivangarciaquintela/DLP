# PRACTICA DLP
## Mateo Amado Ares, Iván García Quintela

1. Improvements on the declaration of lambda functions:

    1. Recognition of multi-line exppressions:
        - We modified the following files:
            * main.ml
                ```
                let ends_with_semicolon str =
                    let len = String.length str in
                    if len < 2 then
                        false
                    else
                        let last_2_char = String.sub str (len - 2) 2 in
                        last_2_char = ";;";;

                let rec process_line line = 
                    if ends_with_semicolon line 
                        then s token (from_string line)
                else
                    begin
                    print_string "  ";
                    flush stdout;
                    process_line(line ^ " " ^ read_line())
                    end
                ;;

                let rec execute = function
                | [] ->
                    ()
                | Eval (term)::tail ->
                    print_endline (string_of_term (eval term) ^ " : " ^ string_of_ty (typeof emptyctx term));
                    execute tail
                ;;
                ```
                We then call process line from the top_level_loop() function.

            * lexer.ml, lambda.mli, parser.mli, parser.mly
                
                Added support for the SEMICOLON token.
                ```
                | ";;" { SEMICOLON }
                
                %token SEMICOLON
                ```
            * Examples
            
                We can now run an expression on many lines:

                    true
                    ;;

                    if false 
                    then true 
                    else false;;

    2. "pretty-printer" :
        - We modified the following files:
            * lambda.ml
            ![Alt text](image.png)
                Se eliminaron los paréntesis redundantes


            * main.ml
                
2. Extensions of the lambda-calculus language:

    1. Internal fixed point combiner

    2. Global definition context
        * Using a Hashtbl module, with functional behaviour.
        * main.ml 

                    | Bind (name, term)::tail -> 
                        print_endline (name ^ " : " ^ string_of_ty (typeof emptyctx term));
                        execute tail
        * parser.mly

                open Hashtbl;;

                let table = create 1024;;

                #bajo command term:
                    | IDV EQ term { add table $1 $3; Bind ($1, $3) }
                #en el atomic term
                    //{ TmVar $1 }
                    { try find table $1 with Not_found -> TmVar ($1) }     
        * lambda.ml y lambda.mly añadir al command:

                | Bind of string * term      



    3. String type and concat implementation


        lambda.mli and lambda.ml
            
            type ty
                | TyString
            
            type term
                | TmString of string
                | TmConcat of term * term

        lexer.mll
                
                | "concat"    { CONCAT }
                | "String"    { STRING }

                  | '"'[^ '"' ';' '\n']*'"' {let s = Lexing.lexeme lexbuf in STRINGV (String.sub s l (String.length s - 2)) }

        parser.mly
                
                añadir %token CONCAT, %token STRING, %token <string> STRINGV

        lambda.ml
              
              | TmConcat (TmString s1, TmString s2) ->
                    TmString (s1 ^ s2)

              | TmConcat (TmString s1, TmString t2) ->
                    let t2' = eval1 t2 in
                    TmConcat (TmString s1, t2')
  
              | TmConcat (TmString t1, TmString s2) ->
                    let t1' = eval1 t1 in
                    TmConcat (TmString t1, s2')
      
        

