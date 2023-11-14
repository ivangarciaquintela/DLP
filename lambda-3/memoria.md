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

2. Extensions of the lambda-calculus language:

    1. Internal fixed point combiner

    2. Global definition context
        * Using a Hashtbl module
        * en el main.ml 

                    | Bind (name, term)::tail -> 
                        print_endline (name ^ " : " ^ string_of_ty (typeof emptyctx term));
                        execute tail

    3. String type
