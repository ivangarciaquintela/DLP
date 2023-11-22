
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open List;;
open Str;;

let ends_with_semicolon str =
  let len = String.length str in
  if len < 2 then
    false
  else
    let last_2_char = String.sub str (len - 2) 2 in
    last_2_char = ";;"
;;



let rec process_line line = 
  if ends_with_semicolon line then s token (from_string line)
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
      print_endline ("- : " ^ string_of_ty (typeof emptyctx term) ^ " = " ^string_of_term (eval term) );
      execute tail
  | Bind (name, term)::tail ->
    print_endline (name ^ " : " ^ string_of_ty (typeof emptyctx term));
    execute tail
        
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
        execute (process_line line);
        loop ctx
    with
      Lexical_error ->
          print_endline "lexical error";
          loop ctx
      | Parse_error ->
          print_endline "syntax error";
          loop ctx
      | Type_error e ->
          print_endline ("type error: " ^ e);
          loop ctx
      | End_of_file ->
          print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

