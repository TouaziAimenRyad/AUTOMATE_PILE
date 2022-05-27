open Parser
open Lexer
open Lexing
open Ast

let lexbuf = Lexing.from_channel stdin 

let ast = Parser.s Lexer.next_token lexbuf 

let main () =
  print_string "--test_mot to check if a word is accepted by the automate \n";
  print_string "--verifier_automate to check if the automate is correct  \n";

  match Sys.argv with 
    |[|_;"--test_mot";word|]-> Ast.check_mot ast word
    |[|_;"--verifier_automate"|]-> Ast.verfier_automate ast
    |_-> print_string " redo ";
;;
(* lancement de ce main *)

let _ = Printf.printf "Parse:\n%s\n" (Ast.automateToString ast); main()
