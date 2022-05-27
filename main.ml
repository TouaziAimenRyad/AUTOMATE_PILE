open Parser
open Lexer
open Lexing
open Ast

let lexbuf = Lexing.from_channel stdin 

let ast = Parser.s Lexer.next_token lexbuf 

let usage()=
  print_string "--test_mot to check if a word is accepted by the automate \n";
  print_string "--verifier_automate to check if the automate is correct  \n";
;;
let main () =
  
  match Sys.argv with 
    |[|_;"--test_mot";word|]-> (try Ast.check_mot ast word with |Ast.Word_accepted->print_string "the word is accepted by the automate\n" |Ast.Word_not_accepted s ->print_string ("the word is not accepted by the automate because :"^s^"\n"))
    |[|_;"--verifier_automate"|]-> Ast.verfier_automate ast
    |_-> usage();
;;
(* lancement de ce main *)

let _ = Printf.printf "Parse:\n%s\n" (Ast.automateToString ast); main()