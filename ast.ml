type lettre=Lettre of char;;

type lettre_ou_vide=Nlettre of char|Epsilon;; (* no usefule in transition but if not correct transform lettre to it *)

type suit_lettre_nonvide=lettre list;; (* useful in declaration to make writing simp*)

type input_symbols=Input_symbols of (suit_lettre_nonvide);;

type stack_symbols=Stack_symbols of (suit_lettre_nonvide);;

type states=States of (suit_lettre_nonvide);;

type initial_state=Initial_state of (lettre);;

type initial_stack=Initial_stack of (lettre) ;;

type declaration = Declaration of(input_symbols*stack_symbols*states*initial_state*initial_stack);;

type stack=Stack of (lettre list) |Empty_stack

type transition=Transition of (lettre*lettre_ou_vide*lettre*lettre*stack) ;;

type automate = declaration * transition list ;;


exception Empty_transitions ;;
exception Syntax_err of string ;;
exception Word_not_accepted of string;;
exception Word_accepted;;
exception Check_automate_err of string
exception Check_automate_accepted of string

let lettreToString =function
  |Lettre(a)-> String.make 1 a
;;

let lettre_ou_videToString = function
  |Nlettre(a)->String.make 1 a
  |Epsilon->"ε"
;;

let rec suit_lettre_nonvideToString=function
  |[]->""
  |h::t->(lettreToString h)^", "^(suit_lettre_nonvideToString t)
;;

let input_symbolsToString=function
  |Input_symbols(lnv)->"Input Symbols : "^(suit_lettre_nonvideToString lnv)^"\n"
;;
let stack_symbolsToString=function
  |Stack_symbols(lnv)->"Stack Symbols : "^(suit_lettre_nonvideToString lnv)^"\n"
;;

let statesToString=function
  |States(lnv)->"States : "^(suit_lettre_nonvideToString lnv)^"\n"
;;

let initial_stateToString=function
  |Initial_state(ltr)->"Initial state : "^(lettreToString ltr)^"\n"
;;

let initial_stackToString=function
  |Initial_stack(ltr)->"Initial stack : "^(lettreToString ltr)^"\n"
;;

let declarationToString=function
  |Declaration(iss,sss,sts,is,ist)->(input_symbolsToString iss)^(stack_symbolsToString sss)^(statesToString sts)^(initial_stateToString is)^(initial_stackToString ist)
;;

let rec non_empty_stack=function
  |[]->""
  |[x]->(lettreToString x)
  |h::t->(lettreToString h) ^";" ^(non_empty_stack t)
;;
let stackToString=function
  |Stack(l)->non_empty_stack l
  |Empty_stack->"ε"
;;

let transitionToString=function
  |Transition(l1,lov,l2,l3,s)->"("^(lettreToString l1)^","^(lettre_ou_videToString lov)^","^(lettreToString l2)^","^(lettreToString l3)^","^(stackToString s)^")\n"
;;

let rec automateToString automate =
  let declaration=fst(automate) in
  let transitions=snd(automate) in
  let rec list_transitionToString=function
    |[]->""
    |h::t->(transitionToString h) ^ (list_transitionToString t) in
  (declarationToString declaration) ^ (list_transitionToString transitions)

;;

(***************************************************************)



(*etape 1*)
let compare_ltr (lt1:lettre) (lt2:lettre)=
  match lt1, lt2 with
    |Lettre(a) ,Lettre(b)->if (a==b) then true else false  
;;

let compare_Nltr (lt1:lettre_ou_vide) (lt2:lettre_ou_vide)=
  match lt1, lt2 with
    |Epsilon,Epsilon->true
    |Epsilon,Nlettre(a)->false
    |Nlettre(a), Epsilon->false
    |Nlettre(a),Nlettre(b)->if(a==b) then true else false
;;
let rec edit_stack_aux stack trans_stk=
  Stack.pop stack;
  let rec loop stack trans_stk=
    match trans_stk with  
      |[]->()
      |h::t->Stack.push h stack; loop stack t
  in
  loop stack trans_stk    
;;

let edit_stack stack st=
  if Stack.is_empty stack then raise (Word_not_accepted "stack empty and word isn't all consumed")
  else
    match st with 
      |Empty_stack->Stack.pop stack ;()
      |Stack(l)->edit_stack_aux stack l
;;



let rec chercher_transition ltran (cur_entree:lettre_ou_vide) (cur_st:lettre) (pile_sommet:lettre) =
  (*print_string (lettre_ou_videToString cur_entree);
  print_string (lettreToString cur_st);
  print_string (lettreToString pile_sommet);
  print_string "\n";*)
  
  match ltran with
    |[]-> None
    |h::t->(match h with 
            |Transition(st1,ent,old_pile,st2,new_pile)->if (((compare_Nltr ent cur_entree)==true &&(compare_ltr old_pile pile_sommet)==true &&(compare_ltr cur_st st1)==true))then (Some h ) else (chercher_transition t cur_entree cur_st pile_sommet))
;;

let rec chercher_transition_epsilon ltran  (cur_st:lettre) (pile_sommet:lettre)=
  match ltran with
    |[]-> None
    |h::t->(match h with 
              |Transition(st1,ent,old_pile,st2,new_pile)->if ((compare_Nltr ent Epsilon)==true && (compare_ltr old_pile pile_sommet)&&(compare_ltr cur_st st1))then (Some h) else chercher_transition_epsilon t cur_st pile_sommet)
;;


