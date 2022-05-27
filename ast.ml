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


let rec check_mot (automate:automate) mot =

  let declaration=fst(automate) in
  let getInitState (dec:declaration) : lettre=
    match dec with
      |Declaration(a,b,c,Initial_state(d),e)->d
  in
  let init_state=getInitState declaration in 
  let getInitStack (dec:declaration) :lettre=
    match dec with
      |Declaration(a,b,c,d,Initial_stack(e))->e 
  in
  let init_stack=getInitStack declaration in 
  
  let transitions=snd(automate) in
  let stack=Stack.create () in 
  Stack.push init_stack stack ;
  let current_state=ref init_state in 

  let mot_len=String.length mot in 
  let i=ref 0 in
 
  while (!i<mot_len) do 
    let t=chercher_transition transitions (Nlettre(mot.[!i]))  !current_state (Stack.top stack)in 
    (match t with
      |None->(match (chercher_transition_epsilon transitions !current_state (Stack.top stack)) with
              |None->raise (Word_not_accepted "pas de transition")
              |Some Transition(st1,ent,old_pile,st2,new_pile)->edit_stack stack new_pile;current_state:=st2
              )
      |Some Transition(st1,ent,old_pile,st2,new_pile)->(edit_stack stack new_pile;current_state:=st2;i:=!i+1)
    );
    
    if (Stack.is_empty stack) then raise (Word_not_accepted "stack is empty but the word is not") 

  done;
  if (Stack.is_empty stack) then 
    raise (Word_accepted) 
  else
     (
     while ((Stack.is_empty stack)==false) do
        match (chercher_transition_epsilon transitions !current_state (Stack.top stack)) with 
          |None->raise (Word_not_accepted "stack not empty and word is empty and no epsilon transition to empty the stack ")
          |Some Transition(st1,ent,old_pile,st2,new_pile)->edit_stack stack new_pile;current_state:=st2
     done;
     if (Stack.is_empty stack) then raise (Word_accepted); )
;;

(***************************************************************)


(*etape 2*)

let check_init_state (init_st:initial_state) (st_symbs:states)=
  let rec loop a l=
    match l with  
      |[]->false
      |h::t->if ((compare_ltr a h)==true) then true else loop a t in
  match init_st , st_symbs with 
    |Initial_state(a), States(l)->loop a l 
;;

let check_init_stack (init_stk:initial_stack) (stk_symbs:stack_symbols)=
  let rec loop a l=
    match l with  
      |[]->false
      |h::t->if ((compare_ltr a h)==true) then true else loop a t in

  match init_stk,stk_symbs with 
    |Initial_stack(a),Stack_symbols(l)->loop a l
;;




let chercher_state_transition st trl=
  let rec loop st trl acc=
    match trl with 
      |[]->acc
      |h::t->(match h with 
              |Transition(st1,ent,old_pile,st2,new_pile)->if (compare_ltr st st1)==true then (loop st t (h::acc) ) else (loop st t acc )) in
  loop st trl []
;;

let freq_tran (entr:lettre_ou_vide) (pile:lettre) ltr=
  let rec loop entr pile ltr acc=
    match ltr with
      |[]->acc
      |(Transition(st1,ent,old_pile,st2,new_pile))::t->if (((compare_Nltr entr ent ==true) ||(compare_Nltr ent Epsilon == true))&&(compare_ltr old_pile pile == true)) then loop entr pile t (1+acc) else loop entr pile t (acc) in 
  loop entr pile ltr 0 

;;

let rec check_state_tran s trl =
  match trl with 
  |[]->true
  |(Transition(st1,ent,old_pile,st2,new_pile))::t->if (freq_tran ent old_pile trl)>1 then false else check_state_tran s t

;;


let check_determinist (automate:automate) =
  let transitions=snd(automate) in
  let declaration=fst(automate)in 

  let getStates a=
    match a with
      |Declaration(b,c,States(l),e,f)-> l in 
  let states=getStates declaration in
  let rec loop_st states=
    match states with 
      |[]->true
      |h::t->if((check_state_tran h (chercher_state_transition h transitions) == false)) then false else loop_st t in
  loop_st states 
  
;;

let lettre_exist (ltr:lettre) ls=
  List.exists (fun x->compare_ltr x ltr) ls
;;




let check_tansitions_states tr states=
  let lstates=match states with |States(a)->a in 

  let rec loop tr lstates=
    match tr with
    |[]->true
    |h::t->match h with 
            |Transition(st1,ent,old_pile,st2,new_pile)->if ((lettre_exist st1 lstates ==false ) || (lettre_exist st2 lstates ==false)) then false else loop t lstates
  in
  loop tr lstates 

;;


let check_tansitions_pile tr piles=
let lpile=match piles with |Stack_symbols(a)->a in 
  let rec loop tr lpile=
    match tr with
    |[]->true
    |h::t->match h with 
            |Transition(st1,ent,old_pile,st2,new_pile)->match new_pile with 
                                                          |Empty_stack->if (lettre_exist old_pile lpile ==false ) then false else loop t lpile 
                                                          |Stack(a)->if((lettre_exist old_pile lpile ==false )||((List.for_all (fun x-> lettre_exist x lpile) a)==false )) then false else loop t lpile  in
  loop tr lpile

;;

let rec verfier_automate (automate:automate)=
  let declaration=fst(automate) in 
  let transitions=snd(automate) in 
  (match declaration with 
    |Declaration(a,b,c,d,e)->if (check_init_stack e b )==false then print_string "Not correct: innitial stack is not included in stack symbols \n" else print_string "correct: innitial stack is included in stack symbols \n";
     if (check_init_state d c) == false then print_string "not correct: initial state is not inclided in state symbols\n" else  print_string "correct: initial state is inclided in state symbols\n";
      if (check_determinist automate == false) then print_string "not correct: automate is not determinist\n" else print_string "correct: automate is  determinist\n";
      if (check_tansitions_pile transitions b) ==false then print_string "Not correct: there is a transition containing a stack symbol that doesn't exixst\n" else print_string "correct: all stack symbols that appears in all transitions are declared in stack symbols \n" ;
      if (check_tansitions_states transitions c) ==false then print_string "Not correct: there is a transition containing a state symbol that doesn't exixst\n" else print_string "correct: all state symbols that appears in all transitions are declared in state symbols \n" ;)

;;