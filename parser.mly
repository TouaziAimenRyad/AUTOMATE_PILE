%{ open Ast %}
%token INPUT_SYMBOLS STACK_SYMBOLS STATES INITIAL_STATE INITIAL_STACK TRANSITIONS
%token PARAG PARAD VIR PVIR DP
%token <char> LETTRE
%token EOF
%start <Ast.automate> s
%%

s: a=automate EOF {a}

automate: 
    |ds=declarations ts=transitions {(ds,ts)}

declarations : 
    |is=inputsymbols ss=stacksymbols st=states init_st=initialstate init_s=initialstack {Declaration(is,ss,st,init_st,init_s)}

inputsymbols: 
    |INPUT_SYMBOLS DP snv=suitelettres_nonvide { if snv==[] then failwith "empty input symbols list" else Input_symbols(snv) }

stacksymbols: 
    |STACK_SYMBOLS DP snv=suitelettres_nonvide {if snv==[] then failwith "empty stack symbols list" else Stack_symbols(snv) }

states:
    |STATES DP snv=suitelettres_nonvide  {if snv==[] then failwith "empty states symbols list" else States(snv) }

initialstate:
    |INITIAL_STATE DP l=lettre  {if l==' ' then failwith "initial state empty" else Initial_state(Lettre(l))}

initialstack:
    |INITIAL_STACK DP l=lettre  {if l==' ' then failwith "initial statck empty" else Initial_stack(Lettre(l))}

suitelettres_nonvide: 
    |{[]}
    |l=LETTRE  {[Lettre(l)]}
    |l=LETTRE VIR snv=suitelettres_nonvide {Lettre(l)::snv}

transitions:
    |TRANSITIONS DP trl=translist {trl}

translist: 
    |{[]}
    |tr=transition trl=translist {tr::trl}

transition:
    |PARAG l1=lettre VIR lv=lettre_ou_vide VIR l2=lettre VIR l3=lettre VIR s=stack PARAD {if (l1==' ' || l2==' '||l3=' ') then failwith "some paramaters for a transition are missing" else Transition(Lettre(l1),lv,Lettre(l2),Lettre(l3),s)}

lettre_ou_vide: 
    |{Epsilon}
    |l=LETTRE {Nlettre(l)}

lettre:
    |{' '}
    |l=LETTRE {l}
stack: 
    |{Empty_stack}
    |nes=nonempty_stack {Stack(nes)}

nonempty_stack: 
    |l=LETTRE {[Lettre(l)]}
    |l=LETTRE PVIR nes=nonempty_stack {Lettre(l)::nes}