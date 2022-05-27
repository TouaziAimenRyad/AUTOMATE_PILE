{
open Lexing
open Parser
 

}
let space=[ ' ' '\t' '\n' ]
let lettre=['0'-'9''a'-'z''A'-'Z']

rule next_token=parse
|"input symbols"{ INPUT_SYMBOLS}
|"stack symbols" {STACK_SYMBOLS}
|"states" {STATES}
|"initial state" {INITIAL_STATE}
|"initial stack symbol" {INITIAL_STACK}
|"transitions" {TRANSITIONS}
|"(" {PARAG}
|")" {PARAD}
|"," {VIR}
|";"{PVIR}
|":"{DP}
|lettre lettre+ {failwith "unexpected token"}
|lettre as l {LETTRE l } 
|space {next_token lexbuf}
|eof {EOF}
|_ {failwith "unexpected token"}
