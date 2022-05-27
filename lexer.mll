{
open Lexing
open Parser
exception Error of string 

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}
let space=[' ''\t']
let lettre=['0'-'9''a'-'z''A'-'Z']
let symb_pile=['A'-'Z']
let symb_input=['a'-'z']
let state=['0'-'9']

rule next_token=parse
|"input symbols"{ INPUT_SYMBOLS}
|"stack symbols" {STACK_SYMBOLS}
|"states" {STATES}
|"initial state" {INITIAL_STATE}
|"initial stack" {INITIAL_STACK}
|"transitions" {TRANSITIONS}
|"(" {PARAG}
|")" {PARAD}
|"," {VIR}
|";"{PVIR}
|":"{DP}
|lettre as l {LETTRE l } 
|space* {next_token lexbuf}
|"\n" {next_line lexbuf ; next_token lexbuf}
|eof {EOF}
|_ {raise(Error (Lexing.lexeme lexbuf ))}
