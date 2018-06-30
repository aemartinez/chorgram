{ open Parser }

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = num | alpha
let id = alpha alphanum*
let blank = [ ' ' '\r' '\t' '\n' ]

rule token = parse
  | blank           { token lexbuf }
  | "Ops "          { OPS }
  | "Automaton "    { AUTOMATON }
  | "States "       { STATES }
  | "Transitions"   { TRANSITIONS }
  | "Final States " { FINAL_STATES }
  | '('             { LPAR }
  | ')'             { RPAR }
  | ':'             { COLON }
  | "->"            { TO }
  | id as s         { ID s }
  | num+ as n       { INT(int_of_string n) }
  | eof             { EOF }
  | _ as c          { Printf.kprintf failwith "lexing error near `%c'" c }
