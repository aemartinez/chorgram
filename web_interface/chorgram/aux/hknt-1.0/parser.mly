%token <int> INT
%token <string> ID
%token EOF OPS AUTOMATON STATES TRANSITIONS FINAL_STATES LPAR RPAR TO COLON

%type <(string*int) list*string*string list*string list*(string*string option*string) list> nfa

%start nfa

%%

nfa: 
| OPS alphabet 
  AUTOMATON ID 
  STATES states 
  FINAL_STATES states 
  TRANSITIONS transitions
  { ($2,$4,$6,$8,$10) }

alphabet:
| { [] }
| ID COLON INT alphabet { ($1,$3)::$4 }

states:
| { [] }
| ID states { $1::$2 }

transitions:
| { [] }
| transition transitions { $1::$2 }

transition:
| ID LPAR RPAR TO ID { ($1,None,$5) }
| ID LPAR ID RPAR TO ID { ($1,Some $3,$6) }
