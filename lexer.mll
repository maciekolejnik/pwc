{

  open Parser
  open Lexing

  exception EOF
  exception UnexpToken 

  let incrline lexbuf =
      lexbuf.lex_curr_p <- 
        { lexbuf.lex_curr_p with 
            pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum;
            pos_bol  = lexbuf.lex_curr_p.pos_cnum;
        }

  let rational_of_string s =
    let r = (Str.split (Str.regexp "//") s) in
    let p = int_of_string (String.trim (List.nth r 0))
    and q = int_of_string (String.trim (List.nth r 1))
    in  (p,q)
}

(***********************************************************************)

let blank = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let nonzero = ['1'-'9']['0'-'9']*
let number = '0' | nonzero  
let rational = number blank* "//" blank* nonzero

(***********************************************************************)

rule token = parse

  blank              { token lexbuf }

| '#' [^'\n'] * '\n' { incrline lexbuf; token lexbuf }
| '\n'               { incrline lexbuf; token lexbuf }

| "("		     { LPAR }
| ")"		     { RPAR }
| "["		     { LSQ }
| "]"		     { RSQ }
| "{"		     { LBR }
| "}"		     { RBR }

| ":"		     { COLON }
| ";"		     { SEMICOL }
| "."		     { DOT }
| ".."		     { DOTDOT }
| ","		     { COMMA }

| "int"	             { INT }
| "bool"	     { BOOL }
| "para"	     { PARA }

| "+"		     { PLUS }
| "-"		     { MINUS }
| "*"		     { TIMES }
| "/"		     { DIV }
| "%"		     { MOD }
| "<"		     { LE }
| "<="		     { LEQ }
| "=="		     { EQ }
| "!="		     { NEQ }
| ">="		     { GEQ }
| ">"		     { GR }
| "!"		     { NOT }
| "&&"		     { AND }
| "||"		     { LOR }
| "&"                { BITAND }
| "|"                { BITOR }
| "^"                { BITXOR }
| "~"                { BITNOT }

| ":="		     { ASSIGN }
| "?="		     { RANDOM }
 
| "begin"            { BEGIN }
| "end"              { END }
| "var"		     { VAR }
| "const"	     { CONST }
| "skip"	     { SKIP }
| "skipIf"	     { SKIPIF }
| "skipAsn"	     { SKIPASN }
| "stop"	     { STOP }
| "if"		     { IF }
| "then"	     { THEN }
| "else"	     { ELSE }
| "fi"		     { FI }
| "while"	     { WHILE }
| "do"		     { DO }
| "od"		     { OD }
| "for"              { FOR }
| "in"               { IN } 
| "rof"              { ROF }
| "repeat"           { REPEAT }
| "until"            { UNTIL }
| "choose"	     { CHOOSE }
| "or"		     { OR }
| "ro"		     { RO }
| "case"	     { CASE }
| "of"	             { OF }
| "default"          { DEFAULT }
| "esac"	     { ESAC }
| "output"	     { OUTPUT }
| "goto"             { GOTO }
  
| "true"	     { TRUE }
| "false"	     { FALSE }
| "inf"	             { INFINTE }

| number as n        { NUM (int_of_string n) }
| rational as r      { RAT (rational_of_string r) }
| id as id           { ID id }

| _                  { raise UnexpToken }
| eof                { raise EOF }

(***********************************************************************)
