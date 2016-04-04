{

  open Parser
  open Lexing

  exception EOF

  let incrline lexbuf =
      lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum }

}

(***********************************************************************)

let blank  = ' ' | '\t' | '\r'
let number = ['0'-'9'] +
let name   = ['A' - 'Z' 'a'-'z' '_' '/' '0'-'9' '\'' '?'] +

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
| "&"		     { REF }
| "!"		     { BANG }
| "|"		     { BAR }
| "\\"		     { BACK }

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
| ">="		     { GEQ }
| ">"		     { GR }
| "¬"		     { NOT }
| "&&"		     { AND }
| "||"		     { LOR }
| "%&"               { BITAND }
| "%|"               { BITOR }
| "%+"               { BITXOR }
| "%-"               { BITNOT }

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
| "makearray"	     { MKARRAY }
| "goto"             { GOTO }
  
| "true"	     { TRUE }
| "false"	     { FALSE }
| "inf"	             { INFINTE }

| number as n        { NUM (int_of_string n) }
| name as n          { ID n }

| eof                { raise EOF }

(***********************************************************************)
