open Global

type aexpr =
     Const of int
   | Var of string
   | Minus of aexpr
   | Sum  of aexpr * aexpr
   | Diff of aexpr * aexpr
   | Prod of aexpr * aexpr
   | Div of aexpr * aexpr
   | Mod of aexpr * aexpr
   | BXor of aexpr * aexpr
   | BAnd of aexpr * aexpr
   | BOr of aexpr * aexpr
;;

(***********************************************************************)

type bexpr =
     True
   | False
   | Not of bexpr
   | And of bexpr * bexpr
   | Or  of bexpr * bexpr
   | Lesser  of aexpr * aexpr
   | LeEqual of aexpr * aexpr
   | Equal   of aexpr * aexpr
   | GrEqual of aexpr * aexpr
   | Greater of aexpr * aexpr
;;

(***********************************************************************)
(** Generic Output                                                     *)
(***********************************************************************)

let rec output_aexpr outch e =
  match e with 
    Const(v) ->   output_int outch v
  | Var(v) ->     output_string outch v
  | Minus(e) ->   output_string outch "(-";
                  output_aexpr outch e;
                  output_string outch ")"
  | Sum(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "+"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | Diff(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "-"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | Prod(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "*"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | Div(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "/"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | Mod(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "%"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | BXor(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "$"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | BAnd(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "&"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
  | BOr(e1,e2) -> output_string outch "("; 
                  output_aexpr outch e1; 
                  output_string outch "|"; 
                  output_aexpr outch e2; 
                  output_string outch ")"
;;

(***********************************************************************)
	
let rec output_bexpr outch e =
  match e with 
    True ->       output_string outch "true"
  | False ->      output_string outch "false"
  | Not(e) ->     output_string outch "~";
                  output_bexpr outch e
  | And(e1,e2) -> output_string outch "(";
                  output_bexpr outch e1;
                  output_string outch "&";
                  output_bexpr outch e2;
                  output_string outch ")"
  | Or(e1,e2) ->  output_string outch "(";
                  output_bexpr outch e1;
                  output_string outch "|";
                  output_bexpr outch e2;
                  output_string outch ")"
  | Lesser(e1,e2) ->  output_string outch "(";
                  output_aexpr outch e1;
                  output_string outch "<";
                  output_aexpr outch e2;
                  output_string outch ")"
  | LeEqual(e1,e2) ->  output_string outch "(";
                  output_aexpr outch e1;
                  output_string outch "<=";
                  output_aexpr outch e2;
                  output_string outch ")"
  | Equal(e1,e2) ->  output_string outch "(";
                  output_aexpr outch e1;
                  output_string outch "==";
                  output_aexpr outch e2;
                  output_string outch ")"
  | GrEqual(e1,e2) ->  output_string outch "(";
                  output_aexpr outch e1;
                  output_string outch ">=";
                  output_aexpr outch e2;
                  output_string outch ")"
  | Greater(e1,e2) ->  output_string outch "(";
                  output_aexpr outch e1;
                  output_string outch ">";
                  output_aexpr outch e2;
                  output_string outch ")"
;;

(***********************************************************************)
(** Text Output                                                        *)
(***********************************************************************)

let print_aexpr e = output_aexpr stdout e
;;
	
let print_bexpr e = output_bexpr stdout e
;;

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

let rec julia_aexpr aexpr =
  match aexpr with 
  | Var(v) ->
      julia_string "values[id2ord[\"";
      julia_string v;
      julia_string "\"]]"
  | Div(e1, e2) ->
      julia_string "div(";
      julia_aexpr e1;
      julia_string ", ";
      julia_aexpr e2;
      julia_string ")"
  | _ -> 
      output_aexpr !fidJulia aexpr
;;

let rec julia_aexpr e =
  match e with 
    Const(v) ->   julia_int v
  | Var(v) ->     julia_string "id2rng[\"";
                  julia_string v;
                  julia_string "\"][";
                  julia_string "values[id2ord[\"";
                  julia_string v;
                  julia_string "\"]]]"
  | Minus(e) ->   julia_string "(-";
                  julia_aexpr e;
                  julia_string ")"
  | Sum(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "+"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | Diff(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "-"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | Prod(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "*"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | Div(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "/"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | Mod(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "%"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | BXor(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "$"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | BAnd(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "&"; 
                  julia_aexpr e2; 
                  julia_string ")"
  | BOr(e1,e2) -> julia_string "("; 
                  julia_aexpr e1; 
                  julia_string "|"; 
                  julia_aexpr e2; 
                  julia_string ")"
;;

let rec julia_bexpr e =
  match e with 
    True ->       julia_string "true"
  | False ->      julia_string "false"
  | Not(e) ->     julia_string "~";
                  julia_bexpr e
  | And(e1,e2) -> julia_string "(";
                  julia_bexpr e1;
                  julia_string "&";
                  julia_bexpr e2;
                  julia_string ")"
  | Or(e1,e2) ->  julia_string "(";
                  julia_bexpr e1;
                  julia_string "|";
                  julia_bexpr e2;
                  julia_string ")"
  | Lesser(e1,e2) ->  julia_string "(";
                  julia_aexpr e1;
                  julia_string "<";
                  julia_aexpr e2;
                  julia_string ")"
  | LeEqual(e1,e2) ->  julia_string "(";
                  julia_aexpr e1;
                  julia_string "<=";
                  julia_aexpr e2;
                  julia_string ")"
  | Equal(e1,e2) ->  julia_string "(";
                  julia_aexpr e1;
                  julia_string "==";
                  julia_aexpr e2;
                  julia_string ")"
  | GrEqual(e1,e2) ->  julia_string "(";
                  julia_aexpr e1;
                  julia_string ">=";
                  julia_aexpr e2;
                  julia_string ")"
  | Greater(e1,e2) ->  julia_string "(";
                  julia_aexpr e1;
                  julia_string ">";
                  julia_aexpr e2;
                  julia_string ")"
;;
