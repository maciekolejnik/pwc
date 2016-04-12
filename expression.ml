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

(**
 *     asp
 *
 * Special printer data structure for aexpr 
 *)
type asp =
  {  const_sp : (int -> string) option;
     var_sp   : (string -> string) option;
     minus_sp : (aexpr -> string) option;
     sum_sp   : (aexpr * aexpr -> string) option;
     diff_sp  : (aexpr * aexpr -> string) option;
     prod_sp  : (aexpr * aexpr -> string) option;
     div_sp   : (aexpr * aexpr -> string) option;
     mod_sp   : (aexpr * aexpr -> string) option;
     bxor_sp  : (aexpr * aexpr -> string) option;
     band_sp  : (aexpr * aexpr -> string) option;
     bor_sp   : (aexpr * aexpr -> string) option;
  }
;;

let default_asp =
  {  const_sp = None;  
     var_sp   = None;  
     minus_sp = None;  
     sum_sp   = None;  
     diff_sp  = None; 
     prod_sp  = None; 
     div_sp   = None;  
     mod_sp   = None;  
     bxor_sp  = None;  
     band_sp  = None;  
     bor_sp   = None;  
  }
;;

(**
 *     aexpr_to_string ?aspo aexpr
 *
 * Return the string representation of `aexpr`, using 
 * the aexpr special printer `aspo` if present
 *)
let rec aexpr_to_string ?aspo aexpr = 
  let asp = some_or_default aspo default_asp in
  let infix_operator sp op e1 e2 = 
    let e1s = aexpr_to_string ~aspo:asp e1 
    and e2s = aexpr_to_string ~aspo:asp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") in
  match aexpr with 
  | Const(c) -> 
      to_string asp.const_sp c (string_of_int c)
  | Var(v) ->  
      to_string asp.var_sp v v
  | Minus(e) ->   
      let es = aexpr_to_string ~aspo:asp e
      in  to_string asp.minus_sp e ("(- " ^ es ^ ")")
  | Sum(e1,e2) -> 
      infix_operator asp.sum_sp "+" e1 e2
  | Diff(e1,e2) -> 
      infix_operator asp.diff_sp "-" e1 e2
  | Prod(e1,e2) -> 
      infix_operator asp.prod_sp "*" e1 e2
  | Div(e1,e2) -> 
      infix_operator asp.div_sp "/" e1 e2
  | Mod(e1,e2) ->
      infix_operator asp.mod_sp "%" e1 e2
  | BXor(e1,e2) -> 
      infix_operator asp.bxor_sp "$" e1 e2
  | BAnd(e1,e2) ->
      infix_operator asp.band_sp "&" e1 e2
  | BOr(e1,e2) -> 
      infix_operator asp.bor_sp "|" e1 e2
;;

(***********************************************************************)
	
(**
 *     bsp
 *
 * Special printer data structure for bexpr 
 *)
type bsp =
  {  true_sp    : (unit -> string) option;
     false_sp   : (unit -> string) option;
     not_sp     : (bexpr -> string) option;
     and_sp     : (bexpr * bexpr -> string) option;
     or_sp      : (bexpr * bexpr -> string) option;
     lesser_sp  : (aexpr * aexpr -> string) option;
     lequal_sp  : (aexpr * aexpr -> string) option;
     equal_sp   : (aexpr * aexpr -> string) option;
     grequal_sp : (aexpr * aexpr -> string) option;
     greater_sp : (aexpr * aexpr -> string) option;
  }
;;

let default_bsp =
  {  true_sp    = None;  
     false_sp   = None;  
     not_sp     = None;  
     and_sp     = None;  
     or_sp      = None; 
     lesser_sp  = None; 
     lequal_sp  = None;  
     equal_sp   = None;  
     grequal_sp = None;  
     greater_sp = None;  
  }
;;

(**
 *     bexpr_to_string ?aspo ?bspo bexpr
 *
 * Return the string representation of `bexpr`, using 
 * the aexpr special printer `aspo` and bexpr special
 * printer `bspo` if present
 *)
let rec bexpr_to_string ?aspo ?bspo bexpr = 
  let asp = some_or_default aspo default_asp 
  and bsp = some_or_default bspo default_bsp in
  let aexpr_infix_operator sp op e1 e2 = 
    let e1s = aexpr_to_string ~aspo:asp e1 
    and e2s = aexpr_to_string ~aspo:asp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") 
  and bexpr_infix_operator sp op e1 e2 = 
    let e1s = bexpr_to_string ~aspo:asp ~bspo:bsp e1 
    and e2s = bexpr_to_string ~aspo:asp ~bspo:bsp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") in
  match bexpr with 
  | True -> 
      to_string bsp.true_sp () "true"
  | False ->  
      to_string bsp.false_sp () "false"
  | Not(e) ->   
      let es = bexpr_to_string ~aspo:asp ~bspo:bsp e
      in  to_string bsp.not_sp e ("(- " ^ es ^ ")")
  | And(e1,e2) -> 
      bexpr_infix_operator bsp.and_sp "&&" e1 e2
  | Or(e1,e2) -> 
      bexpr_infix_operator bsp.or_sp "||" e1 e2
  | Lesser(e1,e2) -> 
      aexpr_infix_operator bsp.lesser_sp "<" e1 e2
  | LeEqual(e1,e2) -> 
      aexpr_infix_operator bsp.lequal_sp "=<" e1 e2
  | Equal(e1,e2) ->
      aexpr_infix_operator bsp.equal_sp "==" e1 e2
  | GrEqual(e1,e2) -> 
      aexpr_infix_operator bsp.grequal_sp ">=" e1 e2
  | Greater(e1,e2) ->
      aexpr_infix_operator bsp.greater_sp ">" e1 e2
;;

(***********************************************************************)

(** Default output *)
let output_aexpr outch e = output_string outch (aexpr_to_string e)
;;

let output_bexpr outch e = output_string outch (bexpr_to_string e)
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


(**
 *     asp (aexpr special printer)
 *
 * Overwrites the default printing with julia specific ways of printing
 * variables and division
 *)
let rec asp = 
  { default_asp with 
    var_sp = (Some var); 
    div_sp = (Some div);
  }
and var v = "id2rng[\"" ^ v ^ "\"][" ^ "values[id2ord[\"" ^ v ^ "\"]]]" 
and div (e1,e2) = 
  let e1 = aexpr_to_string ~aspo:asp e1
  and e2 = aexpr_to_string ~aspo:asp e2 
  in "div(" ^ e1 ^ ", " ^ e2 ^ ")" 
;;

let aexpr_to_julia_string e =
  aexpr_to_string ~aspo:asp e
;;

let bexpr_to_julia_string e =
  bexpr_to_string ~aspo:asp e
;;

(** 
 *     julia_aexpr e
 *
 *  Writes arithmetic expression `e` to the julia file.
 *
 *  Uses special printer `asp` defiend above to overwrite the 
 *  default way of printing (in this case) variables and 
 *  division operation
 *)
let julia_aexpr e = 
  julia_string (aexpr_to_string ~aspo:asp e)
;;

(** 
 *     julia_bexpr e
 *
 *  Writes boolean expression `e` to the julia file.
 *
 *  Uses special printer `asp` defiend above to overwrite the 
 *  default way of printing (in this case) variables and 
 *  division operation
 *)
let julia_bexpr e =
  julia_string (bexpr_to_string ~aspo:asp e)
;;

