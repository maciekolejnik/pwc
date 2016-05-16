open Global
open Declaration

type id = string
;;

type aexpr =
   | Num of int
   | Varref of varref
   | Minus of aexpr
   | Sum  of aexpr * aexpr
   | Diff of aexpr * aexpr
   | Prod of aexpr * aexpr
   | Div of aexpr * aexpr
   | Mod of aexpr * aexpr
   | BNot of aexpr
   | BXor of aexpr * aexpr
   | BAnd of aexpr * aexpr
   | BOr of aexpr * aexpr
and varref = 
   | Var of id
   | ArrElem of id * aexpr  
;;

(***********************************************************************)

type bexpr =
   | True
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
(** Auxiliary                                                          *)
(***********************************************************************)

let rec aexpr_vars aexpr = 
  match aexpr with
  | Num(n) -> []
  | Varref(v) -> [v]
  | Minus(e) -> aexpr_vars e
  | Sum(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Diff(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Prod(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Div(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Mod(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | BNot(e) -> aexpr_vars e
  | BXor(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | BAnd(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | BOr(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
;;

let rec bexpr_vars bexpr =
  match bexpr with
  | True | False -> []
  | Not(e) -> bexpr_vars e
  | And(e1,e2) -> bexpr_vars e1 @ bexpr_vars e2
  | Or(e1,e2) -> bexpr_vars e1 @ bexpr_vars e2
  | Lesser(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | LeEqual(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Equal(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | GrEqual(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Greater(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
;;

let id varref = 
  match varref with
  | Var(id) -> id
  | ArrElem(id,e) -> id
;;

(** Right now assumes that varref is a valid reference (ie id exists) *)
let ordinals varref =
  let m = Declaration.meta (id varref) in
  let size = Declaration.size m in
  match varref with
  | Var(id) -> 
      if size = 1 then [id2ord id] else []
  | ArrElem(id,e) -> 
      let elem_ord i = id2ord id ^ " + " ^ (string_of_int i)
      in  List.map elem_ord (interval 0 (size - 1))
;;

(***********************************************************************)
(** Error checking                                                     *)
(***********************************************************************)

(**
 *   check_assigned_varref varref
 *
 * Check that the varref 
 * (1) has been declared
 * (2) is not a constant (as it is not allowed to assign to a constant) 
 * (3) is used in the right context, i.e. variable that was declared as 
 * primitive is used as primitive, and similarly for array
 *)
let check_assigned_varref varref =
  let id = id varref in
  try 
    let m = Declaration.meta id in
    begin match varref with
    | Var(id) -> 
        Declaration.assign_primitive id m
    | ArrElem(id,e) -> 
        Declaration.assign_array id m
    end
  with Not_found ->
    failwith ("Variable " ^ id ^ " assigned to, but not declared")
;;

(**
 *   check_varref varref 
 *
 * Check that `varref` exists and is used in the right context - 
 * similar to above, but allows constants to be used 
 *)
let check_varref varref =
  let id = id varref in
  try 
    let m = Declaration.meta id in
    begin match varref with
    | Var(id) -> 
        Declaration.use_primitive id m
    | ArrElem(id,e) ->
        Declaration.use_array id m
    end
  with Not_found ->
    failwith ("Variable " ^ id ^ " used, but not declared")
;;

(**
 *     check_aexpr aexpr
 *
 * Use function above to check all variable references in an 
 * arithmetic expression
 *)
let rec check_aexpr aexpr =
  match aexpr with
  | Num(n) -> ()
  | Varref(v) -> check_varref v
  | Minus(e) -> check_aexpr e
  | Sum(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Diff(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Prod(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Div(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Mod(e1,e2) -> check_aexpr e1; check_aexpr e2
  | BNot(e) -> check_aexpr e
  | BXor(e1,e2) -> check_aexpr e1; check_aexpr e2
  | BAnd(e1,e2) -> check_aexpr e1; check_aexpr e2
  | BOr(e1,e2) -> check_aexpr e1; check_aexpr e2
;;

(**
 *     check_bexpr bexpr
 *
 * As above, but for binary expressions
 *)
let rec check_bexpr bexpr =
  match bexpr with
  | True | False -> ()
  | Not(e) -> check_bexpr e
  | And(e1,e2) -> check_bexpr e1; check_bexpr e2
  | Or(e1,e2) -> check_bexpr e1; check_bexpr e2
  | Lesser(e1,e2) -> check_aexpr e1; check_aexpr e2
  | LeEqual(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Equal(e1,e2) -> check_aexpr e1; check_aexpr e2
  | GrEqual(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Greater(e1,e2) -> check_aexpr e1; check_aexpr e2
;;


(***********************************************************************)
(** Generic Output                                                     *)
(***********************************************************************)

(**
      asp
 
  Special printer data structure for aexpr 
 *)
type asp =
  {  num_sp   : (int -> string) option;
     vr_sp    : vrsp;
     minus_sp : (aexpr -> string) option;
     sum_sp   : (aexpr * aexpr -> string) option;
     diff_sp  : (aexpr * aexpr -> string) option;
     prod_sp  : (aexpr * aexpr -> string) option;
     div_sp   : (aexpr * aexpr -> string) option;
     mod_sp   : (aexpr * aexpr -> string) option;
     bnot_sp  : (aexpr -> string) option;
     bxor_sp  : (aexpr * aexpr -> string) option;
     band_sp  : (aexpr * aexpr -> string) option;
     bor_sp   : (aexpr * aexpr -> string) option;
  }
(**
      vrsp
 
  Special printer data stricture for variable references
 *)
and vrsp =
  {  var_sp : (id -> string) option;
     arr_sp : (id * aexpr -> string) option;
  }
;;

let rec default_asp =
  {  num_sp   = None;  
     vr_sp    = default_vrsp;
     minus_sp = None;  
     sum_sp   = None;  
     diff_sp  = None; 
     prod_sp  = None; 
     div_sp   = None;  
     mod_sp   = None;  
     bnot_sp  = None;  
     bxor_sp  = None;  
     band_sp  = None;  
     bor_sp   = None;  
  }
and default_vrsp =
  {  var_sp = None;
     arr_sp = None;
  }
;;

(**
 *     aexpr_to_string ?asp aexpr
 *
 * Return the string representation of `aexpr`, using 
 * the aexpr special printer `asp` if present
 *)
let rec aexpr_to_string ?(asp=default_asp) aexpr = 
  (*let vrsp = some_or_default asp.vr_sp default_vrsp in*)
  let infix_operator sp op e1 e2 = 
    let e1s = aexpr_to_string ~asp:asp e1 
    and e2s = aexpr_to_string ~asp:asp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") in
  match aexpr with 
  | Num(c) -> 
      to_string asp.num_sp c (string_of_int c)
  | Varref(v) -> 
      varref_to_string ~vrsp:asp.vr_sp v
  | Minus(e) ->   
      let es = aexpr_to_string ~asp:asp e
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
  | BNot(e) -> 
      let es = aexpr_to_string ~asp:asp e
      in  to_string asp.bnot_sp e ("~ " ^ es)
  | BXor(e1,e2) -> 
      infix_operator asp.bxor_sp "$" e1 e2
  | BAnd(e1,e2) ->
      infix_operator asp.band_sp "&" e1 e2
  | BOr(e1,e2) -> 
      infix_operator asp.bor_sp "|" e1 e2
and
(**
 *     varref_to_string ?asp aexpr
 *
 * Return the string representation of `vr`, using 
 * the aexpr special printer `asp` and varref special printer `vrsp`,
 * if present
 *)
varref_to_string ?(vrsp=default_vrsp) ?(asp=default_asp) vr =
  match vr with
  | Var(v) -> 
      to_string vrsp.var_sp v v
  | ArrElem(a,e) -> 
      let s = a ^ "[" ^ aexpr_to_string ~asp:asp e ^ "]" (* TODO: change!! *)
      in  to_string vrsp.arr_sp (a,e) s
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
 *     bexpr_to_string ?asp ?bsp bexpr
 *
 * Return the string representation of `bexpr`, using 
 * the aexpr special printer `asp` and bexpr special
 * printer `bsp` if present
 *)
let rec bexpr_to_string ?(asp=default_asp) ?(bsp=default_bsp) bexpr = 
  let aexpr_infix_operator sp op e1 e2 = 
    let e1s = aexpr_to_string ~asp:asp e1 
    and e2s = aexpr_to_string ~asp:asp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") 
  and bexpr_infix_operator sp op e1 e2 = 
    let e1s = bexpr_to_string ~asp:asp ~bsp:bsp e1 
    and e2s = bexpr_to_string ~asp:asp ~bsp:bsp e2
    in to_string sp (e1,e2) ("(" ^ e1s ^ op ^ e2s ^ ")") in
  match bexpr with 
  | True -> 
      to_string bsp.true_sp () "true"
  | False ->  
      to_string bsp.false_sp () "false"
  | Not(e) ->   
      let es = bexpr_to_string ~asp:asp ~bsp:bsp e
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
let output_varref outch vr = output_string outch (varref_to_string vr)
;;

let output_aexpr outch e = output_string outch (aexpr_to_string e)
;;

let output_bexpr outch e = output_string outch (bexpr_to_string e)
;;


(***********************************************************************)
(** Text Output                                                        *)
(***********************************************************************)
let print_varref vr = output_varref stdout vr
;;

let print_aexpr e = output_aexpr stdout e 
;;
	
let print_bexpr e = output_bexpr stdout e
;;

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

(**
 *     julia_asp (aexpr special printer)
 *
 * Overwrites the default printing with julia specific ways of printing
 * variables and division
 *)
let rec julia_asp = 
  let varref = 
    {  var_sp = (Some var);
       arr_sp = (Some arr);
    }
  in
  { default_asp with 
    vr_sp  = varref; 
    div_sp = (Some div);
  }
and var v = 
  match meta v with
  | Constant(i) -> string_of_int i
  | Primitive(_) -> id2rng v ^ in_sq_brackets (values (id2ord v)) 
  | _ -> failwith "Internal error: unexpected variable"
and arr (a,e) = 
  let ordinal = id2ord a ^ " + " ^ (aexpr_to_string ~asp:julia_asp e) (* TODO *)
  in id2rng a ^ in_sq_brackets (values ordinal) 
and div (e1,e2) = 
  let e1 = aexpr_to_string ~asp:julia_asp e1
  and e2 = aexpr_to_string ~asp:julia_asp e2 
  in "div(" ^ e1 ^ ", " ^ e2 ^ ")" 
;;

let aexpr_to_julia_string e =
  aexpr_to_string ~asp:julia_asp e
;;

let bexpr_to_julia_string e =
  bexpr_to_string ~asp:julia_asp e
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
  julia_string (aexpr_to_string ~asp:julia_asp e)
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
  julia_string (bexpr_to_string ~asp:julia_asp e)
;;

