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
   | Lesser   of aexpr * aexpr
   | LeEqual  of aexpr * aexpr
   | Equal    of aexpr * aexpr
   | NotEqual of aexpr * aexpr
   | GrEqual  of aexpr * aexpr
   | Greater  of aexpr * aexpr
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
  | NotEqual(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | GrEqual(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
  | Greater(e1,e2) -> aexpr_vars e1 @ aexpr_vars e2
;;

let id varref = 
  match varref with
  | Var(id) -> id
  | ArrElem(id,e) -> id
;;

let ord varref = 
  match varref with
  | Var(id) -> id2ord id
  | ArrElem(id,e) -> id2ord id
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
  | NotEqual(e1,e2) -> check_aexpr e1; check_aexpr e2
  | GrEqual(e1,e2) -> check_aexpr e1; check_aexpr e2
  | Greater(e1,e2) -> check_aexpr e1; check_aexpr e2
;;


(***********************************************************************)
(** Generic Output                                                     *)
(***********************************************************************)

let apply_infix op e1 e2 = "(" ^ e1 ^ op ^ e2 ^ ")"
;;

(**
      aexpr_printer
 
  Printer data structure for aexpr 
 *)
type aexpr_printer =
  {  print_num   : int -> string;
     print_vr    : varref_printer;
     print_minus : string -> string;
     print_sum   : string -> string -> string;
     print_diff  : string -> string -> string;
     print_prod  : string -> string -> string;
     print_div   : string -> string -> string;
     print_mod   : string -> string -> string;
     print_bnot  : string -> string;
     print_bxor  : string -> string -> string;
     print_band  : string -> string -> string;
     print_bor   : string -> string -> string;
  }
(**
      varref_printer
 
  Printer data stricture for variable references
 *)
and varref_printer =
  {  print_var : id -> string;
     print_arr : id -> string -> string;
  }
;;

let print_num i = string_of_int i 
and print_varref v = v
and print_minus e = "(- " ^ e ^ ")"
and print_sum e1 e2 = apply_infix "+" e1 e2
and print_diff e1 e2 = apply_infix "-" e1 e2
and print_prod e1 e2 = apply_infix "*" e1 e2
and print_div e1 e2 = apply_infix "/" e1 e2
and print_mod e1 e2 = apply_infix "%" e1 e2
and print_bxor e1 e2 = apply_infix "$" e1 e2
and print_band e1 e2 = apply_infix "&" e1 e2
and print_bor e1 e2 = apply_infix "|" e1 e2
and print_bnot e = "~" ^ e
and print_var v = v
and print_arr id e = id ^ "[" ^ e ^ "]"
;;

let rec default_ap =
  {  print_num   = print_num;  
     print_vr    = default_vrp;
     print_minus = print_minus;  
     print_sum   = print_sum;  
     print_diff  = print_diff; 
     print_prod  = print_prod; 
     print_div   = print_div;  
     print_mod   = print_mod;  
     print_bnot  = print_bnot;  
     print_bxor  = print_bxor;  
     print_band  = print_band;  
     print_bor   = print_bor;  
  }
and default_vrp =
  {  print_var = print_var;
     print_arr = print_arr;
  }
;;

(**
 *     aexpr_to_string ?ap aexpr
 *
 * Return the string representation of `aexpr`, using 
 * the aexpr printer `ap` if present
 *)
let rec aexpr_to_string ?(ap=default_ap) aexpr = 
  match aexpr with 
  | Num(c) -> 
      ap.print_num c
  | Varref(v) -> 
      varref_to_string ~vrp:ap.print_vr ~ap:ap v
  | Minus(e) ->   
      let e = aexpr_to_string ~ap:ap e
      in  ap.print_minus e
  | Sum(e1,e2) -> 
      apply ap.print_sum ap e1 e2
  | Diff(e1,e2) -> 
      apply ap.print_diff ap e1 e2
  | Prod(e1,e2) -> 
      apply ap.print_prod ap e1 e2
  | Div(e1,e2) -> 
      apply ap.print_div ap e1 e2
  | Mod(e1,e2) ->
      apply ap.print_mod ap e1 e2
  | BNot(e) -> 
      let e = aexpr_to_string ~ap:ap e
      in  ap.print_bnot e
  | BXor(e1,e2) -> 
      apply ap.print_bxor ap e1 e2
  | BAnd(e1,e2) ->
      apply ap.print_band ap e1 e2
  | BOr(e1,e2) -> 
      apply ap.print_bor ap e1 e2
and
(**
 *     varref_to_string ?asp aexpr
 *
 * Return the string representation of `vr`, using 
 * the aexpr special printer `asp` and varref special printer `vrsp`,
 * if present
 *)
varref_to_string ?(vrp=default_vrp) ?(ap=default_ap) vr =
  match vr with
  | Var(v) -> 
      vrp.print_var v
  | ArrElem(a,e) -> 
      let e = aexpr_to_string ~ap:ap e
      in  vrp.print_arr a e
and apply print_fun ap e1 e2 =
  let e1 = aexpr_to_string ~ap:ap e1 
  and e2 = aexpr_to_string ~ap:ap e2 
  in print_fun e1 e2
;;

(***********************************************************************)
	
(**
 *     bexpr_printer
 *
 * Printer data structure for bexpr 
 *)
type bexpr_printer =
  {  print_true    : unit -> string;
     print_false   : unit -> string;
     print_not     : string -> string;
     print_and     : string -> string -> string;
     print_or      : string -> string -> string;
     print_lesser  : string -> string -> string;
     print_lequal  : string -> string -> string;
     print_equal   : string -> string -> string;
     print_notequal: string -> string -> string;
     print_grequal : string -> string -> string;
     print_greater : string -> string -> string;
  }
;;

let print_true () = "true"
and print_false () = "false"
and print_not e = "(~" ^ e ^ ")"
and print_and e1 e2 = apply_infix "&" e1 e2
and print_or e1 e2 = apply_infix "||" e1 e2
and print_lesser e1 e2 = apply_infix "<" e1 e2
and print_lequal e1 e2 = apply_infix "=<" e1 e2
and print_equal e1 e2 = apply_infix "==" e1 e2
and print_notequal e1 e2 = apply_infix "!=" e1 e2
and print_grequal e1 e2 = apply_infix ">=" e1 e2
and print_greater e1 e2 = apply_infix ">" e1 e2
;;

let default_bp =
  {  print_true    = print_true;  
     print_false   = print_false;  
     print_not     = print_not;  
     print_and     = print_and;  
     print_or      = print_or; 
     print_lesser  = print_lesser; 
     print_lequal  = print_lequal;  
     print_equal   = print_equal;  
     print_notequal= print_notequal;  
     print_grequal = print_grequal;  
     print_greater = print_greater;  
  }
;;

(**
 *     bexpr_to_string ?ap ?bp bexpr
 *
 * Return the string representation of `bexpr`, using 
 * the aexpr printer `ap` and bexpr 
 * printer `bp` if present
 *)
let rec bexpr_to_string ?(ap=default_ap) ?(bp=default_bp) bexpr = 
  match bexpr with 
  | True -> 
      bp.print_true () 
  | False ->  
      bp.print_false () 
  | Not(e) ->   
      let e = bexpr_to_string ~ap:ap ~bp:bp e
      in  bp.print_not e 
  | And(e1,e2) -> 
      apply_bexpr bp.print_and ap bp e1 e2
  | Or(e1,e2) -> 
      apply_bexpr bp.print_or ap bp e1 e2
  | Lesser(e1,e2) -> 
      apply_aexpr bp.print_lesser ap e1 e2
  | LeEqual(e1,e2) -> 
      apply_aexpr bp.print_lequal ap e1 e2
  | Equal(e1,e2) ->
      apply_aexpr bp.print_equal ap e1 e2
  | NotEqual(e1,e2) ->
      apply_aexpr bp.print_notequal ap e1 e2
  | GrEqual(e1,e2) -> 
      apply_aexpr bp.print_grequal ap e1 e2
  | Greater(e1,e2) ->
      apply_aexpr bp.print_greater ap e1 e2
(** Aixiliary *)
and apply_bexpr print_fun ap bp e1 e2 =
  let e1 = bexpr_to_string ~ap:ap ~bp:bp e1 
  and e2 = bexpr_to_string ~ap:ap ~bp:bp e2 
  in print_fun e1 e2
and apply_aexpr print_fun ap e1 e2 =
  let e1 = aexpr_to_string ~ap:ap e1 
  and e2 = aexpr_to_string ~ap:ap e2 
  in print_fun e1 e2
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
 *     julia_ap (aexpr printer)
 *
 * Overwrites the default printing with julia specific ways of printing
 * variables and division
 *)
let rec julia_ap = 
  let varref = 
    {  print_var = var;
       print_arr = arr;
    }
  in
  { default_ap with 
    print_vr  = varref; 
    print_div = div;
  }
and var v = 
  match meta v with
  | Constant(i) -> string_of_int i
  | Primitive(_) -> id2rng v ^ in_sq_brackets (values (id2ord v)) 
  | _ -> failwith "Internal error: unexpected variable"
and arr a e = 
  let ordinal = id2ord a ^ " + " ^ e (* TODO *)
  in id2rng a ^ in_sq_brackets (values ordinal) 
and div e1 e2 = 
  "div(" ^ e1 ^ ", " ^ e2 ^ ")" 
;;

let aexpr_to_julia_string e =
  aexpr_to_string ~ap:julia_ap e
;;

let bexpr_to_julia_string e =
  bexpr_to_string ~ap:julia_ap e
;;

(** 
      julia_aexpr e
 
   Writes arithmetic expression `e` to the julia file.
*)
let julia_aexpr e = 
  julia_string (aexpr_to_julia_string e)
;;

(** 
      julia_bexpr e
 
   Writes boolean expression `e` to the julia file.
*)
let julia_bexpr e =
  julia_string (bexpr_to_julia_string e)
;;

