(***********************************************************************)
(** This file contains all the functionality that has to do with blocks.
    In particular
    - data types to represent blocks
    - function that computes blocks from the labeled syntax tree
    - functions to print blocks in a human readable format 
    - functions to generate appropriate parts of the julia file which
      produces the semantics of the program *)
(***********************************************************************)

open Global

open Expression
open Declaration
open Statement
open Label

type block = 
    BStop
  | BSkip
  | BChoose
  | BGoto of tag
  | BTest  of bexpr
  | BAsn  of varref * expr
  | BRnd  of varref * range
;;

type lblock =
    label * block
;;

(** Computes blocks from the labeled syntax tree of the program *)
let rec blocks lstmt =
  match lstmt with 
     LStop(l) ->
       [(l, BStop)]
   | LSkip(l) ->
       [(l, BSkip)]
   | LTagged(l,s) ->
       blocks s
   | LAssign(l,x,e) ->
       [(l, BAsn(x,e))]
   | LRandom(l,x,r) ->
       [(l, BRnd(x,r))]
   | LSequence(s1,s2) ->
       (blocks s1) @ (blocks s2)
   | LIf(l,b,s1,s2) ->
       [(l, BTest(b))] @ (blocks s1) @ (blocks s2)
   | LWhile(l,b,s) ->
       [(l, BTest(b))] @ (blocks s)
   | LFor(l,i,b,u,s) ->
       (blocks i) @ [(l, BTest(b))] @ (blocks u) @ (blocks s)
   | LCase(a,cls,d) ->
       (List.flatten (List.map (caseBlock a) cls)) @
       (blocks d)
   | LRepeat(l,s,b) ->
       (blocks s) @ [(l, BTest(b))]
   | LChoose(l,wls) ->
       [(l, BChoose)] @ (List.flatten (List.map blocks (List.map snd wls)))
   | LGoto(l,gl) ->
       [(l, BGoto(gl))]
and 
caseBlock a ((l,i),s) = 
  [(l, BTest(Equal(a,Num(i))))] @ (blocks s)
;;

(***********************************************************************)
(** Generic Output                                                      *)
(***********************************************************************)

let output_block outch (l,b) =
  begin
    output_int outch l;
    output_string outch ":\t";
    begin
      match b with
      | BStop -> output_stmt outch Stop
      | BSkip -> output_stmt outch Skip
      | BChoose -> output_string outch "choose"
      | BGoto(l) -> output_stmt outch (Goto(l))
      | BTest(b) -> output_bexpr outch b
      | BAsn(x,a) -> output_stmt outch (Assign(x,a))
      | BRnd(x,r) -> output_stmt outch (Random(x,r))
    end;
    output_newline outch;
  end
;;

let output_blocks outch blocks =
  output_string outch "Labelled blocks:\n";
  List.iter (output_block outch) blocks;
  output_newline outch
;;


(***********************************************************************)
(** Text Output                                                        *)
(***********************************************************************)

let print_block b = output_block stdout b
;;

let print_blocks bs = output_blocks stdout bs
;;

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

(** 
      idx2val ord index

  Return julia string which retrieves the actual value of variable with 
  ordinal `ord`, given the index `idx` of this value in variable's range
*)
let idx2val ord idx = 
  "findfirst(" ^ ord2rng ord ^ ", " ^ idx ^ ")"
;;

(** Several auxiliary functions to make string building process sligthly
 * less painful 
 * Conventions for variable names:
   * r = range
   * l = label (converted to string already)
   * b = boolean
   * id = identifier [ie variable name]
 *)

let fraction r = "1//" ^ string_of_int (List.length r)
;;

let p_opt l ords b = 
  apply_julia_func "P" ["dims"; "test" ^ l; ords; b]
;;

let p l b = 
  apply_julia_func "P" ["dims"; "test" ^ l; b]
;;

let ue ord l = 
  apply_julia_func "Ue" ["dims"; ord; "assign" ^ l]
;;

let ue_opt ord ords l = 
  apply_julia_func "Ue" ["dims"; ord; "assign" ^ l; ords]
;;

let ua ord size l = 
  apply_julia_func "Ua" ["dims"; ord; "arr_index" ^ l; size; "assign" ^ l]
;;

let ua_opt ord idx_ords size asn_ords l = 
  apply_julia_func "Ua" 
      ["dims"; ord; "arr_index" ^ l; idx_ords; size; "assign" ^ l; asn_ords]
;;

let uc ord c = 
  apply_julia_func "U_xk_c" ["dims"; ord; idx2val ord c]
;;

let ua_c ord size l c =
  apply_julia_func "Ua_c" ["dims"; ord; "arr_index" ^ l; size; idx2val ord c]
;;

let ua_c_opt ord ords size l c =
  apply_julia_func "Ua_c" ["dims"; ord; "arr_index" ^ l; 
                           ords; size; idx2val ord c]
;;

let ucs ord r = 
  List.map (uc ord) (List.map string_of_int r) 
;; 

let ua_cs ord size r l = 
  List.map (ua_c ord size l) (List.map string_of_int r) 
;;

let ua_cs_opt ord ords size r l = 
  List.map (ua_c_opt ord ords size l) (List.map string_of_int r) 
;;

(***********************************************************************)
let ur ord r = 
  fraction r ^ "*(" ^ (String.concat " +\n\t" (ucs ord r)) ^ ")" 
;;

let ura ord size r l =
  fraction r ^ "*(" ^ (String.concat " +\n\t" (ua_cs ord size r l)) ^ ")"
;;

let ura_opt ord ords size r l =
  fraction r ^ "*(" ^ (String.concat " +\n\t" (ua_cs_opt ord ords size r l)) ^ ")"
;;

(***********************************************************************)

(** 
 *     ordinals_julia_list varrefs
 *
 * Return ordinals corresponding to variable references `varrefs` as 
 * string which is a valid julia list. 
 *)
let ordinals_julia_list varrefs =
  let ords = List.flatten (List.map Expression.ordinals varrefs) in
  apply_julia_func "round" ["Int"; "[" ^ (String.concat ", " ords) ^ "]"]
;;

(*
let normal_assign x e l =
  let varrefs = aexpr_vars a in
  let ords = ordinals_julia_list varrefs
  and fl = "const F" ^ l in
  match x with
  | Var(id) -> 
      if !flagOpt
      then julia_assignment fl (ue_opt (id2ord id) ords l)
      else julia_assignment fl (ue (id2ord id) l) 
  | ArrElem(id,e) ->
      let size = string_of_int (Declaration.size (Declaration.meta id)) 
      and idx_varrefs = aexpr_vars e in
      let idx_ords = ordinals_julia_list idx_varrefs in
      if !flagOpt
      then julia_assignment fl (ua_opt (id2ord id) idx_ords size ords l)
      else julia_assignment fl (ua (id2ord id) size l)
;;*)

let normal_assign x e l =
  let varrefs = expr_vars e in
  let ords = ordinals_julia_list varrefs
  and fl = "const F" ^ l in
  match x with
  | Var(id) -> 
      if !flagOpt
      then julia_assignment fl (ue_opt (id2ord id) ords l)
      else julia_assignment fl (ue (id2ord id) l) 
  | ArrElem(id,e) ->
      let size = string_of_int (Declaration.size (Declaration.meta id)) 
      and idx_varrefs = aexpr_vars e in
      let idx_ords = ordinals_julia_list idx_varrefs in
      if !flagOpt
      then julia_assignment fl (ua_opt (id2ord id) idx_ords size ords l)
      else julia_assignment fl (ua (id2ord id) size l)
;;

let random_assign x r l =
  let fl = "const F" ^ l in
  match x with
  | Var(id) -> julia_assignment fl (ur (id2ord id) r)
  | ArrElem(id,e) -> 
      let size = string_of_int (Declaration.size (Declaration.meta id))
      and varrefs = aexpr_vars e in
      let ords = ordinals_julia_list varrefs in
      if !flagOpt
      then julia_assignment fl (ura_opt (id2ord id) ords size r l)
      else julia_assignment fl (ura (id2ord id) size r l)
;;

let conditional b l =
  let varrefs = bexpr_vars b in
  let ords = ordinals_julia_list varrefs
  and fl = "const F" ^ l in
  if !flagOpt then begin
    julia_assignment (fl ^ "t") (p_opt l ords "1");
    julia_assignment (fl ^ "f") (p_opt l ords "0")
  end else begin
    julia_assignment (fl ^ "t") (p l "1");
    julia_assignment (fl ^ "f") (p l "0")
  end;
  julia_assignment fl ("F" ^ l ^  "f")
;;

(**
 *     julia_operator (l,blk)
 *
 * Write to the julia file operator(s) corresponding to block
 * `blk`, labelled `l`. 
 *)
let julia_operator (l,blk) =
  let l = string_of_int l in
  let fl = "const F" ^ l in
  match blk with
  | BTest(b) ->
      conditional b l
  | BAsn(x,a) ->
      normal_assign x a l
  | BRnd(x,r) ->
      random_assign x r l
  | _ -> 
      julia_assignment fl "I(d)" 
;;

let julia_operators blocks =
  List.iter julia_operator blocks
;;

(***********************************************************************)

(**
 *     julia_test_function l bexpr
 *
 * Generate 'test' function corresponding to label `l` 
 * and expression `bexpr`
 * *)
let julia_test_function l bexpr =
  let name = "test" ^ l
  and ret_val = apply_julia_func "convert" ["Int"; bexpr_to_julia_string bexpr]
  in  julia_function name ["values"] ["return " ^ ret_val]
;;

(**
    julia_assign_function l expr
 
 Generate 'assign' function corresponding to label `l` 
 and RHS expression `expr`
*)
let julia_assign_function l varref expr =
  let name = "assign" ^ l
  and ret = "return " ^ idx2val (ord varref) (expr_to_julia_string expr)
  in  julia_function name ["values"] [ret]
;;

(**
 *     julia_arr_index_function l aexpr
 *
 * Generate 'arr_index' function corresponding to label `l` 
 * and expression `aexpr` describing the position in the array
 * *)
let julia_arr_index_function l aexpr =
  let name = "arr_index" ^ l
  and ret = "return " ^ aexpr_to_julia_string aexpr
  in  julia_function name ["values"] [ret]
;;

let julia_func_if_array l x =
  match x with
  | ArrElem(_,e) -> julia_arr_index_function l e
  | _ -> ()
;;

(**
 *     julia_helper (l,b)
 *
 * Generate helper function (test, assignment) for operators above 
 * corresponding to block `blk`, labelled `l`
 * *)
let julia_helper (l,blk) =
  let l = string_of_int l in
  match blk with
  | BTest(b) -> 
      Expression.check_bexpr b;
      julia_test_function l b
  | BAsn(x,e) ->
      Expression.check_assigned_varref x;
      (*Expression.check_aexpr e;*)
      Expression.check_expr e;
      julia_assign_function l x e;
      julia_func_if_array l x
  | BRnd(x,r) -> 
      Expression.check_assigned_varref x;
      (*TODO check_range (args) *)
      julia_func_if_array l x
  | _ -> ()
;;

let julia_helpers blocks =
  List.iter julia_helper blocks
;;

(***********************************************************************)

let julia_blocks_number blocks =
  julia_string "const b = ";
  julia_int (List.length blocks);
  julia_string " # number of blocks\n"
;;

(**
      julia_blocks blocks
 
  @param `blocks` list of labelled blocks (ie pairs (label, block))
 
  Main julia output function in this file, writes the number of 
  blocks and state update + filter operators along with corresponding
  test and assignment functions to the julia file. 
 *)
let julia_blocks blocks =
  julia_separator ();
  julia_string "# Translation of blocks\n";
  julia_separator ();

  julia_blocks_number blocks;

  julia_separator ();

  julia_helpers blocks;

  julia_separator ();

  julia_string (println "Compute state updates and filter operators...");

  julia_operators blocks;
;;
