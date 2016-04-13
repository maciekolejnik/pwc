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
  | BAsn  of id * aexpr
  | BRnd  of id * range
;;

type lblock =
    label * block
;;

(** Computes blocks from the labeled syntax tree of the program *)
let rec blocks lstmt =
  match lstmt with 
     LStop(l) ->
       [(l, BSkip)]
   | LSkip(l) ->
       [(l, BSkip)]
   | LTagged(l,s) ->
       blocks s
   | LAssign(l,x,a) ->
       [(l, BAsn(x,a))]
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
  [(l, BTest(Equal(a,Const(i))))] @ (blocks s)
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

(** Several auxiliary functions to make string building process sligthly
 * less painful 
 * Conventions for variable names:
   * r = range
   * l = label (converted to string already)
   * b = boolean
   * id = identifier [ie variable name]
 *)
let id2ord id = "id2ord[" ^ in_quotes id ^ "]"
and id2rng id = "id2rng[" ^ in_quotes id ^ "]"
;; 

let fraction r = "1//" ^ string_of_int (List.length r)
;;

let p l b   = "P(dims, test" ^ l ^ ", " ^ string_of_bool b ^ ")"
and ue id l = "Ue(dims, " ^ id2ord id ^ ", " ^ "assign" ^ l ^")"
and uc id i = "U_xk_c(dims, " ^ id2ord id ^ ", " ^ i ^ ")"
and ff id i = "findfirst(" ^ id2rng id ^ ", " ^ i ^ ")"
;;

let ucs id r = List.map (fun i -> uc id (ff id (string_of_int i))) r 
;; 

let ur id r = fraction r ^ " * (" ^ (String.concat " +\n\t" (ucs id r))  ^ ")" 
;;

(***********************************************************************)

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
      julia_assignment (fl ^ "t") (p l true);
      julia_assignment (fl ^ "f") (p l false);
      julia_assignment (fl) ("F" ^ l ^ "f")
  | BAsn(x,a) ->
      julia_assignment fl (ue x l) 
  | BRnd(x,r) ->
      julia_assignment fl (ur x r)
  | _ -> 
      julia_assignment fl "I(d)" 
;;

let julia_operators blocks =
  List.iter julia_operator blocks
;;

(***********************************************************************)


(**
 *     julia_helper (l,b)
 *
 * Generate helper functions (tests, assignments) for operators above 
 * corresponding to block `blk`, labelled `l`
 * *)
let julia_helper (l,blk) =
  match blk with
  | BTest(b) -> 
      let name = "test" ^ string_of_int l
      and b = "return " ^ bexpr_to_julia_string b 
      in  julia_function name ["values"] [b]
  | BAsn(x,a) -> 
      let name = "assign" ^ string_of_int l
      and ret = "return " ^ ff x (aexpr_to_julia_string a) 
      in  julia_function name ["values"] [ret]
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
 *     julia_blocks blocks
 *
 * @param `blocks` list of labeled blocks (ie pairs (label, block))
 *
 * Main julia output function in this file, writes the number of 
 * blocks and state update + filter operators with corresponding
 * test and assignment functions to the julia file. 
 *)
let julia_blocks blocks =
  julia_separator ();
  julia_string "# Translation of blocks\n";
  julia_separator ();

  julia_blocks_number blocks;

  julia_separator ();

  julia_helpers blocks;

  julia_separator ();

  julia_operators blocks;
;;
