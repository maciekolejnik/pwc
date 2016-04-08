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

type operator =
  | Filter of label * bool
  | UpdateExpr of id * label
  | UpdateConst of id * string
  | UpdateRnd of id * range  
  | Id 
  | Other of label

(***********************************************************************)

(** Deprecated *)
let rec output_operator outch op =
  match op with
  | Filter(l,b) -> 
      output_string outch "P(dims, test";
      output_int outch l;
      output_string outch ", ";
      output_bool outch b;
      output_string outch ")";
  | UpdateExpr(id,l) ->
      output_string outch "Ue(dims, id2ord[\"";
      output_string outch id;
      output_string outch "\"], assign";
      output_int outch l;
      output_string outch ")"
  | UpdateConst(id,i) ->
      output_string outch "Uc(dims, id2ord[\"";
      output_string outch id;
      output_string outch "\"], ";
      output_string outch i;
      output_string outch ")"
  | UpdateRnd(id,r) ->
      output_string outch "Ur(dims, id2ord[\"";
      output_string outch id;
      output_string outch "\"], id2rng[\"";
      output_string outch id;
      output_string outch "\"], [";
      output_range outch r ;
      output_string outch "])"
  | Id ->
      output_string outch "I(d)"
  | Other(l) ->
      output_string outch "other"
;;

(***********************************************************************)
(** Generate state update and filter operators corresponding to blocks *)
(***********************************************************************)

let blk2ops (l,b) =
  match b with
  | BTest(_) ->  [Filter(l,true); Filter(l,false); Other(l)]
  | BAsn(x,a) -> [UpdateExpr(x,l)]
  | BRnd(x,r) -> [UpdateRnd(x,r)]
  | _ -> [Id]
;;

let blk2ids (l,b) =
  let s = string_of_int l in
  match b with 
  | BTest(_) -> [s ^ "t"; s ^ "f"; s]
  | _ -> [s]
;;

let id2ord id = "id2ord[\"" ^ id ^ "\"]"
and id2rng id = "id2rng[\"" ^ id ^ "\"]"
;; 

let fraction r = "1//" ^ string_of_int (List.length r)
;;

let p l b   = "P(dims, test" ^ string_of_int l ^ ", " ^ string_of_bool b ^ ")"
and ue id l = "Ue(dims, " ^ id2ord id ^ ", " ^ "assign" ^ string_of_int l ^")"
and uc id i = "Uc(dims, " ^ id2ord id ^ ", " ^ i ^ ")"
and ff id i = "findfirst(" ^ id2rng id ^ ", " ^ i ^ ")"
;;

let ucs id r = List.map (fun i -> uc id (ff id (string_of_int i))) r 
;; 

let ur id r = fraction r ^ " * (" ^ (String.concat " +\n\t" (ucs id r))  ^ ")" 
;;

let rec op_to_string op =
  match op with 
  | Filter(l,b) -> p l b
  | UpdateExpr(id,l) -> ue id l
  | UpdateConst(id,i) -> uc id i
  | UpdateRnd(id,r) -> ur id r
  | Id -> "I(d)"
  | Other(l) -> "F" ^ string_of_int l ^ "f"
;;


let julia_operator id op =
  julia_string "F";
  julia_string id;
  julia_string " = ";
  julia_string (op_to_string op);
  (*output_operator !fidJulia op;*)
  julia_string "\n"
;;

let julia_operators blocks =
  List.iter2 julia_operator (List.flatten (List.map blk2ids blocks))
                            (List.flatten (List.map blk2ops blocks))   
;;

(***********************************************************************)
(** Generate helper functions (tests, assignments) for operators above *)
(***********************************************************************)

let julia_helper (l,b) =
  match b with
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

let julia_blocks_number blocks =
  julia_string "const b = ";
  julia_int (List.length blocks);
  julia_string " # number of blocks\n"
;;

let julia_helpers blocks =
  List.iter julia_helper blocks
;;

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
