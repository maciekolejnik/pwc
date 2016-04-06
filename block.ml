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
   | LTaggedStmt(l,s) ->
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
  [(l, BTest(Expression.Equal(a,Const(i))))] @ (blocks s)
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
      | BTest(b) -> Expression.output_bexpr outch b
      | BAsn(x,a) -> output_stmt outch (Assign(x,a))
      | BRnd(x,r) -> output_stmt outch (Random(x,r))
    end;
    output_newline outch;
  end
;;

let output_blocks outch blocks =
  output_string outch "Labelled blocks:\n";
  List.map (output_block outch) blocks
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
;;

(***********************************************************************)
(** Generate state update and filter operators corresponding to blocks *)
(***********************************************************************)

let blk2ops (l,b) =
  match b with
  | BTest(_) ->  [Filter(l,true); Filter(l,false)]
  | BAsn(x,a) -> [UpdateExpr(x,l)]
  | BRnd(x,r) -> [UpdateRnd(x,r)]
  | _ -> [Id]
;;

let blk2ids (l,b) =
  let s = string_of_int l in
  match b with 
  | BTest(_) -> [s^"t";s^"f"]
  | _ -> [s]
;;

(** Representation of each operator as julia string.
 *  The most interesting case is 'UpdateRnd' operator
 *  which consists of a sum of 'UpdateConst' operators.
 *  For details see the paper *)
let rec to_string op =
  match op with 
  | Filter(l,b) ->
      let ls = string_of_int l
      and bs = string_of_bool b 
      in "P(dims, test" ^ ls ^ ", " ^ bs ^ ")"
  | UpdateExpr(id,l) ->
      let ls = string_of_int l 
      in "Ue(dims, id2ord[\"" ^ id ^ "\"], assign" ^ ls ^ ")"
  | UpdateConst(id,i) ->
      "Uc(dims, id2ord[\"" ^ id ^ "\"], " ^ i ^ ")"
  | UpdateRnd(id,r) -> 
      let tot = string_of_int (List.length r)
      and value i = "findfirst(id2rng[\""^id^"\"], "^string_of_int i^")"
      in
      let ops = List.map (fun i -> (to_string (UpdateConst(id,(value i))))) r
      in  "1//" ^ tot ^ " * (" ^ (String.concat " +\n\t " ops)  ^ ")" 
      (*let rs = "[" ^ (String.concat ", " (List.map string_of_int r)) ^ "]"
      in "Ur(dims, id2ord(\"" ^ id ^ "\"), id2rng(\"" ^ id ^ "\"), " ^ rs ^ ")"*)
  | Id -> "I(d)"
;;


let julia_operator id op =
  julia_string "F";
  julia_string id;
  julia_string " = ";
  julia_string (to_string op);
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
      julia_string "function test";
      julia_int l;
      julia_string "(values)\n  return ";
      julia_bexpr b; 
      julia_string "\nend\n\n"
  | BAsn(x,a) -> 
      julia_string "function assign";
      julia_int l;
      julia_string "(values)\n  return findfirst(id2rng[\"";
      julia_string x;
      julia_string "\"], ";
      julia_aexpr a;
      julia_string ")\nend\n\n"
  | _ -> ()
;;

let julia_helpers blocks =
  List.iter julia_helper blocks
;;

let julia_blocks blocks =
  julia_separator ();
  julia_string "# Translation of blocks\n";
  julia_separator ();

  julia_helpers blocks;

  julia_separator ();

  julia_operators blocks;
;;
