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
  | BAsn  of aexpr * aexpr
  | BRnd  of aexpr * range
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
(** Text Output                                                         *)
(***********************************************************************)

let print_block b = output_block stdout b
;;

let print_blocks bs = output_blocks stdout bs
;;

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

