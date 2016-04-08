open Global
open Expression
open Declaration
open Statement


type lstmt =
   | LStop of label
   | LSkip of label
   | LTagged of tag * lstmt
   | LAssign of label * id * aexpr
   | LRandom of label * id * range
   | LSequence of lstmt * lstmt
   | LIf of label * bexpr * lstmt * lstmt
   | LWhile of label * bexpr * lstmt 
   | LFor of label * lstmt * bexpr * lstmt * lstmt
   | LCase of aexpr * (clstmt list) * lstmt
   | LRepeat of label * lstmt * bexpr
   | LChoose of label * (wlstmt list)
   | LGoto of label * tag
and wlstmt = 
    Statement.weight * lstmt
and clstmt =
    (label * int) * lstmt
and label =
    int
;;


(***********************************************************************)
(* Auxilliary                                                          *)
(***********************************************************************)

let currlabel = ref 0
;;

let rec newlabel () = 
  currlabel := !currlabel + 1;
  !currlabel
;;

(***********************************************************************)
(* Label a statement                                                   *)
(***********************************************************************)

let rec label_stmt s =
  match s with
    Skip ->
      LSkip(newlabel())
  | Stop ->
      LStop(newlabel())
  | Tagged(t,s) ->
      let ls = label_stmt s in
      LTagged(t,ls)
  | Assign(x,a) ->
      LAssign(newlabel(),x,a)
  | Random(x,r) ->
      LRandom(newlabel(),x,r)
  | Sequence(s1,s2) ->
      let ls1 = label_stmt s1 
      and ls2 = label_stmt s2 in
      LSequence(ls1,ls2)
  | If(b,s1,s2) ->
      let l = newlabel() 
      and ls1 = label_stmt s1 
      and ls2 = label_stmt s2 in
      LIf(l,b,ls1,ls2)
  | While(b,s) ->
      let l = newlabel() 
      and ls = label_stmt s in
      LWhile(l,b,ls)
  | For(i,b,u,s) ->
      let li = label_stmt i 
      and l  = newlabel()   
      and lu = label_stmt u 
      and ls = label_stmt s in
      LFor(l,li,b,lu,ls)
  | Case(a,csl,d) ->
      let clsl = List.map label_case csl 
      and ld = label_stmt d in
      LCase(a,clsl,ld)
  | Repeat(s,b) ->
      let ls = label_stmt s 
      and l = newlabel() in
      LRepeat(l,ls,b)
  | Choose(wsl) ->
      let l = newlabel() 
      and wlsl = List.map label_wstmt wsl in
      LChoose(l,wlsl)
  | Goto(t) ->
      LGoto(newlabel(),t)
and
  label_case (i,s) = 
    let l = newlabel() in 
    ((l,i), label_stmt s)
and
  label_wstmt (w,s) =
    (w, label_stmt s)
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)

(** TODO: THIS IS NOT USED ANYWHERE. DO WE NEED IT ACTUALLY? *)

let rec output_lstmt outch s =
  match s with 
    LStop(l) ->        
      output_string outch "[stop]^";
      output_int outch l;
      output_string outch " "
  | LSkip(l) ->        
      output_string outch "[skip]^";
      output_int outch l;
      output_string outch " "
  | LTagged(l,s) ->
      output_string outch l;
      output_string outch ": ";
      output_lstmt outch s
  | LAssign(l,x,a) ->  
      output_string outch "[";
      (*Expression.output_aexpr outch x;*)
      output_string outch x;
      output_string outch " := ";
      Expression.output_aexpr outch a;
      output_string outch "]^";
      output_int outch l;
      output_string outch " "
  | LRandom(l,x,r) ->  
      output_string outch "[";
      (*Expression.output_aexpr outch x;*)
      output_string outch x;
      output_string outch " ?= ";
      Declaration.output_range stdout  r;
      output_string outch "]^";
      output_int outch l;
      output_string outch " "
  | LSequence(s1,s2) -> 
      output_lstmt outch s1;
      output_string outch "; ";
      output_newline outch;
      output_lstmt outch s2
  | LIf(l,b,s1,s2) ->  
      output_string outch "if ";
      output_string outch "[";
      Expression.output_bexpr outch b;
      output_string outch "]^";
      output_int outch l;
      output_newline outch;
      output_string outch " then ";
      output_newline outch;
      output_lstmt outch s1;
      output_newline outch;
      output_string outch " else ";
      output_newline outch;
      output_lstmt outch s2;
      output_string outch " fi";
      output_newline outch
  | LWhile(l,b,s) ->   
      output_string outch "while ";
      output_string outch "[";
      Expression.output_bexpr outch b;
      output_string outch "]^";
      output_int outch l;
      output_string outch " do ";
      output_newline outch;
      output_lstmt outch s;
      output_newline outch;
      output_string outch "od";
      output_newline outch
  | LFor(l,i,b,u,s) ->
      output_string outch "for ";
      output_lstmt outch i;
      output_string outch "; [";
      Expression.output_bexpr outch b;
      output_string outch "]^";
      output_int outch l;
      output_string outch "; ";
      output_lstmt outch u;
      output_string outch " do";
      output_newline outch;
      output_lstmt outch s;
      output_newline outch;
      output_string outch "od";
      output_newline outch
  | LCase(a,clsl,ld) ->
      output_string outch "case ";
      Expression.output_aexpr outch a;
      output_newline outch;
      output_clstmts outch clsl;
      output_string outch "default: ";
      output_lstmt outch ld;
      output_newline outch;
      output_string outch "esac";
      output_newline outch
  | LRepeat(l,s,b) ->
      output_string outch "repeat ";
      output_newline outch;
      output_lstmt outch s;
      output_newline outch;
      output_string outch "until ";
      output_string outch "[";
      Expression.output_bexpr outch b;
      output_string outch "]^";
      output_int outch l;
      output_newline outch
  | LChoose(l,wlsl) -> 
      output_string outch "[choose]^";
      output_int outch l;
      output_newline outch;
      output_wlstmts outch wlsl;
      output_string outch "ro";
      output_newline outch
  | LGoto(l,gl) ->
      output_string outch "[goto]^";
      output_int outch l;
      output_string outch " ";
      output_string outch gl;
      output_newline outch
and
output_wlstmt outch (w,s) =
  output_weight outch w "/";
  output_string outch ":";
  output_lstmt outch s;
  output_newline outch
and
output_wlstmts outch wlsl =
  if ((List.length wlsl) > 1) then
    begin
    output_wlstmt outch (List.hd wlsl);
    output_string outch "or ";
    output_newline outch;
    output_wlstmts outch (List.tl wlsl)
    end
  else if ((List.length wlsl) = 1) then
    begin
    output_wlstmt outch (List.hd wlsl);
    end
and
output_clstmt outch ((l,i),s) =
  output_string outch "of [";
  output_int outch i;
  output_string outch "]^";
  output_int outch l;
  output_string outch ": ";
  output_lstmt outch s;
  output_newline outch
and
output_clstmts outch clsl =
  List.iter (output_clstmt outch) clsl
;;

(***********************************************************************)
(* Text Output                                                         *)
(***********************************************************************)

let print_lstmt s = output_lstmt stdout s;;

let print_wlstmt wls = output_wlstmts stdout wls;;

let print_wlstmts wlsl = output_wlstmts stdout wlsl;;

let print_clstmt cls = output_clstmt stdout cls;;

let print_clstmts clsl = output_clstmts stdout clsl;;

