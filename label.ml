open Global
open Expression
open Declaration
open Statement


type lstmt =
     LStop of label
   | LSkip of label
   | LTaggedStmt of tag * lstmt
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
  | TaggedStmt(t,s) ->
      let ls = label_stmt s in
      LTaggedStmt(t,ls)
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
  | LTaggedStmt(l,s) ->
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
  output_int outch w;
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

(*
let rec print_lstmt s =
  match s with 
    LStop(l) ->        
      print_string "[stop]^";
      print_int l;
      print_string " "
  | LSkip(l) ->        
      print_string "[skip]^";
      print_int l;
      print_string " "
  | LTaggedStmt(l,s) ->
      print_string l;
      print_string ": ";
      print_lstmt s
  | LSkipAsn(l,x,a) ->
      print_string "[skipAsn ";
      print_aexpr x;
      print_string " ";
      print_aexpr a;
      print_string "]^";
      print_int l;
      print_string " "
  | LSkipIf(l,b,s) ->
      print_string "if ";
      print_string "[";
      print_bexpr b;
      print_string "]^";
      print_int l;
      print_newline ();
      print_lstmt s;
      print_newline ();
      print_string " fi";
      print_newline ()
  | LAssign(l,x,a) ->  
      print_string "[";
      print_aexpr x;
      print_string " := ";
      print_aexpr a;
      print_string "]^";
      print_int l;
      print_string " "
  | LRandom(l,x,r) ->  
      print_string "[";
      print_aexpr x;
      print_string " ?= ";
      Declaration.output_range stdout  r;
      print_string "]^";
      print_int l;
      print_string " "
  | LSequence(s1,s2) -> 
      print_lstmt s1;
      print_string "; ";
      print_newline ();
      print_lstmt s2
  | LIf(l,b,s1,s2) ->  
      print_string "if ";
      print_string "[";
      Expression.print_bexpr b;
      print_string "]^";
      print_int l;
      print_newline ();
      print_string " then ";
      print_newline ();
      print_lstmt s1;
      print_newline ();
      print_string " else ";
      print_newline ();
      print_lstmt s2;
      print_string " fi";
      print_newline ()
  | LWhile(l,b,s) ->   
      print_string "while ";
      print_string "[";
      Expression.print_bexpr b;
      print_string "]^";
      print_int l;
      print_string " do ";
      print_newline ();
      print_lstmt s;
      print_newline ();
      print_string "od";
      print_newline ()
  | LFor(l,i,b,u,s) ->
      print_string "for ";
      print_lstmt i;
      print_string "; [";
      Expression.print_bexpr b;
      print_string "]^";
      print_int l;
      print_string "; ";
      print_lstmt u;
      print_string " do";
      print_newline ();
      print_lstmt s;
      print_newline ();
      print_string "od";
      print_newline ()
  | LCase(a,clsl,ld) ->
      print_string "case ";
      Expression.print_aexpr a;
      print_newline ();
      print_clstmts clsl;
      print_string "default: ";
      print_lstmt ld;
      print_newline ();
      print_string "esac";
      print_newline ()
  | LRepeat(l,s,b) ->
      print_string "repeat ";
      print_newline ();
      print_lstmt s;
      print_newline ();
      print_string "until ";
      print_string "[";
      Expression.print_bexpr b;
      print_string "]^";
      print_int l;
      print_newline ()
  | LChoose(l,wlsl) -> 
      print_string "[choose]^";
      print_int l;
      print_newline ();
      print_wlstmts wlsl;
      print_string "ro";
      print_newline ()
  | LGoto(l,gl) ->
      print_string "[goto]^";
      print_int l;
      print_string " ";
      print_string gl;
      print_newline ()
and 
print_wlstmt (p,s) =
  match p with (***************)
    CWeight(w) -> print_int w;
  | PWeight(w) -> print_string w;
  print_string ":";
  print_lstmt s;
  print_newline ()
and 
print_wlstmts wlsl =
  if ((List.length wlsl) > 1) then
    begin
    print_wlstmt (List.hd wlsl);
    print_string "or ";
    print_newline ();
    print_wlstmts (List.tl wlsl)
    end
  else if ((List.length wlsl) = 1) then
    begin
    print_wlstmt (List.hd wlsl);
    end
and
print_clstmt ((l,i),s) =
  print_string "of [";
  print_int i;
  print_string "]^";
  print_int l;
  print_string ": ";
  print_lstmt s;
  print_newline ()
and
print_clstmts clsl =
  List.iter print_clstmt clsl
;;
*)

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)


