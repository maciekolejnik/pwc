open Global
open Declaration 
open Expression


type stmt = 
     Stop
   | Skip
   | TaggedStmt of tag * stmt
   | Assign of aexpr * aexpr
   | Random of aexpr * range
   | Sequence of stmt * stmt
   | If of bexpr * stmt * stmt
   | While of bexpr * stmt 
   | For of stmt * bexpr * stmt * stmt
   | Case of aexpr * (cstmt list) * stmt
   | Repeat of stmt * bexpr
   | Choose of (wstmt list)
   | Goto of tag 
and wstmt = 
    weight * stmt 
and cstmt =
    int * stmt
and tag =
    string
and weight =
    int
and dup = string
;;

(** Auxilliary: re-normalise alternatives *)
let renorm alts = 
  let getw (w1,s1) = w1 in
  let sum = (List.fold_left (+.) 0.0 (List.map getw alts)) in
  let norm (w2,s2) = (w2 /. sum,s2) in
  List.map norm alts
;;

(***********************************************************************)
(** Generic Output                                                      *)
(***********************************************************************)

let rec output_stmt outch s =
  match s with 
    Stop ->         output_string outch "stop"
  | Skip ->         output_string outch "skip"
  | TaggedStmt(l,s) -> output_string outch l;
                    output_string outch " : ";
                    output_stmt outch s
  | Assign(x,a) ->  output_aexpr outch x;
                    output_string outch ":=";
                    output_aexpr outch a
  | Random(x,r) ->  output_aexpr outch x;
                    output_string outch " ?= { ";
                    output_range outch r;
                    output_string outch " }"
  | Sequence(s1,s2) -> output_stmt outch s1;
                    output_string outch "; ";
                    output_stmt outch s2
  | If(b,s1,s2) ->  output_string outch "if ";
                    output_bexpr outch b;
                    output_string outch " then ";
                    output_stmt outch s1;
                    output_string outch " else ";
                    output_stmt outch s2;
                    output_string outch " fi"
  | While(b,s) ->   output_string outch "while ";
                    output_bexpr outch b;
                    output_string outch " do ";
                    output_stmt outch s;
                    output_string outch " od"
  | For(i,b,u,s) -> output_string outch "for ";
                    output_stmt outch i;
                    output_string outch "; ";
                    output_bexpr outch b;
                    output_string outch "; ";
                    output_stmt outch u;
                    output_string outch " do ";
                    output_stmt outch s;
                    output_string outch "od"
  | Case(a,l,d) ->  output_string outch "case ";
                    output_aexpr outch a;
                    output_cstmts outch l;
                    output_string outch "default: ";
                    output_stmt outch d;
                    output_string outch "esac"
  | Repeat(s,b) ->  output_string outch "repeat ";
                    output_stmt outch s;
                    output_string outch "until ";
                    output_bexpr outch b
  | Choose(l) ->    output_string outch "choose ";
                    output_wstmts outch l;
                    output_string outch "ro"
  | Goto(t) ->      output_string outch "goto ";
                    output_string outch t;
and 
output_wstmt outch (p,s) =
  output_int outch p;
  output_string outch ":";
  output_stmt outch s;
  output_string outch " "
and 
output_wstmts outch wsl =
  if ((List.length wsl) > 1) then
    begin
    output_wstmt outch (List.hd wsl);
    output_string outch "or ";
    output_string outch " ";
    output_wstmts outch (List.tl wsl)
    end
  else if ((List.length wsl) = 1) then
    begin
    output_wstmt outch (List.hd wsl);
    end
and
output_cstmt outch (i,s) =
  output_string outch "of ";
  output_int outch i;
  output_string outch ": ";
  output_stmt outch s;
  output_string outch " " 
and
output_cstmts outch csl =
  List.iter (output_cstmt outch) csl
;;

(***********************************************************************)

(***********************************************************************)
(** Text Output                                                         *)
(***********************************************************************)

let print_stmt s = output_stmt stdout s;;

let print_wstmt ws = output_wstmt stdout ws;;

let print_wstmts wsl = output_wstmts stdout wsl;;

let print_cstmt cs = output_cstmt stdout cs;;

let print_cstmts csl = output_cstmts stdout csl;;

(*
let rec print_stmt s =
  match s with 
    Stop ->         print_string "stop"
  | Skip ->         print_string "skip"
  | TaggedStmt(l,s) -> print_string l;
                    print_string " : ";
                    print_stmt s
  | Assign(x,a) ->  Expression.print_aexpr x;
                    print_string " := ";
                    Expression.print_aexpr a
  | Random(x,r) ->  Expression.print_aexpr x;
                    print_string " ?= { ";
                    Declaration.output_range stdout  r;
                    print_string " }"
  | Sequence(s1,s2) -> print_stmt s1;
                    print_string "; ";
                    print_newline ();
                    print_stmt s2
  | If(b,s1,s2) ->  print_string "if ";
                    Expression.print_bexpr b;
                    print_string " then ";
                    print_stmt s1;
                    print_string " else ";
                    print_stmt s2;
                    print_string " fi"
  | While(b,s) ->   print_string "while ";
                    Expression.print_bexpr b;
                    print_string " do ";
                    print_newline ();
                    print_stmt s;
                    print_string "od"
  | For(i,b,u,s) ->   print_string "for ";
                    print_stmt i;
                    print_string "; ";
                    Expression.print_bexpr b;
                    print_string "; ";
                    print_stmt u;
                    print_string " do";
                    print_newline ();
                    print_stmt s;
                    print_string "od"
  | Case(a,l,d) ->  print_string "case ";
                    Expression.print_aexpr a;
                    print_newline ();
                    print_cstmts l;
                    print_string "default: ";
                    print_stmt d;
                    print_string "esac"
  | Repeat(s,b) ->  print_string "repeat ";
                    print_newline ();
                    print_stmt s;
                    print_newline ();
                    print_string "until ";
                    Expression.print_bexpr b
  | Choose(l) ->    print_string "choose ";
                    print_newline ();
                    print_wstmts l;
                    print_string "ro";
                    print_newline ()
  | Goto(t) ->      print_string "goto ";
                    print_string t;
and 
print_wstmt (p,s) =
  match p with (***************)
    CWeight(w) -> print_int w;
  | PWeight(w) -> print_string w;
  print_string ":";
  print_stmt s;
  print_newline ()
and 
print_wstmts wsl =
  if ((List.length wsl) > 1) then
    begin
    print_wstmt (List.hd wsl);
    print_string "or ";
    print_newline ();
    print_wstmts (List.tl wsl)
    end
  else if ((List.length wsl) = 1) then
    begin
    print_wstmt (List.hd wsl);
    end
and 
print_cstmt (i,s) =
  print_string "of ";
  print_int i;
  print_string ": ";
  print_stmt s;
  print_newline ();
and
print_cstmts csl =
  List.iter print_cstmt csl
;;
*)

(***********************************************************************)
(* Julia Output                                                       *)
(***********************************************************************)

let julia_stmt s =
  if !flagJulia then output_stmt !fidJulia s
;;

let julia_test b =
  if !flagJulia then output_bexpr !fidJulia b
;;

let julia_aexprs s =
  if !flagJulia then output_aexpr !fidJulia s
;;

(***********************************************************************)
(* LateX Output                                                        *)
(***********************************************************************)

(* latex_ *)

(***********************************************************************)
