open Global
open Declaration 
open Expression


type stmt = 
   | Stop
   | Skip
   | Tagged of tag * stmt
   | Assign of varref * aexpr
   | Random of varref * range
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
and cstmt = (** Case statement *)
    int * stmt 
and tag = (** Used for tagged [ie labelled] statements *)
    string
and weight =
    int * int (** weight represented as a positive rational number *)
;;

(** Auxilliary: normalise alternatives *)
let normalise alts = 
  let sum = (List.fold_left add (0,1) (List.map fst alts)) in
  let norm (w,s) = (divide w sum,s) in
  List.map norm alts
;;

(***********************************************************************)
(** Generic Output                                                      *)
(***********************************************************************)

(**
 *     weight_to_string (p,q) sep
 *
 * Return string representation of `(p,q)`, where `sep` is the character
 * used to separate `p` and `q`
 *)
let weight_to_string (p,q) sep =
  let ps = string_of_int p
  and qs = string_of_int q in
  if q == 1 then ps else ps ^ sep ^ qs
;;

(**   stmt_printer (statement printer)
 * 
 * Holds print functions for all different variants of statement
 * *)
type stmt_printer = 
  {  print_stop     : unit -> string;
     print_skip     : unit -> string;
     print_tagged   : string -> string-> string;
     print_assign   : string -> string -> string;
     print_random   : string -> string -> string;
     print_sequence : string -> string -> string;
     print_if       : string -> string -> string -> string;
     print_while    : string -> string -> string;
     print_for      : string -> string -> string -> string -> string;
     print_case     : string -> string -> string -> string;
     print_repeat   : string -> string -> string;
     print_choose   : string -> string;
     print_goto     : string -> string;
  }
;;

let print_stop () = "stop"
and print_skip () = "skip"
and print_tagged t s = t ^ ": " ^ s
and print_assign vr a = vr ^ ":= " ^ a
and print_random vr r = vr ^ "?= {" ^ r ^ "}"
and print_sequence s1 s2 = s1 ^ "; " ^ s2
and print_if b s1 s2 = "if " ^ b ^ " then " ^ s1 ^ " else " ^ s2
and print_while b s = "while " ^ b ^ " do " ^ s ^ " od"
and print_for i b u s = "for " ^ i ^ "; " ^ b ^ "; " ^ u ^ " do" ^ s ^ " od"
and print_case a css d = "case " ^ a ^ css ^ " default: " ^ d
and print_repeat s b = "repeat " ^ s ^ " until " ^ b
and print_choose l = "choose " ^ l ^ " ro"
and print_goto t = "goto " ^ t
;;

let default_sp = 
  {  print_stop     = print_stop;  
     print_skip     = print_skip;   
     print_tagged   = print_tagged;
     print_assign   = print_assign; 
     print_random   = print_random;
     print_sequence = print_sequence; 
     print_if       = print_if;
     print_while    = print_while;
     print_for      = print_for;
     print_case     = print_case;
     print_repeat   = print_repeat;
     print_choose   = print_choose;
     print_goto     = print_goto;
  }
;;

let rec stmt_to_string 
            ?(sp=default_sp) ?(ap=default_ap) ?(bp=default_bp) stmt =
  match stmt with 
  | Stop -> 
      sp.print_stop () 
  | Skip -> 
      sp.print_skip () 
  | Tagged(t,s) -> 
      let s = stmt_to_string ~sp:sp s 
      in  sp.print_tagged t s
  | Assign(x,a) -> 
      let x = varref_to_string ~vrp:ap.print_vr x
      and a = aexpr_to_string ~ap:ap a
      in  sp.print_assign x a
  | Random(x,r) -> 
      let x = varref_to_string ~vrp:ap.print_vr x
      and r = range_to_string r
      in  sp.print_random x r
  | Sequence(s1,s2) ->
      let s1 = stmt_to_string ~sp:sp s1
      and s2 = stmt_to_string ~sp:sp s2
      in  sp.print_sequence s1 s2
  | If(b,s1,s2) -> 
      let b  = bexpr_to_string ~ap:ap ~bp:bp b
      and s1 = stmt_to_string ~sp:sp s1
      and s2 = stmt_to_string ~sp:sp s2
      in  sp.print_if b s1 s2
  | While(b,s) ->
      let b = bexpr_to_string ~bp:bp b
      and s = stmt_to_string ~sp:sp s
      in  sp.print_while b s
  | For(i,b,u,s) ->
      let i = stmt_to_string ~sp:sp i
      and b = bexpr_to_string ~ap:ap ~bp:bp b
      and u = stmt_to_string ~sp:sp s
      and s = stmt_to_string ~sp:sp s
      in  sp.print_for i b u s 
  | Case(a,l,d) -> 
      let a = aexpr_to_string ~ap:ap a
      and l = cstmts_to_string l
      and d = stmt_to_string ~sp:sp d
      in  sp.print_case a l d 
  | Repeat(s,b) ->
      let s = stmt_to_string ~sp:sp s
      and b = bexpr_to_string ~ap:ap ~bp:bp b
      in  sp.print_repeat s b 
  | Choose(l) ->
      let l = wstmts_to_string l
      in  sp.print_choose l 
  | Goto(t) ->
       sp.print_goto t 
and
cstmt_to_string (i,s) = 
  let i = string_of_int i 
  and s = stmt_to_string s
  in  "of " ^ i ^ ": " ^ s
and 
cstmts_to_string csl = 
  String.concat "\n" (List.map cstmt_to_string csl)
and 
wstmt_to_string (w,s) =
  let w = weight_to_string w "/"
  and s = stmt_to_string s
  in  w ^ ": " ^ s
and
wstmts_to_string  wsl =
  String.concat " or " (List.map wstmt_to_string wsl)
;;

let output_stmt outch s = output_string outch (stmt_to_string s)
;;

let output_weight outch w sep = output_string outch (weight_to_string w sep)
;;
(***********************************************************************)
(** Text Output                                                         *)
(***********************************************************************)

let print_stmt s = output_stmt stdout s;;

(***********************************************************************)
(* Julia Output                                                       *)
(***********************************************************************)

let julia_stmt s =
  if !flagJulia then output_stmt !fidJulia s
;;

let julia_weight w =
  if !flagJulia then output_weight !fidJulia w "//"
;;

