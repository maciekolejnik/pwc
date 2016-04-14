open Global
open Declaration 
open Expression


type stmt = 
   | Stop
   | Skip
   | Tagged of tag * stmt
   | Assign of varref * aexpr
   | Random of varref * range
   (*| Assign of aexpr * aexpr
   | Random of aexpr * range*)
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

(**   type ssp (statement special print)
 * 
 * Holds special print functions for all different variants of statement
 * *)
type ssp = 
  {  stop_sp     : (unit -> string) option;
     skip_sp     : (unit -> string) option;
     tagged_sp   : (tag * stmt -> string) option;
     assign_sp   : (varref * aexpr -> string) option;
     random_sp   : (varref * range -> string) option;
     (*assign_sp   : (aexpr * aexpr -> string) option;
     random_sp   : (aexpr * range -> string) option;*)
     sequence_sp : (stmt * stmt -> string) option;
     if_sp       : (bexpr * stmt * stmt -> string) option;
     while_sp    : (bexpr * stmt -> string) option;
     for_sp      : (stmt * bexpr * stmt * stmt -> string) option;
     case_sp     : (aexpr * cstmt list * stmt -> string) option;
     repeat_sp   : (stmt * bexpr -> string) option;
     choose_sp   : (wstmt list -> string) option;
     goto_sp     : (tag -> string) option;
  }
;;

let default_ssp = 
  {  stop_sp     = None;  
     skip_sp     = None;   
     tagged_sp   = None;
     assign_sp   = None; 
     random_sp   = None;
     sequence_sp = None; 
     if_sp       = None;
     while_sp    = None;
     for_sp      = None;
     case_sp     = None;
     repeat_sp   = None;
     choose_sp   = None;
     goto_sp     = None;
  }
;;

let rec stmt_to_string ?sspo ?aspo ?bspo stmt =
  let ssp = some_or_default sspo default_ssp 
  and asp = some_or_default aspo default_asp 
  and bsp = some_or_default bspo default_bsp in 
  let vrsp = some_or_default asp.vr_sp default_vrsp
  in
  match stmt with 
  | Stop -> 
      to_string ssp.stop_sp () "stop"
  | Skip -> 
      to_string ssp.skip_sp () "skip"
  | Tagged(t,s) -> 
      let ss = stmt_to_string ~sspo:ssp s 
      in  to_string ssp.tagged_sp (t,s) (t ^ ": " ^ ss)
  | Assign(x,a) -> 
      (*let xs = aexpr_to_string ~aspo:asp x*)
      let xs = varref_to_string ~vrspo:vrsp x
      and e = aexpr_to_string ~aspo:asp a
      in  to_string ssp.assign_sp (x,a) (xs ^ " := " ^ e)
  | Random(x,r) -> 
      (*let xs = aexpr_to_string ~aspo:asp x*)
      let xs = varref_to_string ~vrspo:vrsp x
      and rs = range_to_string r
      in  to_string ssp.random_sp (x,r) (xs ^ " ?= {" ^ rs ^ "}")
  | Sequence(s1,s2) ->
      let s1s = stmt_to_string ~sspo:ssp s1
      and s2s = stmt_to_string ~sspo:ssp s2
      in  to_string ssp.sequence_sp (s1,s2) (s1s ^ "; " ^ s2s)
  | If(b,s1,s2) -> 
      let bs  = bexpr_to_string ~aspo:asp ~bspo:bsp b
      and s1s = stmt_to_string ~sspo:ssp s1
      and s2s = stmt_to_string ~sspo:ssp s2
      in  to_string ssp.if_sp (b,s1,s2) ("if "^ bs ^" then "^ s1s ^" else "^ s2s)
  | While(b,s) ->
      let bs = bexpr_to_string ~bspo:bsp b
      and ss = stmt_to_string ~sspo:ssp s
      in  to_string ssp.while_sp (b,s) ("while " ^ bs ^ " do " ^ ss ^ " od") 
  | For(i,b,u,s) ->
      let is = stmt_to_string ~sspo:ssp i
      and bs = bexpr_to_string ~aspo:asp ~bspo:bsp b
      and us = stmt_to_string ~sspo:ssp s
      and ss = stmt_to_string ~sspo:ssp s
      in  to_string ssp.for_sp (i,b,u,s) 
          ("for " ^ is ^ "; " ^ bs ^ "; " ^ us ^ " do" ^ ss ^ " od")
  | Case(a,l,d) -> 
      let e = aexpr_to_string ~aspo:asp a
      and ls = cstmts_to_string l
      and ds = stmt_to_string ~sspo:ssp d
      in  to_string ssp.case_sp (a,l,d) ("case " ^ e ^ ls ^ " default: " ^ ds)
  | Repeat(s,b) ->
      let ss = stmt_to_string ~sspo:ssp s
      and bs = bexpr_to_string ~aspo:asp ~bspo:bsp b
      in  to_string ssp.repeat_sp (s,b) ("repeat " ^ ss ^ " until " ^ bs)
  | Choose(l) ->
      let ls = wstmts_to_string l
      in  to_string ssp.choose_sp l ("choose " ^ ls ^ " ro")
  | Goto(t) ->
      to_string ssp.goto_sp t ("goto " ^ t)
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

