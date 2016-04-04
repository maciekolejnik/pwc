open Global
open Label 

(** Computes initial label of a labelled statement *)
let rec init lstmt =
  match lstmt with 
  | LStop(l) -> l
  | LSkip(l) -> l
  | LTaggedStmt(l,s) -> (init s)
  | LAssign(l,x,a) -> l
  | LRandom(l,x,r) -> l
  | LSequence(s1,s2) -> (init s1)
  | LIf(l,b,s1,s2) -> l
  | LWhile(l,b,s) -> l
  | LFor(l,i,b,u,s) -> (init i)
  | LCase(a,cls,d) -> (fst (fst (List.hd cls)))
  | LRepeat(l,s,b) -> (init s)
  | LChoose(l,wls) -> l
  | LGoto(l,t) -> l
;;

(** Computes final label of a labelled statement *)
let rec final lstmt =
  match lstmt with 
  | LStop(l) -> [l]
  | LSkip(l) -> [l]
  | LTaggedStmt(l,s) -> final s
  | LAssign(l,x,a) -> [l]
  | LRandom(l,x,r) -> [l]
  | LSequence(s1,s2) -> final s2
  | LIf(l,b,s1,s2) -> (final s1) @ (final s2)
  | LWhile(l,b,s) -> [l]
  | LFor(l,i,b,u,s) -> [l]
  | LCase(a,cls,d) -> (List.flatten (List.map final (List.map snd cls))) @
                    (final d)
  | LRepeat(l,s,b) -> [l]
  | LChoose(l,wls) -> (List.flatten (List.map final (List.map snd wls)))
  | LGoto(l,t) -> []
;;


(***********************************************************************)

type context = Statement.weight * Statement.weight list
;;

let drop_context flowlist =
   let decontext (i,(w,c),f) = (i,w,f) in
   List.map decontext flowlist
;;

let rec print_tags tags =
  match tags with
    [] -> ()
  | (lbl, block) :: ls ->  print_string "(";
                           print_string lbl;
                           print_string ", ";
                           print_int block;
                           print_string "),";
                           print_tags ls
;;

let rec tags lstmt = 
  match lstmt with 
    LWhile(l,b,s) -> (tags s)
  | LFor(l,i,b,u,s) -> (tags i) @ (tags u) @ (tags s)
  | LCase(a,cls,d) -> (List.flatten (List.map tags (List.map snd cls)))
  | LRepeat(l,s,b) -> (tags s)
  | LChoose(l,wsl) -> (List.flatten (List.map tags (List.map snd wsl)))
  | LIf(l,b,s1,s2) -> (tags s1) @ (tags s2)
  | LSequence(s1,s2) -> (tags s1) @ (tags s2)
  | LTaggedStmt(l,s) -> [(l, init(s))] 
  | _ -> []

let flow lstmt =
  let tags = tags lstmt in 
  let rec auxFlow lstmt =
    let link i f    = (i,(1,[]),f) and 
        linkInv f i = (i,(1,[]),f) and 
        linkNeg i f = (i,(-1,[]),f) and 
        compose f g x = f (g x) and
        dropLast l = List.rev (List.tl (List.rev l))
        (********* Provisional********)
        (* wlink i w f = (i,(w,[]),f) *) in 
    match lstmt with 
      LStop(l) ->
        [(link l l)]
    | LSkip(l) ->
        []
    | LTaggedStmt(l,s) ->
        (auxFlow s)
    | LAssign(l,x,a) ->
        []
    | LRandom(l,x,r) ->
        []
    | LSequence(s1,s2) ->
        (auxFlow s1) @ (List.map (linkInv (init s2)) (final s1)) @ (auxFlow s2)
    | LIf(l,b,s1,s2) ->
        [linkNeg l (init s1)] @ (auxFlow s1) @ 
        [link l (init s2)] @ (auxFlow s2)
    | LWhile(l,b,s) ->
        [linkNeg l (init s)] @ (auxFlow s) @ 
        (List.map (linkInv l) (final s))
    | LFor(l,i,b,u,s) ->
        (auxFlow i) @ (List.map (linkInv l) (final i)) @
        [linkNeg l (init s)] @
        (List.map (linkInv (init u)) (final s)) @
        (List.map (linkInv l) (final u))
    | LCase(a,cls,d) ->
        let pairConsecutive l = List.map2 link (dropLast l) (List.tl l) and
            caseLabel ((l,i),ls) = linkNeg l (init ls) in
        (List.map caseLabel cls) @
        (List.flatten (List.map auxFlow (List.map snd cls))) @
        (pairConsecutive ((List.map (compose fst fst) cls) @ [(init d)]))
    | LRepeat(l,s,b) ->
        (auxFlow s) @ [linkNeg l (init s)] @
        (List.map (linkInv l) (final s))
    | LChoose(l,wls) ->
        let 
        (* w weighted context flow triple from i *)
        wlink i w f = (i,(w,wls),f) in (*************)
        (List.map2 (wlink l) (List.map fst wls)
                   (List.map init (List.map snd wls))) @
        (List.flatten (List.map auxFlow (List.map snd wls)))
    | LGoto(l,t) ->
        try 
          [link l (List.assoc t tags)]
        with Not_found ->
          failwith ("Label " ^ t ^ " not matched")
  in 
  auxFlow lstmt
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)

let output_weight outch w = output_int outch w;;

let output_step outch (i,w,f) =
  output_string outch "(";
  output_int outch i;
  output_string outch ", ";
  output_weight outch w;
  output_string outch ", ";
  output_int outch f;
  output_string outch ")";
  output_newline outch
;;

let output_flow outch flow =
  output_string outch "Control flow:\n";
  let output_numbered_step i step = 
    begin 
      output_int outch i; 
      output_string outch ". "; 
      output_step outch step 
    end
  in
  List.iter2 output_numbered_step (interval 1 (List.length flow)) flow
;;


(***********************************************************************)
(* Text Output                                                         *)
(***********************************************************************)

let print_weight w = output_weight stdout w;;

let print_step s = output_step stdout s;;

let print_flow f = output_flow stdout f;;

(*
let print_weight p =
  match p with (***************)
    Statement.CWeight(w) -> print_int w;
  | Statement.PWeight(w) -> print_string w;
;;

let print_step (i,p,f) =
  print_string "(";
  print_int i;
  print_string ", ";
  print_weight p; (***************)
  print_string ", ";
  print_int f;
  print_string ")";
  print_newline ()
;;

let print_flow steps =
  (* List.map print_step steps *)
  let si = ref 1 in
  let print_steps (i,p,f) =
    begin
      print_int !si; si := !si + 1;
      print_string " - (";
      print_int i;
      print_string ", ";
      print_weight p; (***************)
      print_string ", ";
      print_int f;
      print_string ")";
      print_newline ()
    end
  in
  print_string "Control Flow:\n";
  List.iter print_steps steps
;;
*)

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

(** TODO *)
