open Global
open Label 

(** Computes initial label of a labelled statement *)
let rec init lstmt =
  match lstmt with 
  | LStop(l) -> l
  | LSkip(l) -> l
  | LTagged(l,s) -> (init s)
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
  | LTagged(l,s) -> final s
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

let rec tags lstmt = 
  match lstmt with 
    LWhile(l,b,s) -> (tags s)
  | LFor(l,i,b,u,s) -> (tags i) @ (tags u) @ (tags s)
  | LCase(a,cls,d) -> (List.flatten (List.map tags (List.map snd cls)))
  | LRepeat(l,s,b) -> (tags s)
  | LChoose(l,wsl) -> (List.flatten (List.map tags (List.map snd wsl)))
  | LIf(l,b,s1,s2) -> (tags s1) @ (tags s2)
  | LSequence(s1,s2) -> (tags s1) @ (tags s2)
  | LTagged(l,s) -> [(l, init(s))] 
  | _ -> []

let flow lstmt =
  let tags = tags lstmt in 
  let rec auxFlow lstmt =
    let link i f    = (i,(1,1),f)  
    and linkInv f i = (i,(1,1),f) 
    and linkNeg i f = (i,(-1,1),f) 
    and wlink i w f = (i,w,f) 
    and compose f g x = f (g x) 
    and dropLast l = List.rev (List.tl (List.rev l)) in
    match lstmt with 
      LStop(l) ->
        [(link l l)]
    | LSkip(l) ->
        []
    | LTagged(l,s) ->
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

let output_step outch (i,w,f) =
  output_string outch "(";
  output_int outch i;
  output_string outch ", ";
  Statement.output_weight outch w "/";
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

let print_flow f = output_flow stdout f;;

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

let triple_to_operator (i,w,f) =
  "T" ^ string_of_int i ^ string_of_int f
;;

let julia_markov_operator flow =
  julia_string "const T = ";
  julia_string (String.concat " + " (List.map triple_to_operator flow));
  julia_newline ()
;;

let julia_transfer_operator (i,w,f) = 
  julia_string "const T";
  julia_int i;
  julia_int f;
  julia_string " = abs(";
  Statement.julia_weight w;
  julia_string ") * kron(F";
  julia_int i;
  if fst w < 0 then julia_string "t";
  julia_string ", E(b,";
  julia_int i;
  julia_string ",";
  julia_int f;
  julia_string "))\n"
;;

let julia_transfer_operators flow = 
  List.iter julia_transfer_operator flow 
;;

let julia_flow flow = 
  julia_separator ();
  julia_string "# Translation of flow\n";
  julia_separator ();

  julia_transfer_operators flow;

  julia_separator ();

  julia_markov_operator flow;
  
  julia_string "\n# Translation of flow finished"
;;


