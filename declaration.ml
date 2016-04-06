open Global


type range = 
    int list
;;

type id =
    string
;;

type meta =
    Variable of range
    (* add constants and arrays later? *)
;;

type decl =
    id * meta  
;;

type decls =
    decl list
;;  

(***********************************************************************)

let minInt = -4 (* truncates integers *)
and maxInt =  4 (* truncates integers *)
;;

(***********************************************************************)
(** Auxilliary functions                                               *)
(***********************************************************************)

(** Creates list of integers [a..b] *)
(*let rec interval a b =
  if a>b then [] else a::(interval (a+1) b) 
*)
(*  
let interval a b =
  let rec adduntil lb il =
    let i = List.hd il in 
    if lb >= i                 (* lower bound lb above head of il *)
    then il                    (* done, return integer list il *)
    else adduntil lb (i-1::il) (* put i-1 in il and continue *)
  in 
  if a>b
  then []              (* empty interval *)
  else adduntil a [b]  (* add to interval starting with [b] *)
;;
*)

(***********************************************************************)
(* Allocate variables in ds -> (id,start)                              *)
(***********************************************************************)
(** IS THIS NEEDED - DELETE IF NECESSARY *)
(* allocations = existsing allocation table *)

let add_alloc identifier start allocations =
    allocations @ [(identifier, start)]
;;

let rec allocate st ds dt =
  match ds with
    [] -> dt
  | d::dss ->  
      match d with
         (x, Variable(_)) -> 
	   let nt = add_alloc x st dt
	   in allocate (st+1) dss nt
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)
let output_range outch rng =
  output_string outch (String.concat ", " (List.map string_of_int rng))

  (*
let rec output_range outch rng =
  match rng with
  | []   -> output_string outch ""
  | [hd] -> output_int outch hd   
  | hd::tl -> 
      output_int outch hd;
      output_string outch ", "; 
      output_range outch tl
;;
*)

let output_decl outch (id,m) =
  output_string outch id;
  begin 
    match m with
    | Variable(r) -> 
        output_string outch ": range {";
        output_range outch r;
        output_string outch "}"
  end;
  output_newline outch
;;

let output_decls outch ds =
  output_newline outch;
  output_string outch "Variables:\n";
  List.iter (output_decl outch) ds;
  output_newline outch
;;


(***********************************************************************)
(* Text Output                                                         *)
(***********************************************************************)

let print_decls ds = output_decls stdout ds
;;

(*
let print_decls ds =
  let print_decl (id,d) =
    let print_range rng = output_range stdout rng
    in 
    begin
      print_string id;
      begin
	match d with
	  Parameter -> print_string " parameter"  (******************)
	| Range(r) -> print_string " in {";
	    print_range r;
	    print_string "}"
	| Array(n,r) -> print_string " [1..";
	    print_int n;
	    print_string "] in {";
	    print_range r;
	    print_string "}";
      end;
      print_newline ();
    end
  in
  begin
    print_newline ();
    print_string "Identifiers:";
    print_newline ();
    List.iter print_decl ds;
    print_newline ();
  end
;;
*)
(***********************************************************************)

(* NOT NEEDED RIGHT NOW IS IT??
let print_alloc (i,s) =
  print_string i;
  print_string " starts at ";
  print_int s;
  print_newline ()
;;

let print_allocation atable =
    List.map print_alloc atable
;;
*)

(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)

let julia_decls decls = 
  julia_string "#=\n";
  output_decls !fidJulia decls;
  julia_string "=#\n"
;;

let julia_id2ord (id,m) i =
  julia_string "\t\"";
  julia_string id;
  julia_string "\" => ";
  julia_int i;
  julia_string ",\n";
;;

let julia_ids2ord decls =
  julia_string "const id2ord = Dict(\n";
  List.iter2 julia_id2ord decls (interval 1 (List.length decls));
  julia_string ")\n";
;;

let julia_id2rng (id,m) =
  julia_string "\t\"";
  julia_string id;
  julia_string "\" => ";
  begin
    match m with
    | Variable(r) -> 
        julia_string "[";
        julia_string (String.concat ", " (List.map string_of_int r));
        julia_string "],\n"
  end
;;

let julia_ids2rng decls =
  julia_string "const id2rng = Dict(\n";
  List.iter julia_id2rng decls;
  julia_string ")\n";
;;

let julia_ord2rng (id,m) i =
  julia_string "\t";
  julia_int i;
  julia_string " => ";
  begin
    match m with 
    | Variable(r) ->
        julia_string "[";
        julia_string (String.concat ", " (List.map string_of_int r));
        julia_string "],\n"
  end
;;

let julia_ords2rng decls =
  julia_string "const ord2rng = Dict(\n";
  List.iter2 julia_ord2rng decls (interval 1 (List.length decls));
  julia_string ")\n";
;;

let julia_variables decls = 
  if !flagJulia then 
    begin
      julia_string "include(\"../../LOS.jl\") # Linear operator semantics";

      julia_separator ();

      julia_string "# Generated from ";
      julia_string !srcName;
      julia_string " by pWhile compiler - (c) 2016 H.Wiklicky, M.Olejnik\n";

      julia_separator ();

      julia_decls decls;

      julia_separator ();

      julia_ids2rng decls;

      julia_separator ();

      julia_ids2ord decls;

      julia_separator ();

      julia_ords2rng decls;

      julia_separator ();

      julia_string "const v = ";
      julia_int (List.length decls);
      julia_string " # number of variables\n\n";

      julia_string "const dims = ";
      julia_string "convert(Array{Int,1}, map(length, values(id2rng)))\n\n";
      
      julia_string "const d = prod(dims)\n\n";
      
      julia_separator ();

      julia_string "# Translation of declarations finished\n";

      julia_separator ();
    end
;;

