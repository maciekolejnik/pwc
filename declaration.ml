open Global


type range = 
    int list
;;

type id =
    string
;;

type meta = (** metadata of a variable *)
  | Constant of int
  | Primitive of range
  | Array of int * range
;;

type decl =
    id * meta  
;;

type decls =
    decl list
;;  

(***********************************************************************)

(* TODO: SOMETHING BETTER HERE? *)
let minInt = -4 (* truncates integers *)
and maxInt =  4 (* truncates integers *)
;;

(***********************************************************************)
(** Symbol Table                                                       *)
(***********************************************************************)

let symTbl : (id, meta) Hashtbl.t = Hashtbl.create 10
;; 

let populateSymTbl decls = 
  let add (id,m) = Hashtbl.add symTbl id m
  in  List.iter add decls
;;

let meta id =
  Hashtbl.find symTbl id
;;

(***********************************************************************)
(** Auxiliary                                                          *)
(***********************************************************************)

let range m =
  match m with 
  | Constant(i) -> [i]
  | Primitive(r) -> r
  | Array(_,r) -> r
;;

let size m =
  match m with
  | Constant(_) -> 0
  | Primitive(_) -> 1
  | Array(l,_)  -> l
;;

(***********************************************************************)
(** Error checking                                                     *)
(***********************************************************************)

let assign_primitive id m =
  match m with
  | Constant(_) -> 
      failwith ("Can't assign to a constant " ^ id)
  | Primitive(_) -> ()
  | Array(_,_) ->
      failwith ("Index required when assigning to array " ^ id)
;;

let assign_array id m =
  match m with
  | Constant(_) -> 
      failwith ("Can't assign to a constant " ^ id)
  | Array(_,_) -> ()
  | Primitive(_) -> 
      failwith ("Index not allowed when assigning to primitive " ^ id)
;;

let use_primitive id m =
  match m with
  | Constant(_) | Primitive(_) -> ()
  | Array(_,_) ->
      failwith ("Can't reference array " ^ id ^ " without index")
;;

let use_array id m =
  match m with
  | Constant(_) ->
      failwith ("Can't use constant " ^ id ^ " with an index")
  | Primitive(_) ->
      failwith ("Can't use primitive " ^ id ^ " with an index")
  | Array(_,_) -> ()
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)

let range_to_string rng = 
  String.concat ", " (List.map string_of_int rng)
;;

let range_in_square_brackets rng =
  in_sq_brackets (range_to_string rng)
;;

let range_in_braces rng =
  in_braces (range_to_string rng)
;;

let meta_to_string meta =
  match meta with
  | Constant(i) ->
      "value " ^ string_of_int i
  | Primitive(r) ->
      "range " ^ range_in_braces r
  | Array(l,r) ->
      let l = string_of_int l
      in "array, size: " ^ l ^ ", elements in range: " ^ (range_in_braces r)
;;

(***********************************************************************)

let output_range outch rng =
  output_string outch (range_to_string rng)
;;

let output_decl outch (id,m) =
  output_string outch id;
  output_string outch " : ";
  output_string outch (meta_to_string m);
  output_newline outch
;;

let output_decls outch ds =
  output_string outch "Variables:\n";
  List.iter (output_decl outch) ds;
  output_newline outch
;;


(***********************************************************************)
(* Text Output                                                         *)
(***********************************************************************)

let print_decls ds = output_decls stdout ds
;;


(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)


(**
 *     julia_dict name entries 
 *
 * @param `name` name of the dictionary
 * @param `entries` list of (key,value) pairs (both are strings) 
 *
 * Declare a dictionary `name` in a julia file with 
 * entries `entries`
 *)
let julia_dict name entries =
  julia_string "const ";
  julia_string name;
  julia_string " = Dict(\n\t";
  julia_string (String.concat ",\n\t" (List.map dict_entry entries));
  julia_string "\n)\n"
;;

(***********************************************************************)

let ordinals decls =
  let rec ordinals_aux decls cur =
    match decls with
    | [] -> []
    | hd::tl -> cur :: ordinals_aux tl (cur + size (snd hd))
  in ordinals_aux decls 1
;;

let id2ord_entry (id,m) i =
  (in_quotes id, string_of_int i)
;;

let julia_ids2ord decls =
  julia_dict "id2ord" (List.map2 id2ord_entry decls (ordinals decls));
;;

(***********************************************************************)

let id2rng_entry (id,m) =
  (in_quotes id, range_in_square_brackets (range m))
;;

let julia_ids2rng decls =
  julia_dict "id2rng" (List.map id2rng_entry decls);
;;

(***********************************************************************)
let ord2rng_entries decls =
  let rec ord2rng_entries_aux decls i m =
    let is = string_of_int i in
    match decls with
    | [] -> []
    | hd::tl -> 
        let r = range_in_square_brackets (range (snd hd)) in
        begin match (snd hd) with
        | Primitive(_) -> 
            (is,r) :: ord2rng_entries_aux tl (i+1) m
        | Array(l,_) -> 
            if m == l 
            then ord2rng_entries_aux tl i 0
            else (is,r) :: ord2rng_entries_aux decls (i+1) (m+1)
        | _ -> ord2rng_entries_aux tl i m
        end
  in ord2rng_entries_aux decls 1 0
;;

let ord2rng_entry i (id,m) =
  (string_of_int i, range_in_square_brackets (range m))
;;

let julia_ords2rng decls =
  julia_dict "ord2rng" (ord2rng_entries decls)
;;

(** 
 *    julia_decls decls
 * 
 * @param `decls` lsit of declarations ie list of (id, meta) pairs
 *         where `id` is the variable identifier and `meta` is the
 *         metadata of that variable
 *
 * Main julia output function, prints all the relevant things
 * to the julia file, including variable mappings (id => range, 
 * id => ordinal, ordinal => range) and several constants used later
 *)
let julia_decls decls = 
  if !flagJulia then 
    begin
      julia_string "include(\"../LOS.jl\") # Linear operator semantics";

      julia_separator ();

      julia_string "# Generated from ";
      julia_string !srcName;
      julia_string " by pWhile compiler - (c) 2016 H.Wiklicky, M.Olejnik\n";

      julia_separator ();

      julia_ids2rng decls;

      julia_separator ();

      julia_ids2ord decls;

      julia_separator ();

      julia_ords2rng decls;

      julia_separator ();

      julia_string "const v = length(ord2rng) # number of variables\n\n";

      julia_string "dims = Array{Int}(v)\n";
      julia_string "for i=1:v dims[i] = length(ord2rng[i]) end\n\n";
      
      julia_string "const d = prod(dims)\n\n";
      
      julia_separator ();

      julia_string "# Translation of declarations finished\n";

      julia_separator ();
    end
;;

