open Global


type range = 
    int list
;;

type id =
    string
;;

type meta =
  | Primitive of range
  | Array of int * range
    (* add constants later? *)
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
(** Auxiliary                                                          *)
(***********************************************************************)

let range_of_variable meta =
  match meta with 
  | Primitive(r) -> r
  | Array(l,r) -> r
;;

(**
 *     size meta
 *
 * Return size of variable with metadata `meta`
 *)
let size meta =
  match meta with
  | Primitive(_) -> 1
  | Array(l,_)  -> l
;;

(**
 *     ordinals decls
 *
 * Return the list of ordinals corresponding to declarations `decls`
 *)
let ordinals decls =
  let rec ordinals_aux decls cur =
    match decls with
    | [] -> []
    | hd::tl -> cur :: ordinals_aux tl (cur + size(snd(hd)))
  in ordinals_aux decls 1
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)

let range_to_string rng = 
  String.concat ", " (List.map string_of_int rng)
;;

let range_in_square_brackets rng =
  "[" ^ range_to_string rng ^ "]"
;;

let range_in_braces rng =
  "{" ^ range_to_string rng ^ "}"
;;

let meta_to_string meta =
  match meta with
  | Primitive(r) ->
      "range " ^ (range_in_braces r) 
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


(***********************************************************************)
(** Julia Output                                                       *)
(***********************************************************************)


(**
 *     julia_dict name entries 
 *
 * @param `name` name of the dictionary
 * @param `entries` list of (key,value) pairs 
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

let id2ord_entry (id,m) i =
  (in_quotes id, string_of_int i)
;;

let julia_ids2ord decls =
  julia_dict "id2ord" (List.map2 id2ord_entry decls (ordinals decls));
;;

(***********************************************************************)

let id2rng_entry (id,m) =
  (in_quotes id, range_in_square_brackets (range_of_variable m))
;;

let julia_ids2rng decls =
  julia_dict "id2rng" (List.map id2rng_entry decls);
;;

(***********************************************************************)
let ord2rng_entries decls =
  let rec ord2rng_entries_aux decls i ai =
    match decls with
    | [] -> []
    | hd::tl -> 
        begin match (snd hd) with
        | Primitive(r) -> 
            (string_of_int i, range_in_square_brackets r) 
            :: ord2rng_entries_aux tl (i+1) ai
        | Array(l,r) -> 
            if ai == l then ord2rng_entries_aux tl i 0
            else (string_of_int i, range_in_square_brackets r)
            :: ord2rng_entries_aux decls (i+1) (ai+1)
        end
  in ord2rng_entries_aux decls 1 0
;;

let ord2rng_entry i (id,m) =
  (string_of_int i, range_in_square_brackets (range_of_variable m))
;;

let julia_ords2rng decls =
  (*julia_dict "ord2rng" (List.map2 ord2rng_entry (ordinals decls) decls);*)
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

