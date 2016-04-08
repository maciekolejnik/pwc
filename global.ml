(***********************************************************************)
(* Global: Flags and I/O File Names                                    *)
(***********************************************************************)

let flagVerbose = ref false
and flagBinary  = ref false
and flagText    = ref false
and flagLaTeX   = ref false
and flagJulia   = ref true 
and flagUndef   = ref false
;;

let baseName = ref ""
and srcName  = ref ""
and binName  = ref ""
and txtName  = ref ""
and texName  = ref ""
and julName  = ref ""
;;

let fidVerbose = ref stdout
and fidBinary  = ref stdout
and fidText    = ref stdout
and fidLaTeX   = ref stdout
and fidJulia   = ref stdout
;;

(***********************************************************************)
(* Generic Output                                                      *)
(***********************************************************************)

let output_int outch i =
  output_string outch (string_of_int i)
;;

let output_float outch f =
  output_string outch (string_of_float f)
;;

let output_bool outch b =
  output_string outch (string_of_bool b)
;;

let output_newline outch =
  output_string outch "\n"
;;

(***********************************************************************)
(* Julia Output                                                       *)
(***********************************************************************)

let julia_string s =
  if !flagJulia then output_string !fidJulia s
;;

let julia_int i =
  if !flagJulia then output_int !fidJulia i
;;

let julia_float f =
  if !flagJulia then output_float !fidJulia f
;;

let julia_newline () =
  if !flagJulia then output_string !fidJulia "\n"
;;

(***********************************************************************)

let julia_separator () =
  julia_newline ();
  julia_string "#";
  for i = 1 to 69 do
    julia_string "-"
  done;
  julia_newline ();
  julia_newline ()
;;

let julia_function name args lines =
  julia_string "function ";
  julia_string name;
  julia_string "(";
  julia_string (String.concat ", " args);
  julia_string ")\n";
  julia_string (String.concat "\n" (List.map ((^) "  ") lines));
  julia_string "\nend\n\n"
;;

let in_quotes id = 
  "\"" ^ id ^ "\""
;;

let dict_entry (k,v) =
  k ^ " => " ^ v
;;

(***********************************************************************)

(* Global List of Parameters *)
let paraList = ref ( []: string list )
;;

let rec is_in id list = 
  match list with
  [] -> false
  | hd :: tail -> (id = hd) || is_in id tail
;;

let is_para id =
  is_in id !paraList
;;


(***********************************************************************)
(** Some auxiliary functions used throughout the code                  *)
(***********************************************************************)

(** 
 *     interval a b 
 *
 * @param `a` lower bound of the interval
 * @param `b` upper bound of the interval
 *
 * @return list of integers [a..b] 
 *)
let rec interval a b =
  if a>b then [] else a::(interval (a+1) b) 
;;

(** 
 *     to_string po v d
 *
 * @param `po` printing function option 
 * @param `v` value to be printed
 * @param `d` default string representation
 *
 * @return string representation of `v`
 *)
let to_string po v d =
  match po with
  | None -> d
  | Some p -> p v 
;;

(**
 *     some_or_default opt def
 *
 * @param `opt` is an option of any type ('a option)
 * @param `def` is the default value
 *
 * @return `def` if option is None, `opt` value otherwise
 *)
let some_or_default opt def =
  match opt with
  | None -> def
  | Some x -> x
;;

(** 
 * The following four functions deal with arithemtic of
 * (nonnegative) rational numers. They are used to 
 * renormalise the weights associated with alternatives
 * of a 'choose' statement, since weights are represented 
 * as rational numbers
 *)

let rec gcd a b = 
  if b = 0 then a else gcd b (a mod b)
;;

(** Cancel any common factors of p and q *)
let normalise (p,q) = 
  let gcd = gcd p q 
  in (p / gcd, q / gcd) 
;;

let add (p1,q1) (p2,q2) = 
  normalise (p1 * q2 + p2 * q1, q1 * q2)
;;

let divide (p1,q1) (p2,q2) = 
  normalise (p1 * q2, p2 * q1)
;;

