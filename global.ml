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

let julia_seperator () =
  julia_newline ();
  julia_string "#";
  for i = 1 to 69 do
    julia_string "-"
  done;
  julia_newline ();
  julia_newline ()
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

(** Creates list of integers [a..b] *)
let rec interval a b =
  if a>b then [] else a::(interval (a+1) b) 

