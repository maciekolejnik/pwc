(***********************************************************************)
(* Global: Flags and I/O File Names                                    *)
(***********************************************************************)

let flagVerbose = ref false
and flagBinary  = ref false
and flagText    = ref false
and flagLaTeX   = ref false
and flagOctave  = ref false 
and flagUndef   = ref false
;;

let baseName = ref ""
and srcName  = ref ""
and binName  = ref ""
and txtName  = ref ""
and texName  = ref ""
and octName  = ref ""
;;

let fidVerbose = ref stdout
and fidBinary  = ref stdout
and fidText    = ref stdout
and fidLaTeX   = ref stdout
and fidOctave  = ref stdout
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
(* Octave Output                                                       *)
(***********************************************************************)

let octave_string s =
  if !flagOctave then output_string !fidOctave s
;;

let octave_int i =
  if !flagOctave then output_int !fidOctave i
;;

let octave_float f =
  if !flagOctave then output_float !fidOctave f
;;

let octave_newline () =
  if !flagOctave then output_string !fidOctave "\n"
;;

(***********************************************************************)

let octave_seperator () =
  octave_newline ();
  octave_string "#";
  for i = 1 to 69 do
    octave_string "-"
  done;
  octave_newline ();
  octave_newline ()
;;

(***********************************************************************)

let octave_introw il =
  let elem i =
    begin
    octave_string ", ";
    octave_int i
    end
  in
  if il != [] then 
    begin
    octave_int (List.hd il);
    List.iter elem (List.tl il);
    end
;;

let octave_intcol il =
  let elem i =
    begin
    octave_string "; ";
    octave_int i
    end
  in
  if il != [] then 
    begin
    octave_int (List.hd il);
    List.iter elem (List.tl il);
    end
;;

let octave_floatcol fl =
  let elem f =
    begin
    octave_string "; ";
    octave_float f
    end
  in
  if fl != [] then 
    begin
    octave_float (List.hd fl);
    List.iter elem (List.tl fl);
    end
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

