(***********************************************************************)
(* Modules                                                             *)
(***********************************************************************)

open Lexer
open Parser

open Lexing
open Parsing

open Declaration
open Expression
open Statement

open Label
open Block
open Flow

open Global

(***********************************************************************)

let arguments () =
  let arglist = [
    ("-v", Arg.Clear (flagVerbose), "Verbose Mode off  [default: off]");
    ("-V", Arg.Set   (flagVerbose), "Verbose Mode on   ");
    ("-b", Arg.Clear (flagBinary),  "Binary Output off [default: off]");
    ("-B", Arg.Set   (flagBinary),  "Binary Output on  ");
    ("-t", Arg.Clear (flagText),    "Text Output off   [default: off] ");
    ("-T", Arg.Set   (flagText),    "Text Output on    ");
    ("-x", Arg.Clear (flagLaTeX),   "LaTeX Output off  [default: off] ");
    ("-X", Arg.Set   (flagLaTeX),   "LaTeX Output on   ");
    ("-m", Arg.Clear (flagJulia),   "Julia Output off [default: on] ");
    ("-M", Arg.Set   (flagJulia),   "Julia Output on  ");
    ("-u", Arg.Clear (flagUndef),   "Undefined off [default: off] ");
    ("-U", Arg.Set   (flagUndef),   "Undefined used \n")]
  and set_basename s =
    begin
      baseName := s;
      srcName := !baseName^".pw";
      binName := !baseName^".pwi";
      txtName := !baseName^".txt";
      texName := !baseName^".tex";
      julName := !baseName^".jl";
    end
  and usage =
    "Usage:\n\npwc basename [-v/-V][-b/-B][-t/-T][-x/-X][-m/-M][-u/-U]\n"
  in
  Arg.parse arglist (set_basename) usage 
;;

(***********************************************************************)

let open_files () =
  begin
    if !flagVerbose then print_string "Open Output Files";
    if !flagVerbose then print_newline ();
    if !flagBinary then fidBinary := open_out !binName;
    if !flagText then fidText := open_out !txtName;
    if !flagLaTeX then fidLaTeX := open_out !texName;
    if !flagJulia then fidJulia := open_out !julName;
  end
;;

(***********************************************************************)

let show_files () =
  begin
    print_newline ();
    print_string "Source File: "; 
    print_string !srcName; 
    print_newline ();
    print_string "Output File: "; 
    print_string !binName; 
    print_newline ();
  end
;;

let show_stat lexbuf =
  begin
    print_string "Final Position: line ";
    print_int lexbuf.lex_curr_p.pos_lnum;
    print_string " in `";
    print_string !srcName;
    print_string "'";
    print_newline ();
  end
;;

let show_result declList syntaxTree =
  begin
    print_decls declList;
    print_newline ();
    print_string "Program: ";
    print_newline ();
    print_stmt syntaxTree;
    print_newline (); 
  end
;;

(***********************************************************************)

let start () =
  begin
    print_string "pwc 0.98 - pWhile compiler\n";
    print_string "(c) 2008-16 H.Wiklicky, M.Olejnik\n";
    if !flagVerbose then show_files ();
  end
;;

let success lexbuf =
  begin
    if !flagVerbose then show_stat lexbuf;
    if !flagVerbose then print_newline ();
    print_string "Done with `"; 
    print_string !srcName; 
    print_string "'."; 
    print_newline ();
  end
;;

let error lexbuf =
  begin
    print_string "Parsing Error in line ";
    print_int lexbuf.lex_curr_p.pos_lnum;    
    print_string " of ";
    print_string !srcName;
    print_newline ();
  end
;;

(***********************************************************************)

let main () =

  arguments ();
  start ();
  open_files ();

  let srcFile = open_in !srcName in
  let lexBuffer = Lexing.from_channel srcFile in
  begin
    try

      let (declList, syntaxTree) = Parser.prog Lexer.token lexBuffer in
      begin
	
	if !flagVerbose then 
	  show_result declList syntaxTree;
	
	let syntaxLabel = label_stmt syntaxTree in
	let syntaxBlocks = blocks syntaxLabel in
	let syntaxFlow = flow syntaxLabel in
	(*let allocTable = (allocate 1 declList []) in*)
	begin
	  if !flagVerbose then ignore (print_lstmt syntaxLabel);
	  if !flagVerbose then print_newline ();
	  print_newline ();
	  ignore (print_blocks syntaxBlocks);
	  print_newline ();
	  ignore (print_flow (Flow.drop_context syntaxFlow));
	  ignore (print_decls declList);
	  (*ignore (print_allocation allocTable);*)
          (*-----*)
	  ignore (julia_variables declList);
	  ignore (julia_blocks syntaxBlocks);
	  (*ignore (octave_flow syntaxFlow);*)
	  print_newline ();
	end;
	success lexBuffer;
      end

    with Parse_error -> 
      error lexBuffer
  end;
;;

(***********************************************************************)

main ()
;;

(***********************************************************************)
