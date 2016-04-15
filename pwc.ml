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
    if !flagText   then fidText := open_out !txtName;
    if !flagLaTeX  then fidLaTeX := open_out !texName;
    if !flagJulia  then fidJulia := open_out !julName;
  end
;;

let abort msg =
  if !flagBinary then Sys.remove !binName; 
  if !flagText   then Sys.remove !txtName; 
  if !flagLaTeX  then Sys.remove !texName; 
  if !flagJulia  then Sys.remove !julName;
  output_string stderr msg;
  output_string stderr ". Aborting...\n";
  exit 1
;;

(***********************************************************************)

let show_files () =
  begin
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
    print_string "(c) 2008-16 H.Wiklicky, M.Olejnik\n\n";
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

let position lexbuf = 
  let line = string_of_int lexbuf.lex_curr_p.pos_lnum
  and col  = string_of_int 
               (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
  in  (line, col)
;;

let error lexbuf =
  let line, col = position lexbuf
  and filename = in_quotes !srcName in
  let loc = "File " ^ filename ^ ", line " ^ line ^ ", character " ^ col ^ ":\n"
  and err = "Error: Unexpected token"
  in abort(loc ^ err)
;;

(***********************************************************************)

let main () =

  arguments ();
  start ();
  open_files ();

  try 
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
          begin
            if !flagVerbose then ignore (print_lstmt syntaxLabel);
            if !flagVerbose then print_newline ();
            populateSymTbl declList;
            ignore (print_decls declList);
            ignore (print_blocks syntaxBlocks);
            ignore (print_flow syntaxFlow);
            print_newline ();
            (*-----*)
            ignore (julia_decls declList);
            ignore (julia_blocks syntaxBlocks);
            ignore (julia_flow syntaxFlow);
            print_newline ();
          end;
          success lexBuffer;
        end

      with
        | Parse_error | UnexpToken -> error lexBuffer
        | Failure msg -> abort msg

    end;
  with Sys_error msg -> 
    abort msg
;;

(***********************************************************************)

main ()
;;

(***********************************************************************)
