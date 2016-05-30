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
    ("-t", Arg.Clear (flagText),    "Text Output off   [default: off] ");
    ("-T", Arg.Set   (flagText),    "Text Output on    ");
    ("-j", Arg.Clear (flagJulia),   "Julia Output off [default: on] ");
    ("-J", Arg.Set   (flagJulia),   "Julia Output on  ");
    ("-o", Arg.Clear (flagOpt),     "Optimisations off [default: off] ");
    ("-O", Arg.Set   (flagOpt),     "Optimisations used");
    ("-e", Arg.Set_string (inputProgram), "Input program passed as string \n")]
  and set_filenames s =
    begin try 
      baseName := String.sub s 0 (String.index s '.')
    with Not_found -> 
      baseName := s
    end;
    begin
      srcName := s;
      txtName := !baseName ^ ".txt";
      julName := !baseName ^ ".jl"
    end
  and usage =
    "Usage:\n\npwc [-e program | filename] [-v/-V][-t/-T][-j/-J][-o/-O]\n"
  in
  begin
    Arg.parse arglist (set_filenames) usage; 
    (** check that program was passed either as string or as file *)
    if !baseName = "" && !inputProgram = "" 
    then begin 
      Arg.usage arglist usage; 
      exit 1 
    end
  end
;;

(***********************************************************************)

let open_files () =
  begin
    if !flagVerbose then print_string "Open Output Files";
    if !flagVerbose then print_newline ();
    if !flagText   then fidText := open_out !txtName;
    if !flagJulia  then fidJulia := open_out !julName;
  end
;;

let abort msg =
  if !flagText   then Sys.remove !txtName; 
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
    print_string !julName; 
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

    let lexBuffer = 
      if !baseName = "" 
      then Lexing.from_string !inputProgram
      else Lexing.from_channel (open_in !srcName) in


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
