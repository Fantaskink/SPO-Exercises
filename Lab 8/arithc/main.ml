
(* Main file for the arithc compiler *)

open Format
open Lexing

(* Compilation option, to stop just after parsing. *)
let parse_only = ref false

(* Names of the source and output files. *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

(* Compiler options to print with --help option. *)
let options =
  ["-parse-only", Arg.Set parse_only,
   "  To only parse the source file";
   "-o", Arg.String (set_file ofile),
   "<file>  To indicate the name of the output file"]

let usage = "usage: arithc [option] file.exp"

(* To localize a error (e.g. lexing or syntax error) showing line and column
   in the source file *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  (* Parsing the command line *)
  Arg.parse options (set_file ifile) usage;

  (* Check that the name of the source file was given. *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;

  (* And that this file must have extension .exp *)
  if not (Filename.check_suffix !ifile ".exp") then begin
    eprintf "File must have extension .exp\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* By default, the target file has the same name as the source file,
     only the extension changes.*)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".exp" ^ ".s";

  (* Opening the input channel for the source file. *)
  let f = open_in !ifile in

  (* Creating a lexical buffer to recognize tokens and perform lexical analysis. *)
  let buf = Lexing.from_channel f in

  try
  (* Parsing: the function Parser.prog transforms the lexical buffer
       into an abstract syntax tree if no lexical or syntax errors has been
       detected during the parsing phase.
       The function Lexer.token is used by Parser.prog to get the next token
       from the lexer. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;

    (* We stop here if we only do parsing. *)
    if !parse_only then exit 0;

    (* Compiling the abstract syntax tree p. The assembly code resulting
       from this transformation must be written in the target ofile. *)
    Compile.compile_program p !ofile
  with
    | Lexer.Lexing_error c ->
	(* Lexical error. We get the absolute position in the input file
           and we convert it into the line number. *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Lexical Error: %c@." c;
	exit 1
    | Parser.Error ->
	(* Syntax error. Same for the localization. *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Syntax error@.";
	exit 1
    | Compile.VarUndef s->
	(* Incorrect variable usage error during compilation *)
	eprintf
	  "Compilation error: the variable %s is undefined@." s;
	exit 1
