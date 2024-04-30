(* MIPS code generation for the Arith language *)

open Format
open Mips
open Ast

(* exception to signal an undefined variable *)
exception VarUndef of string

(* Frame size, in octets (each local variable will occupy 4 octets of memory,
   i.e. 8 * 4 = 32 bits.) *)
let frame_size = ref 0

(* Global variables are stores in the hashtable *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* For the local variables, we are using an association map whose keys are
   local variable names (strings) and for each variable the associated value
   is its position (in octets) on the stack w.r.t. $fp. *)
module StrMap = Map.Make(String)

(* Compiling expressions. *)
let compile_expr =
  (* Recursive function compile_expr used to generate MIPS code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the pile *)
  let rec comprec (env : int StrMap.t) (next : int) (e : Ast.expr) : Mips.text =
    match e with
    | Cst i ->
      nop (* TODO 1 *)
    | Var x ->
      nop (* TODO 1 *)
    | Binop (o,e1,e2)->
      nop (* TODO 1 *)
    | Letin (x,e1,e2) ->
      if !frame_size = next then frame_size := 4 + !frame_size;
      nop (* TODO 1 *)
  in
  comprec StrMap.empty 0

(* Instruction compilation *)
let compile_instr (i : Ast.stmt): Mips.text =
  match i with
  | Set (x, e) ->
    nop (* TODO 2 *)
  | Print e ->
    nop (* TODO 2 *)


(* Program p compilation and writing the MIPS in the output ofile. *)
let compile_program p ofile =
  let code = List.map compile_instr p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
	label "main" ++
	nop ++ (* TODO 3 *)
	code ++
	nop (* TODO 3 *);
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dword [1] ++ l) genv
          (label "newline" ++ asciiz "\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  (* we "flush" the buffer to make sure that everything has been written
     before we close it. *)
  fprintf fmt "@?";
  close_out f
