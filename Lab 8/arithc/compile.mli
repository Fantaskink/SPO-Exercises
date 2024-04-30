
exception VarUndef of string
  (** exception to signal an undefined variable *)

val compile_program : Ast.program -> string -> unit
  (** [compile_program p f] compiles the program [p] and writes
      the corresponding  MIPS code in the file [f] *)
