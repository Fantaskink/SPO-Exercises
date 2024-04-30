
(* Library to generate MIPS code

   by 2008 Jean-Christophe Filliâtre (CNRS)
   - first version

   2013 Kim Nguyen (Université Paris Sud)
   - subtypes text and data
   - ghost types for oreg and oi
   - more ops and directives
   - stack management
   - ocamldoc xs

   2024 Léon Gondelman (AAU)
   - just translated to English description
*)

(** {0 Library for writing MIPS programs } *)


(** Module {!Mips} allows to write MIPS code inside OCaml code without
    need for a preprocessor. A complete example is given in
    {{:#1_Exemple} below in the section example}. *)

type 'a asm
(** abstract type to represent assembly code. The parameter
    ['a] is used as ghost type. *)

type text = [ `text ] asm
(** type representing assembly code in the section following the text directive. *)

type data = [ `data ] asm
(** type representing assembly code in the section following the data directive. *)

type program = {
  text : text;
  data : data;
}
(** a program consists of a text zone and data zone *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] pretty-printsthe code of the program [p] in the formatter
      [fmt] *)

val print_in_file: file:string -> program -> unit

type register
(** Abstract type for registers *)

val v0 : register
val v1 : register
val a0 : register
val a1 : register
val a2 : register
val a3 : register
val t0 : register
val t1 : register
val t2 : register
val t3 : register
val s0 : register
val s1 : register
val ra : register
val sp : register
val fp : register
val gp : register
val zero : register
(** Constantes representing the registrers. [zero] is fixed to 0 *)


type label = string
(** Address labes are represented by strings. *)

type 'a operand
val oreg : register operand
val oi : int operand
val oi32 : int32 operand

(** abstract type to represent the last operand of an arithmetic expression
    as well as 3 constants (either a register, or an integer, or a 32b integer).
*)



(** {1 Arithmetic operations } *)


val li : register -> int -> text
val li32 : register -> int32 -> text
(** Loading integer constants *)

val abs : register -> register -> text
(** [abs r1 r2] stores in r1 the absolute value of r2 *)

val neg : register -> register -> text
(** [neg r1 r2] stores in r1 the opposite of r2 *)

val add : register -> register -> 'a operand -> 'a -> text
val sub : register -> register -> 'a operand -> 'a -> text
val mul : register -> register -> 'a operand -> 'a -> text
val rem : register -> register -> 'a operand -> 'a -> text
val div : register -> register -> 'a operand -> 'a -> text

(** The 5 base arithmetic operations: [add rdst rsrc1 ospec o]
    stores destination register rdst the result of operation between source register rscr1 and o.
    The constant ospec specifies whether o is an immediate, an immediate over 32 bits or a register.
    Example:

   [add v0 v1 oreg v2]

   [div v0 v1 oi 424]

   [sub t0 a0 oi32 2147483647l]
 *)

(** {1 Logical Operations } *)

val and_ : register -> register -> register -> text
val or_ : register -> register -> register -> text
val not_ : register -> register -> text
val clz : register -> register -> text
(** Logical operations "and", "or", and "not" manipulating bits pointwise
    and "clz" (counting leading zero) *)


(** {1 Comparisons } *)

val seq : register -> register -> register -> text
val sge : register -> register -> register -> text
val sgt : register -> register -> register -> text
val sle : register -> register -> register -> text
val slt : register -> register -> register -> text
val sne : register -> register -> register -> text
  (** conditional [sop ra rb rc] sets [ra] to 1 if [rb op rc] and to 0
      otherwise (eq : ==, ge : >=, gt : >, le : <=, lt : <=,
      ne : !=) *)

(** {1 Jumps } *)

val b : label -> text
(** unconditional jump *)

val beq : register -> register -> label -> text
val bne : register -> register -> label -> text
val bge : register -> register -> label -> text
val bgt : register -> register -> label -> text
val ble : register -> register -> label -> text
val blt : register -> register -> label -> text
(** [bop ra rb label] branches to the label [label] if [ra op rb] *)

val beqz : register -> label -> text
val bnez : register -> label -> text
val bgez : register -> label -> text
val bgtz : register -> label -> text
val blez : register ->  label -> text
val bltz : register ->  label -> text
(** [bopz ra rb label] branchse to le label [label] if [ra op 0] *)


val jr : register -> text
(** [jr r] Continues the execution at the address specified by the register [r] *)
val jal : label -> text
(** [jal l] Continues the execution at the address specified by the label [l],
    and stores the return address in $ra.
*)
val jalr : register -> text
(** [jalr r]  Continues the execution at the address specified by the
    register [r], and stores the return address in $ra.
*)

(** {1 Reading from / writing to the memory } *)

type 'a address
(** abstract type to represent memory addresses *)

val alab : label address
val areg : (int * register) address
(** The adresses are either given by a label, or by a pair (offset, register) *)

val la : register -> 'a address -> 'a -> text
(** [la reg alab "foo"] stores in [reg] the address of the label "foo"
    [la reg1 areg (x, reg2)] stores in [reg1] the address stores in the register
    [reg2] at the offset of [x] octets
 *)

val lbu : register -> 'a address -> 'a -> text
(** stores the octet at a given address without sign extension (value between 0 and 255) *)
val lw : register -> 'a address -> 'a -> text
(** stores 32bits integer at a given address *)
val sb : register -> 'a address -> 'a -> text
(** writes the 8 least significant bits of a given register at a given address *)
val sw : register -> 'a address -> 'a -> text
(** write the content of the register at a given address *)
val move : register -> register -> text

(** {1 Miscellaneous } *)

val nop : [> ] asm
(** Empty instruction instruction. Can be used both in .text and .data *)

val label : label ->  [> ] asm
(** A label. Can be used both in .text and .data *)

val syscall : text
(** System call instruction *)

val comment : string -> [> ] asm
(** Includes a comment in the generated code. Can be used both in .text and .data *)

val align : int ->  [> ] asm
(** [align n] alignes the code following an instruction on 2^n octets *)

val asciiz : string -> data
(** places a string value in the data zone *)

val dword : int list -> data
(** places a list of memory words in the data zone *)

val address : label list -> data
(** places a list of adresses (denoted by labels) in data zone *)

val space: int -> data
(** [space n] allocates [n] octets in the data segment *)

val inline: string -> [> ] asm
(** [inline s] copies the string  [s] as such in the assembly file *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
(** concatenates two pieces of code (either text with text, or data with
    data) *)

(** {1 Manipulation of the stack} *)

val push : register -> text
(** [push r] stores the content of [r] at the top of the stack
    Note : $sp points to the addrees of the last occupied cell of the stack *)

val pop : register -> text
(** [pop r] stores the mot at the top of the stack in [r] et pops the stack head *)

val popn: int -> text
(** [popn n] pops [n] octets from the stack *)

val peek : register -> text
(** [peek r] stores the word at the stack head in [r] without popping the stack head *)

(** {1 Example } *)

(** In the the program below, on the left is pure MIPS code and on the right is
    its representation using OCaml commands above. The program loads two
    constants, performs a few arithmetic  operations and prints the result on
    the screen.

    {[
        .text                                                |  { text =
        main:                                                |        label "main"
         #stores 42 in $a0 and 23 in $a1                     |    ++  comment "stores 42 in $a0 and 23 in $a1"
         li $a0,  42                                         |    ++  li  a0 42
         li $a1,  23                                         |    ++  li  a1 23
         mul $a0, $a0, $a1                                   |    ++  mul a0 a0 oreg a1 (* we use  oreg to indicate that the
                                                             |                             last operand is a register *)
         #stores the content of $a0 on the stack             |    ++  comment "stores the content of $a0 on the stack"
         sub $sp, $sp, 4                                     |    ++  sub sp sp oi 4
         sw  $a0,  0($sp)                                    |    ++  sw a0 areg (0, sp)
                                                             |
         jal print_int                                       |    ++  jal "print_int"
                                                             |
         #termines                                           |    ++  comment "termines"
         li $v0, 10                                          |    ++  li v0 10
         syscall                                             |    ++  syscall
                                                             |
      print_int:                                             |    ++  label "print_int"
         lw $a0,  0($sp)                                     |    ++  lw a0 areg (0, sp)
         add $sp, $sp, 4                                     |    ++  add sp sp oi 4
         li $v0, 1                                           |    ++  li v0 1
         syscall                                             |    ++  syscall
         la $a0, newline                                     |    ++  la a0 alab "newline"
         li $v0, 4                                           |    ++  li v0  4
         syscall                                             |    ++  syscall
         jr $ra                                              |    ++  jr ra  ; (* end the of text label *)
                                                             |
        .data                                                |    data =
       newline:                                              |        label "newline"
        .asciiz  "\n"                                        |    ++  asciiz "\n" ;
                                                             |  } (* end of the OCaml record *)
    ]}
*)
