
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES
  | RP
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEWLINE
  | MOD
  | MINUS
  | LP
  | IF
  | IDENT of (string)
  | EQUAL
  | EOF
  | END
  | ELSE
  | DIV
  | CST of (Ast.constant)
  | COMMA
  | COLON
  | CMP of (Ast.binop)
  | BEGIN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)
