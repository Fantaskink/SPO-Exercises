
open Ast
open Format

(* Exception raised to signal a runtime error *)
exception Error of string
let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python
     integers are arbitrary-precision integers (we could use an OCaml
     library for big integers, such as zarith, but we opt for simplicity
     here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python,
     there is no way to modify the length, so a mere OCaml array can be used.
*)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0,
   the empty string, and the empty list are all considered to be
   False, and any other value to be True.
*)
let is_false = function
  | Vnone
  | Vbool false
  | Vstring ""
  | Vlist [||] -> true
  | Vint n -> n = 0
  | _ -> false

  let is_true v = not (is_false v)

(* We only have global functions in Mini-Python *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret `return` *)

exception Return of value

(* Local variables (function parameters and local variables introduced
   by assignments) are stored in a hash table that is passed to the
   following OCaml functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t

(* Interpreting an expression (returns a value) *)

let rec expr ctx = function
  | Ecst Cnone ->
      Vnone
  | Ecst (Cstring s) ->
      Vstring s
  (* arithmetic *)
  | Ecst (Cint n) ->
    let converted_n = Int64.to_int n in
    Vint converted_n
  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      begin match op, v1, v2 with
        | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
        | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
        | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
        | (Bdiv | Bmod), Vint _, Vint 0 -> error "division by zero" 
        | Bdiv, Vint n1, Vint n2 -> Vint (n1/n2) 
        | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2) 
        | Beq, Vint n1, Vint n2  -> if n1 = n2 then Vbool true else Vbool false
        | Bneq, Vint n1, Vint n2 -> if n1 != n2 then Vbool true else Vbool false
        | Blt, Vint n1, Vint n2  -> if n1 < n2 then Vbool true else Vbool false
        | Ble, Vint n1, Vint n2  -> if n1 <= n2 then Vbool true else Vbool false
        | Bgt, Vint n1, Vint n2  -> if n1 > n2 then Vbool true else Vbool false
        | Bge, Vint n1, Vint n2  -> if n1 >= n2 then Vbool true else Vbool false
        | Badd, Vstring s1, Vstring s2 ->
            assert false (* TODO (question 3) *)
        | Badd, Vlist l1, Vlist l2 ->
            assert false (* TODO (question 5) *)
        | _ -> error "unsupported operand types"
      end
      | Eunop (Uneg, e1) ->
        begin match expr ctx e1 with
         | Vint n -> Vint (-n)
         | _ -> error "unsupported operand types" end (* DONE (question 1) *)
  (* Boolean *)
  | Ecst (Cbool b) ->
    if b then 
        Vbool true
    else
        Vbool false
  | Ebinop (Band, e1, e2) ->
    let v1 = expr ctx e1 in
    let v2 = expr ctx e2 in
    if is_true v1 && is_true v2 then
        Vbool true
    else
        Vbool false
  | Ebinop (Bor, e1, e2) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      if is_true v1 || is_true v2 then
        Vbool true
      else
        Vbool false
  | Eunop (Unot, e1) ->
      let v1 = expr ctx e1 in
      if is_true v1 then
        Vbool false
      else
        Vbool true
  | Eident {id} ->
      assert false (* TODO (question 3) *)
  (* function call *)
  | Ecall ({id="len"}, [e1]) ->
      assert false (* TODO (question 5) *)
  | Ecall ({id="list"}, [Ecall ({id="range"}, [e1])]) ->
      assert false (* TODO (question 5) *)
  | Ecall ({id=f}, el) ->
      assert false (* TODO (question 4) *)
  | Elist el ->
      assert false (* TODO (question 5) *)
  | Eget (e1, e2) ->
      assert false (* TODO (question 5) *)

(* Interpreting a statement

   returns nothing but may raise exception `Return` *)

and stmt ctx = function
  | Seval e ->
      ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e); printf "@."
  | Sblock bl ->
      block ctx bl
  | Sif (e, s1, s2) ->
      let condition = expr ctx e in
      if is_true condition then
        stmt ctx s1
      else
        stmt ctx s2
  | Sassign ({id}, e1) ->
      assert false (* TODO (question 3) *)
  | Sreturn e ->
      assert false (* TODO (question 4) *)
  | Sfor ({id}, e, s) ->
      assert false (* TODO (question 5) *)
  | Sset (e1, e2, e3) ->
      assert false (* TODO (question 5) *)

(* Interpreting a block (a sequence of statements) *)

and block ctx = function
  | [] -> ()
  | s :: sl -> stmt ctx s; block ctx sl

(* Interpreting a file
   - `dl` is a list of function definitions (see type `def` in ast.ml)
   - `s` is a statement (the toplevel code)
*)

let file (dl, s) =
  (* TODO (question 4) *)
  stmt (Hashtbl.create 16) s
