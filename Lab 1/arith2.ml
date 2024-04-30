type binop = Add
| Mul
| Div

type expr =
| Const of int
| Binop of binop * expr * expr
  
(* --- Interpreter --- *)
let rec interp e = match e with
  | Const c -> c
  | Binop (Add, e1, e2) -> interp e1 + interp e2
  | Binop (Mul, e1, e2) -> interp e1 * interp e2
  | Binop (Div, e1, e2) -> interp e1 / interp e2

let rec simplify e = match e with
  | Const c -> e
  | Binop (Add, Const 0, e)
  | Binop (Add, e, Const 0) -> simplify e
  | Binop (Add, e1, e2) -> Binop (Add, simplify e1, simplify e2)

  | Binop (Mul, e, Const 0) -> Const 0
  | Binop (Mul, Const 0, e) -> Const 0
  | Binop (Mul, e, Const 1) -> simplify e
  | Binop (Mul, Const 1, e) -> simplify e

  | Binop (Div, e, Const 1) -> simplify e
  | _ -> e  (* Catch-all case *)


exception DivByZero of int

let rec check_div_by_zero e = match e with
  | Const c -> if c = 0 then raise (DivByZero c) else Const c
  | Binop (Div, e1, e2) -> Binop (Div, check_div_by_zero e1, check_div_by_zero e2)
  | _ -> e  (* Catch-all case *)

  let rec expr_to_string e = match e with
  | Const c -> string_of_int c
  | Binop (Mul, e1, e2) -> 
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " * " ^ s2 ^ ")"

  | Binop (Add, e1, e2) -> 
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " + " ^ s2 ^ ")"
  
  | Binop (Div, e1, e2) ->
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " / " ^ s2 ^ ")"