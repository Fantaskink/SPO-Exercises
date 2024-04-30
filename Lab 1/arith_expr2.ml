type binop = Add | Mul | Div

type expr =
| Const of int
| Binop of binop * expr * expr

let rec expr_to_string e = match e with
  | Const c -> string_of_int c
  | Binop (Add, e1, e2) ->
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " + " ^ s2 ^ ")"
  | Binop (Mul, e1, e2) -> 
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " * " ^ s2 ^ ")"
  | Binop (Div, e1, e2) -> 
    let s1 = expr_to_string e1 in
    let s2 = expr_to_string e2 in
    "(" ^ s1 ^ " / " ^ s2 ^ ")"

exception DivByZero

let rec interp e = match e with
  | Const c -> c
  | Binop (Add, e1, e2) -> interp e1 + interp e2
  | Binop (Mul, e1, e2) -> interp e1 * interp e2 
  | Binop (Div, e1, e2) ->
    if interp e2 = 0 then raise DivByZero
    else interp e1 / interp e2

    let rec simplify e = match e with
    | Const c -> e
    | Binop (Add, Const 0, e)
    | Binop (Add, e, Const 0) -> e
    | Binop (Add, e1, e2) -> Binop (Add, e1, e2)
    
    | Binop (Mul, Const 0, e) 
    | Binop (Mul, e, Const 0) -> Const 0
    | Binop (Mul, e1, e2) -> Binop (Mul, e1, e2)

    | Binop (Div, e, Const 1) -> e
    | Binop (Div, e1, e2) -> Binop (Div, e1, e2)


let rec expr_to_string e = match e with
    | Const c -> string_of_int c
    | Binop (Add, e1, e2) ->
      let s1 = expr_to_string e1 in
      let s2 = expr_to_string e2 in
      "(" ^ s1 ^ " + " ^ s2 ^ ")"
    | Binop (Mul, e1, e2) -> 
      let s1 = expr_to_string e1 in
      let s2 = expr_to_string e2 in
      "(" ^ s1 ^ " * " ^ s2 ^ ")"
    
    | Binop (Div, e1, e2) -> 
      let s1 = expr_to_string e1 in
      let s2 = expr_to_string e2 in
      "(" ^ s1 ^ " / " ^ s2 ^ ")"

exception Negative_Const of int

let rec check_valid e = match e with
  | Const c -> if c < 0 then raise (Negative_Const c) else Const c
  | Binop (Add, e1, e2) ->
      Binop (Add, check_valid e1, check_valid e2)
  
  | Binop (Mul, e1, e2) ->
      Binop (Mul, check_valid e1, check_valid e2)
  | Binop (Div, e, Const 0) -> raise DivByZero
  | Binop (Div, e1, e2) ->
      Binop (Div, check_valid e1, check_valid e2)


let print_expr e =
  try
    Printf.printf "%s\n" (expr_to_string  (check_valid e))
  with Negative_Const c ->
    Printf.printf "Invalid input: the constant %d is a negative number\n" c
  | DivByZero -> 
    Printf.printf "Invalid input: division by zero\n"
  

let _ = print_expr (Binop (Add, Const 1, Const 2))
let _ = print_expr (Binop (Add, Const 1, Const (-2)))
let _ = print_expr (Binop (Mul, Const 1, Const 2))
let _ = print_expr (Binop (Mul, Const 1, Const (-2)))
let _ = print_expr (Binop (Div, Const 1, Const 2))
let _ = print_expr (Binop (Div, Const 1, Const (-2)))
let _ = print_expr (Binop (Div, Const 1, Const 0))

