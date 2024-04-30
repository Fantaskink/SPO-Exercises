type binop = Add
| Mul

type expr =
| Const of int
| Binop of binop * expr * expr

(* Values *)
let one = Const 1
let two = Const 2
let ten = Binop (Add, Const 3, Const 7)
let e42 = Binop (Add, Binop (Add, Const 12, Const 20), Const 10)
let e42 = Binop (Add, Binop (Add, Const 12, Const 20), ten)

(*  2*(20 + 1)  *)
let result = Binop (Mul, Const 2, Binop (Add, Const 20, Const 1))

(* Two ways of declaring functions *)
let square (e : expr) : expr = Binop (Mul, e, e)
let square e = Binop (Mul, e, e)

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

let () = print_endline (expr_to_string (square two))

(* --- Interpreter --- *)
let rec interp e = match e with
  | Const c -> c
  | Binop (Add, e1, e2) -> interp e1 + interp e2
  | Binop (Mul, e1, e2) -> interp e1 * interp e2

let _ = interp (square one)

let rec simplify e = match e with
  | Const c -> e
  | Binop (Add, Const 0, e)
  | Binop (Add, e, Const 0) -> simplify e
  | Binop (Add, e1, e2) -> Binop (Add, simplify e1, simplify e2)

  | Binop (Mul, e, Const 0) -> Const 0
  | Binop (Mul, Const 0, e) -> Const 0
  | Binop (Mul, e, Const 1) -> simplify e
  | Binop (Mul, Const 1, e) -> simplify e
  | _ -> e  (* Catch-all case *)


(* ----------------------------- Exceptions ----------------------------------*)

exception Negative_Const

let rec check_valid e = match e with
  | Const c -> if c < 0 then raise Negative_Const else Const c
  | Binop (Add, e1, e2) ->
    Binop (Add, check_valid e1, check_valid e2)
  | Binop (Mul, e1, e2) -> 
    Binop (Mul, check_valid e1, check_valid e2)

let c = check_valid (Const (-1))

exception Negative_Const of int

let rec check_valid e = match e with
  | Const c -> if c < 0 then raise (Negative_Const c) else Const c
  | Binop (Add, e1, e2) -> Binop (Add, check_valid e1, check_valid e2)
  | Binop (Mul, e1, e2) -> Binop (Mul, check_valid e1, check_valid e2)


