(* Base term AST *)
type expr = 
  | Var of string
  | Lam of string * expr
  | App of expr * expr

(* Printing *)
let rec string_of_expr e = 
  match e with
  | Var v -> v
  | Lam (v, e1) -> "(λ" ^ v ^ "." ^ string_of_expr e1 ^ ")"
  | App (e1, e2) -> "(" ^ (string_of_expr e1) ^ " " ^ (string_of_expr e2) ^ ")" 