open Ast

exception NoRuleApplies

let rec subst e s x =
  match e with
  | Var y -> if x=y then s else e
  | Lam (y, e1) -> if x=y then Lam (y, e1) else Lam (y, subst e1 s x)
  | App (e1, e2) -> App (subst e1 s x, subst e2 s x)

let rec step e =
  match e with
  | App (Lam (v, e1), e2) -> subst e1 e2 v
  | App (e1, e2) -> App (step e1, e2)
  | _ -> raise NoRuleApplies

let rec eval e =
  try let e' = step e
    in eval e'
  with NoRuleApplies -> e

let rec eval_print_steps e =
  try let e' = step e
    in print_endline (string_of_expr e'); eval_print_steps e'
  with NoRuleApplies -> e