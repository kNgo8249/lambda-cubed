open Ast

exception NoRuleApplies

module StringSet = Set.Make(String)

let rec free_vars e =
  match e with
  | Var v -> StringSet.singleton v
  | Lam (v, e1) -> StringSet.remove v (free_vars e1)
  | App (e1, e2) -> StringSet.union (free_vars e1) (free_vars e2)

let var_counters = Hashtbl.create 100

let alpha_rename v =
  let v_ctr = 
    match Hashtbl.find_opt var_counters v with
    | None -> 0
    | Some ctr -> ctr + 1 
  in 
  Hashtbl.replace var_counters v v_ctr;
  v ^ "$" ^ string_of_int v_ctr

(* Capture avoiding substitution: e[s/x] = e[s substituted for x] = Substitute s for all free occurrences of x in e *)
let rec subst e s x =
  match e with
  | Var y -> if x=y then s else e
  | Lam (y, e1) -> 
      if x=y then 
        Lam (y, e1) 
      else if not (StringSet.mem y (free_vars s)) then 
        Lam (y, subst e1 s x) 
      else 
        let renamed_y = alpha_rename y in
        let renamed_e1 = subst e1 (Var renamed_y) y in
        Lam (renamed_y, subst renamed_e1 s x)
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