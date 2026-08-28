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

type eval_strat = CallByName | CallByValue

let is_val e =
  match e with
  | Lam _ -> true
  | _ -> false

let rec step_cbn e =
  match e with
  | App (Lam (v, e1), e2) -> subst e1 e2 v
  | App (e1, e2) -> App (step_cbn e1, e2) 
  | _ -> raise NoRuleApplies

let rec step_cbv e =
  match e with
  | App (Lam (v, e1), v2) when is_val v2 -> subst e1 v2 v
  | App (v1, e2) when is_val v1 -> App (v1, step_cbv e2) 
  | App (e1, e2) -> App (step_cbv e1, e2)
  | _ -> raise NoRuleApplies

let rec step strat e =
  match strat with 
  | CallByName -> step_cbn e
  | CallByValue -> step_cbv e

let rec eval strat ?(f=fun x -> ()) e =
  match step strat e with
  | e' -> f e'; eval strat e' ~f
  | exception NoRuleApplies -> e