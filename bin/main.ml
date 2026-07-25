open Lambda_cubed.Ast
open Lambda_cubed.Eval

let test_term = App (App (Lam ("x", Lam ("y", App (Var "x", Var "y"))), Lam ("y", Var "y")), Lam ("z", Var "z"))

let () =
  print_endline ("FULL EVALUATION:");
  print_endline ("Original term: " ^ string_of_expr test_term);
  print_endline ("Reduced term: " ^ string_of_expr (eval test_term));

  print_endline ("\nSTEP BY STEP EVALUATION:");
  print_endline ("Original term: " ^ string_of_expr test_term);
  print_endline ("Reduced term: " ^ string_of_expr (eval_print_steps test_term))