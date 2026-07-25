open Lambda_cubed.Ast

let test_term = App (App (Lam ("x", Lam ("y", App (Var "x", Var "y"))), Lam ("y", Var "y")), Lam ("z", Var "z"))

let () = print_endline (string_of_expr test_term)