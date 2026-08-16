open Lambda_cubed.Ast
open Lambda_cubed.Eval
open Lambda_cubed

let test_term = App (App (Lam ("x", Lam ("y", App (Var "x", Var "y"))), Lam ("y", Var "y")), Lam ("z", Var "z"))
let cas_test = App (Lam ("x", Lam ("y", App (Var "x", Var "y"))), Var "y")

let omega_combinator = App (Lam ("x", App (Var "x", Var "x")), Lam ("x", App (Var "x", Var "x")))
let diverge_test = App (Lam ("x", Lam ("y", Var "y")), omega_combinator)

let test_eval () =
  print_endline ("FULL EVALUATION:");
  print_endline ("Original term: " ^ string_of_expr test_term);
  print_endline ("Reduced term: " ^ string_of_expr (eval CallByName test_term));

  print_endline ("\nSTEP BY STEP EVALUATION:");
  print_endline ("Original term: " ^ string_of_expr test_term);
  print_endline ("Reduced term: " ^ string_of_expr (eval CallByName test_term ~f:(fun e -> print_endline (string_of_expr e))));

  print_endline ("\nCAPTURE AVOIDING SUBSTITUTION:");
  print_endline ("Original term: " ^ string_of_expr cas_test);
  print_endline ("Reduced term: " ^ string_of_expr (eval CallByName cas_test));

  print_endline ("\nCBN vs CBV:");
  print_endline ("cbn");
  print_endline ("Original term: " ^ string_of_expr diverge_test);
  print_endline ("Reduced term: " ^ string_of_expr (eval CallByName diverge_test));
  print_endline ("cbv (should diverge)");
  print_endline ("Original term: " ^ string_of_expr diverge_test);
  print_endline ("Reduced term: " ^ string_of_expr (eval CallByValue diverge_test))

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let test_parsing () =
  print_endline (string_of_expr (parse "a b c"));
  print_endline (string_of_expr (parse "\\x.x y"));

  print_endline (string_of_expr (parse "(((λx.(λy.(x y))) (λy.y)) (λz.z))"));
  print_endline (string_of_expr (parse "(((\\x.(\\y.(x y))) (\\y.y)) (\\z.z))"));
  print_endline (string_of_expr (parse "(((lambda x.(lambda y.(x y))) (lambda y.y)) (lambda z.z))"));
  
  print_endline (string_of_expr (parse "\\x     .       x"));
  print_endline (string_of_expr (parse "\\x\t.\tx"));
  print_endline (string_of_expr (parse "\\\nx.x"))

let () =
  (*test_eval ();*)
  test_parsing ()