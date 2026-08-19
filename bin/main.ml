open Lambda_cubed

let strategy = ref Eval.CallByName
let file_to_run = ref None

let usage_msg = "Usage: lambda-cubed [options]\n       lambda-cubed [options] <filename>\n\nOptions:"

let anon_fun filename = 
  match !file_to_run with
  | None -> file_to_run := Some filename
  | Some _ -> raise (Arg.Bad ("Too many arguments. Only specify one file to run or none for REPL mode"))

let select_strat strat = 
  match strat with
  | "cbn" -> strategy := Eval.CallByName 
  | "cbv" -> strategy := Eval.CallByValue
  | s -> raise (Arg.Bad ("'" ^ s ^ "' is not a valid evaluation strategy. Choices are: cbv, cbn"))

let speclist = 
  [
    ("--strat", Arg.String select_strat, "  Evaluation strategy: cbv or cbn (default: cbn)")
  ]

let rec run_repl () =
  print_endline "λ> "

let run_file filename =
  print_endline ("Run file: " ^ filename)

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !file_to_run with
  | Some filename -> run_file filename
  | None -> run_repl ()