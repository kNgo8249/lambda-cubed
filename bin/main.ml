open Lambda_cubed

let strategy = ref Eval.CallByName
let file_to_run = ref None
let trace_term = ref true

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

let get_pos lexbuf =
  let pos = lexbuf.Lexing.lex_start_p in
  let fname = if pos.pos_fname = "" then "<stdin>" else pos.pos_fname in
  (fname, string_of_int pos.pos_lnum, string_of_int (pos.pos_cnum - pos.pos_bol + 1))

let red = "\x1b[38;5;9m"
let bold = "\x1b[1m"
let reset = "\x1b[0m"

let parse_with_error lexbuf =
  try 
    Some (Parser.prog Lexer.read lexbuf)
  with
  | Lexer.LexicalError msg -> 
    let (fname, lnum, col) = get_pos lexbuf in
    prerr_endline (bold ^ red ^ "Error: " ^ reset ^ msg ^ " in " ^ fname ^ " on line " ^ lnum ^ " at character " ^ col); None
  | Parser.Error -> 
    let (fname, lnum, col) = get_pos lexbuf in
    prerr_endline (bold ^ red ^ "Error:" ^ reset ^ " Syntax error in " ^ fname ^ " on line " ^ lnum ^ " at character " ^ col); None

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let result = parse_with_error lexbuf in
  close_in ic;
  result

let run_command cmd =
  match cmd with
  | Syntax.Eval expression -> print_endline ("==> " ^ Syntax.string_of_expr (Eval.eval !strategy expression ?f:(
    if !trace_term then 
      Some (fun e -> print_endline ("  --> " ^ Syntax.string_of_expr e))
    else 
      None)))
  | Syntax.Import file -> print_endline ("Importing file " ^ file)
  | Syntax.Def (name, expression) -> print_endline ("Defining " ^ name ^ " as " ^ Syntax.string_of_expr expression)

let rec run cmds =
  match cmds with
  | [] -> ()
  | cmd::rest -> run_command cmd; run rest

let rec ends_with_semi lexbuf prev_token =
  match Lexer.read lexbuf with
  | Parser.EOF -> 
    begin match prev_token with
    | Some Parser.SEMICOLON -> true
    | _ -> false
    end
  | token -> ends_with_semi lexbuf (Some token)
  | exception Lexer.LexicalError msg -> 
    begin match msg with
    | "Unterminated filename" | "Unterminated comment" -> false
    | _ -> true
    end

let rec read_to_semicolon buffer =
  let trimmed = String.trim buffer in
  if String.ends_with ~suffix:";" trimmed then 
    buffer
  else begin
    print_string "...";
    let next_line = read_line() in
    read_to_semicolon (buffer ^ "\n" ^ next_line)
  end

let rec run_repl () =
  print_string "λ> ";
  let line = read_line () in
  if String.trim line = "" then
    run_repl ()
  else begin
    let full_input = read_to_semicolon line in
    let lexbuf = Lexing.from_string full_input in
    (match parse_with_error lexbuf with
    | Some prog -> run prog
    | None -> ());

    run_repl ()
  end

let run_file filename =
  match parse_file filename with
  | Some prog -> run prog
  | None -> exit 1

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !file_to_run with
  | Some filename -> run_file filename
  | None -> run_repl ()