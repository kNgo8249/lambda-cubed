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

let rec expand_defs defs e =
  match defs with
  | [] -> e
  | (name, value)::rest -> expand_defs rest (Eval.subst e value name)

let eval_expr e =
  Eval.eval !strategy e ?f:(
    if !trace_term then 
      Some (fun e -> print_endline ("  --> " ^ Syntax.string_of_expr e))
    else 
      None
  )

let run_command defs cmd =
  match cmd with
  | Syntax.Eval expression -> 
      let expanded = expand_defs defs expression in
      let value = eval_expr expanded in
      print_endline ("==> " ^ Syntax.string_of_expr value); defs
  | Syntax.Import file -> print_endline ("Importing file " ^ file); defs
  | Syntax.Def (name, expression) ->
      let expanded = expand_defs defs expression in
      let value = eval_expr expanded in
      print_endline (name ^ " = " ^ Syntax.string_of_expr value); 
      defs @ [(name, value)]

let rec run defs cmds =
  match cmds with
  | [] -> defs
  | cmd::rest -> 
    let new_defs = run_command defs cmd in 
    run new_defs rest

let ends_with_semi buffer =
  let lexbuf = Lexing.from_string buffer in
  let rec loop prev =
    match Lexer.read lexbuf with
    | Parser.EOF -> prev = Some Parser.SEMICOLON
    | token -> loop (Some token)
    | exception Lexer.LexicalError _ -> false
  in
  loop None

let rec read_til_semi buffer =
  if ends_with_semi buffer then
    buffer
  else begin
    print_string "...";
    let next_line = read_line () in
    read_til_semi (buffer ^ "\n" ^ next_line)
  end

let rec run_repl defs =
  print_string "λ> ";
  let line = read_line () in
  if String.trim line = "" then
    run_repl defs
  else begin
    let full_input = read_til_semi line in
    let lexbuf = Lexing.from_string full_input in
    match parse_with_error lexbuf with
    | Some prog -> 
        let new_defs = run defs prog in
        run_repl new_defs
    | None -> run_repl defs
  end

let run_file filename =
  match parse_file filename with
  | Some prog -> ignore (run [] prog)
  | None -> exit 1

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !file_to_run with
  | Some filename -> run_file filename
  | None -> run_repl []