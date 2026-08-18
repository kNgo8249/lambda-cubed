{
    open Parser

    exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule read = parse
    | white { read lexbuf }
    | newline { Lexing.new_line lexbuf; read lexbuf }
    | "import" { IMPORT }
    | "=" { EQUALS }
    | ";" { SEMICOLON }
    | "λ" | "\\" | "lambda" { LAMBDA }
    | "." { DOT }
    | '"' { read_string (Buffer.create 16) lexbuf }
    | id { ID (Lexing.lexeme lexbuf) }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }

and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+{ Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
    | _ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("Unterminated filename")) }