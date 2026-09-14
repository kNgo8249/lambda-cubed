{
    open Parser

    exception LexicalError of string
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
    | "(*" { comment 0 lexbuf }
    | _ { raise (LexicalError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
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
    | _ { raise (LexicalError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (LexicalError ("Unterminated filename")) }

and comment depth = parse
    | "(*" { comment (depth + 1) lexbuf}
    | "*)" { if depth = 0 then read lexbuf else comment (depth - 1) lexbuf }
    | _ { comment depth lexbuf }
    | eof { raise (LexicalError ("Unterminated comment")) }