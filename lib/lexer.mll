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
    | "λ" | "\\" | "lambda" { LAMBDA }
    | "." { DOT }
    | id { ID (Lexing.lexeme lexbuf) }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }