%{
    open Syntax
%}

%token LAMBDA
%token<string> ID
%token<string> STRING
%token DOT
%token LPAREN
%token RPAREN
%token IMPORT
%token EQUALS
%token SEMICOLON
%token EOF

%start <Syntax.command list> prog
%%

prog:
    | commands = list(command); EOF { commands }
    ;

command:
    | e = expr; SEMICOLON { Eval e }
    | IMPORT; filename = STRING; SEMICOLON { Import filename }
    | name = ID; EQUALS; e = expr; SEMICOLON { Def (name, e) }
    ;

expr:
    | e = abs { e }
    | e = app { e }
    ;

abs:
    | LAMBDA; v = ID; DOT; e = expr { Lam (v, e) }
    ;

app:
    | e1 = app; e2 = atom { App (e1, e2) } 
    | e = atom { e }
    ;

atom:
    | LPAREN; e = expr; RPAREN { e }
    | v = ID { Var v }
    ;