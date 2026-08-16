%{
    open Ast
%}

%token LAMBDA
%token<string> ID
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%start <Ast.expr> prog
%%

prog:
    | e = expr; EOF { e }

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