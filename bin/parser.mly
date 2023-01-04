%{
    open Types
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID
%token SET
%token ADD
%token SUB
%token MUL
%token DIV
%token PRINTLN
%token PRINT
%token LPAREN
%token RPAREN
%token EOF

%start <Types.ttype> prog
%%

prog:
    | p = proc { p }
    | EOF { Eof }
    ;

atom:
    | i = INT { Int i }
    | f = FLOAT { Float f }
    | c = CHAR { Char c }
    | s = STRING { String s }
    ;

param:
    | b=atom { b }
    | x = ID { Var x }
    | LPAREN; p=proc; RPAREN { p }
    ;


proc: 
    | b=atom { b }
    | SET; x=ID; e=param { Set (x, e) }
    | ADD; e1=param; e2=param { Add (e1,e2) }
    | SUB; e1=param; e2=param { Sub (e1,e2) }
    | MUL; e1=param; e2=param { Mul (e1,e2) }
    | DIV; e1=param; e2=param { Div (e1,e2) }
    | PRINT; e=param { Print e }
    | PRINTLN; e=param { Println e }
    ;
