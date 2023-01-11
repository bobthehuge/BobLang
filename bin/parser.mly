%{
    open Types
%}

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING

%token ADD
%token SUB
%token MUL
%token DIV

%token PRINTLN
%token PRINT

%token LPAREN
%token RPAREN

%token EQ
%token EXCLAM
%token AND
%token OR
%token LANGLE
%token RANGLE

%token DOLLAR
%token <string> ID

%token EOF

%right PRINT PRINTLN
%right EQ

%left ADD SUB LANGLE RANGLE
%left MUL DIV
%left AND OR

%nonassoc EXCLAM

%type <ttype> proc
%type <ttype> atom
%type <ttype list> list(proc)
%type <string> un_op
%type <string> bin_op

%start <ttype list> prog
%%

prog:
    | proc_lst=list(proc); EOF { proc_lst }
    ;

atom:
    | i = INT { Int i }
    | b = BOOL { Bool b }
    | f = FLOAT { Float f }
    | c = CHAR { Char c }
    | s = STRING { String s }
    ;

proc:
    | LPAREN; p=proc; RPAREN { p }
    | a=atom { a }
    | op=un_op; e=proc { new_un_op op e }
    | e1=proc; op=bin_op; e2=proc { new_bin_op op e1 e2 }
    | DOLLAR; x=ID { Var x }
    | x=ID; EQ; e=proc { Set (x, e) }
    | PRINT; e=proc { Print e }
    | PRINTLN; e=proc { Println e }
    ;

%inline un_op:
    | EXCLAM { "not" }
    ;

%inline bin_op:
    | ADD { "add" }
    | SUB { "sub" }
    | MUL { "mul" }
    | DIV { "div" }
    | AND { "and" }
    | OR { "or" }
    | EQ EQ { "eq" }
    | RANGLE { "gt" }
    | LANGLE { "lt" }
    | RANGLE EQ { "geq" }
    | LANGLE EQ { "leq" }
    ;
