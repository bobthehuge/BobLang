%{
    open Types
    open Memory
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

%token RETURN

%token LPAREN
%token RPAREN

%token EQ
%token EXCLAM
%token AND
%token OR
%token LANGLE
%token RANGLE

%token COMMA
%token DOLLAR
%token <string> ID

%token EOF

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
    | id=ID; EQ; e=proc { Set (id, e) }
    | id=ID; LPAREN; params=separated_list(COMMA, proc); RPAREN { Call (id, params) }
    | RETURN; e=proc { Return e }
    ;

%inline un_op:
    | EXCLAM { "not" }
    | SUB { "sub" }
    ;

%inline bin_op:
    | ADD { "add" }
    | SUB { "sub" }
    | MUL { "mul" }
    | DIV { "div" }
    | AND { "and" }
    | OR { "or" }
    | EXCLAM EQ { "neq" }
    | EQ EQ { "eq" }
    | RANGLE { "gt" }
    | LANGLE { "lt" }
    | RANGLE EQ { "geq" }
    | LANGLE EQ { "leq" }
    ;
