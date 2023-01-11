type ttype =
    | Null
    | Int of int 
    | Bool of bool
    | Float of float 
    | Char of char 
    | String of string
    | Var of string
    | Set of string * ttype
    | Add of ttype * ttype
    | Sub of ttype * ttype
    | Mul of ttype * ttype
    | Div of ttype * ttype
    | Neq of ttype * ttype
    | Eq of ttype * ttype
    | Gt of ttype * ttype
    | Lt of ttype * ttype
    | Geq of ttype * ttype
    | Leq of ttype * ttype
    | Not of ttype
    | And of ttype * ttype
    | Or of ttype * ttype
    | Print of ttype
    | Println of ttype

let new_un_op ttype v =
    match ttype with
    | "not" -> Not v
    | _ -> failwith (Printf.sprintf "Type '%s' is not valid at this point" ttype)

let new_bin_op ttype v1 v2 =
    match ttype with
    | "add" -> Add (v1, v2)
    | "sub" -> Sub (v1, v2)
    | "mul" -> Mul (v1, v2)
    | "div" -> Div (v1, v2)
    | "neq" -> Neq (v1, v2)
    | "eq" -> Eq (v1, v2)
    | "gt" -> Gt (v1, v2)
    | "lt" -> Lt (v1, v2)
    | "geq" -> Geq (v1, v2)
    | "leq" -> Leq (v1, v2)
    | "and" -> And (v1, v2)
    | "or" -> Or (v1, v2)
    | _ -> failwith (Printf.sprintf "Type '%s' is not valid at this point" ttype)

let new_val ttype value =
    match ttype with
    | "int" -> Int (int_of_string value)
    | "bool" -> Bool (value = "true")
    | "float" -> Float (float_of_string value)
    | "char" -> if String.length value > 1 then failwith "cannot convert type string to type Char"
                else Char (value.[0])
    | "string" -> String value
    | _ -> failwith (Printf.sprintf "cannot resolve type '%s'" ttype)

let type_of x =
    match x with
    | Null -> "Null"
    | Int _ -> "Int"
    | Bool _ -> "Bool"
    | Float _ -> "Float"
    | Char _ -> "Char"
    | String _ -> "String"
    | _ -> "Proc"

let rec to_printable x =
    match x with
    | Null -> "null"
    | Int i -> string_of_int i
    | Bool b -> if b then "true" else "false"
    | Float f -> string_of_float f
    | Char c -> Printf.sprintf "%s" (String.make 1 c)
    | String s -> s
    | Var _ -> "Var"
    | Set (id, dt) -> Printf.sprintf "set %s %s" id (to_printable dt)
    | Add (e1, e2) -> Printf.sprintf "add %s %s" (to_printable e1) (to_printable e2)
    | Sub (e1, e2) -> Printf.sprintf "sub %s %s" (to_printable e1) (to_printable e2)
    | Mul (e1, e2) -> Printf.sprintf "mul %s %s" (to_printable e1) (to_printable e2)
    | Div (e1, e2) -> Printf.sprintf "div %s %s" (to_printable e1) (to_printable e2)
    | Eq (e1, e2) -> Printf.sprintf "eq %s %s" (to_printable e1) (to_printable e2)
    | Neq (e1, e2) -> Printf.sprintf "neq %s %s" (to_printable e1) (to_printable e2)
    | Gt (e1, e2) -> Printf.sprintf "gt %s %s" (to_printable e1) (to_printable e2)
    | Lt (e1, e2) -> Printf.sprintf "lt %s %s" (to_printable e1) (to_printable e2)
    | Geq (e1, e2) -> Printf.sprintf "geq %s %s" (to_printable e1) (to_printable e2)
    | Leq (e1, e2) -> Printf.sprintf "leq %s %s" (to_printable e1) (to_printable e2)
    | And (e1, e2) -> Printf.sprintf "and %s %s" (to_printable e1) (to_printable e2)
    | Or (e1, e2) -> Printf.sprintf "or %s %s" (to_printable e1) (to_printable e2)
    | Not dt -> Printf.sprintf "not %s" (to_printable dt)
    | Print dt -> Printf.sprintf "print %s" (to_printable dt)
    | Println dt -> Printf.sprintf "println %s" (to_printable dt)

let debug x =
    let ttype = type_of x
    and value = to_printable x
    in Printf.sprintf "<%s: %s>" ttype value

let to_int x =
    match x with
    | Int _ -> x
    | Float f -> Int (int_of_float f)
    | Char c -> Int (Char.code c)
    | String s -> Int (int_of_string s)
    | _ -> failwith (Printf.sprintf "can't convert %s to int" (type_of x))

let to_float x =
    match x with
    | Int i -> Float (float_of_int i)
    | Float _ -> x
    | String s -> Float (float_of_string s)
    | _ -> failwith (Printf.sprintf "can't convert %s to int" (type_of x))

let to_char x =
    match x with
    | Int i -> Char (char_of_int i)
    | Char _ -> x
    | _ -> failwith (Printf.sprintf "can't convert %s to int" (type_of x))

let to_string x =
    match x with
    | Int i -> String (string_of_int i)
    | Float f -> String (string_of_float f)
    | Char c -> String (String.make 1 c)
    | String _ -> x
    | _ -> failwith (Printf.sprintf "can't convert %s to int" (type_of x))


