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
    | Call of string * (ttype list)
    | Return of ttype
    | Func of (string list) * (ttype list)

let new_un_op ttype v =
    match ttype with
    | "not" -> Not v
    | "sub" -> Sub((Int 0), v)
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

let to_printable dt =
    match dt with
    | Null -> "null"
    | Int i -> string_of_int i
    | Bool b -> if b then "true" else "false"
    | Float f -> string_of_float f
    | Char c -> String.make 1 c
    | String s -> s
    | _ -> "proc"

let type_of x =
    match x with
    | Null -> "Null"
    | Int _ -> "Int"
    | Bool _ -> "Bool"
    | Float _ -> "Float"
    | Char _ -> "Char"
    | String _ -> "String"
    | _ -> "Proc"

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
    | _ -> failwith (Printf.sprintf "can't convert %s to float" (type_of x))

let to_char x =
    match x with
    | Int i -> Char (char_of_int i)
    | Char _ -> x
    | _ -> failwith (Printf.sprintf "can't convert %s to char" (type_of x))

let to_string x =
    match x with
    | Int i -> String (string_of_int i)
    | Float f -> String (string_of_float f)
    | Char c -> String (String.make 1 c)
    | String _ -> x
    | _ -> failwith (Printf.sprintf "can't convert %s to string" (type_of x))


