type ttype =
    | Eof
    | Null
    | Int of int 
    | Float of float 
    | Char of char 
    | String of string
    | Var of string
    | Set of string * ttype
    | Add of ttype * ttype
    | Sub of ttype * ttype
    | Mul of ttype * ttype
    | Div of ttype * ttype
    | Print of ttype
    | Println of ttype

let new_val ttype value =
    match ttype with
    | "int" -> Int (int_of_string value)
    | "float" -> Float (float_of_string value)
    | "char" -> if String.length value > 1 then failwith "cannot convert type string to type Char"
                else Char (value.[0])
    | "string" -> String value
    | _ -> failwith (Printf.sprintf "cannot resolve type '%s'" ttype)

let type_of x =
    match x with
    | Null -> "Null"
    | Int _ -> "Int"
    | Float _ -> "Float"
    | Char _ -> "Char"
    | String _ -> "String"
    | _ -> "Proc"

let rec to_printable x =
    match x with
    | Null -> "null"
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Char c -> Printf.sprintf "%s" (String.make 1 c)
    | String s -> s
    | Var _ -> "Var"
    | Set (id, dt) -> Printf.sprintf "set %s %s" id (to_printable dt)
    | Add (e1, e2) -> Printf.sprintf "add %s %s" (to_printable e1) (to_printable e2)
    | Sub (e1, e2) -> Printf.sprintf "sub %s %s" (to_printable e1) (to_printable e2)
    | Mul (e1, e2) -> Printf.sprintf "mul %s %s" (to_printable e1) (to_printable e2)
    | Div (e1, e2) -> Printf.sprintf "dib %s %s" (to_printable e1) (to_printable e2)
    | Print dt -> Printf.sprintf "print %s" (to_printable dt)
    | Println dt -> Printf.sprintf "println %s" (to_printable dt)
    | Eof -> "end_of_file"

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


