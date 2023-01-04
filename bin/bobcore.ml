open Printf;;
open Memory;;
open Types;;
open Operators;;

let print dt =
    print_string (to_printable dt)

let println dt =
    print_endline (to_printable dt)

let rec eval tok =
    match tok with
    | Null | Int _ | Float _ | Char _ | String _ -> tok
    | Set (id, dt) -> 
            mem_add id (eval dt);
            Null
    | Var v -> find_local v
    | Add (e1, e2) -> add (eval e1) (eval e2)
    | Sub (e1, e2) -> sub (eval e1) (eval e2)
    | Mul (e1, e2) -> mul (eval e1) (eval e2)
    | Div (e1, e2) -> div (eval e1) (eval e2)
    | Print dt -> 
            print (eval dt);
            Null
    | Println dt ->
            println (eval dt);
            Null
    | _ -> failwith (sprintf "can't eval '%s'" (debug tok))


