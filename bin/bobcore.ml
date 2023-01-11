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
    | Null | Int _ | Bool _ | Float _ | Char _ | String _ -> tok
    | Set (id, dt) -> 
            mem_add id (eval dt);
            Null
    | Var v -> find_local v
    | Add (e1, e2) -> add (eval e1) (eval e2)
    | Sub (e1, e2) -> sub (eval e1) (eval e2)
    | Mul (e1, e2) -> mul (eval e1) (eval e2)
    | Div (e1, e2) -> div (eval e1) (eval e2)
    | Eq (e1, e2) -> Bool (e1 = e2)
    | Neq (e1, e2) -> Bool (not (e1 = e2))
    | Geq (e1, e2) -> Bool (e1 >= e2)
    | Leq (e1, e2) -> Bool (e1 <= e2)
    | Not e -> 
            begin
                match (eval e) with 
                | (Bool b) -> Bool (not b)
                | _ -> failwith ("can't match with type Bool")
            end
    | And (e1, e2) -> 
            begin
                match (eval e1, eval e2) with 
                | (Bool b1, Bool b2) -> Bool (b1 && b2)
                | _ -> failwith ("can't match with type Bool")
            end
    | Or (e1, e2) -> 
            begin
                match (eval e1, eval e2) with 
                | (Bool b1, Bool b2) -> Bool (b1 || b2)
                | _ -> failwith ("can't match with type Bool")
            end
    | Gt (e1, e2) -> gt (eval e1) (eval e2)
    | Lt (e1, e2) -> lt (eval e1) (eval e2)
    | Print dt -> 
            print (eval dt);
            Null
    | Println dt ->
            println (eval dt);
            Null

