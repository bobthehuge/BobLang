open Stdenv;;
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
    | Var v -> find_local v
    | Add (e1, e2) -> add (eval e1) (eval e2)
    | Sub (e1, e2) -> sub (eval e1) (eval e2)
    | Mul (e1, e2) -> mul (eval e1) (eval e2)
    | Div (e1, e2) -> div (eval e1) (eval e2)
    | Eq (e1, e2) -> Bool ((eval e1) = (eval e2))
    | Neq (e1, e2) -> Bool (not ((eval e1) = (eval e2)))
    | Geq (e1, e2) -> eval (Not(eval (Lt(e1, e2))))
    | Leq (e1, e2) -> eval (Not(eval (Gt(e1, e2))))
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
    | Call (id, params) -> 
            begin
                create_scope(); 
                load_env();
                let params = List.map (eval) params
                in let res = exec id params
                in begin
                    remove_scope();
                    res
                end
            end
    | Set (id, dt) -> 
            mem_add id (eval dt);
            Null
    | Print dt -> 
            print (eval dt);
            Null
    | Println dt ->
            println (eval dt);
            Null
    | _ -> failwith ("unexpected token")

and exec id params =
    let func = find_local id
    in let (params_name, proc_list) =
        match func with
        | Func (params, procs) -> (params, procs)
        | _ -> (remove_scope(); failwith ("invalid function call"))
    in begin
        List.iter2 (fun name -> fun value -> mem_add name value) params_name params;
        run_func proc_list
       end

and run_func proc_list =
    match proc_list with
    | [] -> Null
    | e::l ->
            match e with
            | Null -> Null
            | Return t -> eval t
            | _ -> run_func ((eval e)::l)
