(*
    memory is a Stack containing hashmaps corresponding to scopes, each scope
    containing his variables:

        head -> actual scope
        tail -> previous scopes
*)

open Types

let mem : ((string, ttype) Hashtbl.t list ref)= ref [Hashtbl.create 128];;

let create_scope() = 
    mem := (Hashtbl.create 128)::(!mem)

let remove_scope() =
    match !mem with
    | [] -> mem:=[]
    | _::l -> mem:=l

let get_local_scope() =
    List.hd !mem

let mem_add = Hashtbl.add (List.hd !mem)

let find_local e = 
    let res = Hashtbl.find_opt (get_local_scope()) e
    in Option.value res ~default:Null
