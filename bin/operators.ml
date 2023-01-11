open Types

let add a b =
    match (a, b) with
    | (Int i1, Int i2) -> Int (Int.add i1 i2)
    | (Float f1, Float f2) -> Float (Float.add f1 f2)
    | (Int i, Float f) | (Float f, Int i) -> Float (Float.add (float_of_int i) f)
    | _ -> failwith (Printf.sprintf "can't add type %s with type %s" (type_of a) (type_of b))

let sub a b =
    match (a, b) with
    | (Int i1, Int i2) -> Int (Int.sub i1 i2)
    | (Float f1, Float f2) -> Float (Float.sub f1 f2)
    | (Int i, Float f) | (Float f, Int i) -> Float (Float.sub (float_of_int i) f)
    | _ -> failwith (Printf.sprintf "can't substract type %s with type %s" (type_of a) (type_of b))

let mul a b =
    match (a, b) with
    | (Int i1, Int i2) -> Int (Int.mul i1 i2)
    | (Float f1, Float f2) -> Float (Float.mul f1 f2)
    | (Int i, Float f) | (Float f, Int i) -> Float (Float.mul (float_of_int i) f)
    | _ -> failwith (Printf.sprintf "can't multiply type %s with type %s" (type_of a) (type_of b))

let div a b =
    match (a, b) with
    | (Int i1, Int i2) -> Int (Int.div i1 i2)
    | (Float f1, Float f2) -> Float (Float.div f1 f2)
    | (Int i, Float f) | (Float f, Int i) -> Float (Float.div (float_of_int i) f)
    | _ -> failwith (Printf.sprintf "can't divide type %s with type %s" (type_of a) (type_of b))

let gt a b =
    match (a, b) with
    | (Int i1, Int i2) -> Bool ( i1 > i2)
    | (Float f1, Float f2) -> Bool (f1 > f2)
    | _ -> failwith (Printf.sprintf "can't compare type %s with type %s" (type_of a) (type_of b))

let lt a b =
    match (a, b) with
    | (Int i1, Int i2) -> Bool ( i1 < i2)
    | (Float f1, Float f2) -> Bool (f1 < f2)
    | _ -> failwith (Printf.sprintf "can't compare type %s with type %s" (type_of a) (type_of b))
