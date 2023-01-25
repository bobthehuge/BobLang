open Memory;;
open Types;;

let load_env() =
    mem_add "print" (Func (["a"], [(Print(Var("a")))]));
    mem_add "println" (Func (["a"], [(Println(Var("a")))]));
    mem_add "add" (Func (["a"; "b"], [(Return(Add(Var("a"), Var("b"))))]));
;;
