(* File Project/Inter.fs
*)

module Inter

open Absyn
open Env


(* A runtime value is an integer, a list, or a function closure *)

type value = 
  | Int of int
  | List of value list
  | Closure of string option * string * expr * value env   
  

(*This is for print statement*)
let rec toString v t = 
  match (v, t) with
  | (_, AnyT)          -> "value" 
  | (_, ArrowT (_, _)) -> "closure"  
  | (_, UnitT)         -> "null"
  | (Int i, IntT)      -> string i
  | (Int 0, BoolT)     -> "false"
  | (Int 1, BoolT)     -> "true"
  | (List l, ListT t1) -> "[" + listToString l t1 + "]"
  | _ ->  failwith "toString: mismatched type and value"
and listToString l t =
  match l with
  |      [] -> ""
  | v :: [] -> toString v t
  | v :: vs -> (toString v t) + "; " + (listToString vs t)


let e1 = (Op2("<>",(Con 4, IntT),(Con 2, IntT)),IntT)
let e2 = (EListC, IntT)
let e3 = (Op1("tl",(Op2("::",(Con 2, IntT), (Op2("::",(Con 4, IntT),(Op2("::",(Con 7, IntT), (EListC, IntT)),IntT)),IntT)), IntT)),IntT)

let e4 = (Let(V("G",(Op2("+",(Con 6,IntT),(Con 4,IntT)),IntT)),(Op2("+",(Var "G",IntT),(Con 4,IntT)),IntT)),IntT)

eval e1 []
eval e2 []
eval e3 []

eval e4 []

let rec eval (e : expr) (env : value env) : value =
    match e with 
    | (Con 0, BoolT) -> Int 0
    | (Con 1, BoolT) -> Int 1
    | (Con i,_) -> Int i
    | (EListC,_) -> List []
    | (Var x,_)  -> lookup env x

    | (Op1(op, e1),_) ->
        let v1 = eval e1 env in
        match(op, v1) with
        | ("not", Int i1) -> Int -i1
        | ("ise", List i1) -> if i1=[] then Int 1 else Int 0
        | ("hd", List (h::t)) -> List [h]
        | ("tl", List (h::t)) -> List t
        (*| ("null", UnitT) ->
        | ("print", _) -> *)
        | _ -> failwith "unknown primitive or wrong type"
        
    | (Op2(op, e1, e2),_) ->
       let v1 = eval e1 env in
       let v2 = eval e2 env in
       match(op,v1,v2) with
          | ("::", Int i, List j) -> (List (Int i :: j))
          | ("*", Int i1, Int i2) -> Int (i1 * i2)
          | ("/", Int i1, Int i2) -> Int (i1 / i2)
          | ("+", Int i1, Int i2) -> Int (i1 + i2)
          | ("-", Int i1, Int i2) -> Int (i1 - i2)
          | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
          | ("<>", Int i1, Int i2) -> Int(if i1 = i2 then 0 else 1)
          | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
          | ("<=", Int i1, Int i2) -> Int(if i1 <= i2 then 1 else 0)
          | _ -> failwith "unknown primitive or wrong type"

    | (If (e1, e2, e3),_) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"


    //Everything below needs to be worked on

    (*| Let (x, e1, e2) -> 
      let v = eval e1 env in
      let env2 = (x, v) :: env in
      eval e2 env2*)

    (*Trying to figure out how to create the let for our Interpretor *)

    | (Let (x, e1),_) ->
        match x with
        | V(str, v1) -> let v = eval v1 env in
                            let env2 = (str,v) :: env in
                               eval e1 env2
        | F(f,(x,t),fbody,fenv) -> let env2 = (f, Closure(Some f,x,e1,env)) :: env in
                                    eval e1 env2

    | _ -> failwith "holder Primitives"

    (*Need to go into office hours to see what Lam is supposed to do
    | (Lam(x,e1),_) ->

    *)
   
   (*
    | Letfun (f, x, e1, e2) -> 
      let env2 = (f, Closure(f, x, e1, env)) :: env in
      eval e2 env2*)

    | (Call (e1, e2),_) -> 
    (*e1 = Name and Type of Function, e2 = Argument you are feeding the function*)
      let c = eval e1 env
      match c with
      | Closure (f, x, fbody, fenv) ->
        let v = eval e2 env in
        if f = None then
            let env1 = (x, v) :: ("", c) :: fenv in
            eval fbody env1
        else
            let env1 = (x, v) :: ((string)f, c) :: fenv in
            eval fbody env1
      | _ -> failwith "eval Call: not a function"


let run e = eval e []
