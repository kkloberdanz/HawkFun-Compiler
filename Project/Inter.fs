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


let e1 = (Con 2, BoolT)
let e2 = (EListC, IntT)
let e3 = (Op1("tl",(Op2("::",(Con 2, IntT), (Op2("::",(Con 4, IntT),(Op2("::",(Con 7, IntT), (EListC, IntT)),IntT)),IntT)), IntT)),IntT)
eval e1 []
eval e2 []
eval e3 []

let rec eval (e : expr) (env : value env) : value =
    match e with 
    | (Con i, BoolT) -> if i=1 then Int 1 else Int 0
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
          | ("::", Int i, List j) -> (List (Int i :: j)) (*Not Correct, Holder*)
          | ("*", Int i1, Int i2) -> Int (i1 * i2)
          | ("/", Int i1, Int i2) -> Int (i1 / i2)
          | ("+", Int i1, Int i2) -> Int (i1 + i2)
          | ("-", Int i1, Int i2) -> Int (i1 - i2)
          | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
          | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
          | _ -> failwith "unknown primitive or wrong type"

    | (If (e1, e2, e3),_) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"

    | _ -> failwith "unknown Test Primitive"

    | (Call (e1, e2),_) -> 
      let c = eval e1 env
      match c with
      | Closure (f, x, fbody, fenv) ->
        let v = eval e2 env in
        let env1 = (x, v) :: (f, c) :: fenv in
        eval fbody env1
      | _ -> failwith "eval Call: not a function"


    (*| Prim (op, e1, e2) -> 
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | ("*", Int i1, Int i2) -> Int (i1 * i2)
      | ("+", Int i1, Int i2) -> Int (i1 + i2)
      | ("-", Int i1, Int i2) -> Int (i1 - i2)
      | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
      | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
      |  _ -> failwith "unknown primitive or wrong type"*)

    (*| Let (x, e1, e2) -> 
      let v = eval e1 env in
      let env2 = (x, v) :: env in
      eval e2 env2*)

    (*Trying to figure out how to create the let for our Interpretor *)
    | Let (x, e1) ->
        match x with
        | V(str, v1) -> v1 (* Holder values for now*)
        | F(f,x,fbody,fenv) -> f
  
    | Letfun (f, x, e1, e2) -> 
      let env2 = (f, Closure(f, x, e1, env)) :: env in
      eval e2 env2


let run e = eval e []
