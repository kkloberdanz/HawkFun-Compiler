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




let rec eval (e : expr) (env : value env) : value =
    match e with
    | Con i -> Int i
    | EListC -> []
    | CstI i -> Int i
    | CstB b -> Int (if b then 1 else 0)

    | Var x  -> lookup env x

    | Op1(op, e1) ->
        let v1 = eval e1 env in
        match(op, v1) with
        | ("not", Int i1) -> Int -i1
        | _ -> failwith "unknown primitive or wrong type"
        
    | Op2(op, e1, e2) ->
       let v1 = eval e1 env in
       let v2 = eval e2 env in
       match(op,v1,v2) with
          | ("::", ListT i, ListT j) -> i :: j (*Not Correct, Holder*)
          | ("*", Int i1, Int i2) -> Int (i1 * i2)
          | ("/", Int i1, Int i2) -> Int (i1 / i2)
          | ("+", Int i1, Int i2) -> Int (i1 + i2)
          | ("-", Int i1, Int i2) -> Int (i1 - i2)
          | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
          | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
          | _ -> failwith "unknown primitive or wrong type"


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
 
    | If (e1, e2, e3) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"
  
    | Letfun (f, x, e1, e2) -> 
      let env2 = (f, Closure(f, x, e1, env)) :: env in
      eval e2 env2
  
    | Call (e1, e2) -> 
      let c = eval e1 env
      match c with
      | Closure (f, x, fbody, fenv) ->
        let v = eval e2 env in
        let env1 = (x, v) :: (f, c) :: fenv in
        eval fbody env1
      | _ -> failwith "eval Call: not a function";;


let run e = eval e []
