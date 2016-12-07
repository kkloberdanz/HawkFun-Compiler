(* File Project/Inter.fs
*)

module TypeCheck

open Absyn
open Env


let rec check (e : expr) (env : value env) : expr =
    match e with
    | (Con 0, BoolT) -> (Con 0, BoolT)
    | (Con 1, BoolT) -> (Con 1, BoolT)
    | (Con 0, UnitT) -> (Con 0, UnitT)
    | (Con i, IntT) -> (Con i, IntT)
    | (EListC, ListT BoolT) -> (EListC, ListT BoolT)
    | (EListC, ListT IntT) ->  (EListC, ListT IntT)
    | (Var i, IntT) -> (Var i, IntT)
    | (Var i, BoolT) -> (Var i, BoolT)
    | (Op1(op, e1),x) ->
        match(op, x) with
        | ("not", _) -> (Op1(op, check e1 env),BoolT)
        | ("ise", ListT j ) -> (Op1(op,check e1 env), BoolT)
        | ("hd", ListT j ) -> (Op1(op,check e1 env), j)
        | ("tl", ListT j ) -> if j != AnyT then (Op1(op,check e1 env), x) else failwith "AnyT"
        | ("print", _ ) -> (Op1(op, check e1 env), UnitT)
        | _ -> failwith "unknown primitive or wrong type"
    | (Op2(op, e1, e2),x) ->
       match(op,x) with
          | ("::", _) -> Op2(op, check e1 evn, check e2 env, ListT x)
          | ("*", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("/", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("+", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("-", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("=", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("<>", _) -> Op2(op, check e1 evn, check e2 env, IntT)
          | ("<", _) -> Op2(op, check e1 evn, check e2 env, BoolT)
          | ("<=", _) -> Op2(op, check e1 evn, check e2 env, BoolT)
          | _ -> failwith "unknown primitive or wrong type"












(*
    match e with 
    | (Con 0, BoolT) -> Int 0
    | (Con 1, BoolT) -> Int 1
    | (Con 0, UnitT) -> Int 0
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

    (*Need to go into office hours to see what Lam is supposed to do*)
    | (Lam((x,y),e1),_) ->
        Closure(None,x,e1,env)

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
            let env1 = (x, v) :: ("None", c) :: fenv in
            eval fbody env1
        else
            let env1 = (x, v) :: ((string)f, c) :: fenv in
            eval fbody env1
      | _ -> failwith "eval Call: not a function" *)
