(* File Project/Inter.fs
*)

module TypeCheck

open Absyn
open Env


let e1 = (Op1("not",((Con 0, BoolT))),AnyT)

let e2 = (If((Op1("not",((Con 0, BoolT))),AnyT),(Con 5, IntT) , (Con 1, IntT)) , AnyT)

let e3 = (Let(V("G",(Op2("+",(Con 6,IntT),(Con 4,IntT)),AnyT)),(Op2("+",(Var "G",AnyT),(Con 4,IntT)),AnyT)),AnyT)

let e4 = (Let (V ("x", (Con 0, BoolT)), (Op1 ("not", (Var "x", AnyT)), AnyT)), AnyT)

//e5 should fail
let e5 = (Let (V ("x", (Con 0, BoolT)) ,(Op2 ("*", (Con 2, IntT), (Var "x", AnyT)), AnyT)), AnyT)



let e6 = (Let (V ("f", (Lam (("x", IntT), (Var "x", IntT)), AnyT)), (Call ((Var "f", AnyT), (Con 1, IntT)), AnyT)), AnyT)




check e1 []
check e2 []
check e3 []
check e4 []
check e5 []
check e6 []

let rec check (e : expr) (env : htype env) : expr =
    match e with
    | (Con 0, BoolT) -> (Con 0, BoolT)
    | (Con 1, BoolT) -> (Con 1, BoolT)
    | (Con 0, UnitT) -> (Con 0, UnitT)
    | (Con i, IntT) -> (Con i, IntT)
    | (EListC, ListT x) -> (EListC, ListT x) //Idk if this is right
    | (Var x, _) -> (Var x, lookup env x)

    (*
    | (Var i, IntT) -> (Var i, IntT)
    | (Var i, BoolT) -> (Var i, BoolT)
    | (Var i, UnitT) -> (Var i, UnitT)
    | (Var i, ListT BoolT) -> (Var i, ListT BoolT)
    | (Var i, ListT IntT) -> (Var i, ListT IntT)
    | (Var i, ListT UnitT) -> (Var i, ListT UnitT)
    | (Var i, ArrowT(IntT, IntT))
    | (Var i, ArrowT(IntT, BoolT))
    | (Var i, ArrowT(BoolT, IntT))
    | (Var i, ArrowT(BoolT, BoolT))
    | (Var i, ArrowT(IntT, UnitT))
    | (Var i, ArrowT(UnitT, IntT))
    | (Var i, ArrowT(UnitT, UnitT))   *)

    
    | (Op1(op, e1),x) ->
        let (v1,t) = check e1 env
        match(op, t) with
        | ("not", BoolT ) -> (Op1(op, (v1,t)),BoolT)
        | ("ise", ListT j ) -> (Op1(op, (v1,t)), BoolT)
        | ("hd", ListT j ) -> (Op1(op, (v1,t)), j)
        | ("tl", ListT j ) -> (Op1(op, (v1,t)), ListT j) 
        | ("print", _ ) -> (Op1(op, (v1,t)), UnitT)
        | _ -> failwith "unknown primitive or wrong type"

    | (Op2(op, e1, e2),x) ->
       let (v1, h) = check e1 env
       let (v2,j) = check e2 env
       match(op, h, j) with
          | ("::", h, ListT q) -> if h=q then (Op2(op, (v1,h), (v2,j)), ListT h) else failwith "Mismatched types"
          | (";", h, j) -> (Op2(op, (v1,h), (v2,j)), j)
          | ("*", IntT, IntT) -> (Op2(op, (v1,h), (v2,j)), IntT)
          | ("/", IntT, IntT) -> (Op2(op, (v1,h), (v2,j)), IntT)
          | ("+", IntT, IntT) -> (Op2(op, (v1,h), (v2,j)), IntT)
          | ("-", IntT, IntT) -> (Op2(op, (v1,h), (v2,j)), IntT)
          | ("=", _ , _) -> if h=j then (Op2(op, (v1,h), (v2,j)), BoolT) else failwith "Not equal types"
          | ("<>", _, _) -> if h=j then (Op2(op, (v1,h) , (v2,j)), BoolT) else failwith "Not equal types"
          | ("<", IntT,IntT) -> (Op2(op, (v1,h), (v2,j)), BoolT)
          | ("<=", IntT, IntT) -> (Op2(op, (v1,h), (v2,j)), BoolT)
          | _ -> failwith "unknown primitive or wrong type"

    | (If (e1, e2, e3),x) -> 
        let (v1,j) = check e1 env
        let (v2,h) = check e2 env
        let (v3,g) = check e3 env
        match j with
            | BoolT -> if h = g then (If ((v1,j), (v2,h), (v3,g)), h) else failwith "MisMatched types"
            | _ -> failwith "first expression not BoolT"

(* Need to do rules 1,6,9,10,11,12 for typechecking, then done*)
    | (Let(bind, e2), j) ->
        //let (v2,q) = check e2 env
        match bind with
        | V(h,e1) -> let (v1,m) = check e1 env 
                     let env2 = (h,m) :: env 
                     let (v2,q) = check e2 env2
                     (Let(V(h,(v1,m)), (v2,q)), q)
                               
        //Not sure about this, #10 on rules
        | F(f,(x,t),fbody,fenv) -> let (hold, hd) = check fenv env
                                   let env2 = (f, hd) :: env
                                   let (v2,q) = check e2 env2
                                   (Let(bind, (v2,q)), q)

        
    | (Lam((x,q),y),z) ->
        let env2 = (x,q) :: env
        let (v1, t1) = check y env2
        (Lam((x,q),(v1,t1)), ArrowT(q, t1))
        

    | (Call(e1,e2),k) ->
        let (v1,z1) = check e2 env
        let (v2,ArrowT(m,n)) = check e1 env
        match z1 with
           | m -> (Call( (v2,ArrowT(m,n)) , (v1,z1) ),m)
           | _ -> failwith "Mismatched call types"
        


    | _ -> failwith "Holder Typecheckers"











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
