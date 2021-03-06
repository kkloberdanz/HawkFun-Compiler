(* File Project/Inter.fs
*)

module Inter

open Absyn
open Env
open TypeCheck


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

let e5 = (Op2 ("::",(Con 1, IntT), (Op2 ("::",(Op2 ("+",(Con 1, IntT),(Con 2, IntT)), IntT), (EListC, ListT IntT)), ListT IntT)), ListT IntT)

let e6 = (Let (V ("f", (Lam (("x", IntT), (Var "x", IntT)), AnyT)), (Call ((Var "f", AnyT), (Con 1, IntT)), AnyT)), AnyT)

let e7 = (Op1("print",(Con 1, BoolT)),IntT)

(*eval e1 []
eval e2 []
eval e3 []
eval e4 []
eval e5 []
eval e6 []
eval e7 []*)

let rec eval (e : expr) (env : value env) : value =
    match e with 
    | (Con 0, BoolT) -> Int 0
    | (Con 1, BoolT) -> Int 1
    | (Con 0, UnitT) -> Int 0
    | (Con i, IntT) -> Int i
    | (EListC,_) -> List []
    | (Var x,_)  -> lookup env x

    | (Op1(op, e1),j) ->
        //let (c1, type1) = typeCheck e1 [] //need this for print statement
        let v1 = eval e1 env in
        match(op, v1) with
        | ("not", Int i1) -> Int -i1
        | ("ise", List i1) -> if i1=[] then Int 1 else Int 0
        | ("hd", List j1 ) -> match j1 with
                                | h::t -> List [h]
                                | [] -> List []
        | ("tl", List j1 ) -> match j1 with
                                | h::t -> List t
                                | [] -> List []
        | ("print", _) -> let (c1, type1) = typeCheck e1 [] 
                          let hold = Printf.TextWriterFormat<unit>((toString v1 type1))
                          printfn(hold) ; Int 0
                            //let f = Printf.TextWriterFormat<unit>((toString v1 type1)) printfn f
                            
        | _ -> failwith "unknown primitive or wrong type for Op1"
        
    | (Op2(op, e1, e2),_) ->
       let v1 = eval e1 env in
       let v2 = eval e2 env in
       match(op,v1,v2) with
          | ("::", i , List j) -> (List ( i :: j))
          | (";", _, _) -> v2
          | ("*", Int i1, Int i2) -> Int (i1 * i2)
          | ("/", Int i1, Int i2) -> Int (i1 / i2)
          | ("+", Int i1, Int i2) -> Int (i1 + i2)
          | ("-", Int i1, Int i2) -> Int (i1 - i2)
          | ("=", i1, i2) -> Int (if i1 = i2 then 1 else 0)
          | ("<>", i1, i2) -> Int(if i1 = i2 then 0 else 1)
          | ("<", i1, i2) -> Int (if i1 < i2 then 1 else 0)
          | ("<=", i1, i2) -> Int(if i1 <= i2 then 1 else 0)
          | _ -> failwith "unknown primitive or wrong type for Op2"

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
        | F(f,(x,t),ftype,fexp) ->  let env2 = (f, Closure(Some f,x,fexp, env)) :: env in printfn "env2: %A;\n e1 %A" env2 e1;
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
      | _ -> failwith "eval Call: not a function"

    | _ -> failwith "Not an expression"

let run e = eval e []
