(* File Project/Env.fs
 *)

module Env

(* Environment operations *)

type 'v env = (string * 'v) list

let rec lookup env x =
    match env with 
    | []          -> failwith (x + " not found in environment")
    | (y, v) :: r -> printfn "x: %A; \n y: %A \n x=y %b" x y (x=y) ; if x = y then v else if "Some(" + x + ")" = y then v else lookup r x
