(* Project/Absyn.fs  *)

module Absyn

type expr = expr1 * htype
and expr1 =
  | Con of int
  | EListC
  | Var of string
  | Op1 of string * expr
  | Op2 of string * expr * expr
  | If of expr * expr * expr
  | Let of binding * expr
  | Lam of tname * expr //what is this for?
  | Call of expr * expr
and binding = 
  | V of string * expr //binding of variable
  | F of string * tname * htype * expr //binding of function
and htype =
  | AnyT
  | IntT
  | UnitT
  | BoolT
  | ArrowT of htype * htype
  | ListT of htype
and tname = string * htype


let htype ((_, t) : expr) = t


