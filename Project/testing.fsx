(* 

 Mac Os
   cd /Users/tinelli/Desktop/Project
   mono bin/fslex.exe --unicode Lexer.fsl
   mono bin/fsyacc.exe --module Parser Parser.fsy


*)

#r "System.Drawing.dll"
#r "System.Windows.Forms.dll"


// Windows only
#r "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\\bin\FsLexYacc.Runtime.dll"
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Absyn.fs" 
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Parser.fs"
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Lexer.fs" 
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Parse.fs"
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Env.fs"
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\TypeCheck.fs" 
#load "C:\Users\Max\Documents\GitHub\HawkFun-Compiler\Project\Inter.fs" 




// Mac Os
 (*  #r "/Users/tinelli/Desktop/Project/bin/FsLexYacc.Runtime.dll"
   #load "/Users/tinelli/Desktop/Project/Absyn.fs" 
   #load "/Users/tinelli/Desktop/Project/Parser.fs"
   #load "/Users/tinelli/Desktop/Project/Lexer.fs" 
   #load "/Users/tinelli/Desktop/Project/Parse.fs" 
   #load "/Users/tinelli/Desktop/Project/Env.fs" 
   #load "/Users/tinelli/Desktop/Project/TypeCheck.fs"
   #load "/Users/tinelli/Desktop/Project/Inter.fs"*)


open Absyn

let fromString = Parse.fromString
let check = TypeCheck.check
let eval = Inter.eval
let run = Inter.run
//let crun e = run (check e)

//Things that aren't working

//doesn't interpret
let holld = fromString "
  local fun rec f (x:int) : bool = f (x - 1) in f 2 end
"

check holld

run holld

//Interpretor can't find variable x, not typechecking either
let ex1 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
  var x = add 3 4
in
 (print x);
 x
end
"

check ex1

run ex1 


//Won't Typecheck
let ex1 = fromString "
local
  var add = fn (x:int) => fn (y:int) => x + y end end
in
  add 3 4
end
"
check ex1


//Doesn't Interpret
let ex = fromString "
local
  fun rec fib (n:int) : int =
    if n = 0 then 1 else n * (fib (n - 1))
in 
  fib 4
end
"


check ex

run ex

//Not typechecking
let ex = fromString "
(false :: ([]:bool)) :: ([]:bool list list)
"
check ex
run ex

//Both type and Inter not working
let ex = fromString "
local
  fun rec map (f: int -> int) : (int list -> int list) = 
    fn (l: int list) => 
      if ise l then l else (f (hd l)) :: (map f (tl l)) end
  fun flip (x:bool) = not x
  var e = ([]:bool)
in
  map (fn (x:bool) => not x end) (true::false::e)
end
"

check ex

run ex

//End of things that aren't working

let ex4 = fromString "
local
  fun compose (f:int -> int) = fn (g:int -> int) => fn (x:int) => f (g x) end end
  fun inc (x:int) = x + 1
  fun square (x:int) = x * x 
in
  compose inc square 3
end 
"

check ex4

run ex4

let ex = fromString "
  local var x = false in 2 * x end
"
let tester = fromString " ([]:int list)"

check ex

check (Let (V ("x",(Con 0, BoolT)),(Op2 ("*",(Con 2, IntT),(Var "x", AnyT)), AnyT)),AnyT)

check tester

run ex

run tester


check (fromString "fn (x:int) => x end")

check (fromString "local fun f (x:int) = x in (f 1) end")

let tester = fromString "local var f = fn (x:int) => x end in (f 1) end"

check tester

run tester


//doesn't interpret
let holld = fromString "
  local fun rec f (x:int) : bool = f (x - 1) in f 2 end
"

check holld

run holld


let ex = fromString "
  print ((1::2::3::([]:int list)) :: (4::3::([]:int list)) :: ([]:int list list))
"

check ex

run ex


//supposed to fail
let ex1 = fromString "
local 
  fun add (x:int) = 
  local 
    fun addx (y:int) = x + true
  in 
    addx
  end 
in
  add 3 4
end
"

check ex1

run ex1

let ex1 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
in
    3 + 4
end
"

check ex1

run ex1

let ex1 = fromString "
  1 :: (1 + 2) :: ([]:int list)
"

check ex1 

run ex1


//Interpretor can't find variable x, not typechecking either
let ex1 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
  var x = add 3 4
in
 (print x);
 x
end
"

check ex1

run ex1 

let ex2 = check ex1

run ex2

//Won't Typecheck
let ex1 = fromString "
local
  var add = fn (x:int) => fn (y:int) => x + y end end
in
  add 3 4
end
"
check ex1
run ex1


let ex2 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
in
  add
end
"

check ex2

run ex2

let ex2 = fromString "
  (fn (y:int) => y + 1 end) 4
"

check ex2

run ex2


//why does this one work in type and inter???
let ex3 = fromString "
local 
  fun twice (f:int -> int) = fn (x:int) => f (f x) end
  fun inc (x:int) = x + 1 
in
  twice inc 3
end 
"

check ex3

run ex3

let ex4 = fromString "
local
  fun compose (f:int -> int) = fn (g:int -> int) => fn (x:int) => f (g x) end end
  fun inc (x:int) = x + 1
  fun square (x:int) = x * x 
in
  compose inc square 3
end 
"

check ex4

run ex4


//Doesn't Interpret
let ex = fromString "
local
  fun rec fib (n:int) : int =
    if n = 0 then 1 else n * (fib (n - 1))
in 
  fib 4
end
"


check ex

run ex



let ex = fromString "
4 ; null
"

check ex
run ex

let ex = fromString "
4 ; true
"

let ex = fromString "
local
  var x = tl (4 :: 5 :: ([]:int list))
in
  if x = ([]:int list) then 10 else 11
end
"

let ex2 = fromString " tl 1 :: (1 + 2) :: ([]:int list) " //works

check ex2
run ex2

check ex

run ex

let ex = fromString "
local
  var x = true :: false :: ([]:bool list)
in
  x
end
"

let ex = fromString "
local
  var e = ([]:bool list)
  var x = (true :: true :: e) :: (false :: e) :: ([]:bool list list)
in
  x
end
"
check ex
run ex

//Not typechecking
let ex = fromString "
(false :: ([]:bool)) :: ([]:bool list list)
"
check ex
run ex

let ex = fromString "
  true :: false :: ([]:bool list)
"

check ex


let ex = fromString "
local
  var e = ([]:bool list)
  var v0 = false :: e
  var v1 = true :: e
  var v2 = ([]:bool list list)
  var x = v0 :: v1 :: v2
in
  ise e
end
"

check ex

run ex


//Both type and Inter not working
let ex = fromString "
local
  fun rec map (f: int -> int) : (int list -> int list) = 
    fn (l: int list) => 
      if ise l then l else (f (hd l)) :: (map f (tl l)) end
  fun flip (x:bool) = not x
  var e = ([]:bool)
in
  map (fn (x:bool) => not x end) (true::false::e)
end
"

check ex

run ex





