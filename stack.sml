signature stack =
sig
type 'a stack
exception Emptystack
exception Error of string
val create: 'a stack
val push : 'a * 'a stack -> 'a stack
val pop : 'a stack -> 'a stack
val top : 'a stack -> 'a
val empty: 'a stack -> bool
val poptop : 'a stack -> ('a * 'a stack) option
val nth : 'a stack * int -> 'a
val drop : 'a stack * int -> 'a stack
val depth : 'a stack -> int
val app : ('a -> unit) -> 'a stack -> unit
val map : ('a -> 'b) -> 'a stack -> 'b stack
val mapPartial : ('a -> 'b option) -> 'a stack -> 'b stack
val find : ('a -> bool) -> 'a stack -> 'a option
val filter : ('a -> bool) -> 'a stack -> 'a stack
val foldr : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b -> 'a stack -> 'b
val exists : ('a -> bool) -> 'a stack -> bool
val all : ('a -> bool) -> 'a stack -> bool
val list2stack : 'a list -> 'a stack (* Convert a list into a stack *)
val stack2list: 'a stack -> 'a list (* Convert a stack into a list *)
val toString: ('a -> string) -> 'a stack -> string
end


structure Funstack :> stack =
struct
type 'a stack= 'a list
exception Emptystack
exception Error of string
val create :'a stack=[]

fun empty (l : 'a stack) : bool = (
    case l of 
        [] => true
        | _ => false
)

fun push (x:'a, l:'a stack):'a stack = x::l
fun pop (l:'a stack):'a stack = (
    case l of 
        [] => raise Empty
        | (x::xs) => xs)

fun top (l:'a stack):'a = 
        (case l of
           [] => raise Empty
         | (x::xs) => x)

fun top (l:'a stack):'a =
  hd (l) handle List.Empty => raise Emptystack
fun pop (l:'a stack):'a stack =
  tl (l) handle List.Empty => raise Emptystack

fun map (f:'a -> 'b) (l:'a stack):'b stack = List.map f l
fun app (f:'a -> unit) (l:'a stack):unit = List.app f l

fun poptop (l:'a stack) : ('a * 'a stack) option =
(
    case l of
        [] => NONE
    |   (x :: xs) => SOME (x, xs) 
)

fun nth (l:'a stack, x:int) : 'a = List.nth (l, x)
fun drop (l:'a stack, x:int) : 'a stack = List.drop (l, x) 
fun depth (l:'a stack) : int = List.length l
fun mapPartial (f:'a -> 'b option) (l:'a stack) : 'b stack = List.mapPartial f l
fun find (f:'a -> bool) (l:'a stack) : 'a option = List.find f l
fun filter (f:'a -> bool) (l:'a stack) : 'a stack = List.filter f l
fun foldr (f:'a * 'b -> 'b) (x:'b) (l:'a stack) : 'b = List.foldr f x l 
fun foldl (f:'a * 'b -> 'b) (x:'b) (l:'a stack) : 'b = List.foldl f x l 
fun exists (f:'a -> bool) (l:'a stack) : bool = List.exists f l
fun all (f:'a -> bool) (l:'a stack) : bool = List.all f l
fun list2stack (l: 'a list) : 'a stack = l
fun stack2list (l: 'a stack) : 'a list = l
fun toString (f:'a -> string) (l:'a stack) : string = let 
val a = List.map f l
val b = String.concat(a)
in b end


(*
fun toString (f:'a -> string) (l:'a stack) : string = 
(
    case l of 
        [] => ""
    |   (x :: xs) => let 
        val a = f(x)
        val b = toString(f, xs)
        in a^b end
)

*)
end


