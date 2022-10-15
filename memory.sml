open AST 


exception error 

val str = program
    ("good2",
     ([(["A","B","C","D"],"int"),(["E","F","G"],"int")],
      [assign
         ("A",
          plus
            (mult (identifi "A",identifi "B"),
             percent (divi (identifi "C",identifi "D"),identifi "A")))])) 

fun sizelofl(L) = 
(
    case L of
        [] => 0
    |   ((x, y)::xs) => let 
    val a = List.length(x)
    val b = sizelofl(xs)
    in a+b end
)

fun calcno(program(ID,(L1,_)) ) = sizelofl(L1)
(* fun calcno (str1 : string * ((string list * string) list * AST list)) = 
(case str1 of 
    (program (x)) => x
|    (_) => raise error
) *)

fun lofltol ([]) = []
fun lofltol((x, _)::xs) = let
val l = lofltol(xs)
in x @ l end

(*
fun updatehtable (L) =
(
    case L of 
        [x] => HashTable.insert(x, )
) 
*)

val len = calcno(str) ;
val Memory = Array.array(len, 0)

val htable : (string, int) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(len, error)


fun iter(x::xs) = let 
val len = List.length(xs)
val y = HashTable.insert htable(x, len)
val z = Array.update(Memory, len, x)
in iter(xs) end 
| iter(_) = raise error ;



fun defhtable(program(ID,(L1,_))) = let 
val l = lofltol(L1)
in iter(l) end
| defhtable(_) = raise error ;



val meminit = defhtable(str) ;



