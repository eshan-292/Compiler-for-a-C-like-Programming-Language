open AST
open Funstack


val str = program
    ("modulo",
     ([(["X","Y","Z"],"int")],
      [read "X",read "Y",assign ("Z",percent (identifi "X",identifi "Y")),
       write (identifi "Z")]))





fun insideint(Int (x)) = x
|   insideint(_) = raise error;
fun insidebool(Bool (x)) = x
fun insidevalu(valu (x)) = x
|   insidevalu(_) = raise error;


exception error 
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
| calcno(_) = raise error

fun lofltol ([]) = []
| lofltol((x, _)::xs) = let
val l = lofltol(xs)
in x @ l end

val len = calcno(str) ;
val Memory = Array.array(len, 0)

val htable : (string, int) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(32, error)
fun iter([]) = ()
| iter([x]) = HashTable.insert htable(x, 0) 
| iter(x::xs) = let 
val len = List.length(xs)
val y = HashTable.insert htable(x, len)
(*val z = Array.update(Memory, len, x)*)
in iter(xs) end 




fun defhtable(program(ID,(L1,_))) = let 
val l = lofltol(L1)
in iter(l) end
| defhtable(_) = raise error ;

(*
fun init(str) = 
let 

in defhtable(str) end 
*)

val dummy = defhtable(str) ;





fun helpif(e, C) = let val 
a = Funstack.top(C)
in
(
    case a=e of   
        true => C
    |   false => let val
    d = Funstack.pop(C) 
    in helpif(e, d) end
    
) end 






val emplist : AST list = []
val empst = Funstack.list2stack(emplist)


type 'a stack = 'a list
(*
val V = Funstack.create

val C = convpostfix(str)
*)
(*datatype vmc = VMC of (AST stack * int array * AST stack) *)
(*fun toString() = let
val a = Funstack.toString(valu(), V)

in a end
*)

fun ArraytoList arr = Array.foldr (op ::) [] arr



fun ASTtoString(valu x) = x
| ASTtoString(Int x) = Int.toString(x)
| ASTtoString(Bool x) = Bool.toString(x)
| ASTtoString(_) = raise error;

fun ASTstacktoString(x) = Funstack.toString(ASTtoString) x ;

(*fun ListtoString([]) = ""
fun ListtoString([x]) = Int.toString(x)
fun ListtoString( x :: xs ) = let 
val a =  Int.toString(x) 
val c = ListtoString(xs)
val b = a ^ c
in b end
fun ListtoString(_) = raise error *)

fun ListtoString(x) = List.map Int.toString x

fun LofStoS([]) = ""
| LofStoS(x::xs) = x ^ " " ^ LofStoS(xs) 


signature vmc = 
sig 
(*type AST stack * int array * AST stack*)

val rules: AST Funstack.stack * int array * AST Funstack.stack ->  AST Funstack.stack * int array * AST Funstack.stack

val toString: AST Funstack.stack * int array * AST Funstack.stack -> string

end

structure VMC :> vmc =
struct 

fun toString((V,M,C)) = 
let 
val a = ASTstacktoString(V)
val b = ArraytoList M
val f = ListtoString(b)
val g = LofStoS(f)
val c = ASTstacktoString(C)
val d = a^ ", " ^g
val e = d^ ", " ^c
in e end



(*fun rules(V,M,[]) = (V, M, empst) *)
fun rules((V, M, C)) =  let val
empcheck = Funstack.empty(C)
in (
    case empcheck of
        true => (V,M,empst)
        |false =>(
    (*case Funstack.top(C) of 
        valu x  =>  Funstack.push(valu(x), V)
    *)

    case Funstack.top(C) of 
        Int m => let val 
        V' = Funstack.push(Int(m), V)
        val C' = Funstack.pop(C) in
        rules(V', M, C') end        
    |   Bool m => let val 
        V' = Funstack.push(Bool(m), V)
        val C' = Funstack.pop(C) in
        rules(V', M, C') end   
    
    |   valu "plus" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b+a 
        val V' = Funstack.push(Int(c), y) in
        rules(V', M, C') end       
    |   valu "minus" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b-a 
        val V' = Funstack.push(Int(c), y) in
        rules(V', M, C') end 
    |   valu "mult" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b*a 
        val V' = Funstack.push(Int(c), y) in
        rules(V', M, C') end 
    |   valu "div" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b div a 
        val V' = Funstack.push(Int(c), y) in
        rules(V', M, C') end  
    |   valu "percent" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b mod a 
        val V' = Funstack.push(Int(c), y) in
        rules(V', M, C') end  
    |   valu "less" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b<a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end     
    |   valu "greater" => let val 
        a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b>a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end    
    |   valu "lessequal" => let val 
         a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b<=a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end    
    |   valu "greaterequal" => let val 
         a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b>=a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end    
    |   valu "equal" => let val 
         a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b=a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end  
    |   valu "lessgreat" => let val 
         a = insideint(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val y = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c = b<>a 
        val V' = Funstack.push(Bool(c), y) in
        rules(V', M, C') end 
    |   valu "read" => let  
    val str = valOf (TextIO.inputLine TextIO.stdIn)
    val i : int = valOf (Int.fromString str)
         val a = insidevalu(Funstack.top(V))
        val V' = Funstack.pop(V)
        val C' = Funstack.pop(C)
        val c =  HashTable.lookup htable(a)
        val d = Array.update(M, c, i) 
             in
        rules(V', M, C') end 
    
    |   valu "write" => let  
        val a = insideint(Funstack.top(V))
        val V' = Funstack.pop(V)
        val C' = Funstack.pop(C)
        val e = print(Int.toString(a) ^ "\n")
             in
        rules(V', M, C') end 
    
    |   valu "Neg" => let  
        val a = insideint(Funstack.top(V))
        val b = Funstack.pop(V)
        val C' = Funstack.pop(C)
        val d = ~a 
        val V' = Funstack.push(Int(d),b )
             in
        rules(V', M, C') end 
    |   valu "if" => let  
        val a = insidebool(Funstack.top(V))
        val b = Funstack.pop(C) 
        val V' = Funstack.pop(V)
        in
        (
            case a of
                true => rules(V',M,b) (*let 
                val c = Funstack.top(b)
                val d = Funstack.top(c)
                val e = helpif(valu "else", C)*)
            |   false => let
            val x = helpif(valu "else", b)
            val C' = Funstack.pop(x)
            in rules(V', M, C') end
        ) end

    |   valu "else" => let

        val a = helpif(valu "endif", C)
        val C' = Funstack.pop(a)
        in rules (V, M ,C') end
             (*in
        rules(V', M, C') end 
*)
    |   valu "endif" =>
    let val C' = Funstack.pop(C)
    in rules(V,M,C') end


    |  (* valu "assign" =>let val a = insidevalu(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val V' = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c =  HashTable.lookup htable(a)
        val M' = Array.update(M, c, b)  in
        rules(V', M, C') end 
        *)
    
       valu "assign" => let val
        p = Funstack.pop(V)
        val i = Funstack.top(p)
        val q= (case i of 
                Int j => let val a = insidevalu(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insideint(Funstack.top(x))
        val V' = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c =  HashTable.lookup htable(a)
        val M' = Array.update(M, c, b)  in
        rules(V', M, C') end 
        |   Bool k => let val a = insidevalu(Funstack.top(V))
        val x = Funstack.pop(V)
        val b = insidebool(Funstack.top(x))
        val V' = Funstack.pop(x)
        val C' = Funstack.pop(C)
        val c =  HashTable.lookup htable(a)
        val M' = (
            case b of 
                true => Array.update(M, c, 1)  
            |   false => Array.update(M, c, 0) 
        )
        
        in rules(V', M, C') end 
        | _ => raise error
        )
        in q
        end
        
    

    |   valu x =>
        let 
        val w = Funstack.pop(C)
        val y = Funstack.top(w)
        in 
        (
            case y of 
                valu "assign" => let 
                val V'= Funstack.push(valu x , V) 
                val C' = Funstack.pop(C) in
                rules(V', M, C') end
            | valu "read" => let 
                val V'= Funstack.push(valu x , V) 
                val C' = Funstack.pop(C) in
                rules(V', M, C') end
            | (*valu "write" => let 
                val V'= Funstack.push(valu x , V) 
                val C' = Funstack.pop(C) in
                rules(V', M, C') end    
            |*)   _  => let val a = HashTable.lookup htable(x)
        val b = Array.sub(M, a)
        val V'= Funstack.push(Int(b), V)            (* Push the value from the memory array using hashmap *)
        val C' = Funstack.pop(C) in
        rules(V', M, C') end
        ) end
           

    |   _   =>  (V, M, C)
)

)





end


end

(*
[valu "assign",valu "B",valu "minus",valu "B",valu "A",valu "assign",
   valu "A",valu "plus",valu "percent",valu "A",valu "div",valu "D",valu "C",
   valu "mult",valu "B",valu "A"] 
*)




val pfst = convpostfix(str) ;
val pfsttol = Funstack.stack2list(convpostfix(str)) ;
val pfsttolrev = List.rev(pfsttol)
val pfstrev = Funstack.list2stack(pfsttolrev)

val V = Funstack.create ;

val beforeans = (V, Memory, pfstrev)
val strbeforeans = VMC.toString(beforeans) ;

val ans = VMC.rules(V, Memory, pfstrev) ;
(*val strans = VMC.toString(V, Memory, pfst) ;*)
val strans = VMC.toString(ans) ;
(*print(Funstack.toString(pfst)) ;*)

