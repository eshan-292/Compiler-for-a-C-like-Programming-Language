open AST 
open Funstack

exception error 

val str = program
    ("modulo",
     ([(["X","Y","Z"],"int")],
      [read "X",read "Y",assign ("Z",percent (identifi "X",identifi "Y")),
       write (identifi "Z")]))


val postfixstack = Funstack.create
fun convpostfix(str) = let 

fun Comm(identifi x, pfstack) = 
let
val b = Funstack.push(valu(x), pfstack)
in b end 

| Comm(x, pfstack) = 
let 
fun Commplus(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("plus"), d)
in z end

fun Commmult(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("mult"), d)
in z end

fun Commdivi(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("div"), d)
in z end

fun Commpercent(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("percent"), d)
in z end

fun Commminus(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("minus"), d)
in z end
fun Commless(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("less"), d)
in z end

fun Commgreater(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("great"), d)
in z end

fun Commlessgreat(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("lessgreat"), d)
in z end

fun Commgreaterequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("greaterequal"), d)
in z end

fun Commlessequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("lessequal"), d)
in z end

fun Commequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("equal"), d)
in z end

 in 
    (case x of 
        plus(a,b) => Commplus(a, b, pfstack)
    |   mult(a,b) => Commmult(a, b, pfstack)
    |   divi(a,b) => Commdivi(a, b, pfstack)
    |   minus(a,b) => Commminus(a, b, pfstack)
    |   percent(a,b) => Commpercent(a, b, pfstack)
    |   less(a, b)  => Commless(a,b, pfstack)
    |   equal(a, b)  => Commequal(a,b, pfstack)
    |   lessgreat(a, b)  => Commlessgreat(a,b, pfstack)
    |   greater(a, b)  => Commgreater(a,b, pfstack)
    |   lessequal(a, b)  => Commlessequal(a,b, pfstack)
    |   greaterequal(a, b)  => Commgreaterequal(a,b, pfstack)
    |   Neg(x)  => let 
    val a = Comm(x, pfstack)
    val b = Funstack.push(valu("Neg"), a)
    in b end
    |   Int x   =>  let 
        val a = Funstack.push(Int(x), pfstack)
        in a end
    |   _           => raise error
    )
end

;



fun Commplus(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("plus"), d)
in z end

fun Commmult(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("mult"), d)
in z end

fun Commdivi(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("divi"), d)
in z end

fun Commpercent(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("percent"), d)
in z end

fun Commminus(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("minus"), d)
in z end

fun Commless(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("less"), d)
in z end

fun Commgreater(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("greater"), d)
in z end

fun Commlessgreat(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("lessgreat"), d)
in z end

fun Commgreaterequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("greaterequal"), d)
in z end

fun Commlessequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("lessequal"), d)
in z end

fun Commequal(a, b, pfstack) = let
val c = Comm(a, pfstack)
val d = Comm(b, c)
val z = Funstack.push(valu("equal"), d)
in z end


fun Commassign(assign(x,y),pfstack) = 
(
    case y of 
        plus(a,b) => Commplus(a, b, pfstack)

    |   mult(a,b) => Commmult(a, b, pfstack)
    |   divi(a,b) => Commdivi(a, b, pfstack)
    |   minus(a,b) => Commminus(a, b, pfstack)
    |   percent(a,b) => Commpercent(a, b, pfstack)
    |   less(a, b)  => Commless(a,b, pfstack)
    |   equal(a, b)  => Commequal(a,b, pfstack)
    |   lessgreat(a, b)  => Commlessgreat(a,b, pfstack)
    |   greater(a, b)  => Commgreater(a,b, pfstack)
    |   lessequal(a, b)  => Commlessequal(a,b, pfstack)
    |   greaterequal(a, b)  => Commgreaterequal(a,b, pfstack)
    |   Int x   =>  let 
        val a = Funstack.push(Int(x), pfstack)
        in a end
    | _ => raise error
)
(*
fun Commassign(assign(x,y),pfstack) = 
(
    case y of 
        plus(a,b) => let 
        val d = Commplus(a, b, pfstack)
        val e = Funstack.push(valu(x), d)
        in e end

    |   mult(a,b) => let 
        val d = Commmult(a, b, pfstack)
        val e = Funstack.push(valu(x), d)
        in e end
    |   divi(a,b) => let 
        val d = Commdivi(a, b, pfstack)
        val e = Funstack.push(valu(x), d)
        in e end
    |   minus(a,b) => let 
        val d = Commminus(a, b, pfstack)
        val e = Funstack.push(valu(x), d)
        in e end
    |   percent(a,b) => let 
        val d = Commplus(a, b, pfstack)
        val e = Funstack.push(valu(x), d)
        in e end
    | _ => raise error
)

*)

| Commassign(_, _) = raise error
(*
fun helpassign(assign(x,y), pfstack) = let
val a = Funstack.push(x, pfstack)
val b = Funstack.push(valu("assign"),a)
in b end
*)

fun Commcond(x, pfstack) =
(   
    case x of 
    less(a, b)  => Commless(a,b, pfstack)
    |   equal(a, b)  => Commequal(a,b, pfstack)
    |   lessgreat(a, b)  => Commlessgreat(a,b, pfstack)
    |   greater(a, b)  => Commgreater(a,b, pfstack)
    |   lessequal(a, b)  => Commlessequal(a,b, pfstack)
    |   greaterequal(a, b)  => Commgreaterequal(a,b, pfstack)
    |   Int x   =>  let 
        val a = Funstack.push(Int(x), pfstack)
        in a end
    |   _ => raise error
)

fun help([], pfstack) = pfstack
| help((assign(x,y))::xs, pfstack) = 
let val a = Commassign(assign(x,y), pfstack)
    (*val c = helpassign(x, a)*)
    val c = Funstack.push(valu(x), a)
    val d = Funstack.push(valu("assign"), c)
    val b = help(xs, d)
in b end
| help((read x)::xs, pfstack) = 
let 
    val c = Funstack.push(valu(x), pfstack)
    val d = Funstack.push(valu("read"), c)
    val b = help(xs, d)
in b end
| help((write (identifi x))::xs, pfstack) = 
let 
    val c = Funstack.push(valu(x), pfstack)
    val d = Funstack.push(valu("write"), c)
    val b = help(xs, d)
in b end
| help((write (x))::xs, pfstack) = 
let 
    val c = Comm(x, pfstack)
    val d = Funstack.push(valu("write"), c)
    val b = help(xs, d)
in b end
| help((WHILE(x, y)) :: xs, pfstack) = 
let val a = Commcond(x, pfstack)
    val b = help(y, a)
    val c = Funstack.push(valu("while"), b)
    val d = help(xs, c)
    (* val b = help(xs, d)*)
in d end
| help((ifthel(x, y, z)) :: xs, pfstack) = 
let val a = Commcond(x, pfstack)
    val f = Funstack.push(valu("if"), a)
    val b = help(y, f)
    val g = Funstack.push(valu("else"),b)
    val e = help(z, g)
    val c = Funstack.push(valu("endif"), e)
    val d = help(xs, c)
    (* val b = help(xs, d)*)
in d end
| help(_, _) = raise error 



fun postfixgen (program(ID,(_,L2))) = let 
val pfstack = Funstack.create
(*val b = List.app Commassign L2 *)
val b = help(L2, pfstack)
in b end
| postfixgen(x) = raise error ;



val temp = postfixgen(str)
(* val tans = Funstack.depth(temp)*)


val andss = Funstack.stack2list(temp)


in temp end


(* val ans = List.length(Funstack.stack2list(postfixstack)) *)

(*
val test = Comm(identifi "A", postfixstack)
val testsize = Funstack.depth(test)
*)



val ans = List.rev(Funstack.stack2list(convpostfix(str)))