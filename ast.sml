(* ast.sml *)
structure AST =
struct
 
datatype AST = AST of AST
    | mult of AST * AST
    | mul of AST * AST
    | divi of AST * AST
    | divv of AST * AST
    | percent of AST * AST 
    | percen of AST * AST 
    | plus of AST * AST 
    | plu of AST
    | minus of AST * AST 
    | minu of AST * AST 
    | Neg of AST
    | AND of AST * AST
    | OR of AST * AST
    | less of AST * AST
    | lessequal of AST * AST
    | greater of AST * AST
    | greaterequal of AST * AST
    | equal of AST * AST
    | lessgreat of AST * AST
    | assign of string * AST
    | read of string
    | write of AST
    | ifthel of AST * AST list * AST list
    | WHILE of AST * AST list
    | Not of AST
    | Bool of bool
    | Int of int
    | identifi of string
    | program of string * ((string list * string) list * AST list)
    | valu of string


val TableSize = 500;
val HashFactor = 5;
val HashFunction = fn s =>
        List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s);
val HashTable = Array.array (TableSize,[("","")]) : (string * string) list array;

fun add ([],_) = ()
  | add (s::tail,v) =
  	let
  		val i = HashFunction s
  		val _ = Array.update(HashTable,i,(s,v)::(Array.sub(HashTable,i)))
  	in
  		add (tail,v)
  	end
fun find s =
	let
		val i = HashFunction s
		fun findInList ((key,v)::tail) = if key=s then SOME v else findInList tail
		  | findInList ([]) = NONE
	in
		findInList(Array.sub(HashTable,i))
	end
fun check ([]) = NONE
    |check (x::xs) = 
	let 
		val f = find (x) 
	in 
		case f of SOME t => SOME x | NONE => check (xs)
	end 

(*
type var = string

datatype Type = int | bool 


datatype Variable = variab of Identifier
and Identifier = identifier of LorDSeq  
and LorDSeq = lordseq of LorD * LorDSeq | emplordseq    (* To CHECK *)
and LorD = Letter | Digit 


datatype VariableList = variable of Variable | variablelist of Variable * VariableList

datatype Declaration = declaration of VariableList * Type

datatype DeclarationSeq = Decl of Declaration | DeclSeq of Declaration * DeclarationSeq

datatype Expression = intexp of IntExpression | boolexp of BoolExpression
and IntExpression = intexpp of IntExpression * IntTerm | intterm of IntTerm
and IntTerm = inttermm of IntTerm * IntFactor | intfac of IntFactor
and IntFactor = num of Numeral | vari of Variable | intexppp of IntExpression | intfact of IntFactor
and BoolExpression = boolexpp of BoolExpression * BoolTerm | boolterm of BoolTerm
and BoolTerm = booltermm of BoolTerm * BoolFactor | boolfac of BoolFactor
and BoolFactor = tr | fal | varia of Variable | comp of Comparison | boolexppp of BoolExpression | boolfact of BoolFactor
and Comparison = compar of IntExpression * IntExpression
and Numeral = numer of PorTSeq * DigitSeq
and DigitSeq = digseq of DigitSeq | emp  (* TO CHECK AGAIN *)
and PorTSeq = portseq of PorT * PorTSeq | port of PorT 
and PorT = plusport | tildeport

datatype Command = command of Variable * Expression | readcommand of Variable | writecommand of Variable | ite of BoolExpression * CommandSeq * CommandSeq | whilecommand of BoolExpression * CommandSeq
and CommandSeq =  InCommSeq of IncommandSeq 
and IncommandSeq = incomm of Command | incommseq of Command * IncommandSeq


datatype Program = program of Identifier * Block 
and Block = block of DeclarationSeq * CommandSeq



datatype RelOp = less | lessequal | greater | greaterequal | lessgreat | equal

datatype AddOp = plusaddop | minusaddop

datatype MultOp = mult | divi | percent
*)















(*
datatype v = Arrow of v * v 
            |  Int of string
            |  Bool of string

*)



(*
datatype decl = ValDecl of var * exp
and exp = NumExp of int
        | StringExp of string
        | BracketExp of exp 
        | VarExp of var
        | BinExp of binop * exp * exp
        | DeclExp of decl
        | UnExp of unop * exp
        | LetExp of decl * exp
        | ItefExp of exp * exp * exp 
        | ConstExp of string
        | StmtExp of exp
        | fnExp of var * v * v * exp
        | FunExp of var * var * v * v * exp
        | AppExp of exp * exp


*)


(*fun exp2string (e:exp):string =
    case e of
          NumExp(i) => Int.toString(i)
        | StringExp (s) => s
        | BracketExp (e1) => "("^exp2string(e1)^")"
        | VarExp (v) => v
        | BinExp (b, e1, e2) => exp2string(e1)^" "^bop2string(b)^" "^exp2string(e2)
        | DeclExp (ValDecl(x, e1)) => "val "^x^" = "^exp2string(e1)
        | UnExp (u, e1) => uop2string(u)^" "^exp2string(e1)
        | LetExp (ValDecl(x, e1), e2) => "let val "^x^" = "^exp2string(e1)^" in "^exp2string(e2)
        | ItefExp (e1, e2, e3) => "if "^exp2string(e1)^" then "^exp2string(e2)^" else "^exp2string(e3)^" fi "
        | ConstExp (s) => s
        | StmtExp (e1) => exp2string(e1)
        | fnExp (v,t1,t2,e1) => "fn ("^v^":"^typ2string(t1)^"):"^typ2string(t2)^" => "^exp2string(e1)
        | FunExp (v1,v2,t1,t2,e1) => "fun "^v1^"("^v2^":"^typ2string(t1)^"):"^typ2string(t2)^" => "^exp2string(e1)
        | AppExp (e1, e2) => "("^exp2string(e1)^" "^exp2string(e2)^")";

*)
(*
type environment = (var * value) list
type fenvironment = (var * var * exp * v * v) list

fun envAdd_iter(variable,v,env,nenv, p) =
    case env of
        []=> if(p>0) then nenv else (variable,v)::nenv
    |    (x,vx)::ls => if (variable = x) then envAdd_iter(variable,v,ls,(variable,v)::nenv,p+1) else envAdd_iter(variable,v,ls,(x,vx)::nenv,p);

fun fenvAdd_iter(f,x,e,t1,t2,fenv,nfenv,p) =
    case fenv of
        []=> if(p>0) then nfenv else (f,x,e,t1,t2)::nfenv
    |    (ff,xx,ee,tt1,tt2)::ls => if (f = ff) then fenvAdd_iter(f,x,e,t1,t2,ls,(f,x,e,t1,t2)::nfenv,p+1) else fenvAdd_iter(f,x,e,t1,t2,ls,(ff,xx,ee,tt1,tt2)::nfenv,p);

fun envAdd (variable:var, v:value, env:environment) = envAdd_iter(variable,v,env,[],0);

fun fenvAdd (f,x,e,t1,t2,fenv) = fenvAdd_iter(f,x,e,t1,t2,fenv,[],0);

fun envLookup (variable:var, env:environment):value =
    case List.find(fn (x, _) => x = variable) env of
				        SOME (x, v)   => v
				    |   NONE => error(print(""));

fun fenvLookup (f:var,fenv:fenvironment):value =
    case List.find(fn (x, _,_,_,_) => x = f) fenv of
				        SOME (x, v,e,t1,t2)   => FunVal (v,e,t1,t2)
				    |   NONE => error(print(""));

*)                    

end
