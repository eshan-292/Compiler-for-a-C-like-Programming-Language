open AST
exception Error

%%

%name AST


%term program | cons | var of string | variab | int | bool | colon | semicolon | comma | lbrace | rbrace | colonequal | read | write | if_term | then_term | else_term | endif | while_term | do_term | endwh | lparen | rparen | tilde | parallel | andand | tr | fal | exclam | less | lessequal | equal | greater | greaterequal | lessgreat | plus | minus | mult | divi | percent | Letter | Digit | number of int |
(* A | B | C | D | E | F | G | H |
I | J | K | L | M | N | O | P | Q |
R | S | T | U | V | W | X | Y | Z |
a | b | c | d | e | f | g | h |
i | j | k | l | m | n | o | p | q |
r | s | t | u | v | w | x | y | z | zero | *)
EOF

(* %nonterm Expressionar of AST * string | Termar of AST * string | Termand of AST * string | Factor of AST * string | Factorbool of AST * string | Expressionbool of AST * string| Program of AST | Identifier of AST | Block of (string list * string) list * AST list | DeclarationSeq of (string list * string) list | Declaration of string list * string | Type of AST.Type | VariableList of AST.VariableList | CommandSeq of AST.CommandSeq | Command of AST.Command | Expression of AST.Expression | IntExpression of AST.IntExpression | IntTerm of AST.IntTerm| IntFactor of AST.IntFactor | BoolExpression of AST.BoolExpression| BoolTerm of AST.BoolTerm | BoolFactor of AST.BoolFactor | Comparison of AST.Comparison | Variable of AST | RelOp of AST.RelOp | AddOp of AST.AddOp | MultOp of AST.MultOp | Numeral of AST.Numeral | DigitSeq of AST.DigitSeq | PorT  of AST.PorT| PorTSeq of AST.PorTSeq | LorD of AST.LorD | LorDSeq of AST.LorDSeq | IncommandSeq of AST list *)
%nonterm Expressionar of AST * string | Termar of AST * string | Termand of AST * string | Factor of AST * string | Factorbool of AST * string | Expressionbool of AST * string | Program of AST | Identifier of string | Block of (string list * string) list * AST list | DeclarationSeq of (string list * string) list | Declaration of string list * string | VariableList of string list | Seqe of AST list | CommandSeq of AST list | Comm | Commseq | Command of AST | Identifierr | Comparison of AST * string | Variable of string | Numeral of int | IncommandSeq of AST list | va of AST

%pos int
%arg (fileName) : string
%eop EOF
%noshift EOF
%start Program 
%verbose 

%left equal greater greaterequal less lessequal plus minus percent parallel andand 

%left divi mult

%left tilde

%nodefault

%verbose

%% 

Program : program Identifier cons Block (program(Identifier, Block))

Block : DeclarationSeq CommandSeq ((DeclarationSeq, CommandSeq))

(* DeclarationSeq :  Declaration (AST.Decl(Declaration)) | Declaration DeclarationSeq (AST.DeclSeq(Declaration, DeclarationSeq))*)
DeclarationSeq :  ([]) | Declaration DeclarationSeq(Declaration :: DeclarationSeq)


(* Declaration : var VariableList colon Type semicolon (AST.declaration(VariableList, Type)) *)
Declaration : variab VariableList colon int semicolon (case check(VariableList) of SOME x => raise Error | None => add(VariableList, "int"); (VariableList, "int") )
            | variab VariableList colon bool semicolon (case check(VariableList) of SOME x => raise Error | None => add(VariableList, "bool"); (VariableList, "bool") )

(* Type : int (AST.int) | bool (AST.bool) *)

Numeral : number(number) 
| plus number (number)
CommandSeq: lbrace IncommandSeq rbrace (IncommandSeq)
IncommandSeq : ([]) | Command semicolon IncommandSeq (Command :: IncommandSeq)



Commseq : lbrace IncommandSeq rbrace()
Comm: Variable colonequal Expressionbool()

(* VariableList  : Variable (AST.variable(Variable)) | Variable comma VariableList (AST.variablelist(Variable, VariableList)) *)
VariableList : Variable (Variable :: []) | Variable comma VariableList (Variable :: VariableList)
(*  Command : Variable colonequal Expression (AST.command(Variable, Expression)) | read Variable (AST.readcommand(Variable)) | write Variable (AST.writecommand(Variable)) | if_term BoolExpression then_term CommandSeq else_term CommandSeq endif (AST.ite(BoolExpression, CommandSeq, CommandSeq)) | while_term BoolExpression do_term CommandSeq endwh (AST.whilecommand(BoolExpression, CommandSeq)) *)
Command: Variable colonequal Expressionbool (case find(Variable) of SOME x => (if ((#2 Expressionbool) = x) then (assign(Variable, (#1 Expressionbool))) else raise Error) | NONE => raise Error)
        | read Variable (read(Variable))
        | write Expressionbool (write(#1 Expressionbool))
        | if_term Expressionbool then_term CommandSeq else_term CommandSeq endif (if((#2 Expressionbool) = "bool") then ifthel((#1 Expressionbool), CommandSeq1, CommandSeq2) else raise Error)
        | while_term Expressionbool do_term CommandSeq endwh (if((#2 Expressionbool) = "bool") then WHILE((#1 Expressionbool), CommandSeq) else raise Error)
(* CommandSeq : lbrace Command semicolon rbrace () | lbrace Command semicolon rbrace () *)

Factor : Numeral ((Int(Numeral), "int")) | Variable (case find(Variable) of SOME x => (identifi(Variable), x) | NONE => raise Error) | lparen Expressionbool rparen (Expressionbool) | tr(Bool(true), "bool") | fal(Bool(false), "false")| tilde Factor (if((#2 Factor) = "int") then (Neg((#1 Factor)), "int") else raise Error)

Factorbool : Comparison(Comparison) | exclam Factorbool (Not((#1 Factorbool)), "bool" ) 

Expressionbool: Expressionbool parallel Termand (if(((#2 Termand) = "bool") andalso ((#2 Expressionbool) = "bool")) then (OR((#1 Expressionbool),(#1 Termand)),"bool") else raise Error)
        | Termand (Termand)

Expressionar : Expressionar plus Termar (if(((#2 Termar) = "int") andalso ((#2 Expressionar) = "int")) then (plus((#1 Expressionar), (#1 Termar)),"int") else raise Error) 
            | Expressionar minus Termar (if(((#2 Termar) = "int") andalso ((#2 Expressionar) = "int")) then (minus((#1 Expressionar), (#1 Termar)),"int") else raise Error) 
            | Termar (Termar)



Termar: Termar divi Factor (if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (divi((#1 Termar), (#1 Factor)),"int") else raise Error)
        | Termar mult Factor (if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (mult((#1 Termar), (#1 Factor)),"int") else raise Error)
        | Termar percent Factor (if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (percent((#1 Termar), (#1 Factor)),"int") else raise Error)
        | Factor (Factor)

Termand: Termand andand Factorbool (if(((#2 Termand) = "bool") andalso ((#2 Factorbool) = "bool")) then (AND((#1 Termand), (#1 Factorbool)),"bool") else raise Error)
        | Factorbool (Factorbool)

Comparison: Expressionar less Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (less((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        | Expressionar lessequal Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (lessequal((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        
        | Expressionar equal Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (less((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        | Expressionar greater Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (greater((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        | Expressionar greaterequal Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (greaterequal((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        | Expressionar lessgreat Expressionar (if((#2 Expressionar1) = (#2 Expressionar2)) then (lessgreat((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error)
        
        | Expressionar (Expressionar)


(*
CommandSeq : lbrace Incommandseq rbrace (AST.InCommSeq(Incommandseq)) 

Incommandseq : Command semicolon (AST.incomm(Command)) | Command semicolon Incommandseq (AST.incommseq(Command, Incommandseq))

Command : Variable colonequal Expression (AST.command(Variable, Expression)) | read Variable (AST.readcommand(Variable)) | write Variable (AST.writecommand(Variable)) | if_term BoolExpression then_term CommandSeq else_term CommandSeq endif (AST.ite(BoolExpression, CommandSeq, CommandSeq)) | while_term BoolExpression do_term CommandSeq endwh (AST.whilecommand(BoolExpression, CommandSeq)) 

Expression : IntExpression (AST.intexp(IntExpression)) | BoolExpression (AST.boolexp(BoolExpression))

IntExpression : IntExpression AddOp IntTerm (AST.intexpp(IntExpression, IntTerm)) | IntTerm (AST.intterm(IntTerm))

IntTerm : IntTerm MultOp IntFactor (AST.inttermm(IntTerm, IntFactor)) | IntFactor (AST.intfac(IntFactor))

IntFactor : Numeral (AST.num(Numeral)) | Variable (AST.vari(Variable)) | lparen IntExpression rparen (AST.intexppp(IntExpression)) | tilde IntFactor (AST.intfact(IntFactor))

BoolExpression : BoolExpression parallel BoolTerm (AST.boolexpp(BoolExpression, BoolTerm)) | BoolTerm (AST.boolterm(BoolTerm))

BoolTerm : BoolTerm andand BoolFactor (AST.booltermm(BoolTerm, BoolFactor)) | BoolFactor (AST.boolfac(BoolFactor))

BoolFactor : tr (AST.tr) | fal (AST.fal) | Variable (AST.varia(Variable)) | Comparison (AST.comp(Comparison)) | lparen BoolExpression rparen (AST.boolexppp(BoolExpression)) | exclam BoolFactor (AST.boolfact(BoolFactor))


Comparison : IntExpression RelOp IntExpression (AST.compar(IntExpression, IntExpression))

*)

(* Variable : Identifier (AST.variab(Identifier)) *)
Variable : Identifier(Identifier)   (*To convert this into an AST object *)


(*
RelOp : less (AST.less) | lessequal (AST.lessequal) | equal (AST.equal) | greater (AST.greater) | greaterequal (AST.greaterequal) | lessgreat (AST.equal)

AddOp : plus (AST.plusaddop) | minus (AST.minusaddop)

MultOp : divi (AST.divi) | mult (AST.mult) | percent (AST.percent)

*)

 Identifier : var(var)

(*
Identifier : Letter LorDSeq (AST.identifier(LorDSeq))            (* Identifier : Letter {Letter | Digit} *)

LorDSeq : LorD LorDSeq (AST.lordseq(LorD, LorDSeq)) | (AST.emplordseq)          (* Assuming {} indicates 0 is included *)

LorD : Letter (AST.Letter) | Digit (AST.Digit)

Numeral : PorTSeq Digit DigitSeq (AST.numer(PorTSeq, DigitSeq))         (* Numeral : [ plus | tilde]Digit{Digit}  *)

PorTSeq : PorT PorTSeq (AST.portseq(PorT, PorTSeq)) | PorT (AST.port(PorT))              (* Assuming [] indicate 0 is not included *)

PorT : plus (AST.plusport) | tilde (AST.tildeport) 

DigitSeq : Digit DigitSeq (AST.digseq(DigitSeq)) | (AST.emp)

*)

Identifierr : Variable()

(*
Letter : UpperCase () | LowerCase ()

UpperCase : A () | B () | C () | D () | E () | F () | G () | H () |
I () | J () | K () | L () | M () | N () | O () | P () | Q () |
R () | S () | T () | U () | V () | W () | X () | Y () | Z ()


LowerCase : a () | b () | c () | d () | e () | f () | g () | h () |
i () | j () | k () | l () | m () | n () | o () | p () | q () |
r () | s () | t () | u () | v () | w () | x () | y () | z ()

Digit : zero ()      (* Just for now to be changed later *)   

*)

(* Digit : “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9” *)

