functor ASTLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : AST_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AST
exception Error


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\003\000\000\000\
\\001\000\002\000\142\000\007\000\142\000\008\000\142\000\009\000\142\000\
\\012\000\142\000\016\000\142\000\020\000\142\000\023\000\142\000\
\\025\000\142\000\026\000\142\000\030\000\142\000\031\000\142\000\
\\032\000\142\000\033\000\142\000\034\000\142\000\035\000\142\000\
\\036\000\142\000\037\000\142\000\038\000\142\000\039\000\142\000\
\\040\000\142\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\003\000\005\000\000\000\
\\001\000\003\000\005\000\011\000\104\000\013\000\023\000\014\000\022\000\
\\015\000\021\000\019\000\020\000\000\000\
\\001\000\003\000\005\000\022\000\044\000\024\000\043\000\027\000\042\000\
\\028\000\041\000\029\000\040\000\036\000\039\000\043\000\038\000\000\000\
\\001\000\003\000\005\000\022\000\044\000\024\000\043\000\027\000\042\000\
\\028\000\041\000\036\000\039\000\043\000\038\000\000\000\
\\001\000\004\000\099\000\010\000\099\000\000\000\
\\001\000\004\000\100\000\010\000\100\000\000\000\
\\001\000\004\000\010\000\010\000\097\000\000\000\
\\001\000\005\000\050\000\006\000\049\000\000\000\
\\001\000\007\000\108\000\009\000\024\000\000\000\
\\001\000\007\000\109\000\000\000\
\\001\000\007\000\141\000\008\000\141\000\009\000\141\000\012\000\141\000\
\\016\000\141\000\020\000\141\000\023\000\141\000\025\000\141\000\
\\026\000\141\000\030\000\141\000\031\000\141\000\032\000\141\000\
\\033\000\141\000\034\000\141\000\035\000\141\000\036\000\141\000\
\\037\000\141\000\038\000\141\000\039\000\141\000\040\000\141\000\000\000\
\\001\000\007\000\025\000\000\000\
\\001\000\008\000\101\000\016\000\101\000\020\000\101\000\023\000\101\000\
\\025\000\101\000\026\000\101\000\030\000\101\000\031\000\101\000\
\\032\000\101\000\033\000\101\000\034\000\101\000\035\000\101\000\
\\036\000\101\000\037\000\101\000\038\000\101\000\039\000\101\000\
\\040\000\101\000\000\000\
\\001\000\008\000\102\000\016\000\102\000\020\000\102\000\023\000\102\000\
\\025\000\102\000\026\000\102\000\030\000\102\000\031\000\102\000\
\\032\000\102\000\033\000\102\000\034\000\102\000\035\000\102\000\
\\036\000\102\000\037\000\102\000\038\000\102\000\039\000\102\000\
\\040\000\102\000\000\000\
\\001\000\008\000\110\000\025\000\053\000\000\000\
\\001\000\008\000\111\000\000\000\
\\001\000\008\000\112\000\025\000\053\000\000\000\
\\001\000\008\000\113\000\000\000\
\\001\000\008\000\114\000\000\000\
\\001\000\008\000\115\000\016\000\115\000\020\000\115\000\023\000\115\000\
\\025\000\115\000\026\000\115\000\030\000\115\000\031\000\115\000\
\\032\000\115\000\033\000\115\000\034\000\115\000\035\000\115\000\
\\036\000\115\000\037\000\115\000\038\000\115\000\039\000\115\000\
\\040\000\115\000\000\000\
\\001\000\008\000\116\000\016\000\116\000\020\000\116\000\023\000\116\000\
\\025\000\116\000\026\000\116\000\030\000\116\000\031\000\116\000\
\\032\000\116\000\033\000\116\000\034\000\116\000\035\000\116\000\
\\036\000\116\000\037\000\116\000\038\000\116\000\039\000\116\000\
\\040\000\116\000\000\000\
\\001\000\008\000\117\000\016\000\117\000\020\000\117\000\023\000\117\000\
\\025\000\117\000\026\000\117\000\030\000\117\000\031\000\117\000\
\\032\000\117\000\033\000\117\000\034\000\117\000\035\000\117\000\
\\036\000\117\000\037\000\117\000\038\000\117\000\039\000\117\000\
\\040\000\117\000\000\000\
\\001\000\008\000\118\000\016\000\118\000\020\000\118\000\023\000\118\000\
\\025\000\118\000\026\000\118\000\030\000\118\000\031\000\118\000\
\\032\000\118\000\033\000\118\000\034\000\118\000\035\000\118\000\
\\036\000\118\000\037\000\118\000\038\000\118\000\039\000\118\000\
\\040\000\118\000\000\000\
\\001\000\008\000\119\000\016\000\119\000\020\000\119\000\023\000\119\000\
\\025\000\119\000\026\000\119\000\030\000\119\000\031\000\119\000\
\\032\000\119\000\033\000\119\000\034\000\119\000\035\000\119\000\
\\036\000\119\000\037\000\119\000\038\000\119\000\039\000\119\000\
\\040\000\119\000\000\000\
\\001\000\008\000\120\000\016\000\120\000\020\000\120\000\023\000\120\000\
\\025\000\120\000\026\000\120\000\030\000\120\000\031\000\120\000\
\\032\000\120\000\033\000\120\000\034\000\120\000\035\000\120\000\
\\036\000\120\000\037\000\120\000\038\000\120\000\039\000\120\000\
\\040\000\120\000\000\000\
\\001\000\008\000\121\000\016\000\121\000\020\000\121\000\023\000\121\000\
\\025\000\121\000\026\000\121\000\000\000\
\\001\000\008\000\122\000\016\000\122\000\020\000\122\000\023\000\122\000\
\\025\000\122\000\026\000\122\000\000\000\
\\001\000\008\000\123\000\016\000\123\000\020\000\123\000\023\000\123\000\
\\025\000\123\000\026\000\055\000\000\000\
\\001\000\008\000\124\000\016\000\124\000\020\000\124\000\023\000\124\000\
\\025\000\124\000\026\000\055\000\000\000\
\\001\000\008\000\125\000\016\000\125\000\020\000\125\000\023\000\125\000\
\\025\000\125\000\026\000\125\000\030\000\125\000\031\000\125\000\
\\032\000\125\000\033\000\125\000\034\000\125\000\035\000\125\000\
\\036\000\125\000\037\000\125\000\038\000\058\000\039\000\057\000\
\\040\000\056\000\000\000\
\\001\000\008\000\126\000\016\000\126\000\020\000\126\000\023\000\126\000\
\\025\000\126\000\026\000\126\000\030\000\126\000\031\000\126\000\
\\032\000\126\000\033\000\126\000\034\000\126\000\035\000\126\000\
\\036\000\126\000\037\000\126\000\038\000\058\000\039\000\057\000\
\\040\000\056\000\000\000\
\\001\000\008\000\127\000\016\000\127\000\020\000\127\000\023\000\127\000\
\\025\000\127\000\026\000\127\000\030\000\127\000\031\000\127\000\
\\032\000\127\000\033\000\127\000\034\000\127\000\035\000\127\000\
\\036\000\127\000\037\000\127\000\038\000\058\000\039\000\057\000\
\\040\000\056\000\000\000\
\\001\000\008\000\128\000\016\000\128\000\020\000\128\000\023\000\128\000\
\\025\000\128\000\026\000\128\000\030\000\128\000\031\000\128\000\
\\032\000\128\000\033\000\128\000\034\000\128\000\035\000\128\000\
\\036\000\128\000\037\000\128\000\038\000\128\000\039\000\128\000\
\\040\000\128\000\000\000\
\\001\000\008\000\129\000\016\000\129\000\020\000\129\000\023\000\129\000\
\\025\000\129\000\026\000\129\000\030\000\129\000\031\000\129\000\
\\032\000\129\000\033\000\129\000\034\000\129\000\035\000\129\000\
\\036\000\129\000\037\000\129\000\038\000\129\000\039\000\129\000\
\\040\000\129\000\000\000\
\\001\000\008\000\130\000\016\000\130\000\020\000\130\000\023\000\130\000\
\\025\000\130\000\026\000\130\000\030\000\130\000\031\000\130\000\
\\032\000\130\000\033\000\130\000\034\000\130\000\035\000\130\000\
\\036\000\130\000\037\000\130\000\038\000\130\000\039\000\130\000\
\\040\000\130\000\000\000\
\\001\000\008\000\131\000\016\000\131\000\020\000\131\000\023\000\131\000\
\\025\000\131\000\026\000\131\000\030\000\131\000\031\000\131\000\
\\032\000\131\000\033\000\131\000\034\000\131\000\035\000\131\000\
\\036\000\131\000\037\000\131\000\038\000\131\000\039\000\131\000\
\\040\000\131\000\000\000\
\\001\000\008\000\132\000\016\000\132\000\020\000\132\000\023\000\132\000\
\\025\000\132\000\026\000\132\000\000\000\
\\001\000\008\000\133\000\016\000\133\000\020\000\133\000\023\000\133\000\
\\025\000\133\000\026\000\133\000\000\000\
\\001\000\008\000\134\000\016\000\134\000\020\000\134\000\023\000\134\000\
\\025\000\134\000\026\000\134\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\135\000\016\000\135\000\020\000\135\000\023\000\135\000\
\\025\000\135\000\026\000\135\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\136\000\016\000\136\000\020\000\136\000\023\000\136\000\
\\025\000\136\000\026\000\136\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\137\000\016\000\137\000\020\000\137\000\023\000\137\000\
\\025\000\137\000\026\000\137\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\138\000\016\000\138\000\020\000\138\000\023\000\138\000\
\\025\000\138\000\026\000\138\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\139\000\016\000\139\000\020\000\139\000\023\000\139\000\
\\025\000\139\000\026\000\139\000\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\140\000\016\000\140\000\020\000\140\000\023\000\140\000\
\\025\000\140\000\026\000\140\000\030\000\066\000\031\000\065\000\
\\032\000\064\000\033\000\063\000\034\000\062\000\035\000\061\000\
\\036\000\060\000\037\000\059\000\000\000\
\\001\000\008\000\028\000\000\000\
\\001\000\008\000\072\000\000\000\
\\001\000\008\000\073\000\000\000\
\\001\000\010\000\098\000\000\000\
\\001\000\010\000\013\000\000\000\
\\001\000\011\000\105\000\000\000\
\\001\000\011\000\026\000\000\000\
\\001\000\012\000\027\000\000\000\
\\001\000\016\000\071\000\025\000\053\000\000\000\
\\001\000\017\000\103\000\018\000\103\000\021\000\103\000\044\000\103\000\000\000\
\\001\000\017\000\091\000\000\000\
\\001\000\018\000\093\000\000\000\
\\001\000\020\000\054\000\025\000\053\000\000\000\
\\001\000\021\000\090\000\000\000\
\\001\000\023\000\088\000\025\000\053\000\000\000\
\\001\000\043\000\067\000\000\000\
\\001\000\044\000\000\000\000\000\
\\001\000\044\000\095\000\000\000\
\\001\000\044\000\096\000\000\000\
\"
val actionRowNumbers =
"\000\000\003\000\002\000\001\000\
\\009\000\009\000\052\000\065\000\
\\003\000\051\000\066\000\004\000\
\\011\000\014\000\013\000\054\000\
\\055\000\048\000\005\000\005\000\
\\005\000\003\000\003\000\010\000\
\\057\000\005\000\004\000\022\000\
\\023\000\028\000\060\000\040\000\
\\038\000\031\000\034\000\047\000\
\\015\000\063\000\005\000\026\000\
\\025\000\006\000\005\000\056\000\
\\019\000\018\000\012\000\049\000\
\\050\000\017\000\053\000\005\000\
\\052\000\005\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\016\000\029\000\027\000\
\\062\000\052\000\008\000\007\000\
\\030\000\061\000\039\000\037\000\
\\035\000\036\000\033\000\032\000\
\\046\000\045\000\044\000\043\000\
\\042\000\041\000\024\000\058\000\
\\021\000\052\000\059\000\020\000\
\\064\000"
val gotoT =
"\
\\007\000\092\000\000\000\
\\008\000\002\000\000\000\
\\000\000\
\\000\000\
\\009\000\007\000\010\000\006\000\011\000\005\000\000\000\
\\010\000\009\000\011\000\005\000\000\000\
\\014\000\010\000\000\000\
\\000\000\
\\008\000\014\000\012\000\013\000\020\000\012\000\000\000\
\\000\000\
\\000\000\
\\008\000\014\000\017\000\017\000\020\000\016\000\022\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\002\000\034\000\003\000\033\000\004\000\032\000\
\\005\000\031\000\006\000\030\000\008\000\014\000\019\000\029\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\035\000\002\000\034\000\003\000\033\000\004\000\032\000\
\\005\000\031\000\006\000\043\000\008\000\014\000\019\000\029\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\035\000\002\000\034\000\003\000\033\000\004\000\032\000\
\\005\000\031\000\006\000\044\000\008\000\014\000\019\000\029\000\
\\020\000\028\000\021\000\027\000\000\000\
\\008\000\014\000\020\000\045\000\000\000\
\\008\000\014\000\012\000\046\000\020\000\012\000\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\002\000\034\000\003\000\033\000\004\000\032\000\
\\005\000\031\000\006\000\049\000\008\000\014\000\019\000\029\000\
\\020\000\028\000\021\000\027\000\000\000\
\\008\000\014\000\017\000\017\000\020\000\016\000\022\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\002\000\034\000\004\000\032\000\005\000\066\000\
\\008\000\014\000\019\000\029\000\020\000\028\000\021\000\027\000\000\000\
\\000\000\
\\000\000\
\\004\000\067\000\008\000\014\000\020\000\028\000\021\000\027\000\000\000\
\\001\000\035\000\002\000\034\000\003\000\033\000\004\000\032\000\
\\005\000\031\000\006\000\068\000\008\000\014\000\019\000\029\000\
\\020\000\028\000\021\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\002\000\034\000\003\000\072\000\004\000\032\000\
\\005\000\031\000\008\000\014\000\019\000\029\000\020\000\028\000\
\\021\000\027\000\000\000\
\\014\000\073\000\000\000\
\\001\000\035\000\002\000\034\000\004\000\032\000\005\000\074\000\
\\008\000\014\000\019\000\029\000\020\000\028\000\021\000\027\000\000\000\
\\004\000\075\000\008\000\014\000\020\000\028\000\021\000\027\000\000\000\
\\004\000\076\000\008\000\014\000\020\000\028\000\021\000\027\000\000\000\
\\004\000\077\000\008\000\014\000\020\000\028\000\021\000\027\000\000\000\
\\002\000\078\000\004\000\032\000\008\000\014\000\020\000\028\000\
\\021\000\027\000\000\000\
\\002\000\079\000\004\000\032\000\008\000\014\000\020\000\028\000\
\\021\000\027\000\000\000\
\\001\000\080\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\081\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\082\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\083\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\084\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\001\000\085\000\002\000\034\000\004\000\032\000\008\000\014\000\
\\020\000\028\000\021\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 93
val numrules = 49
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | number of unit ->  (int) | var of unit ->  (string)
 | va of unit ->  (AST) | IncommandSeq of unit ->  (AST list)
 | Numeral of unit ->  (int) | Variable of unit ->  (string)
 | Comparison of unit ->  (AST*string) | Command of unit ->  (AST)
 | CommandSeq of unit ->  (AST list) | Seqe of unit ->  (AST list)
 | VariableList of unit ->  (string list)
 | Declaration of unit ->  (string list*string)
 | DeclarationSeq of unit ->  ( ( string list * string )  list)
 | Block of unit ->  ( ( string list * string )  list*AST list)
 | Identifier of unit ->  (string) | Program of unit ->  (AST)
 | Expressionbool of unit ->  (AST*string)
 | Factorbool of unit ->  (AST*string)
 | Factor of unit ->  (AST*string) | Termand of unit ->  (AST*string)
 | Termar of unit ->  (AST*string)
 | Expressionar of unit ->  (AST*string)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 43) => true | _ => false
val showTerminal =
fn (T 0) => "program"
  | (T 1) => "cons"
  | (T 2) => "var"
  | (T 3) => "variab"
  | (T 4) => "int"
  | (T 5) => "bool"
  | (T 6) => "colon"
  | (T 7) => "semicolon"
  | (T 8) => "comma"
  | (T 9) => "lbrace"
  | (T 10) => "rbrace"
  | (T 11) => "colonequal"
  | (T 12) => "read"
  | (T 13) => "write"
  | (T 14) => "if_term"
  | (T 15) => "then_term"
  | (T 16) => "else_term"
  | (T 17) => "endif"
  | (T 18) => "while_term"
  | (T 19) => "do_term"
  | (T 20) => "endwh"
  | (T 21) => "lparen"
  | (T 22) => "rparen"
  | (T 23) => "tilde"
  | (T 24) => "parallel"
  | (T 25) => "andand"
  | (T 26) => "tr"
  | (T 27) => "fal"
  | (T 28) => "exclam"
  | (T 29) => "less"
  | (T 30) => "lessequal"
  | (T 31) => "equal"
  | (T 32) => "greater"
  | (T 33) => "greaterequal"
  | (T 34) => "lessgreat"
  | (T 35) => "plus"
  | (T 36) => "minus"
  | (T 37) => "mult"
  | (T 38) => "divi"
  | (T 39) => "percent"
  | (T 40) => "Letter"
  | (T 41) => "Digit"
  | (T 42) => "number"
  | (T 43) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _,
 ( MlyValue.Identifier Identifier1, _, _)) :: ( _, ( _, program1left,
 _)) :: rest671)) => let val  result = MlyValue.Program (fn _ => let
 val  (Identifier as Identifier1) = Identifier1 ()
 val  (Block as Block1) = Block1 ()
 in (program(Identifier, Block))
end)
 in ( LrTable.NT 6, ( result, program1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in ((DeclarationSeq, CommandSeq))
end)
 in ( LrTable.NT 8, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.DeclarationSeq (fn _
 => ([]))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in (Declaration :: DeclarationSeq)
end)
 in ( LrTable.NT 9, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 4, ( ( _, ( _, _, semicolon1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, variab1left,
 _)) :: rest671)) => let val  result = MlyValue.Declaration (fn _ =>
 let val  (VariableList as VariableList1) = VariableList1 ()
 in (
case check(VariableList) of SOME x => raise Error | None => add(VariableList, "int"); (VariableList, "int") 
)
end)
 in ( LrTable.NT 10, ( result, variab1left, semicolon1right), rest671)

end
|  ( 5, ( ( _, ( _, _, semicolon1right)) :: _ :: _ :: ( _, ( 
MlyValue.VariableList VariableList1, _, _)) :: ( _, ( _, variab1left,
 _)) :: rest671)) => let val  result = MlyValue.Declaration (fn _ =>
 let val  (VariableList as VariableList1) = VariableList1 ()
 in (
case check(VariableList) of SOME x => raise Error | None => add(VariableList, "bool"); (VariableList, "bool") 
)
end)
 in ( LrTable.NT 10, ( result, variab1left, semicolon1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.number number1, number1left, number1right))
 :: rest671)) => let val  result = MlyValue.Numeral (fn _ => let val 
 (number as number1) = number1 ()
 in (number)
end)
 in ( LrTable.NT 20, ( result, number1left, number1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.number number1, _, number1right)) :: ( _, (
 _, plus1left, _)) :: rest671)) => let val  result = MlyValue.Numeral
 (fn _ => let val  (number as number1) = number1 ()
 in (number)
end)
 in ( LrTable.NT 20, ( result, plus1left, number1right), rest671)
end
|  ( 8, ( ( _, ( _, _, rbrace1right)) :: ( _, ( MlyValue.IncommandSeq 
IncommandSeq1, _, _)) :: ( _, ( _, lbrace1left, _)) :: rest671)) =>
 let val  result = MlyValue.CommandSeq (fn _ => let val  (IncommandSeq
 as IncommandSeq1) = IncommandSeq1 ()
 in (IncommandSeq)
end)
 in ( LrTable.NT 13, ( result, lbrace1left, rbrace1right), rest671)

end
|  ( 9, ( rest671)) => let val  result = MlyValue.IncommandSeq (fn _
 => ([]))
 in ( LrTable.NT 21, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.IncommandSeq IncommandSeq1, _, 
IncommandSeq1right)) :: _ :: ( _, ( MlyValue.Command Command1, 
Command1left, _)) :: rest671)) => let val  result = 
MlyValue.IncommandSeq (fn _ => let val  (Command as Command1) = 
Command1 ()
 val  (IncommandSeq as IncommandSeq1) = IncommandSeq1 ()
 in (Command :: IncommandSeq)
end)
 in ( LrTable.NT 21, ( result, Command1left, IncommandSeq1right), 
rest671)
end
|  ( 11, ( ( _, ( _, _, rbrace1right)) :: ( _, ( MlyValue.IncommandSeq
 IncommandSeq1, _, _)) :: ( _, ( _, lbrace1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  IncommandSeq1 =
 IncommandSeq1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, lbrace1left, rbrace1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.Expressionbool Expressionbool1, _, 
Expressionbool1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  Variable1 = Variable1 ()
 val  Expressionbool1 = Expressionbool1 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, Variable1left, Expressionbool1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (Variable :: [])
end)
 in ( LrTable.NT 11, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 14, ( ( _, ( MlyValue.VariableList VariableList1, _, 
VariableList1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 val  (VariableList as VariableList1) = VariableList1 ()
 in (Variable :: VariableList)
end)
 in ( LrTable.NT 11, ( result, Variable1left, VariableList1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expressionbool Expressionbool1, _, 
Expressionbool1right)) :: _ :: ( _, ( MlyValue.Variable Variable1, 
Variable1left, _)) :: rest671)) => let val  result = MlyValue.Command
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 val  (Expressionbool as Expressionbool1) = Expressionbool1 ()
 in (
case find(Variable) of SOME x => (if ((#2 Expressionbool) = x) then (assign(Variable, (#1 Expressionbool))) else raise Error) | NONE => raise Error
)
end)
 in ( LrTable.NT 16, ( result, Variable1left, Expressionbool1right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, read1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (read(Variable))
end)
 in ( LrTable.NT 16, ( result, read1left, Variable1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.Expressionbool Expressionbool1, _, 
Expressionbool1right)) :: ( _, ( _, write1left, _)) :: rest671)) =>
 let val  result = MlyValue.Command (fn _ => let val  (Expressionbool
 as Expressionbool1) = Expressionbool1 ()
 in (write(#1 Expressionbool))
end)
 in ( LrTable.NT 16, ( result, write1left, Expressionbool1right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, endif1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.Expressionbool Expressionbool1, _, _)) ::
 ( _, ( _, if_term1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Expressionbool as Expressionbool1)
 = Expressionbool1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (
if((#2 Expressionbool) = "bool") then ifthel((#1 Expressionbool), CommandSeq1, CommandSeq2) else raise Error
)
end)
 in ( LrTable.NT 16, ( result, if_term1left, endif1right), rest671)

end
|  ( 19, ( ( _, ( _, _, endwh1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.Expressionbool 
Expressionbool1, _, _)) :: ( _, ( _, while_term1left, _)) :: rest671))
 => let val  result = MlyValue.Command (fn _ => let val  (
Expressionbool as Expressionbool1) = Expressionbool1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (
if((#2 Expressionbool) = "bool") then WHILE((#1 Expressionbool), CommandSeq) else raise Error
)
end)
 in ( LrTable.NT 16, ( result, while_term1left, endwh1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.Numeral Numeral1, Numeral1left, 
Numeral1right)) :: rest671)) => let val  result = MlyValue.Factor (fn
 _ => let val  (Numeral as Numeral1) = Numeral1 ()
 in ((Int(Numeral), "int"))
end)
 in ( LrTable.NT 3, ( result, Numeral1left, Numeral1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.Factor (fn
 _ => let val  (Variable as Variable1) = Variable1 ()
 in (
case find(Variable) of SOME x => (identifi(Variable), x) | NONE => raise Error
)
end)
 in ( LrTable.NT 3, ( result, Variable1left, Variable1right), rest671)

end
|  ( 22, ( ( _, ( _, _, rparen1right)) :: ( _, ( 
MlyValue.Expressionbool Expressionbool1, _, _)) :: ( _, ( _, 
lparen1left, _)) :: rest671)) => let val  result = MlyValue.Factor (fn
 _ => let val  (Expressionbool as Expressionbool1) = Expressionbool1
 ()
 in (Expressionbool)
end)
 in ( LrTable.NT 3, ( result, lparen1left, rparen1right), rest671)
end
|  ( 23, ( ( _, ( _, tr1left, tr1right)) :: rest671)) => let val  
result = MlyValue.Factor (fn _ => (Bool(true), "bool"))
 in ( LrTable.NT 3, ( result, tr1left, tr1right), rest671)
end
|  ( 24, ( ( _, ( _, fal1left, fal1right)) :: rest671)) => let val  
result = MlyValue.Factor (fn _ => (Bool(false), "false"))
 in ( LrTable.NT 3, ( result, fal1left, fal1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: ( _, (
 _, tilde1left, _)) :: rest671)) => let val  result = MlyValue.Factor
 (fn _ => let val  (Factor as Factor1) = Factor1 ()
 in (
if((#2 Factor) = "int") then (Neg((#1 Factor)), "int") else raise Error
)
end)
 in ( LrTable.NT 3, ( result, tilde1left, Factor1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = 
MlyValue.Factorbool (fn _ => let val  (Comparison as Comparison1) = 
Comparison1 ()
 in (Comparison)
end)
 in ( LrTable.NT 4, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.Factorbool Factorbool1, _, Factorbool1right
)) :: ( _, ( _, exclam1left, _)) :: rest671)) => let val  result = 
MlyValue.Factorbool (fn _ => let val  (Factorbool as Factorbool1) = 
Factorbool1 ()
 in (Not((#1 Factorbool)), "bool" )
end)
 in ( LrTable.NT 4, ( result, exclam1left, Factorbool1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.Termand Termand1, _, Termand1right)) :: _
 :: ( _, ( MlyValue.Expressionbool Expressionbool1, 
Expressionbool1left, _)) :: rest671)) => let val  result = 
MlyValue.Expressionbool (fn _ => let val  (Expressionbool as 
Expressionbool1) = Expressionbool1 ()
 val  (Termand as Termand1) = Termand1 ()
 in (
if(((#2 Termand) = "bool") andalso ((#2 Expressionbool) = "bool")) then (OR((#1 Expressionbool),(#1 Termand)),"bool") else raise Error
)
end)
 in ( LrTable.NT 5, ( result, Expressionbool1left, Termand1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.Termand Termand1, Termand1left, 
Termand1right)) :: rest671)) => let val  result = 
MlyValue.Expressionbool (fn _ => let val  (Termand as Termand1) = 
Termand1 ()
 in (Termand)
end)
 in ( LrTable.NT 5, ( result, Termand1left, Termand1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.Termar Termar1, _, Termar1right)) :: _ :: (
 _, ( MlyValue.Expressionar Expressionar1, Expressionar1left, _)) :: 
rest671)) => let val  result = MlyValue.Expressionar (fn _ => let val 
 (Expressionar as Expressionar1) = Expressionar1 ()
 val  (Termar as Termar1) = Termar1 ()
 in (
if(((#2 Termar) = "int") andalso ((#2 Expressionar) = "int")) then (plus((#1 Expressionar), (#1 Termar)),"int") else raise Error
)
end)
 in ( LrTable.NT 0, ( result, Expressionar1left, Termar1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.Termar Termar1, _, Termar1right)) :: _ :: (
 _, ( MlyValue.Expressionar Expressionar1, Expressionar1left, _)) :: 
rest671)) => let val  result = MlyValue.Expressionar (fn _ => let val 
 (Expressionar as Expressionar1) = Expressionar1 ()
 val  (Termar as Termar1) = Termar1 ()
 in (
if(((#2 Termar) = "int") andalso ((#2 Expressionar) = "int")) then (minus((#1 Expressionar), (#1 Termar)),"int") else raise Error
)
end)
 in ( LrTable.NT 0, ( result, Expressionar1left, Termar1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.Termar Termar1, Termar1left, Termar1right))
 :: rest671)) => let val  result = MlyValue.Expressionar (fn _ => let
 val  (Termar as Termar1) = Termar1 ()
 in (Termar)
end)
 in ( LrTable.NT 0, ( result, Termar1left, Termar1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Termar Termar1, Termar1left, _)) :: rest671)) => let
 val  result = MlyValue.Termar (fn _ => let val  (Termar as Termar1) =
 Termar1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (
if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (divi((#1 Termar), (#1 Factor)),"int") else raise Error
)
end)
 in ( LrTable.NT 1, ( result, Termar1left, Factor1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Termar Termar1, Termar1left, _)) :: rest671)) => let
 val  result = MlyValue.Termar (fn _ => let val  (Termar as Termar1) =
 Termar1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (
if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (mult((#1 Termar), (#1 Factor)),"int") else raise Error
)
end)
 in ( LrTable.NT 1, ( result, Termar1left, Factor1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Termar Termar1, Termar1left, _)) :: rest671)) => let
 val  result = MlyValue.Termar (fn _ => let val  (Termar as Termar1) =
 Termar1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (
if(((#2 Termar) = "int") andalso ((#2 Factor) = "int")) then (percent((#1 Termar), (#1 Factor)),"int") else raise Error
)
end)
 in ( LrTable.NT 1, ( result, Termar1left, Factor1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Factor Factor1, Factor1left, Factor1right))
 :: rest671)) => let val  result = MlyValue.Termar (fn _ => let val  (
Factor as Factor1) = Factor1 ()
 in (Factor)
end)
 in ( LrTable.NT 1, ( result, Factor1left, Factor1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Factorbool Factorbool1, _, Factorbool1right
)) :: _ :: ( _, ( MlyValue.Termand Termand1, Termand1left, _)) :: 
rest671)) => let val  result = MlyValue.Termand (fn _ => let val  (
Termand as Termand1) = Termand1 ()
 val  (Factorbool as Factorbool1) = Factorbool1 ()
 in (
if(((#2 Termand) = "bool") andalso ((#2 Factorbool) = "bool")) then (AND((#1 Termand), (#1 Factorbool)),"bool") else raise Error
)
end)
 in ( LrTable.NT 2, ( result, Termand1left, Factorbool1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.Factorbool Factorbool1, Factorbool1left, 
Factorbool1right)) :: rest671)) => let val  result = MlyValue.Termand
 (fn _ => let val  (Factorbool as Factorbool1) = Factorbool1 ()
 in (Factorbool)
end)
 in ( LrTable.NT 2, ( result, Factorbool1left, Factorbool1right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (less((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 40, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (lessequal((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 41, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (less((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 42, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (greater((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 43, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (greaterequal((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 44, ( ( _, ( MlyValue.Expressionar Expressionar2, _, 
Expressionar2right)) :: _ :: ( _, ( MlyValue.Expressionar 
Expressionar1, Expressionar1left, _)) :: rest671)) => let val  result
 = MlyValue.Comparison (fn _ => let val  Expressionar1 = Expressionar1
 ()
 val  Expressionar2 = Expressionar2 ()
 in (
if((#2 Expressionar1) = (#2 Expressionar2)) then (lessgreat((#1 Expressionar1), (#1 Expressionar2)), "bool") else raise Error
)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar2right),
 rest671)
end
|  ( 45, ( ( _, ( MlyValue.Expressionar Expressionar1, 
Expressionar1left, Expressionar1right)) :: rest671)) => let val  
result = MlyValue.Comparison (fn _ => let val  (Expressionar as 
Expressionar1) = Expressionar1 ()
 in (Expressionar)
end)
 in ( LrTable.NT 18, ( result, Expressionar1left, Expressionar1right),
 rest671)
end
|  ( 46, ( ( _, ( MlyValue.Identifier Identifier1, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (Identifier as Identifier1) = Identifier1 ()
 in (Identifier)
end)
 in ( LrTable.NT 19, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 47, ( ( _, ( MlyValue.var var1, var1left, var1right)) :: rest671)
) => let val  result = MlyValue.Identifier (fn _ => let val  (var as 
var1) = var1 ()
 in (var)
end)
 in ( LrTable.NT 7, ( result, var1left, var1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  Variable1 = Variable1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, Variable1left, Variable1right), rest671
)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : AST_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun program (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun cons (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun var (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.var (fn () => i),p1,p2))
fun variab (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun int (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun bool (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun colon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun semicolon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun comma (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun lbrace (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun rbrace (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun colonequal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun read (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun write (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun if_term (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun then_term (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun else_term (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun endif (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun while_term (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun do_term (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun endwh (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun lparen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun rparen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun tilde (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun parallel (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun andand (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun tr (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun fal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun exclam (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun less (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun lessequal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun equal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun greater (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun greaterequal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun lessgreat (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun plus (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun minus (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun mult (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun divi (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun percent (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun Letter (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun Digit (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun number (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.number (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
