signature AST_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val number: (int) *  'a * 'a -> (svalue,'a) token
val Digit:  'a * 'a -> (svalue,'a) token
val Letter:  'a * 'a -> (svalue,'a) token
val percent:  'a * 'a -> (svalue,'a) token
val divi:  'a * 'a -> (svalue,'a) token
val mult:  'a * 'a -> (svalue,'a) token
val minus:  'a * 'a -> (svalue,'a) token
val plus:  'a * 'a -> (svalue,'a) token
val lessgreat:  'a * 'a -> (svalue,'a) token
val greaterequal:  'a * 'a -> (svalue,'a) token
val greater:  'a * 'a -> (svalue,'a) token
val equal:  'a * 'a -> (svalue,'a) token
val lessequal:  'a * 'a -> (svalue,'a) token
val less:  'a * 'a -> (svalue,'a) token
val exclam:  'a * 'a -> (svalue,'a) token
val fal:  'a * 'a -> (svalue,'a) token
val tr:  'a * 'a -> (svalue,'a) token
val andand:  'a * 'a -> (svalue,'a) token
val parallel:  'a * 'a -> (svalue,'a) token
val tilde:  'a * 'a -> (svalue,'a) token
val rparen:  'a * 'a -> (svalue,'a) token
val lparen:  'a * 'a -> (svalue,'a) token
val endwh:  'a * 'a -> (svalue,'a) token
val do_term:  'a * 'a -> (svalue,'a) token
val while_term:  'a * 'a -> (svalue,'a) token
val endif:  'a * 'a -> (svalue,'a) token
val else_term:  'a * 'a -> (svalue,'a) token
val then_term:  'a * 'a -> (svalue,'a) token
val if_term:  'a * 'a -> (svalue,'a) token
val write:  'a * 'a -> (svalue,'a) token
val read:  'a * 'a -> (svalue,'a) token
val colonequal:  'a * 'a -> (svalue,'a) token
val rbrace:  'a * 'a -> (svalue,'a) token
val lbrace:  'a * 'a -> (svalue,'a) token
val comma:  'a * 'a -> (svalue,'a) token
val semicolon:  'a * 'a -> (svalue,'a) token
val colon:  'a * 'a -> (svalue,'a) token
val bool:  'a * 'a -> (svalue,'a) token
val int:  'a * 'a -> (svalue,'a) token
val variab:  'a * 'a -> (svalue,'a) token
val var: (string) *  'a * 'a -> (svalue,'a) token
val cons:  'a * 'a -> (svalue,'a) token
val program:  'a * 'a -> (svalue,'a) token
end
signature AST_LRVALS=
sig
structure Tokens : AST_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
