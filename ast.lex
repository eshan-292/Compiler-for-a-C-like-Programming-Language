structure Tokens= Tokens

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  type lexarg = string
    type arg = lexarg
  val pos = ref 0
  val linenum = ref 1
val eof = fn fileName => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


%%
%full
%header (functor WhileLexFun(structure Tokens:AST_TOKENS));
%arg (fileName:string);
alpha=[A-Za-z];
alphadig=[A-Za-z0-9];
digit=[0-9];
ws = [\ \t\r];
%%
\n       => (pos := (!pos) + 1; continue());
{ws}+    => (continue());
"/"      => (Tokens.divi(!pos,!pos));
"*"      => (Tokens.mult(!pos,!pos));
"+"      => (Tokens.plus(!pos,!pos));
"-"      => (Tokens.minus(!pos,!pos));
":"      => (Tokens.colon(!pos,!pos));
";"      => (Tokens.semicolon(!pos,!pos));
"("      => (Tokens.lparen(!pos,!pos));
")"      => (Tokens.rparen(!pos,!pos));
"%"      => (Tokens.percent(!pos,!pos));
"<"      => (Tokens.less(!pos,!pos));
">"      => (Tokens.greater(!pos,!pos));
">="      => (Tokens.greaterequal(!pos,!pos));
"<="      => (Tokens.lessequal(!pos,!pos));
"="      => (Tokens.equal(!pos,!pos));
"<>"      => (Tokens.lessgreat(!pos,!pos));
"tt"      => (Tokens.tr(!pos,!pos));
"ff"      => (Tokens.fal(!pos,!pos));
"&&"      => (Tokens.andand(!pos,!pos));
"||"      => (Tokens.parallel(!pos,!pos));
"~"      => (Tokens.tilde(!pos,!pos));
"!"      => (Tokens.exclam(!pos,!pos));
"endwh"      => (Tokens.endwh(!pos,!pos));
"while"      => (Tokens.while_term(!pos,!pos));
"do"      => (Tokens.do_term(!pos,!pos));
"endif"      => (Tokens.endif(!pos,!pos));
"if"      => (Tokens.if_term(!pos,!pos));
"then"      => (Tokens.then_term(!pos,!pos));
"else"      => (Tokens.else_term(!pos,!pos));
"read"      => (Tokens.read(!pos,!pos));
"write"      => (Tokens.write(!pos,!pos));
":="      => (Tokens.colonequal(!pos,!pos));
"{"      => (Tokens.lbrace(!pos,!pos));
"}"      => (Tokens.rbrace(!pos,!pos));
","      => (Tokens.comma(!pos,!pos));
"int"      => (Tokens.int(!pos,!pos));
"bool"      => (Tokens.bool(!pos,!pos));
"var"      => (Tokens.variab(!pos,!pos));
"program"      => (Tokens.program(!pos,!pos));
"::"      => (Tokens.cons(!pos,!pos));
{alpha}{alphadig}* => (Tokens.var(yytext, !pos,!pos));
{digit}+ => (Tokens.number(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             continue());