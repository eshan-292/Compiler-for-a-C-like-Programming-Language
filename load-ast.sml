
structure AST_LrVals = ASTLrValsFun(structure Token = LrParser.Token);
structure AST_Lex = WhileLexFun(structure Tokens = AST_LrVals.Tokens);
structure AST_Parser = JoinWithArg(
    structure ParserData = AST_LrVals.ParserData
    structure Lex=AST_Lex
    structure LrParser=LrParser
);