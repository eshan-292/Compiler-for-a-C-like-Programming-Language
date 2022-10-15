all:
	ml-lex ast.lex
	ml-yacc ast.yacc
	rlwrap sml loader.sml
clean:
	rm ast.lex.sml ast.yacc.desc ast.yacc.sig ast.yacc.sml