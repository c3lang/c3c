c3yacc: grammar.y c3.l
	flex --header-file=lex.yy.h -8 c3.l
	bison -d grammar.y;
	cc -O2 -o c3yacc lex.yy.c grammar.tab.c

clean:
	rm -f c3yacc grammar.output *.o grammar.tab.* lex.yy.*