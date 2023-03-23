c3yacc: grammar.y c3.l
	bison -d grammar.y; cc -c grammar.tab.c
	flex c3.l; gcc -c lex.yy.c
	cc -o c3yacc lex.yy.o grammar.tab.o

clean:
	rm -f c3yacc grammar.output *.o grammar.tab.* lex.yy.c