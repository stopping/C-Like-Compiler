clike: tokenout.l clike.y functions.c functions.h
	bison -v -d clike.y
	flex tokenout.l
	gcc -o $@ clike.tab.c lex.yy.c functions.c -lfl 

.PHONY: clean
clean:
	rm parse
	rm -rf *.o
