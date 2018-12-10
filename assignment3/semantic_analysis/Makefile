#
# Makefile for TINY
# Gnu C Version
# K. Louden 2/3/98
#

CC = gcc

CFLAGS = 

OBJS = lex.yy.o main.o util.o cminus.tab.o analyze.o symtab.o

cminus_analyzer: cminus.tab.h $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o cminus_analyzer -lfl

lex.yy.o: cminus.l scan.h util.h globals.h
	flex cminus.l
	$(CC) $(CLFAGS) -c lex.yy.c -lfl

main.o: main.c globals.h util.h scan.h analyze.h
	$(CC) $(CFLAGS) -c main.c

util.o: util.c util.h globals.h
	$(CC) $(CFLAGS) -c util.c

cminus.tab.h: cminus.y
	bison -d cminus.y

cminus.tab.o: cminus.y globals.h
	bison cminus.y
	$(CC) $(CFLAGS) -c cminus.tab.c

symtab.o: symtab.c symtab.h
	$(CC) $(CFLAGS) -c symtab.c

analyze.o: analyze.c globals.h symtab.h analyze.h util.h
	$(CC) $(CFLAGS) -c analyze.c

clean:
	-rm *.o
	-rm lex.yy.c
	-rm cminus.tab.c cminus.tab.h
	-rm cminus_analyzer
