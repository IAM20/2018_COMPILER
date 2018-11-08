#
# Makefile for TINY
# Gnu C Version
# K. Louden 2/3/98
#

CC = gcc

CFLAGS = 

OBJS = lex.yy.o main.o util.o cminus.tab.o

cminus_parser: cminus.tab.h $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o cminus_parser -lfl

lex.yy.o: cminus.l scan.h util.h globals.h
	flex cminus.l
	$(CC) $(CLFAGS) -c lex.yy.c -lfl

main.o: main.c globals.h util.h scan.h
	$(CC) $(CFLAGS) -c main.c

util.o: util.c util.h globals.h
	$(CC) $(CFLAGS) -c util.c

cminus.tab.h: cminus.y
	bison -d cminus.y

cminus.tab.o: cminus.y globals.h
	bison cminus.y
	$(CC) $(CFLAGS) -c cminus.tab.c

clean:
	-rm *.o
	-rm lex.yy.c
	-rm cminus.tab.c cminus.tab.h
	-rm cminus_parser
