OBJS=processor_helper.o processor.o
# define this one if you want to catch functions missing from headers
#ANNOYINGFLAGS= -Wmissing-declarations

DEBUGFLAGS= -g 
#DEBUGFLAGS= -O6
CFLAGS= $(DEBUGFLAGS) -Wall -pedantic -ansi -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return -Wstrict-prototypes -Wredundant-decls -Wnested-externs $(ANNOYINGFLAGS) -pipe -DMYMALLOCDEBUG 

CFLAGS+= -DQUIET
MCCFLAGS+= -DQUIET

all: processor bas psl

BASOBJS= bas.o y.tab.o lex.yy.o
PSLOBJS= psl.o psl.tab.o psllex.o hash.o bool.o mymalloc.o misc.o expr.o \
	sop.o gen_prs.o gen_c.o

psl.c: psl.tab.h

psllex.c: psl.tab.h pslang.l
	lex -opsllex.c pslang.l
psl.tab.h: pslang.y
	yacc -v -d -b psl pslang.y
psl.tab.c: pslang.y
	yacc -v -d -b psl pslang.y

psllex.o: psllex.c
	cc -g -c psllex.c
psl.tab.o: psl.tab.c
	cc -g -c psl.tab.c

psl: $(PSLOBJS) 
	cc -g -o psl $(PSLOBJS) -ly -lfl

bas: $(BASOBJS) 
	cc -g -o bas $(BASOBJS) -ly -lfl

bas.c: y.tab.h 

processor: $(OBJS)
	mcc -g -o processor $(OBJS)

processor.o: processor.m processor.h processor_helper.h
	mcc $(MCCFLAGS) -g -c processor.m

clean:
	/bin/rm -f *~ a.out *.o *.core core lex.yy.c y.tab.c y.tab.h a.image
	/bin/rm -f psl.tab.c psl.tab.h psllex.c

y.tab.h: asm.y
	yacc -d $>
y.tab.c: asm.y
	yacc -d $>
lex.yy.o: lex.yy.c
	cc -g -c lex.yy.c
y.tab.o: y.tab.c
	cc -g -c y.tab.c
lex.yy.c: y.tab.h asm.l
	lex asm.l

