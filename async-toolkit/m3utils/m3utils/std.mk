# $Id$

# define this one if you want to catch functions missing from headers
#ANNOYINGFLAGS= -Wmissing-declarations

DEBUGFLAGS= -g 
#DEBUGFLAGS= -O6
CFLAGS= $(DEBUGFLAGS) -Wall -pedantic -ansi -Wtraditional -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return -Wstrict-prototypes -Wredundant-decls -Wnested-externs $(ANNOYINGFLAGS) -pipe -DMYMALLOCDEBUG 

CFLAGS+= -DQUIET
MCCFLAGS+= -DQUIET

