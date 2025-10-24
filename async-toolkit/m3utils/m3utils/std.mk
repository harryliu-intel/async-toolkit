# $Id: std.mk,v 1.3 2004/07/14 02:10:05 mika Exp $

# define this one if you want to catch functions missing from headers
#ANNOYINGFLAGS= -Wmissing-declarations -pedantic -ansi -Wredundant-decls 

DEBUGFLAGS= -g 
#DEBUGFLAGS= -O6
CFLAGS= $(DEBUGFLAGS) -Wall -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return -Wstrict-prototypes -Wnested-externs $(ANNOYINGFLAGS) -pipe -DMYMALLOCDEBUG 

#CFLAGS+= -DQUIET
#MCCFLAGS+= -DQUIET

