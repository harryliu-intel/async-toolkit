TOP = ..
LIB_TARGETS = castlib.a
BIN_TARGETS = bigint_calculator calculator
INSTALL_TARGETS = bigint_calculator calculator

include $(TOP)/Makefile.rules

# C library

castlib.a: leak.o list.o minimize.o misc.o newlex.o \
	recalc.o vector.o expression.o path.o bigint.o glob.o \
	archive.o

leak.o: leak.h misc.h
list.o: list.h leak.h
minimize.o: minimize.h vector.h
misc.o: misc.h
newlex.o: newlex.h misc.h list.h leak.h
recalc.o: recalc.h misc.h list.h leak.h
vector.o: vector.h
expression.o: expression.h newlex.h list.h
path.o: newlex.h leak.h
bigint.o: bigint.h newlex.h misc.h leak.h
glob.o: glob.h misc.h
archive.o: leak.h misc.h

# standalone programs

bigint_calculator.o: bigint.h misc.h leak.h newlex.h
bigint_calculator: bigint_calculator.o

calculator.o: calculator.c newlex.h misc.h leak.h expression.h
calculator: calculator.o
