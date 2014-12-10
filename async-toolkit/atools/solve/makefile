TOP = ..
BIN_TARGETS = solve
INSTALL_TARGETS = solve solve_parallel
include $(TOP)/Makefile.rules

solve: solve.o solvelib.o

solvelib.o: solvelib.c $(HEADERS)
