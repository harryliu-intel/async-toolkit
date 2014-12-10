TOP = ../..
BIN_TARGETS = gds2plot
INSTALL_TARGETS = $(BIN_TARGETS)
include $(TOP)/Makefile.rules

gds2plot: gds2plot.o ../aag/gdsiilib.a

gds2plot.o: gds2plot.c ../aag/gdsutil.h
