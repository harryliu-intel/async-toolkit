TOP = ..
BIN_TARGETS = auto
INSTALL_TARGETS = auto $(wildcard auto_*)
MAN_TARGETS = auto.1
include $(TOP)/Makefile.rules

ALL_CFILES := $(wildcard *.c)

#all the programs that this makefile can make.
PROGRAMS := auto make_auto cast_which

#All the cfiles that contain main for all the above programs
PROG_CFILES := $(patsubst %, %.c, $(PROGRAMS))

#All the cfiles needed to complile auto
AUTO_CFILES := auto.c $(filter-out $(PROG_CFILES), $(ALL_CFILES))

#All the object files that we need to link together to make auto
AUTO_OBJ := $(patsubst %.c,%.o, $(AUTO_CFILES))

make_auto: make_auto.o 

cast_which: cast_which.o 

auto: $(AUTO_OBJ) 

auto.o: auto.c auto.h

fullauto.o: fullauto.c auto.h

capacitances.o: capacitances.c

estimate_wirecap.o: estimate_wirecap.c auto.h

size.o: size.c auto.h

arg.o: arg.c auto.h

compare.o: compare.c auto.h

energy.o: energy.c auto.h

overlap.o: overlap.c auto.h

enumlayout.o: enumlayout.c enumlayout.h auto.h

exportskill.o: exportskill.c exportskill.h auto.h

draw.o: draw.c enumlayout.h auto.h

route.o: route.c auto.h

block.o: block.c auto.h

rectangle.o: rectangle.c auto.h

circuit.o: circuit.c auto.h

match.o: match.c auto.h

name.o: name.c auto.h

stack.o: stack.c auto.h

network.o: network.c auto.h

stretch.o: stretch.c auto.h

espresso.o: espresso.c auto.h

extract.o: extract.c auto.h
