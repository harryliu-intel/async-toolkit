# standard setup
TOP = ../..
LIB_TARGETS = bsim4.a
include $(TOP)/Makefile.rules

bsim4.a: bsim4.o b4.o b4mpar.o b4mask.o b4set.o b4temp.o b4geo.o b4check.o b4ld.o 

bsim4.o: bsim4.c ../aspice.h bsim4.h bsim4def.h

b4.o: b4.c ../aspice.h bsim4.h bsim4def.h

b4mpar.o: b4mpar.c ../aspice.h bsim4.h bsim4def.h

b4mask.o: b4mask.c ../aspice.h bsim4.h bsim4def.h

b4set.o: b4set.c ../aspice.h bsim4.h bsim4def.h

b4temp.o: b4temp.c ../aspice.h bsim4.h bsim4def.h

b4geo.o: b4geo.c ../aspice.h bsim4.h bsim4def.h

b4check.o: b4check.c ../aspice.h bsim4.h bsim4def.h

b4ld.o: b4ld.c ../aspice.h bsim4.h bsim4def.h
