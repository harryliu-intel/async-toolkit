TOP = ../..
LIB_TARGETS = gdsiilib.a
include $(TOP)/Makefile.rules

btutil.o: btutil.c btutil.h c_lib.h c_std.h

c_lib.o: c_lib.c c_lib.h c_std.h

gdsutil.o: gdsutil.c gdsutil.h c_lib.h btutil.h

libaag.o: btutil.o c_lib.o gdsutil.o
	/usr/bin/ld -Ur -o libaag.o btutil.o c_lib.o gdsutil.o

gdsiilib.a: btutil.o c_lib.o gdsutil.o
