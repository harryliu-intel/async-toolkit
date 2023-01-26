TOP = ..
LIB_TARGETS = ztracelib.a
BIN_TARGETS = testmain

include $(TOP)/Makefile.rules
CFLAGS += -std=c99

# C library

ztracelib.a: ztrace.o rep16.o arithdecode.o ztrace_read.o
arithdecode.o: minmax.h arithdecode.h
rep16.o: rep16.h minmax.h
ztrace.o: ztrace_read.h arithdecode.h ztrace.h

# standalone programs

testmain.o: ztrace.h etaoin.h mac_freqs.h
testmain: testmain.o ztracelib.a
# DO NOT DELETE
