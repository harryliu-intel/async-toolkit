
.SUFFIXES: .afb

#
FOBJ=courB08.o courB10.o courB12.o courB14.o courB18.o courB24.o \
courBO08.o courBO10.o courBO12.o courBO14.o courBO18.o courBO24.o \
courO08.o courO10.o courO12.o courO14.o courO18.o courO24.o \
courR08.o courR10.o courR12.o courR14.o courR18.o courR24.o \
timB08.o timB10.o timB12.o timB14.o timB18.o timB24.o \
timBI08.o timBI10.o timBI12.o timBI14.o timBI18.o timBI24.o \
timI08.o timI10.o timI12.o timI14.o timI18.o timI24.o \
timR08.o timR10.o timR12.o timR14.o timR18.o timR24.o
FC=${FOBJ:.o=.c}
FAFB=${FOBJ:.o=.afb}
LDFLAGS = 
LDLIBS = -lm

.afb.c :
	tdraw $*.afb > $@

libfont.a : tdraw ${FOBJ}
	rm -f libfont.a
	ar rc libfont.a ${FOBJ}
	-ranlib libfont.a
clean:
	rm -f tdraw *.o *.a ${FC}
