
CINCLUDES=-I.. -I../build_c_api/mby_c/src -I../../../../c 

CFLAGS=

CFLAGS+= -O6
CFLAGS+= -g
CFLAGS+= $(INCLUDES)
CFLAGS+= -std=c99 -Wall -pedantic -Werror

# TODO : we have warnings from -Wextra to fix
CFLAGS+= -Wextra

CFLAGS+= -c $(CINCLUDES)

#
# for performance, these targets really should be built -O6
# performance gain is ~4X
#

AUTOGEN=    addr2ragged.o inorderid2ragged.o ragged2addr.o ragged2arcs.o\
	ragged2inorderid.o\
	hostptr_setup.o	ragged2ptr.o ptr2ragged.o

AUTOSRCS=$(AUTOGEN:.o=.c)

EXECS= test_api test_exceptions test_if
all: $(EXECS)

$(AUTOSRCS): ragged.made

ragged.made:
	cd .. ;  ./make_ragged.sh mby doit.scm exit
	touch ragged.made

test_api.o: $(AUTOGEN)

test_api: $(AUTOGEN) mby_top_map.o test_api.o raggedindex.o

raggedindex.o: ../raggedindex.c ../raggedindex.h
	cc $(CFLAGS) ../raggedindex.c

mby_top_map.o: ../build_c_api/mby_c/src/mby_top_map.h ../build_c_api/mby_c/src/mby_top_map.c
	cc $(CFLAGS) ../build_c_api/mby_c/src/mby_top_map.c

test_exceptions: ies_exception.o test_exceptions.o

clean:
	/bin/rm -f $(EXECS) *.o *~

distclean: clean
	/bin/rm -f `echo $(AUTOGEN) | sed 's/\.o/.[hc]/g'`
	/bin/rm -f ragged.made	

test_if: test_if.o

test_if.o: test_if.c
# test_if has some silly comma code in it just for testing
	cc $(CFLAGS) -Wno-unused $<
