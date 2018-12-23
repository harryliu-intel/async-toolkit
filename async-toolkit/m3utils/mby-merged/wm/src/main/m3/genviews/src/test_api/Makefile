
CINCLUDES=-I.. -I../build_c_api/mby_c/src -I../../../../c 

CFLAGS=

#CFLAGS+= -O6
CFLAGS+= -g
CFLAGS+= $(INCLUDES)
CFLAGS+= -std=c99 -Wall -pedantic -Werror
CFLAGS+= -c $(CINCLUDES)

#
# for performance, these targets really should be built -O6
# performance gain is ~4X
#

AUTOGEN=addr2ragged.o inorderid2ragged.o ragged2addr.o ragged2arcs.o\
	ragged2inorderid.o hostptr.o


test_api: $(AUTOGEN) mby_top_map.o test_api.o raggedindex.o
	cc -o $@ $^

raggedindex.o: ../raggedindex.c ../raggedindex.h
	cc $(CFLAGS) ../raggedindex.c

mby_top_map.o: ../build_c_api/mby_c/src/mby_top_map.h ../build_c_api/mby_c/src/mby_top_map.c
	cc $(CFLAGS) ../build_c_api/mby_c/src/mby_top_map.c

clean:
	/bin/rm -f test_api *.o *~
