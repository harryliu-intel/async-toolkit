#CFLAGS=-O6 -g -c -I.. -I../build_c_api/mby_c/src -I../../../../c -std=c99 -Wall  -pedantic 
CFLAGS= -g -c -I.. -I. -I../build_c_api/mby_c/src -I../../../../c -std=c99 -Wall  -pedantic 

AUTOGEN=addr2ragged.o  inorderid2ragged.o  ragged2addr.o  ragged2arcs.o  ragged2inorderid.o


test_api: $(AUTOGEN) mby_top_map.o test_api.o seqtype.o
	cc -o $@ $^

seqtype.o: ../seqtype.c ../seqtype.h
	cc $(CFLAGS) ../seqtype.c

mby_top_map.o: ../build_c_api/mby_c/src/mby_top_map.h ../build_c_api/mby_c/src/mby_top_map.c
	cc $(CFLAGS) ../build_c_api/mby_c/src/mby_top_map.c

clean:
	/bin/rm -f test_api *.o *~
