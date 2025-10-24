
CFLAGS := -ggdb -funroll-loops -ffast-math -Wall -fPIC -ggdb -march=opteron -m64 -DSIMD -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -I../aag

#LDLIBS := -L../aag -lgdsii -lm   -lreadline   -ltermcap 
LDLIBS := -L../aag -lgdsii -lm -lreadline 


aaggds : aaggds.o
	cd ../aag; make libgdsii.a
	$(CC) $< $(LDLIBS) -o $@

../aag/libgdsii.a :
	cd ../aag; make libgdsii.a
