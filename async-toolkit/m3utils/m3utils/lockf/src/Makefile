# $Id$

all: lockf lockf.1.gz

lockf: lockf.c
	cc -O -o lockf lockf.c

lockf.1.gz: lockf.1
	gzip -cn lockf.1 > lockf.1.gz

clean:
	/bin/rm -f a.out lockf lockf.1.gz *.core core *~ *.o *.exe *.done
