# $Id$
COMMONDIRS= processor crt
SUBDIRS= psl bas newsim psim

all:
	@for s in $(COMMONDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done
	@for s in $(SUBDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done

clean:
	@for s in $(COMMONDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make clean; cd .. ;  \
	done
	@for s in $(SUBDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make clean; cd .. ;  \
	done

