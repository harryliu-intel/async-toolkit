COMMONDIRS= processor crt
SUBDIRS= psl bas newsim

all:
	@for s in $(COMMONDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done
	@for s in $(SUBDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done
