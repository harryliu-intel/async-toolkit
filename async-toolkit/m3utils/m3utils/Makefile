#
# $Id$
#
COMMONDIRS= processor crt
SHIPS=cit_util magic bdd cptr bool
SUBDIRS= bas newsim psim pslnew m3hier Parse
M3SUBS= boxes magraster

all: .bindir commons ships subs m3subs
clean: cleanships cleanm3 cleansubs cleancommons cleanthis

.bindir:
	cd hack ; echo `./bindir.sh` > ../.bindir

cleanthis:
	rm -f .bindir

commons:
	@for s in $(COMMONDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done

subs:
	@for s in $(SUBDIRS); do\
		echo "===> $$s";\
		cd $$s; make; cd .. ;  \
	done


ships: 
	@for s in $(SHIPS); do\
		echo "===> $$s";\
		cd $$s; m3build; m3ship; cd .. ;  \
	done

m3subs:
	@for s in $(M3SUBS); do\
		echo "===> $$s";\
		cd $$s; m3build; cd .. ;  \
	done

cleanships:
	@for s in $(SHIPS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; m3build clean; cd .. ;  \
	done

cleanm3:
	@for s in $(M3SUBS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; m3build clean; cd .. ;  \
	done

cleancommons:
	@for s in $(COMMONDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make clean; cd .. ;  \
	done

cleansubs:
	@for s in $(SUBDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make clean; cd .. ;  \
	done

