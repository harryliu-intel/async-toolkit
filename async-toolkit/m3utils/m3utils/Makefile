#
# $Id$
#

COMMONDIRS= `./dirsexist.sh -f processor crt m3yacc`

# "SHIPS" are subdirectories containing Modula-3 libraries that
# would normally be shipped to /usr/local/lib/m3.
# We need to make sure that we clean them in reverse order of 
# building since they may depend on other libraries (1 way only)
SHIPDIRS=cit_util magic bdd cptr bool boxes
SHIPS=`./dirsexist.sh -f $(SHIPDIRS)`
REVSHIPS=`./dirsexist.sh -r $(SHIPDIRS)`

SUBDIRS= `./dirsexist.sh -f prsparse bas newsim psim pslnew m3hier Parse`

# These depend on nothing else
M3SUBS= `./dirsexist.sh -f boxesdriver magraster router`

M3BUILDOPTS= -O
M3BUILD= m3build $(M3BUILDOPTS)
M3SHIP= echo

all: .bindir .top commons ships subs m3subs
clean: cleanm3 cleansubs cleancommons cleanships cleanbuilddirs cleanthis 

.bindir:
	cd hack ; echo `./bindir.sh` > ../.bindir

.top:
	echo TOP=\"`pwd`\" > .top


cleanthis:
	find . -name `cat .bindir` -print | xargs rm -rf 
	rm -f .bindir .top
	rm -rf hack/.hidden

cleanbuilddirs:

commons:
	@for s in $(COMMONDIRS); do\
		echo "===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\"; cd .. ;  \
	done

subs:
	@for s in $(SUBDIRS); do\
		echo "===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\"; cd .. ;  \
	done


ships: 
	@for s in $(SHIPS); do\
		echo "===> $$s";\
		cd $$s; $(M3BUILD); $(M3SHIP); cd .. ;  \
	done

m3subs:
	@for s in $(M3SUBS); do\
		echo "===> $$s";\
		cd $$s; $(M3BUILD); cd .. ;  \
	done

cleanships:
	@for s in $(REVSHIPS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; $(M3BUILD) clean || true; cd .. ;  \
	done

cleanm3:
	@for s in $(M3SUBS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; $(M3BUILD) clean || true ; cd .. ;  \
	done

cleancommons:
	@for s in $(COMMONDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\" clean; cd .. ;  \
	done

cleansubs:
	@for s in $(SUBDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\" clean; cd .. ;  \
	done

# this is the end of the Makefile
