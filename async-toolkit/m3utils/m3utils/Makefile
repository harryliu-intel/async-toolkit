#
# $Id$
#

# there's a nasty hack here:
# cit_util is entirely in Modula-3, but since it is req'd by prsparse,
# we need to build it first.
#
# The solution would be either:
# 1. to use Make all-around (for the top-level at least)
# 2. to use m3build all-around (ditto)
# 3. to allow a list of m3build and Make target dirs to be mixed in any order
#

#
# currently, this is the build order:
# 0. <some hacks: .bindir, .top ... >
# 1. COMMONDIRS   --   C libraries
# 2. SHIPDIRS     --   Modula-3 libraries
# 3. SUBDIRS      --   C programs
# 4. M3SUBS       --   Modula-3 programs

COMMONDIRS=processor crt m3yacc cit_util prsparse 
COMMONS=`./dirsexist.sh -f $(COMMONDIRS)`
REVCOMMONS=`./dirsexist.sh -r $(COMMONDIRS)`

# "SHIPS" are subdirectories containing Modula-3 libraries that
# would normally be shipped to /usr/local/lib/m3.
# We need to make sure that we clean them in reverse order of 
# building since they may depend on other libraries (1 way only)
#
# m3texthack is a program, but it depends only on cit_util.
#
SHIPDIRS=tasks matrix m3texthack term magic magicextras bdd cptr bool boxes simplegrid rpc router layout ktoklib
SHIPS=`./dirsexist.sh -f $(SHIPDIRS)`
REVSHIPS=`./dirsexist.sh -r $(SHIPDIRS)`

SUBDIRS= `./dirsexist.sh -f bas newsim psim pslnew m3hier Parse ktok klex kyacc`

# These are built last
M3SUBS= `./dirsexist.sh -f boxesdriver magraster routerdriver finite_diff deriv layoutd layoutdemo`

M3BUILDOPTS= -O
M3BUILD= m3build $(M3BUILDOPTS)
M3SHIP= echo

all: .bindir .top commons ships subs m3subs
clean: cleanm3 cleansubs cleanships cleancommons cleanbuilddirs cleanthis 

.bindir:
	cd hack ; echo `./bindir.sh` > ../.bindir

.top:
	echo TOP=\"`pwd`\" > .top
	echo BINDIR=\"`cat .bindir`\" >> .top


cleanthis:
	find . -name `cat .bindir` -print | xargs rm -rf 
	rm -f .bindir .top
	rm -rf hack/.hidden

cleanbuilddirs:

commons:
	@for s in $(COMMONS); do\
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
	@for s in $(REVCOMMONS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\" clean; cd .. ;  \
	done

cleansubs:
	@for s in $(SUBDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; make M3BUILDOPTS=\"$(M3BUILDOPTS)\" clean; cd .. ;  \
	done

# this is the end of the Makefile
