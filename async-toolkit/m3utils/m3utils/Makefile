#
# Copyright (c) 2000 California Institute of Technology
# All rights reserved.
# Department of Computer Science
# Pasadena, CA 91125.
#
# Permission to use, copy, modify, and distribute this software
# and its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies. The California Institute of Technology makes no representations
# about the suitability of this software for any purpose. It is
# provided "as is" without express or implied warranty. Export of this
# software outside of the United States of America may require an
# export license
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
# 5. BROWSEREXPS  --   Export misc programs in subdirectories for m3browser

COMMONDIRS=processor crt m3yacc cit_common m3tmplhack cit_util m3browserhack prsparse term parserlib 
COMMONS=`./dirsexist.sh -f $(COMMONDIRS)`
REVCOMMONS=`./dirsexist.sh -r $(COMMONDIRS)`

# "SHIPS" are subdirectories containing Modula-3 libraries that
# would normally be shipped to /usr/local/lib/m3.
# We need to make sure that we clean them in reverse order of 
# building since they may depend on other libraries (1 way only)
#
# m3texthack is a program, but it depends only on cit_util.
#
SHIPDIRS=tasks angbrmacro matrix m3texthack magic magicextras bdd cptr bool sop boxes simplegrid rpc router layout cit_parse paneman drawcontext drawcontext/dcpane m3-3 minimize convex integrate splines softmax pattern gatematrix colc tree
SHIPS=`./dirsexist.sh -f $(SHIPDIRS)`
REVSHIPS=`./dirsexist.sh -r $(SHIPDIRS)`

SUBDIRS= `./dirsexist.sh -f bas newsim psim pslnew m3hier Parse`

# These are built last
M3SUBS= `./dirsexist.sh -f boxesdriver magraster routerdriver finite_diff deriv layoutd layoutdemo tasktest cit_parse/testprs cit_parse/testchp paneman/kemacs drawcontext/test drawcontext/kgv m3-3/display m3-3/bufchain m3-3/icache et2 testsplines gatematrix/gm angbrmacro/ab cit_util/test_rats cit_util/test_bigs tree/test`

M3BUILDOPTS= -O
M3BUILD= m3build $(M3BUILDOPTS)
M3SHIP= echo

BROWSEREXPS = `./dirsexist.sh -f cit_parse/testprs cit_parse/testchp paneman/kemacs drawcontext/dcpane drawcontext/test drawcontext/kgv m3-3/display m3-3/bufchain m3-3/icache`

all: .bindir .top commons ships subs m3subs browserexps
clean: cleanm3 cleansubs cleanships cleancommons cleanbuilddirs cleanthis
fastprep: .bindir .top .m3deps
fast: fastprep .todo .fastmake
faster: fastprep .lesstodo .fastmake

.bindir:
	cd hack ; echo `./bindir.sh` > ../.bindir

.top:
	echo TOP=\"`pwd`\" > .top
	echo BINDIR=\"`cat .bindir`\" >> .top

.m3deps:
	grep ^import\ \*\( */src/m3makefile */*/src/m3makefile | \
	sed s/\ //g | \
	sed -e s/:import\(\"/:/g -e s/\"\)\.\*//g -e s./src/m3makefile..g | \
	sed s/:ktoklib/:parserlib/g > .m3deps
	grep -v -e ^# otherdeps | grep : >> .m3deps

.todo:
	@echo GROUP COMMONS $(COMMONS) > .todo
	@echo GROUP SHIPS $(SHIPS) >> .todo
	@echo GROUP SUBS $(SUBS) >> .todo
	@echo GROUP M3SUBS $(M3SUBS) >> .todo
	@echo M3DIRS $(SHIPS) $(M3SUBS) >> .todo
	@echo M3OPTS $(M3BUILDOPTS) >> .todo

.lesstodo:
	@echo GROUP ALL $(COMMONS) $(SHIPS) $(SUBS) $(M3SUBS) > .todo
	@echo M3DIRS $(SHIPS) $(M3SUBS) >> .todo
	@echo M3OPTS $(M3BUILDOPTS) >> .todo

.fastmake:
	(cd makehack; m3build)
	cat .todo .m3deps | makehack/`cat .bindir`/makehack > .fast
	sh .fast

cleanthis:
	find . -name `cat .bindir` -print | xargs rm -rf 
	rm -f .bindir .top
	rm -rf hack/.hidden
	rm -rf .browserhack*

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
		(cd $$s; $(M3BUILD); $(M3SHIP));  \
	done

m3subs:
	@for s in $(M3SUBS); do\
		echo "===> $$s";\
		(cd $$s; $(M3BUILD));  \
	done

browserexps:
	if [ -x m3browserhack ]; then \
		m3browserhack/export.sh $(BROWSEREXPS); \
	fi

cleanships:
	@for s in $(REVSHIPS); do\
		echo "(cleaning) ===> $$s";\
		(cd $$s; $(M3BUILD) clean || true);  \
	done

cleanm3:
	@for s in $(M3SUBS); do\
		echo "(cleaning) ===> $$s";\
		(cd $$s; $(M3BUILD) clean || true);  \
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
