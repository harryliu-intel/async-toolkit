#
# Copyright (c) 1999-2006 California Institute of Technology
# All rights reserved.
# Department of Computer Science
# Pasadena, CA 91125, U.S.A.
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

# THIS Makefile IS FOR GNU MAKE ("gmake")

#
# Build order has been revised as of 1/13/2005
#
# New build order reduces dependencies of Modula-3 on C, builds
# Modula-3 first, then C.  Default behavior is to build ONLY
# Modula-3 code in BIN_DIR subdirectories and not to touch
# shared directories.
#
# currently, this is the build order:
#
# Phase I.    Target "std"
# 0. <a hack: .top>
# 1. SHIPDIRS     --   Modula-3 libraries
# 2. M3SUBS       --   Modula-3 programs
#
# Phase II.   Target "withlinks"
# 00. <another hack: .bindir>
# 3. BROWSEREXPS  --   Export misc programs in subdirectories for m3browser
# 4. binlinks     --   make symlinks to programs that were built
# 
# Phase IIb.  Target "regress"
# 3. REGSUBS      --   Modula-3 programs
#
# Phase III.  Target "all"
# 5. COMMONDIRS   --   C libraries
# 6. SUBDIRS      --   C programs
#
# The default target is "std"
#
# default is to make with m3build (PM3)
# if you prefer CM3, run "make -DCM3"
# if you want make to continue past errors, run "make -DPROCEED"
#
#
#
# In the long run, we'd like to get rid of COMMONDIRS and SUBDIRS and
# put everything under the control of m3build.
#

# global defs, esp. M3BUILDPROGRAM
include Make.defs

ifdef PROCEED
PROCEEDCMD="true"
else
PROCEEDCMD="false"
endif

COMMONDIRS=processor crt m3yacc prsparse
COMMONS=`./dirsexist.sh -f $(COMMONDIRS)`
REVCOMMONS=`./dirsexist.sh -r $(COMMONDIRS)`

# "SHIPS" are subdirectories containing Modula-3 libraries that
# would normally be shipped to /usr/local/lib/m3.
# We need to make sure that we clean them in reverse order of 
# building since they may depend on other libraries (1 way only)

PARSERLIBS=parserlib/ktoklib parserlib/kyacclib parserlib/klexlib parserlib/ktok parserlib/klex parserlib/kyacc parserlib/kext parserlib/parserlib
SHIPDIRS=commandrw cit_common m3tmplhack m3texthack cit_util m3browserhack term $(PARSERLIBS) tasks angbrmacro defmacro matrix magic magicextras bdd cptr bool sop mst boxes simplegrid rpc router cit_parse paneman drawcontext drawcontext/dcpane m3-3 minimize convex integrate splines softmax pattern gatematrix colc tree optree operators celldb matcher sysiphus/sysiphus_parse sysiphus/sysiphus_lib verif/tuple calarm/finlib calarm/keylib calarm/alarm calarm/anova calarm/postgresql calarm/htmltable voronoi ld/ld_parse ld cs2/cs2 rdwr labeltest m3curses calarm/wavelets
SHIPS=`./dirsexist.sh -f $(SHIPDIRS)`
REVSHIPS=`./dirsexist.sh -r $(SHIPDIRS)`
M33STUFF=m3-3

SUBDIRS= `./dirsexist.sh -f bas newsim psim pslnew m3hier Parse`

CS2TARGETS=cs2/othello/digest cs2/othello/play cs2/othello/random cs2/othello/recursive2 cs2/othello/recursive4 cs2/othello/recursive6 cs2/othello/recursive7 cs2/othello/referee cs2/othello/setup

M3SUBS= `./dirsexist.sh -f magraster finite_diff deriv tasktest cit_parse/testprs paneman/kemacs drawcontext/test drawcontext/kgv m3-3/display m3-3/bufchain m3-3/icache et2 testsplines gatematrix/gm angbrmacro/ab cit_util/test_rats cit_util/test_bigs tree/test prlink matcher/xprs2stack stackgen sysiphus sysiphus/sysiphlat sysiphus/sysiphix sysiphus/sudafed acat router/pauls router/pauls2 router/pauls3 router/pauls4 router/gentest router/conflictdensity verif dagflat allias myrope magic/magsize playfile calarm calarm/getquotes calarm/transpose_rates decimate cpdag magicextras/testslasher ld/fixdag ld/ld2layoutd $(CS2TARGETS) calarm/keypaste m3curses/test calarm/wavelets/test`


REGSUBS= `./dirsexist.sh -f color calarm/twslib calarm/regresslib calarm/traderulelib calarm/fasthf calarm/smarttraderule calarm/anova/gnuplotter calarm/anova/gnuplotter/test calarm/gridctl calarm/regress calarm/anova/program calarm/anova/tabulate  calarm/picktickers  calarm/twslib/test calarm/twslib/testpm calarm/trade calarm/anova/testtex calarm/anova/curves calarm/findate calarm/regress/ximport calarm/regress/tsximport calarm/regress/adjust calarm/regress/qmctl calarm/regress/hcdriver calarm/twslib/test calarm/twslib/switchtickers calarm/regress/bloombimport calarm/regress/calcmetric calarm/htmltable/program calarm/htmltable/cgidb`


BROWSEREXPS = `./dirsexist.sh -f cit_parse/testprs paneman/kemacs drawcontext/dcpane drawcontext/test drawcontext/kgv m3-3/display m3-3/bufchain m3-3/icache`


#
# RULES BELOW HERE
#

std: .top ships m3subs
regress: std regsubs
withlinks: std .bindir browserexps binlinks
all: withlinks commons subs

clean: cleanm3 cleansubs cleanships cleancommons cleanbuilddirs cleanthis
fastprep: .top .m3deps
fast: fastprep .todo .fastmake
faster: fastprep .lesstodo .fastmake

.bindir:
	mkdir hack || true ; cd hack ; echo `./bindir.sh` > ../.bindir 
.top:
	echo TOP=\"$(PWD)\" > .top

.m3deps:
	grep ^import\ \*\( */src/m3makefile */*/src/m3makefile | \
	sed s/\ //g | \
	sed -e s/:import\(\"/:/g -e s/\"\)\.\*//g -e s./src/m3makefile..g | \
	sed s/:ktoklib/:parserlib/g > .m3deps
	grep -v -e ^# otherdeps >> .m3deps

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
	(cd makehack; $(M3BUILDPROGRAM))
	cat .todo .m3deps | makehack/`cat .bindir`/makehack > .fast
	sh .fast

cleanthis:
	rm -f .bindir
	$(MAKE) .bindir
	find . -name `cat .bindir` -print | xargs rm -rf 
	rm -f .bindir .top
	rm -rf hack/.hidden
	rm -rf .browserhack*

cleanbuilddirs:

commons:
	@for s in $(COMMONS); do\
		echo "===> $$s";\
		cd $$s; $(MAKE) ; cd .. ;  \
	done

subs:
	@for s in $(SUBDIRS); do\
		echo "===> $$s";\
		cd $$s; $(MAKE) ; cd .. ;  \
	done

binlinks:
	(cd bin; $(MAKE))


ships: 
	@for s in $(SHIPS); do\
		echo "===> $$s";\
		(cd $$s; $(M3BUILD) || $(PROCEEDCMD); $(M3SHIP));  \
	done

m3subs:
	@for s in $(M3SUBS); do\
		echo "===> $$s";\
		(cd $$s; $(M3BUILD) || $(PROCEEDCMD));  \
	done

regsubs: 
	@for s in $(REGSUBS); do\
		echo "===> $$s";\
		(cd $$s; $(M3BUILD) || $(PROCEEDCMD); $(M3SHIP));  \
	done

browserexps:
	if [ -x m3browserhack ]; then \
		m3browserhack/export.sh $(BROWSEREXPS); \
	fi

cleanships:
	@for s in $(REVSHIPS); do\
		echo "(cleaning) ===> $$s";\
		(cd $$s; $(M3BUILDCLEAN) || true);  \
	done

cleanm3:
	@for s in $(M3SUBS); do\
		echo "(cleaning) ===> $$s";\
		(cd $$s; $(M3BUILDCLEAN) || true);  \
	done

cleancommons:
	@for s in $(REVCOMMONS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; $(MAKE) clean; cd .. ;  \
	done

cleansubs:
	@for s in $(SUBDIRS); do\
		echo "(cleaning) ===> $$s";\
		cd $$s; $(MAKE) clean; cd .. ;  \
	done



# this is the end of the Makefile
