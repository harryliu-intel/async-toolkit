# $Id$ 
PRE_MAKE?=echo
PRE_MAKE_CLEAN?=echo
all: 
	cd src; $(PRE_MAKE); m3build; cd ..

clean:
	cd src; $(PRE_MAKE_CLEAN); m3build clean; cd ..
