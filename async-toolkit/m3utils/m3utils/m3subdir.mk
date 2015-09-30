# $Id: m3subdir.mk,v 1.3 2000/11/12 00:22:05 mika Exp $ 
PRE_MAKE?=echo
PRE_MAKE_CLEAN?=echo
all: 
	cd src; $(PRE_MAKE); m3build $(M3BUILDOPTS); cd ..

clean:
	cd src; $(PRE_MAKE_CLEAN); m3build $(M3BUILDOPTS) clean || true ; cd ..
