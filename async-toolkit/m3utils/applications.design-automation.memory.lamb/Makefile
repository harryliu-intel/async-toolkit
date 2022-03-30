#
# Top-level Makefile for lamb repo
#
# see README.md for details
#
# Cheetah's latest CM3 as of 3/18/22
#

DEFAULT_RTA_CM3_HOME = /p/cth/rtl/cad/x86-64_linux44/opensource/cm3/d5.11.0-20210425/

ifndef RTA_CM3_HOME
  export RTA_CM3_HOME = $(DEFAULT_RTA_CM3_HOME)
endif

.PHONY: all clean cleanall quick

all: m3utils.done m3.done

quick: m3.done

m3utils.done:
	pushd m3utils && $(MAKE)
	touch $@

m3.done: m3utils.done
	pushd m3 && $(MAKE)
	touch $@

clean:
	rm -f m3.done
	pushd m3 && $(MAKE) clean
	rm -f m3utils.done
	pushd m3utils && $(MAKE) clean
