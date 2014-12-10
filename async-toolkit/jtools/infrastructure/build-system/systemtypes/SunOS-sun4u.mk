# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

JIKES := /usr/intel/bin/jikes
GNUSED  := /usr/intel/bin/sed
GNUGAWK := /usr/intel/bin/gawk
GNUFIND := /usr/intel/bin/find
GNUGREP := /usr/intel/bin/grep
GNUTAR  := /usr/intel/bin/tar
GNUBZIP2 := /usr/bin/bzip2

BZTAR := $(GNUTAR) --use-compress-program=$(GNUBZIP2)

GCC_DIR := /usr/intel
GCC := $(GCC_DIR)/bin/gcc
GCXX := $(GCC_DIR)/bin/g++
GCPP := $(GCC) -E

PYTHON := /usr/intel/bin/python
PYTHON2 := /usr/intel/bin/python2.2
LDSTATICLINKER := /usr/ccs/bin/ld -r

SYSTEM_LINKER := /usr/ccs/bin/ld

ANTLR := $(BUILD_SYSTEM_ROOT)/bin/antlr-2.7.2

PERL := /usr/intel/bin/perl

JAVA_JNI_INCLUDE_DIR := /usr/intel/java/include/solaris

QSUB_ARCH := x86_64
