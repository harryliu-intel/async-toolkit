# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

SHELL := /bin/bash


GNUSED := /bin/sed
GNUGAWK := /usr/bin/gawk
GNUFIND := /usr/bin/find
GNUGREP := /bin/grep
GNUTAR  := /bin/tar
GNUBZIP2 := /usr/bin/bzip2

BZTAR := $(GNUTAR) --use-compress-program=$(GNUBZIP2)

GCC_DIR := /usr
GCC  := $(GCC_DIR)/bin/gcc
GCXX := $(GCC_DIR)/bin/g++
GCPP := $(GCC) -E

PYTHON := /usr/bin/python
PYTHON2 := /usr/bin/python2.2
LDSTATICLINKER := /usr/bin/ld -Ur

JIKES := /usr/intel/bin/jikes

JDK_ROOT := /usr/intel/java
JRE_ROOT = $(JDK_ROOT)/jre

JAR = /usr/intel/bin/jar
JAVAC = /usr/intel/bin/javac
JAVAH = /usr/intel/bin/javah
JAVADOC = $(JDK_ROOT)/bin/javadoc
JAVA_JNI_INCLUDE_DIR = $(JDK_ROOT)/include/linux

SYSTEM_LINKER := /usr/bin/ld

ANTLR := $(BUILD_SYSTEM_ROOT)/bin/antlr-2.7.2

PERL := /usr/intel/bin/perl
