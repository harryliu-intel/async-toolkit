# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

# This file is meant to be included by project Makefiles
# to ensure that specific tool versions are used.

# Gnu C compiler:
GCC = /usr/intel/pkgs/gcc/7.2.0/bin/gcc
ifeq (,$(wildcard $(GCC)))
GCC = gcc
endif

# Gnu C++ compiler:
GPP = /usr/intel/pkgs/gcc/7.2.0/bin/g++
ifeq (,$(wildcard $(GPP)))
GPP = g++
endif

# Valgrind:
VAL = /usr/intel/pkgs/valgrind/3.11.0/bin/valgrind
ifeq (,$(wildcard $(VAL)))
VAL = valgrind
endif

# Doxygen:
DOX = /usr/intel/pkgs/doxygen/1.8.11/bin/doxygen
ifeq (,$(wildcard $(DOX)))
DOX = doxygen
endif

# Make Dir:
MKDIR = /bin/mkdir -p

# Symbolic Link:
LN = /bin/ln -s

# File/Dir Remove:
RM = /bin/rm -rf

# Archive:
AR = /usr/bin/ar -c -r -s

# Ranlib (generate index to archive):
RANLIB = /usr/bin/ranlib
