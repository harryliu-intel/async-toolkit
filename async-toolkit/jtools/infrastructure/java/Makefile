#
#      Makefile - Top-level Makefile for Maelstrom Java tree
#
#      Copyright 2001-2002 Fulcrum Microsystems, Inc.  All rights reserved.
#
#      $Id: //depot/sw/sdk/core/maelstrom/main/build/java/Makefile#40 $
#

# Author: Jesse Rosenstock

# Make settings
# Clean out default rules.  We don't want them.
.SUFFIXES:

# Phony rules
.PHONY: all extraclean clean install makefiles help test javadoc

CLASSES_DIR:=classes
JARS := ../jars/jep-2.4.1.jar
ifndef CLASSPATH
    export CLASSPATH := $(shell ./scripts/compute_class_path)
else
    export CLASSPATH := src:$(CLASSES_DIR):$(CLASSPATH)
endif
export CLASSPATH :=$(CLASSPATH):$(JARS)

UNAME:=$(shell uname)

ifeq ($(UNAME),Darwin)
    export JIKESPATH:=$(CLASSPATH):$(subst $(space),:,$(wildcard /System/Library/Frameworks/JavaVM.framework/Versions/1.4.1/Classes/*.jar))
else
    export JIKESPATH:=$(CLASSPATH):/usr/local/java/jre/lib/rt.jar
endif

corejflags =	+F +P +Pno-modifier-order +Pno-unchecked-exception -depend
fulcumjflags =	+Pno-redundant-modifiers

# programs to use.
# JAVADEPENDS:=jikes +M +P # does not yet work.
JAVAC=/usr/local/jdk1.5.0/bin/javac
JAVA==/usr/local/jdk1.5.0/bin/java
JAR=/usr/local/jdk1.5.0/bin/jar
JAVAH=javah
JAVAC_FLAGS=-g -d classes -classpath $(CLASSPATH)
ANTLR=antlr-2.7.2
CTAGS:=ctags
INSTALL = install -m 775
SED=sed

JAVADOC=javadoc
JAVADOC_FLAGS=-private -version -author \
    -link file:////usr/local/jdk1.3.1/docs/api \
    -link file:////home/httpd/html-internal/technical/docs/antlr-docs \
    -link file:////home/httpd/html-internal/technical/docs/jcsp-docs
JAVADOC_DIR=javadoc

# The [A-Za-z] is to avoid the files that start with "._" on Mac OS X.
# See bug 1776

FIND_JAVA_FILES:=find src -name '[A-Za-z]*.java'
FIND_CLASS_FILES:=find classes -name '[A-Za-z]*.class'
FIND_ANTLR_FILES:=find src -name '[A-Za-z]*.g'

SUBDIRS:=$(shell find src -type d)
SUBDIR_MAKEFILES:=$(SUBDIRS:%=%/Makefile)

ANTLR_SOURCES:=$(shell $(FIND_ANTLR_FILES))
ALL_CVS_JAVA_FILES:=$(filter-out $(ANTLR_TARGETS), $(shell $(FIND_JAVA_FILES)))
ALL_JAVA_FILES:=$(ALL_CVS_JAVA_FILES) $(ANTLR_TARGETS)
ALL_CLASS_FILES:=$(ALL_JAVA_FILES:src/%.java=classes/%.class)

BUILD_INFRASTRUCTURE:=$(SUBDIR_MAKEFILES)

default: install

help:
	@echo '"make" rebuilds all stale .class files in large batches'
	@echo '"make clean" removes all .class files'
	@echo '"make makefiles" rebuilds Makefiles in all dirs'

all: $(BUILD_INFRASTRUCTURE) $(ALL_CLASS_FILES) 

tags: $(BUILD_INFRASTRUCTURE) $(ALL_JAVA_FILES)
	$(FIND_JAVA_FILES) -print0 | xargs -0 $(CTAGS)

%/tags: $(BUILD_INFRASTRUCTURE) $(ANTLR_TARGETS)
	cd `dirname "$@"` && $(CTAGS) *.java

clean:
	find classes -name '*.class' -print0 | xargs -0 rm -f
	find src     -name '*.class' -print0 | xargs -0 rm -f

makefiles: $(SUBDIR_MAKEFILES)

$(SUBDIR_MAKEFILES): scripts/make_subdir_makefile
	scripts/make_subdir_makefile `dirname $@`

# rules for java
# Can not compile some of these until the ANTLR targets they reference
# get made.
$(ALL_CLASS_FILES) : $(ANTLR_TARGETS)

ifdef JAVADEPENDS
###
# Okay, this is tricky...
# We want to batch up compilations to minimize time spent starting
# up the compiler. OTOH, we also don't want to compile java files
# unnecessarily. So, we want make to compile all things that are
# out-of-date whenever any one thing is out-of-date.
# We can do this by having make invoke itself to collect the list of
# files to compile, and only then compiling them.
ifndef JAVA_MAKE_COLLECTION_HACK
classes/%.class classes/%.u : src/%.java
	: > collecting
	make JAVA_MAKE_COLLECTION_HACK=1 all
	sort < collecting | uniq > out-of-date
	$(JAVADEPENDS) $(JAVAC_FLAGS) @out-of-date
	rm out-of-date collecting
else
classes/%.class classes/%.u : src/%.java
	@echo -n .
	@echo $< >> collecting
endif

# - -- do not complain if missing
# $(wildcard ...) -- only use pre-existing depends.  Otherwise make
# will try to build them.
ifneq (,$(wildcard $(ALL_CLASS_FILES:%.class=%.u)))
-include $(wildcard $(ALL_CLASS_FILES:%.class=%.u))
endif
else
####
# not trying to save time with dependencies.  All class files get rebuilt when anything changes.

# might consider adding this inside if statement below:
# 	    if [ -f $(COVERAGE_DATA_DIR)/hitTable ]; then \
# 		gretel reinstrument $(COVERAGE_DATA_DIR); \
# 	    else \
# 	    fi

$(ALL_CLASS_FILES) : $(ALL_JAVA_FILES)
	$(FIND_JAVA_FILES) -print0 | xargs -0 $(JAVAC) $(JAVAC_FLAGS)
#	if [ $(GRETEL_INSTRUMENT) ]; then \
#		mkdir -p $(COVERAGE_DATA_DIR); \
#		find classes/ -name \*.class | gretel instrument -noreduce -s $$PWD/src -f - $(COVERAGE_DATA_DIR); \
#	fi
endif

JAR_DEPENDS:=$(ALL_CLASS_FILES)

util.jar: $(JAR_DEPENDS)
	cd $(CLASSES_DIR) && \
	$(JAR) cvf $@ \
	    com/fulcrummicro/util \
	| grep -v adding
	mv $(CLASSES_DIR)/$@ .

install: ../jars/util.jar
../jars/util.jar: util.jar
	if [ ! -w $@ ]; then p4 edit $@; fi
	cp $< ../jars/

testbench.jar: $(JAR_DEPENDS)
	cd $(CLASSES_DIR) && \
	$(JAR) cvf $@ \
	    com/fulcrummicro \
	| grep -v adding
	mv $(CLASSES_DIR)/$@ .

testbench_combined.jar: $(JAR_DEPENDS) ../jars/dsim.jar
	cp ../jars/dsim.jar $(CLASSES_DIR)/$@
	cd $(CLASSES_DIR) && \
	($(JAR) uvf $@ \
	    com/fulcrummicro \
	    && echo pass) \
	| grep -v adding
	mv $(CLASSES_DIR)/$@ .
