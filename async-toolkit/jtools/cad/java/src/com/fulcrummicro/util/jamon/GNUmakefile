# vim:et:sw=4:syntax=make:ts=4:tw=79:
#
# INTEL TOP SECRET
# Copyright 2013 Intel Corporation
# All Rights Reserved.

###############################################################################
# Variables
###############################################################################

SHAREDIR  = $(shell $(ECHO) $(CURDIR) | $(SED) -r -e "s,(/shared)/.*,\1,")
OBJDIR    = $(SHAREDIR)/java/src
TMPLDIR   = $(CURDIR)

ECHO      = /bin/echo
JAMON     = $(JAVA) $(JFLAGS) org.jamon.compiler.TemplateProcessor
JAVA      = /usr/intel/bin/java
P4        = p4
SED       = sed

JARPATH   = $(SHAREDIR)/jars
CLASSPATH = $(JARPATH)/jamon-api-2.3.0.jar:                                   \
            $(JARPATH)/jamon-runtime-2.4.0.jar:                               \
            $(JARPATH)/jamon-processor-2.4.1.jar
JFLAGS    = -classpath $(shell $(ECHO) $(CLASSPATH) | $(SED) "s/:[ ]*/:/g")

###############################################################################
# Sources, Intermediates & Objects
###############################################################################

dpi_SRCS = $(wildcard $(TMPLDIR)/*.jamon)
dpi_OBJS = $(patsubst %.jamon,%.java,$(dpi_SRCS))
dpi_OBJS += $(patsubst %.jamon,%Impl.java,$(dpi_SRCS))

###############################################################################
# Targets
###############################################################################

.DEFAULT_GOAL := templates

templates : $(dpi_OBJS)

.PHONY: templates

###############################################################################
# Transformation Rules
###############################################################################

%.java %Impl.java : %.jamon
	$(P4) edit $*.java $*Impl.java
	cd $(OBJDIR) && \
		$(JAMON) --destDir=. $$($(ECHO) $* | $(SED) -r -e "s,$(OBJDIR)/*,,")
	$(P4) revert -a $*.java $*Impl.java
