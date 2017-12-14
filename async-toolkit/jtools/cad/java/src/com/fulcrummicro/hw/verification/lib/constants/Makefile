# Makefile for compiling Jamon in this dir
# make sure to p4 edit the target Jamon files

JARPATH = ${CURDIR}/../../../../../../../../../shared/jars

CLASSPATH=.:${JARPATH}/jamon-runtime-2.4.0.jar:${JARPATH}/jamon-api-2.3.0.jar:${JARPATH}/jamon-processor-2.4.1.jar

JAMON_FILES = $(wildcard *.jamon)
JAVA_IMPL_FILES = $(subst .jamon,Impl.java, ${JAMON_FILES})
JAVA_FILES = $(subst .jamon,.java, ${JAMON_FILES})

#abspath not supported on GNU make 3.80 (still used on some machines)
#SRCDIR = $(abspath ./../../../../../../..)
#RELDIR = $(subst ${SRCDIR}/,,$(abspath .))

SRCDIR = ../../../../../..
RELDIR = com/fulcrummicro/hw/verification/lib/constants

JAVA = java

.PHONY: all clean

all: ${JAVA_IMPL_FILES}

t:
	@echo DEBUG
	@echo CURDIR: ${CURDIR}
	@echo JAMON: ${JAMON_FILES}
	@echo JAVA: ${JAVA_FILES}
	@echo SRCDIR: ${SRCDIR}
	@echo RELDIR: ${RELDIR}

%Impl.java : %.jamon
	cd ${SRCDIR} && \
	$(JAVA) -classpath ${CLASSPATH} org.jamon.compiler.TemplateProcessor --destDir=. ${RELDIR}/$*

clean:
	rm -rf ${JAVA_FILES} ${JAVA_IMPL_FILES}
