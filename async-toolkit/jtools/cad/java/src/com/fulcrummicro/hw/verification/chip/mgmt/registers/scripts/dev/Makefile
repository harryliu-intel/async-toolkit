empty =
space = $(empty) $(empty)

JAMON_FILES     = $(wildcard $(SRCDIR)/$(TMPLDIR)/*.jamon)
JAVA_FILES      = $(patsubst %.jamon,%.java,$(JAMON_FILES))
JAVA      = /usr/intel/bin/java
SRCDIR    = ../../../../../../../..
TMPLDIR   = com/fulcrummicro/hw/verification/chip/mgmt/registers/templates
JARDIR    = $(subst /bin/registers,,$(shell fulcrum --path --latest registers))/share/java
JAMONTOOL = ./jars/jamon-api.jar:./jars/jamon-processor.jar
CLASSPATH = $(JARDIR)/all.jar:$(JARDIR)/jamon-runtime-2.4.0.jar
JAMON     = $(JAVA) -classpath $(CLASSPATH):$(JAMONTOOL) org.jamon.compiler.TemplateProcessor
JAVAC     = /usr/intel/bin/javac -source 8 -J-Xmx256M -classpath $(CLASSPATH)

all: $(JAVA_FILES)

%.java: %.jamon
	cd $(SRCDIR) && \
	$(JAMON) --destDir=. $(TMPLDIR)/$(*F) && \
	$(JAVAC) -d classes $(TMPLDIR)/$(subst .java,*.java,$(@F))
