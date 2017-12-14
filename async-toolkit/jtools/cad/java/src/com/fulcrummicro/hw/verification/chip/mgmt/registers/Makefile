# vim:et:sw=4:ts=4:tw=79:
# Makefile for compiling all Jamon templates.

###############################################################################
# Macros
###############################################################################

###############################################################################
## p4-revert
#
# \desc             Checks for each file in the specified set of files if it
#                   has been added to the Perforce depot and if its contents,
#                   apart from the file header, have changed. If both
#                   conditions are true, the file is reverted.
#
# \param[in]        template is the Jamon template used to generate the
#                   specified set of files.
#
# \param[in]        files is the set of Java files to potentially revert.
#
###############################################################################
define p4-revert
for f in $(2); do                                                             \
    e=$$($(P4) -s fstat $${f}                                                 \
        | $(GSED) -e "/^error:.*no such file/d" -e "/^exit:/d"                \
        | $(WC) -l);                                                          \
    [ $${e} -eq 0 ] && continue || $(TRUE);                                   \
    d=$$($(P4) -s diff $${f}                                                  \
        | $(GSED) -nr -e "\,//.*/$(1)\$$,d" -e "/^[<>]/p"                     \
        | $(WC) -l);                                                          \
    [ $${d} -eq 0 ] && $(P4) revert $${f} || $(TRUE);                         \
done
endef

###############################################################################
# Sources
###############################################################################

JAMON_FILES     = $(wildcard $(SRCDIR)/$(TMPLDIR)/*.jamon)
JAVA_FILES      = $(patsubst %.jamon,%.java,$(JAMON_FILES))
JAVA_IMPL_FILES = $(patsubst %.jamon,%Impl.java,$(JAMON_FILES))

###############################################################################
# Global Variables
###############################################################################

empty =
space = $(empty) $(empty)

SHAREDIR  = $(shell echo $(CURDIR) | sed -r -e "s,(/shared)/.*,\1,")
SRCDIR    = $(SHAREDIR)/java/src
TMPLDIR   = com/fulcrummicro/hw/verification/chip/mgmt/registers/templates
JARDIR    = $(SHAREDIR)/jars
CLASSPATH = $(patsubst :%,%,                                                  \
              $(subst $(space):,:,                                            \
                $(addprefix :$(JARDIR)/,                                      \
                  jamon-runtime-2.4.0.jar                                     \
                  jamon-api-2.3.0.jar                                         \
                  jamon-processor-2.4.1.jar)))

GSED      = sed
JAMON     = $(JAVA) -classpath $(CLASSPATH) org.jamon.compiler.TemplateProcessor
JAVA      = java
P4        = p4
TRUE      = true
WC        = wc

###############################################################################
# Targets
###############################################################################

all : $(JAVA_FILES)

clean :
	$(RM) $(JAVA_FILES) $(JAVA_IMPL_FILES)

.PHONY: all clean

###############################################################################
# Transformation Rules
###############################################################################

%.java : %.jamon
	$(P4) edit $@ $*Impl.java >/dev/null 2>&1 || $(TRUE)
	cd $(SRCDIR) && $(JAMON) --destDir=. $(TMPLDIR)/$(*F)
	$(call p4-revert,$(<F),$@ $*Impl.java)
