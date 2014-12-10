#JAVAFILES_GCJ_DEP := /usr/bin/gcj -MM -MG -C


JAVAFILES_CLASSES_JAR_ROOT :=$(call CONONICALIZE_PATH, $(CURR_PROJECT_DIR)/../jars)
JAVAFILES_CLASSPATH := $(shell echo '$(JAVAFILES_CLASSES_JAR_ROOT)/antlr-2.7.2.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/concurrent.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/stringtemplate.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/jdom.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/xercesImpl.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/xml-apis.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/resolver.jar:$(JDK_ROOT)/lib/tools.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/tools.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/JaCoP-3.2.jar' | sed -e 's/ //g')

JAVAFILES_JAVAC_FLAGS := -g -deprecation -source 1.5 -J-Xmx256M

FULCRUM_JAVAC_FLAGS := +P +Pno-modifier-order +F -depend
FULCRUM_JAVAC_FLAGS := $(FULCRUM_JAVAC_FLAGS) +Pno-unchecked-exception

JAVAFILES_JAVAC := $(JAVAC) $(JAVAFILES_JAVAC_FLAGS)



JAVAFILES_JDK_ROOT := $(JDK_ROOT)
JAVAFILES_JRE_ROOT := $(JRE_ROOT)

JAVAFILES_PARSERS_USED := $(CURR_TARGET_DIR)/com/avlsi/cast/impl/CastParser                                            \
                          $(CURR_TARGET_DIR)/com/avlsi/cast/impl/CastParser                                            \
                          $(CURR_TARGET_DIR)/com/avlsi/cast/impl/DumbParser                                            \
                          $(CURR_TARGET_DIR)/com/avlsi/cast/impl/CastTreeParser                                        \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastTwoParser                                        \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastTwoTreeParser                                    \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/DumbTwoParser                                        \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastAliasesParser                                    \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastAssertParser                                     \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastPrsParser                                        \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastSubcellsParser                                   \
                          $(CURR_TARGET_DIR)/com/avlsi/cast2/impl/CastSubtypesParser                                   \
                          $(CURR_TARGET_DIR)/com/avlsi/fast/shapes/stringexpression/impl/parser/StringExpressionParser \
                          $(CURR_TARGET_DIR)/com/avlsi/csp/grammar/CspParser                                           \
                          $(CURR_TARGET_DIR)/com/avlsi/util/mathexpression/impl/parser/MathExpressionParser            \
                          $(CURR_TARGET_DIR)/com/avlsi/file/cdl/parser/CDLParser                                       \
                          $(CURR_TARGET_DIR)/com/avlsi/tools/cosim/spec/CoSimParser                                    \
                          $(CURR_TARGET_DIR)/com/avlsi/tools/jauto/CellHierarchyDumpParser \
                          $(CURR_TARGET_DIR)/com/avlsi/tools/jauto/CastQueryParser \
                          $(CURR_TARGET_DIR)/com/avlsi/file/verilog/grammar/VerilogLexer \
                          $(CURR_TARGET_DIR)/com/avlsi/file/verilog/grammar/VerilogParser \
                          $(CURR_TARGET_DIR)/com/avlsi/file/verilog/grammar/PortDeclarationTreeParser


JAVAFILES_PARSERS_ROOT := $(CURR_TARGET_DIR)
JAVAFILES_CLASSES_TO_BUILD :=
JAVAFILES_APPS_CLASSES := 


JAVAFILES_JAR   := $(JAR)
JAVAFILES_JAVAH := $(JAVAH) -force

JAVAFILES_CLASSES_SRC_ROOT := $(CURR_PROJECT_DIR)
JAVAFILES_CLASSES_CACHE_ROOT := $(CURR_TARGET_DIR)/classcache


JAVAFILES_JAVADOC := $(JAVAFILES_JDK_ROOT)/bin/javadoc
JAVAFILES_JAVADOC_FLAGS := -private -version -author -classpath /usr/local/jars/antlrall.jar:$(JAVAFILES_CLASSES_JAR_ROOT)/jars/antlrall.jar
JAVAFILES_JAVADOC_LINKS := http://java.sun.com/j2se/1.3/docs/api \
                           http://internal/~chrisb/antlr-doc \
                           http://internal/technical/docs/jcsp-docs

JAVAFILES_JAVADOC_DIR := $(JAVADOC)/javadoc
JAVAFILES_JAVADOC_SOURCE_PATH := $(CURR_PROJECT_DIR)


JAVAFILES_JNI_INCLUDE_DIR := $(JAVA_JNI_INCLUDE_DIR)



