# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

JAVA_DIR = ./src/java
LOG_DIR  = ./log

VPATH    = $(JAVA_DIR)

SOURCES  = BridJWm.java

CLASSES  = $(SOURCES:.java=.class)

TARGET   = BridJWm
T_CLASS  = ${JAVA_DIR}/${TARGET}.class
T_ARGS   =

# Java compiler:
JAVAC = javac

# Java runtime:
JAVA = java

ifeq ($(VERBOSE),1)
ECHO := 
else
ECHO := @
endif

ifeq ($(DEBUG),1)
JFLAGS = -Xprint:parse
else
JFLAGS =
endif

AER_JAR = /usr/share/java/jnaerator-0.12.jar
BRD_JAR = /usr/share/java/bridj-0.7.0.jar
HLP_JAR = ./jar/hlp-white-model.jar

CLASSPATH = -cp $(BRD_JAR):$(HLP_JAR):$(JAVA_DIR)

%.class: %.java
	@echo '  Compiling' $<
	$(ECHO) $(JAVAC) $(CLASSPATH) $< 

.PHONY: all
all: $(TARGET)

.PHONY: $(TARGET)
$(TARGET) : $(CLASSES)

$(CLASSES): $(SOURCES)

.PHONY: show_sources
show_sources:
	@echo 'SOURCES :'
	$(ECHO) $(foreach i, $(SOURCES), echo $(i);)

.PHONY: run
run: $(TARGET)
	@echo '  Running $(TARGET)'
	@echo '$(JAVA) $(CLASSPATH) $(TARGET) $(T_ARGS) | tee $(LOG_DIR)/java-run.log'
	$(ECHO) /usr/bin/time --format='Ran for %e seconds.' \
            $(JAVA) $(CLASSPATH) $(TARGET) $(T_ARGS) | tee $(LOG_DIR)/java-run.log

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(JAVA_DIR)/*.class
