# -*- mode:Makefile -*-

# Copyright (C) 2018 Intel Corporation

SCALA_DIR = ./src/scala
LOG_DIR  = ./log
LOG_DIR_FILE  = ./log/scala

VPATH    = $(SCALA_DIR)

SOURCES  = BridJWmS.scala

DATADIR = dat

# DATASOURCES = $(shell find $(DATADIR) -name '*.imsg' | awk -F'.' '{print $1}')
DATASOURCES = $(basename \
                     $(shell find $(DATADIR) -name '*.imsg' | awk -F'.' '{print $1}'))

CLASSES  = $(SOURCES:.scala=.class)

TARGET   = BridJWmS
T_CLASS  = ${SCALA_DIR}/${TARGET}.class
T_ARGS   =

# Scala compiler:
SCALAC = scalac

# Scala runtime:
SCALA = scala

ifeq ($(VERBOSE),1)
ECHO := 
else
ECHO := @
endif

ifndef FILE
FULLFILENAME := 
LOGFILE := $(LOG_DIR)/scala-run.log 
else
FULLFILENAME := $(DATADIR)/$(FILE)
LOGFILE := $(LOG_DIR_FILE)/scala-run-$(FILE).log 
endif

ifeq ($(DEBUG),1)
SFLAGS = -Xprint:parse
else
SFLAGS =
endif

AER_JAR = /usr/share/java/jnaerator-0.12-SNAPSHOT-20130727.jar
BRD_JAR = /usr/share/java/bridj-0.7.0.jar
HLP_JAR = ./jar/hlp-white-model.jar

CLASSPATH = -cp $(BRD_JAR):$(HLP_JAR):${SCALA_DIR}

%.class: %.scala
	@echo '  Compiling' $<
	$(ECHO) $(SCALAC) $(CLASSPATH) -d $(SCALA_DIR) $< 

.PHONY: all
all: $(TARGET)

.PHONY: $(TARGET)
$(TARGET) : $(CLASSES)

$(CLASSES): $(SOURCES)

.PHONY: show_sources
show_sources:
	@echo 'SOURCES :'
	$(ECHO) $(foreach i, $(DATASOURCES), \
              echo $(i);)

.PHONY: run
run: $(TARGET)
	$(ECHO) /usr/bin/time --format='Ran for %e seconds.' \
            $(SCALA) $(CLASSPATH) $(TARGET) $(T_ARGS) $(FULLFILENAME) | tee $(LOGFILE)

# .PHONY: run_all
# run_all: $(TARGET)
# 	@echo '  Running $(TARGET)'
# 	$(foreach i, $(DATASOURCES), \
#             /usr/bin/time --format='Ran for %e seconds.' \
#                 $(SCALA) $(CLASSPATH) $(TARGET) $(T_ARGS) $(i) | tee -a $(LOG_DIR)/scala-run.log;)

.PHONY: clean
clean:
	@echo '  Cleaning'
	$(ECHO) rm -f $(SCALA_DIR)/*.class
