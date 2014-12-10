# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
CFILES_COMPILER     := $(GCC) 
CFILES_PREPROCESSOR := $(CFILES_COMPILER) -E
CFILES_C_COMPILE_FLAGS := -O3 -funroll-loops -ffast-math -Wall -fPIC
PROGFILES_PROJECT_LIBS += readline

ifeq ($(MAKEFILE_KERNEL_NAME),Linux)
   ifeq ($(MAKEFILE_MACHINE_TYPE),x86_64)
     CFILES_C_COMPILE_FLAGS := $(CFILES_C_COMPILE_FLAGS) -ggdb -march=opteron -m64 -DSIMD 
     PROGFILES_PROJECT_LIBS += ncurses m
  else
    CFILES_C_COMPILE_FLAGS := $(CFILES_C_COMPILE_FLAGS) -ggdb -march=pentium3 -DSIMD
    PROGFILES_PROJECT_LIBS += ncurses m
  endif
endif
ifeq ($(MAKEFILE_KERNEL_NAME),SunOS)
  CFILES_C_COMPILE_FLAGS := $(CFILES_C_COMPILE_FLAGS) -ggdb -m64
  PROGFILES_PROJECT_LIBS += termcap m
endif
ifeq ($(MAKEFILE_KERNEL_NAME),Darwin)
  CFILES_C_COMPILE_FLAGS := $(CFILES_C_COMPILE_FLAGS) -DSIMD
  PROGFILES_PROJECT_LIBS += ncurses
endif

CFILES_PROJECT_INCLUDE_DIRS := $(CURR_PROJECT_DIR)/lib
