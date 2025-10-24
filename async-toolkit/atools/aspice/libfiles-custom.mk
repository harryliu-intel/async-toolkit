# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

LIBFILES_DIR_LIB_EXCLUDE_OBJS := $(LIBFILES_DIR_LIB_EXCLUDE_OBJS)   \
                                 $(CURR_TARGET_DIR)/aspice.o        \
                                 $(CURR_TARGET_DIR)/aplot.o         \
                                 $(CURR_TARGET_DIR)/test_model.o    \
                                 $(CURR_TARGET_DIR)/reorder_trace.o \
                                 $(CURR_TARGET_DIR)/convert_trace.o \
                                 $(CURR_TARGET_DIR)/debug_trace.o   \
				 $(CURR_TARGET_DIR)/rc_simplify.o

LIBFILES_NON_LIB_DIRS := $(LIBFILES_NON_LIB_DIRS) asp examples
