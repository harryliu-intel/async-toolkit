# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
LIBFILES_NON_LIB_DIRS := $(LIBFILES_NON_LIB_DIRS) lib
LIBFILES_DIR_LIB_EXCLUDE_OBJS := $(LIBFILES_DIR_LIB_EXCLUDE_OBJS)   \
                                 $(CURR_TARGET_DIR)/auto.o          \
                                 $(CURR_TARGET_DIR)/make_auto.o     \
                                 $(CURR_TARGET_DIR)/cast_which.o
