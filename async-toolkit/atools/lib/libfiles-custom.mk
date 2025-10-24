# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
LIBFILES_DIR_LIB_EXCLUDE_OBJS := $(LIBFILES_DIR_LIB_EXCLUDE_OBJS)   \
                                 $(CURR_TARGET_DIR)/expression.o    \
                                 $(CURR_TARGET_DIR)/recalc.o   \
				 $(CURR_TARGET_DIR)/bigint_calculator.o \
	 			 $(CURR_TARGET_DIR)/calculator.o
