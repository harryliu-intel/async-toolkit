ifeq ($(MAKE_LAYOUT),1)

ifeq ($(LVEFILES_VARS_CHANGED),1)

POP_SCOPED_VAR_VAR_NAME := LVS_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := DRC_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

endif

LVEFILES_VARS_CHANGED :=$(strip $(firstword $(LVEFILES_VARS_CHANGED_STACK)))
LVEFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(LVEFILES_VARS_CHANGED_STACK))

endif
