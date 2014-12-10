

ifeq ($(CFILES_VARS_CHANGED),1)

POP_SCOPED_VAR_VAR_NAME := CFILES_COMPILER
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CFILES_PREPROCESSOR
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CFILES_PRE_PROCESSOR_DEFINES
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CFILES_PROJECT_INCLUDE_DIRS 
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CFILES_GENERATED_INCLUDE_DIRS
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CFILES_C_COMPILE_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

endif

CFILES_VARS_CHANGED :=$(strip $(firstword $(CFILES_VARS_CHANGED_STACK)))
CFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(CFILES_VARS_CHANGED_STACK))
