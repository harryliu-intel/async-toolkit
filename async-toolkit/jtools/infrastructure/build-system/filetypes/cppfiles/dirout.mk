

ifeq ($(CPPFILES_VARS_CHANGED),1)


POP_SCOPED_VAR_VAR_NAME := CPPFILES_COMPILER
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CPPFILES_PREPROCESSOR
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CPPFILES_PRE_PROCESSOR_DEFINES
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CPPFILES_PROJECT_INCLUDE_DIRS 
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CPPFILES_GENERATED_INCLUDE_DIRS
include $(BUILD)/include-functions/popscopedvar.mk

POP_SCOPED_VAR_VAR_NAME := CPPFILES_CPP_COMPILE_FLAGS
include $(BUILD)/include-functions/popscopedvar.mk

endif

CPPFILES_VARS_CHANGED :=$(strip $(firstword $(CPPFILES_VARS_CHANGED_STACK)))
CPPFILES_VARS_CHANGED_STACK := $(call POP_STACK,$(CPPFILES_VARS_CHANGED_STACK))
