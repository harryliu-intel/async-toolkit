#Push the old current project directory.
PROJECT_DIR_STACK := $(CURR_PROJECT_DIR) $(PROJECT_DIR_STACK)
#Set CURR_PROJECT_DIR to the new current project directory
CURR_PROJECT_DIR := $(CURR_PROJECT_DIR)/$(CURR_PROJECT_NAME)

#Push the old target directory.
TARGET_DIR_STACK := $(CURR_TARGET_DIR) $(TARGET_DIR_STACK)
#Set CURR_TARGET_DIR to the new current target directory
CURR_TARGET_DIR := $(CURR_TARGET_DIR)/$(CURR_PROJECT_NAME)

include $(BUILD_SYSTEM_ROOT)/defaults.common.mk

include $(BUILD_SYSTEM_ROOT)/defaults.subincludes.mk

include $(BUILD_SYSTEM_ROOT)/dir_exit.mk

#Pop the old target directory off the stack and store it in CURR_TARGET_DIR thus making it
#the current target directory.
CURR_TARGET_DIR := $(firstword $(TARGET_DIR_STACK) )
TARGET_DIR_STACK := $(wordlist 2, $(words $(TARGET_DIR_STACK) ), $(TARGET_DIR_STACK))

#Pop the old project directory off the stack and store it in CURR_PROJECT_DIR thus making it
#the current project directory.
CURR_PROJECT_DIR := $(firstword $(PROJECT_DIR_STACK) )
PROJECT_DIR_STACK := $(wordlist 2, $(words $(PROJECT_DIR_STACK) ), $(PROJECT_DIR_STACK))

#Pop the old project name off the stack and store it in CURR_PROJECT_NAME thus making it
#the current project name.
CURR_PROJECT_NAME := $(firstword $(SUB_PROJ_STACK) )
SUB_PROJ_STACK := $(wordlist 2, $(words $(SUB_PROJ_STACK) ), $(SUB_PROJ_STACK))
