#Calculates the values of variables that store information about subdirectories of the current directory.

#         Inputs:
#           CURR_NON_BUILD_DIRS         A list of directory names that not to be recursed into.
#           CURR_PROJECT_DIR            The current project directory we are building from.
#           CURR_TARGET_DIR             The current target directory we are building into.
#         Outputs:
#           CURR_SUB_DIRS               A list of directories that are subdirectories 
#                                       of the current project directory.
#           CURR_SUB_TARGETS_DIRS       A list of directories that the directories listed in
#                                       CURR_SUB_DIRS will be build into. 
#           CURR_SUB_DIRS_NAMES         A list of the names of the subdirectories in the current
#                                       project directory
#         Side Effects:
#           None


#Gets list of subdirectories of the directory we are currently building from.
CURR_SUB_DIRS := $(wildcard $(CURR_PROJECT_DIR)/*/. )

ifeq ($(MAKE_PWD_ONLY),1)
CURR_MKPWDONLY_TARGETS := $(filter $(CURR_TARGET_DIR)%,$(MKPWDONLY_TARGETS))
endif

#Get a list of all the subdirectories that we are suppose to recurse into.
#This list contains just the names of the subdirectories, not the path the directories.
CURR_SUB_DIRS_NAMES := $(filter-out $(CURR_NON_BUILD_DIRS), \
				$(patsubst $(CURR_PROJECT_DIR)/%/.,\
				%, $(CURR_SUB_DIRS) ) )

#Clean up the entries in the CURR_SUB_DIRS list.
CURR_SUB_DIRS := $(foreach target, $(CURR_SUB_DIRS_NAMES), \
                 $(CURR_PROJECT_DIR)/$(target))

CURR_SUB_TARGETS_DIRS := $(foreach target, $(CURR_SUB_DIRS_NAMES), \
                        $(CURR_TARGET_DIR)/$(target))

ifeq ($(MAKE_PWD_ONLY),1)
CURR_SUB_TARGETS_DIRS := $(call PARENTSOFDIRS,$(CURR_SUB_TARGETS_DIRS),$(CURR_MKPWDONLY_TARGETS))
CURR_SUB_DIRS := $(foreach dir,$(CURR_SUB_TARGETS_DIRS),$(patsubst $(ROOT_TARGET_DIR)/%,$(ROOT_PROJECT_DIR)/%,$(dir)))

endif
