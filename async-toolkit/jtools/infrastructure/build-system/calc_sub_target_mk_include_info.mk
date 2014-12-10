#Calculate the value of the variables that store information about the
#default.mk files that we are supposed to include from the subdirectories of
#the current target directory.

#         Inputs:
#           CURR_SUB_TARGETS_DIRS       A list of directories that the directories listed in
#                                       CURR_SUB_DIRS will be build into.
#         Outputs:
#           CURR_SUB_TARGET_MK_INCLUDES A list of the defaults.mk files to include from
#                                       from the subdirectories of the current target directory.
#         Side Effects:
#           1.  LOAD_DEP_FILES set to 0 if not all the defaults.mk files for the subdirectory
#               of the current target directory exist.


CURR_SUB_TARGET_MK_INCLUDES := $(foreach targetdir, $(CURR_SUB_TARGETS_DIRS), \
                               $(targetdir)/defaults.mk )


EXISTING_SUB_TARGET_MK_INCLUDES := $(foreach targetdir, $(CURR_SUB_TARGETS_DIRS), \
                                   $(wildcard $(targetdir)/defaults.mk*) )

NUM_SUB_TARGET_MK_INCLUDES := $(words $(CURR_SUB_TARGET_MK_INCLUDES))
NUM_EXISTING_SUB_TARGET_MK_INCLUDES := $(words $(EXISTING_SUB_TARGET_MK_INCLUDES) )


ifneq ("$(strip $(NUM_SUB_TARGET_MK_INCLUDES))","$(strip $(NUM_EXISTING_SUB_TARGET_MK_INCLUDES))")
LOAD_DEP_FILES :=0
endif
