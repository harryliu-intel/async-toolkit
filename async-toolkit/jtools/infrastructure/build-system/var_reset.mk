#Reset all the variables that should be reset for each directory in the
#project.

#A list of object that should be linked into the library for the
#current directory.  See dirlib.mk
LIBFILES_TARGET_LD_OBJS := 

LIBFILES_NON_LIB_DIRS :=

#A list of all the dependency files that contain information
#for building objects in the current directory.
CURR_TARGET_DEPS := 

#A list of all the dependency files that contain information
#for building documentation in the current directory.
CURR_TARGET_DOC_DEPS := 

#Directories that we are not to recurse into.
CURR_NON_BUILD_DIRS := CVS

#A list of directories that are subdirectories 
#of the current project directory.
CURR_SUB_DIRS := 

#A list of the names of the subdirectories in the current
#project directory
CURR_SUB_DIRS_NAMES := 

#A list of directories that the directories listed in
#CURR_SUB_DIRS will be build into. 
CURR_SUB_TARGETS_DIRS := 
