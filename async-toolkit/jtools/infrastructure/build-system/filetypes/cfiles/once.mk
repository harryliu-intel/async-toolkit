
#Get a comprehensive list of library directories.
#LIBDIRS = $(PROJECT_LIB_DIRS) $(SYSTEM_LIB_DIRS)

#Make the command line switches from the library directories.
#LIBPATHSWITCHES := $(foreach dir, $(LIBDIRS), -L$(dir) )

#Get a comprehensive list of all the libraries we need to link against.
#LIBRARIES = $(PROJECT_LIBS) $(SYSTEM_LIBS)

#Make the command line switches from the library directories.
#LIBSWITCHES := $(foreach lib, $(LIBRARIES), -l$(lib) )

#define the link command
#LD := $(CLINKER) $(LIBPATHSWITCHES) $(LIBSWITCHES) $(PROJECT_LINK_FLAGS)

#INTERMEDIATEFILETYPES := $(INTERMEDIATEFILETYPES) .o .d .d.doc

CFILES_PRE_PROCESSOR_DEFINES_FRAME_SIZE  :=0
CFILES_PROJECT_INCLUDE_DIRS_FRAME_SIZE   :=0
CFILES_GENERATED_INCLUDE_DIRS_FRAME_SIZE :=0
CFILES_C_COMPILE_FLAGS_FRAME_SIZE        :=0
