#!/bin/csh

# This script will setup everything and get you going

###############################################
# Set your License Server and LM_PROJECT here #
###############################################
# Use these settings if using the Training License Server
setenv DVTLMD_LICENSE_FILE 20020@dvt01b.elic.intel.com
setenv LM_PROJECT TRAINING1
#setenv DVTLMD_LICENSE_FILE 20020@dvt01p.elic.intel.com
#setenv LM_PROJECT MBY

if ($?LD_LIBRARY_PATH) then                            ## if Previous Path defined, append to it.
   setenv LD_LIBRARY_PATH /usr/intel/pkgs/gtk+/2.24.20/lib64:$LD_LIBRARY_PATH
else                                                   ## Else just set it
   setenv LD_LIBRARY_PATH /usr/intel/pkgs/gtk+/2.24.20/lib64
endif

###################################################
# Edit these to select the tool versions you want #
###################################################
set VCS_VERSION   = `ToolConfig.pl get_tool_version vcsmx`
set VERDI_VERSION = `ToolConfig.pl get_tool_version verdi3`
set OVM_VERSION   = `ToolConfig.pl get_tool_version ovm`
set UVM_VERSION   = `ToolConfig.pl get_tool_version uvm`
#set XVM_VERSION   = `ToolConfig.pl get_tool_version xvm`
set XVM_VERSION   = 1.0.3
set SAOLA_VERSION = `ToolConfig.pl get_tool_version saola`
###################################################

#################
# Programs Used #
#################
set GREP = /bin/grep
set LS = /bin/ls
set SORT = /bin/sort
set TAIL = /usr/bin/tail
set MKDIR = /bin/mkdir
set RM = /bin/rm
set FIND = /usr/bin/find


###########################
# Set default values here #
###########################
set RUN_DVT=1
set HEAP_SIZE="1G"
set PROJ_DIR=$WORKAREA;


#################################
# Process Commandline Arguments #
#################################

while ( $#argv != 0 )
  switch ($argv[1])
    case "-proj":
    case "-project":
      set PROJ_DIR=$2;
      shift
      breaksw
    case "-heap":
      set HEAP_SIZE=$2;
      shift
      breaksw
    case "-cl":
    case "-clean":
      set CLEAN_DVT=1
      breaksw
    case "-b":
    case "-basic":
      set DVT_BASIC=1
      breaksw
    case "-no":
    case "-norun":
      set RUN_DVT=0
      breaksw
    case "-h":
    case "-help":
      set HELP=1
      breaksw
    default:
      echo " ERROR: Invalid argument"
      breaksw
  endsw
  shift
end


##########################
# Print the help message #
##########################
if ( ! $?DVT_BASIC ) then
  if ($?HELP || ! $?PROJ_DIR) then
    printf "\ndvt_runme.csh - Script to launch DVT Eclipse\n"
    printf "                dvt_runme.csh (w/ no options creates a project in the current directory)\n"
    printf "                dvt_runme.csh [-project <dirname>] [-heap <size>|-clean|-norun]\n"
    printf " Options: \n"
    printf "           -help|-h             => print this help message\n"
    printf "           -project|-proj <dir> => specify a project directory\n"
    printf "           -heap <size>         => specify the Eclipse heap size, units are M or G (default is 1G)\n"
    printf "           -clean|-cl           => remove any previously existing workspace/project files\n"
    printf "           -basic|-b            => launch the basic DVT Eclipse GUI (no workspace, no project)\n"
    printf "           -norun|-no           => don't run DVT Eclipse\n"
    exit
  endif
endif


###################################
# Setup the Project and Workspace #
###################################
if ( ! $?DVT_BASIC ) then
  if ( ! -e $PROJ_DIR ) then
    echo "Directory ${PROJ_DIR} doesn't exist, creating it..."
    $MKDIR -p ${PROJ_DIR}
  endif
  set PROJECT_ROOT = `realpath $PROJ_DIR`
#  set WRKSPACE_ROOT = `dirname ${PROJECT_ROOT}`
  set WRKSPACE_ROOT = /tmp/${USER}/dvt_workspaces
  set PROJECT = `basename ${PROJECT_ROOT}`
  set WORKSPACE = ${WRKSPACE_ROOT}/${PROJECT_ROOT}/workspace-${PROJECT}
  printf "\n PROJECT      -> ${PROJECT}\n"
  printf " PROJECT_ROOT -> ${PROJECT_ROOT}\n"
  printf " WORKSPACE    -> ${WORKSPACE}\n\n"
endif


######################
# Clean argument set #
######################
if ( ! $?DVT_BASIC ) then
  if ($?CLEAN_DVT) then
    echo " Deleting previous workspace and project files\n"
    $RM -rf $WORKSPACE
    $RM -f ${PROJECT_ROOT}/.project
    $RM -f ${PROJECT_ROOT}/.dvt/build.config.xml
    $RM -f ${PROJECT_ROOT}/.dvt/dialog.snapshots.xml
  endif
endif


####################
# Scrub up our env #
####################
# Check and remove previous versions on the PATH
if ($?DVT_HOME) then
  modpath -d ${DVT_HOME}/bin
endif

if ($?VCS_HOME) then
  modpath -d ${VCS_HOME}/bin
endif

if ($?VCS_TARGET_ARCH) then
  unsetenv VCS_TARGET_ARCH
endif

if ($?VCS_LIB) then
  unsetenv VCS_LIB
endif

if ($?VERDI_HOME) then
  modpath -d ${VERDI_HOME}/bin
endif

# Add RTL_CAD_ROOT if it's not set
if (! $?RTL_CAD_ROOT) then
  setenv RTL_CAD_ROOT /p/hdk/rtl/cad/x86-64_linux26
endif


###################
# Setup the Tools #
###################

# Setup ACE
setenv ACE_HOME `ToolConfig.pl get_tool_path ace`

# Setup OVM
setenv OVM_HOME `ToolConfig.pl get_tool_path ovm`

# Setup UVM
setenv UVM_HOME  `ToolConfig.pl get_tool_path uvm`

# Setup XVM
#setenv XVM_HOME `ToolConfig.pl get_tool_path xvm`
setenv XVM_HOME  ${RTL_CAD_ROOT}/intel/xvm/${XVM_VERSION}

# Setup SAOLA
setenv SAOLA_HOME `ToolConfig.pl get_tool_path saola`

# Setup VCS
setenv VCS_HOME   `ToolConfig.pl get_tool_path vcsmx`
modpath -q -n 1 ${VCS_HOME}/bin

# Setup VERDI
setenv VERDI_HOME `ToolConfig.pl get_tool_path verdi3`
modpath -q -n 1 ${VERDI_HOME}/bin

# Setup Synopsys License
set SYNOPSYSVCS_LIC = /nfs/site/eda/group/cse/setups/synopsys/synopsysvcs.lic
if ( -f  ${SYNOPSYSVCS_LIC} ) then
  source ${SYNOPSYSVCS_LIC} >& /dev/null
else
  echo "Couldn't find synopsysvcs.lic at ${SYNOPSYSVCS_LIC}"
endif

#Setup SVA_LIB
setenv SVA_LIB_ROOT  /nfs/site/disks/hdk.cad.1/linux_2.6.16_x86-64/sva_lib/6.0p1/SVA_LIB

#############
# Setup DVT #
#############

# Machine detection - Eclipse 4 will not work on SLES10 machines!
$GREP -q 'VERSION = 10' /etc/SuSE-release >& /dev/null
if ($? == 0) then
    set ECLIPSE_VER = 38
    echo "\nThis is a SLES10 machine"
endif

if ($?ECLIPSE_VER) then
  echo "SLES10 Machine: Getting the latest Eclipse 3.8 version"
  set DVT_ECLIPSE_VER = `$LS -1 /p/com/eda/amiq/dvt | $GREP -i "\-e${ECLIPSE_VER}" | $SORT -g | $TAIL -1`

  # If the above doesn't get you the latest version because someone installed other versions into the /p/com/eda/amiq/dvt directory, try this:
  #set DVT_ECLIPSE_VER = `$FIND /p/com/eda/amiq/dvt -name "*.01.*" -exec basename {} \; | $GREP -i "\-e${ECLIPSE_VER}" | $SORT -g | $TAIL -1`
else
  set DVT_ECLIPSE_VER = `$LS -1 /p/com/eda/amiq/dvt | $SORT -g | $TAIL -1`

  # If the above doesn't get you the latest version because someone installed other versions into the /p/com/eda/amiq/dvt directory, try this:
  #set DVT_ECLIPSE_VER = `$FIND /p/com/eda/amiq/dvt -name "*.01.*" -exec basename {} \; | $SORT -g | $TAIL -1`
endif
setenv DVT_HOME /p/com/eda/amiq/dvt/${DVT_ECLIPSE_VER}
modpath -q -n 1 ${DVT_HOME}/bin
setenv DVT_LICENSE_FILE FLEXLM


###########################
# Print our tool versions #
###########################
echo "\n TOOL VERSIONS:"
echo " OVM_HOME    -> ${OVM_HOME}"
echo " UVM_HOME    -> ${UVM_HOME}"
echo " XVM_HOME    -> ${XVM_HOME}"
echo " SAOLA_HOME  -> ${SAOLA_HOME}"
echo " VCS_HOME    -> ${VCS_HOME}"
echo " VERDI_HOME  -> ${VERDI_HOME}"
echo "\n DVT SETUP:"
echo " DVT_HOME            -> ${DVT_HOME}"
echo " DVT_LICENSE_FILE    -> ${DVT_LICENSE_FILE}"
echo " DVTLMD_LICENSE_FILE -> ${DVTLMD_LICENSE_FILE}"
echo " LM_PROJECT          -> ${LM_PROJECT}"
echo " HEAP_SIZE           -> ${HEAP_SIZE}"

#################################
# Check for DVT_COMMON_SETTINGS #
#################################
if ( ! $?DVT_BASIC ) then
  if ( -e "$PROJ_DIR/common_prefs" ) then
    setenv DVT_COMMON_SETTINGS `realpath ${PROJ_DIR}/common_prefs`
    echo " DVT_COMMON_SETTINGS -> ${DVT_COMMON_SETTINGS}"
  endif
endif



###################
# Run DVT Eclipse #
###################
if ($RUN_DVT == "1") then

  if ( $?DVT_BASIC ) then

    # setup up the basic DVT Eclipse GUI and prompt for a workspace
    echo "\n Starting the basic DVT Eclipse GUI..."
    set COMMAND_LINE = "-heap_size ${HEAP_SIZE}"
    echo " Running dvt_run.sh ${COMMAND_LINE}"
    ${DVT_HOME}/bin/dvt_run.sh ${COMMAND_LINE} &
    exit

  else if ( -d $WORKSPACE ) then

    # workspace already exists so let's use it
    echo "\n Opening existing workspace..."
    set COMMAND_LINE = "-workspace ${WORKSPACE} -heap_size ${HEAP_SIZE}"
    echo " Running dvt_run.sh ${COMMAND_LINE}"
    ${DVT_HOME}/bin/dvt_run.sh ${COMMAND_LINE} &
    exit

  else

    # no workspace found, should we create the project or import it?
    if ( -f ./${PROJECT}/.project ) then
      # let's import it
      echo "\n Importing project ${PROJECT}..."
      set COMMAND_LINE = "-workspace ${WORKSPACE} importProject ${PROJECT_ROOT} -heap_size ${HEAP_SIZE}"
      echo " Running dvt_cli_run.sh ${COMMAND_LINE}"
    else
      # let's create it
      echo "\n Creating project ${PROJECT}..."
      set COMMAND_LINE = "-workspace ${WORKSPACE} createProject ${PROJECT_ROOT} -lang vlog -heap_size ${HEAP_SIZE}"
      echo " Running dvt_cli_run.sh ${COMMAND_LINE}"
    endif

    (${DVT_HOME}/bin/dvt_cli_run.sh ${COMMAND_LINE} >& /dev/null)

  endif
endif

