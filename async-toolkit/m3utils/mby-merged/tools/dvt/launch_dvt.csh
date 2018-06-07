#!/usr/intel/bin/tcsh

###########################
# Set default values here #
###########################
set DVT_VERSION=18.01.07-e46
set RUN_DVT=1
set BMAN=0
set HEAP_SIZE="4G"
set LOCAL="-sched local"

#################################
# Process Commandline Arguments #
#################################

while ( $#argv != 0 )
  switch ($argv[1])
    case "-dut":
    case "-d":
    set DUT=$2;
      shift
      breaksw
    case "-model":
    case "-m":
      set MODEL=$2;
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
    case "-bman":
    set BMAN=1
      breaksw
    case "-netbatch":
    case "-nb":
      set LOCAL=""
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
if ($?HELP) then
    printf "\nlaunch_dvt.csh - Script to launch DVT Eclipse\n"
    printf "                launch_dvt.csh -dut <dut> -model <model> [-heap <size>|-clean|-bman|-norun]\n"
    printf " Options: \n"
    printf "           -help|-h             => print this help message\n"
    printf "           -dut <dut>           => specify the Eclipse heap size, units are M or G (default is 1G)\n"
    printf "           -model <model>       => specify the Eclipse heap size, units are M or G (default is 1G)\n"
    printf "           -heap <size>         => specify the Eclipse heap size, units are M or G (default is 1G)\n"
    printf "           -clean|-cl           => remove any previously existing workspace/project files\n"
    printf "           -bman                => run bman to create dvt build files\n"
    printf "           -netbatch|nb         => run bman using Netbatch (default is -sched local)\n"
    printf "           -norun|-no           => don't run DVT Eclipse\n"
    exit
endif


#################
# Programs Used #
#################
set GREP = /bin/grep
set LS = /bin/ls
set SORT = /bin/sort
set TAIL = /usr/bin/tail
set MKDIR = /bin/mkdir
set CP = /bin/cp
set RM = /bin/rm
set FIND = /usr/bin/find


#set MODEL_ROOT = `git rev-parse --show-toplevel`  # this is already set by the clone script
set WORKSPACE = /tmp/${USER}/${MODEL_ROOT}/${DUT}-${MODEL}-dvt/dvt_workspace


######################
# Clean argument set #
######################
if ($?CLEAN_DVT && $RUN_DVT) then
    echo " Deleting previous workspace and project files\n"
    $RM -rf $WORKSPACE
    $RM -f ${MODEL_ROOT}/.project
    $RM -rf ${MODEL_ROOT}/.dvt
endif

source /p/com/env/psetup/prod/bin/setupTool dvt $DVT_VERSION 
source /nfs/site/eda/group/cse/setups/amiq/amiq.lic

if ($BMAN && $RUN_DVT) then
	bman -dut ${DUT} -ASSIGN -mc=${MODEL} -s all +s dvt ${LOCAL}
endif

if ($?CLEAN_DVT) then
	$MKDIR -p .dvt
	$CP -f target/$DUT/dvt/models/$MODEL/*.build .dvt/
    set COMMAND = "-eclipsespace /tmp/$USER/dvt_eclipsespace \
               -workspace $WORKSPACE \
               createProject $MODEL_ROOT \
               -lang vlog \
               -perspective VLOG \
               -exclude 'name=aceroot' \
               -force"
else
    set COMMAND = "-eclipsespace /tmp/$USER/dvt_eclipsespace \
                   -workspace $WORKSPACE"
endif

if ($RUN_DVT) then
	(${DVT_HOME}/bin/dvt_cli.sh ${COMMAND} >& /dev/null)
else
	printf "${DVT_HOME}/bin/dvt_cli.sh "
        echo ${COMMAND}
endif


