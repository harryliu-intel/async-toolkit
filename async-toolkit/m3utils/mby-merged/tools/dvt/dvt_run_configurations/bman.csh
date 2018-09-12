#!/usr/intel/bin/tcsh -f

set env_lib=""
set rtl_lib=""
set test_lib=""
set full=""
set shdv_shared_lib=""
set tb_ti_lib=""
set model=""
set vcs_model=""
set vcs=""


#set model =  `cfg/dvt/dvt_run_configurations/return_build_cfg.pl`
#echo "model is $model"


echo "Command line is: $argv "


while ($#argv)
    switch ($argv[1])
    case -model:
        set model=$2;
        if (($model == "model")) then
        	echo "Please specify the Model to build!"
        	exit 0;
       	endif	
        if (($model == "mplex_np_nphy")) then  
           	set vcs_model=mby_mc 
           	#echo "setting vcs_model:  $vcs_model"
    	endif
        if (($model == "mby")) then  
           	set vcs_model=mby 	
           	#echo "setting vcs_model:  $vcs_model"
        endif   	
        echo "model is $model"
        # shift
        breaksw
   case -full:
        set full=full
        echo "Running full build"
        breaksw
   case -vcs:
        set vcs=vcs
        echo " Compiling only VCS libraries."
        breaksw     
#  Can be used in future to compile only specific libraries.
#    case -tb_env_lib:
#        set env_lib=`echo bman.mby.vcs.vcs_createlib_{$vcs_model}_env_lib,`
#        echo "env_lib is $env_lib"
#        breaksw
#    case -tb_test_lib:
#        set test_lib=`echo bman.mby.vcs.vcs_createlib_{$vcs_model}_test_lib,`
#        echo "test_lib is $test_lib"
#        breaksw
#    case -tb_rtl_lib:
#    	#TODO: Change this to use Mplex RTL.
#        set rtl_lib=`echo bman.mby.vcs.vcs_createlib_mby_rtl_lib,`
#        echo "rtl_lib is $rtl_lib"
#        breaksw     
#    case -shdv_shared_lib:
#        set shdv_shared_lib=`echo vcs.vcs_createlib_shdv_shared_lib,`
#        echo "shdv_shared_lib is $shdv_shared_lib"
#        breaksw 
#    case -tb_ti_lib:
#        set tb_ti_lib=`echo bman.mby.vcs.vcs_createlib_{$vcs_model}_ti_lib,`
#        echo "tb_ti_lib is $tb_ti_lib"
#        breaksw                
    case -sched:
        set sched="-sched $2"
        # echo "sched is $sched"
        shift
        breaksw
    case -stage:
        set stage=$2
        echo "stage is $stage"
        shift
        breaksw
    endsw
    shift
end



if(($full == "")&& ($vcs =="")) then
	echo "Please select either Full bman or VCS only build!"
	exit 0
endif
    

if ($full != "") then
   echo "Running Full bman"
      bman -mc $model $sched
   else
    bman -mc $model -s all +s $vcs $sched      
endif
    
    
#echo "Replacing dut.build"
#cp $MODEL_ROOT/target/vcs_4value/$dut/dvt$dut.build $MODEL_ROOT/.dvt/

