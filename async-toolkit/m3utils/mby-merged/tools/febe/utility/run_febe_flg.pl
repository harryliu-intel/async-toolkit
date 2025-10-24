#!/usr/intel/bin/perl
use strict;
use Getopt::Long	;
my $test;
my $model;
my $help;
my $dut;
my $recipe;
my $logfile_name;
my $get_log_path_command;

my $block;

GetOptions(
           "h!"             => \$help		,
           "t=s"	        => \$test		,
#           "model=s"	    => \$model		,
           "lintra_exe_scrag_recipe=s"    => \$recipe	,
           "dut=s"          => \$dut            ,
);

if ( $help ) {

    help(); exit(0);


};

if ( $test =~ /fmc_top/ ) {
$model = "rtl_top_only";}
elsif ( $test ){
my @string_split = split (/_/, $test);
$block = $string_split[0];
$model = "rtl_$block";}

print "LAYHOCK EDIT : $block $model $test\n";

my $command = "cp $ENV{MODEL_ROOT}/target/cwv/lintra/lintra_lib/15.3p102_shOpt64/lintra.map_rtl_fmc_top_lintra $ENV{MODEL_ROOT}/target/cwv/lintra/lintra_lib/15.3p102_shOpt64/lintra.map_$model && cd $ENV{MODEL_ROOT}/target/$dut/lintra/ && mkdir -p flg_tests && cd flg_tests && export LINTRA_LINTRA_ARGS=\"--disable_persistency --stage_limit lira_unreachable_code_elimination --config_file $ENV{MODEL_ROOT}/target/$dut/aceroot/results/DC/LintraFEBE.xml\";   trex $test -dut $dut  -model $model -no_compress -save -ace_args -model_compile_results_dir $ENV{MODEL_ROOT}/target/$dut/lintra -tool_debug -ace_args- -facet -lintra_mode lint -facet- -ace_args -static_check -lira_use_filter Synthesis -lintra_exe_scrag_recipe $recipe -ace_args-" ;

print "RUN_FEBE_FLG : $command \n ";

$logfile_name = "$ENV{MODEL_ROOT}/target/$dut/lintra/flg_tests/$test*/$test.log";

print "LINTRA FLG RUN LOG : Latest $logfile_name\n";

$get_log_path_command = "ls -ltr $logfile_name | tail -1 | awk '{print \$NF}'";

print "GET FLG RUN LOG : Run:  $get_log_path_command\n ";


if ( system ($command) ){
 exit(1);
};

sub help {

print << "END_OF_HELP" 


Usage: $0 <-model model_name> <-dut dut_name> < -t test_name > [ -lintra_exe_scrag_recipe /path/to/file ] 

-model <model name>     ; Name of the model. Sets \$MODEL and passes it down to acerun
-dut <dut name>         : Tells TRex which RTL DUT to run the test under.
-t   <test_name>        : Test Name 
-lintra_exe_scrag_recipe: Path the Lintra Recipe ( Post processing the results )

Note: When $0 is used under febe lintra_elab stage , "-t test_name" and "-lintra_exe_scrag_recipe recipe_path" 
      are automatically added by febe. This only -model nad -dut are used
`
      If you run $0 stand alone you must supply -t <test_name>

      The full command line outcome can be seen at : 
         \$MODEL_ROOT/target/<dut>/log/*lintra_elab.log
         
         and will look something like:

         run_febe_flg.pl -model pqm_rtl -dut hif  -t hif_xfib_lintra_flg -lintra_exe_scrag_recipe /tmp/hif_xfib_28343_06022018_130845_t1AjeQ9NLz.lintra.recipe

END_OF_HELP

};

