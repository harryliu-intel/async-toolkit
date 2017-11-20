#-*-perl-*-

package common::eclipse;
use strict;
use FileHandle;
use Carp;
use Switch;
require "dumpvar.pl";

use Utilities::ProgramTools qw(check_args test_eval deepcopy);
use Utilities::Print;

use Utilities::System qw(Exit Mkpath);
use Data::Dumper;
use File::Basename;


use vars qw(@ISA @EXPORT_OK);
use Ace::GenericScrag qw($CWD add_cmd);
use Ace::WorkModules::HDL;
use Intel::WorkWeek qw(:all);
use vars qw($hdl_spec %super);

require Exporter;
@ISA = qw(Exporter Ace::WorkModules::HDL Ace::GenericScrag );

use vars qw(%params);

#-------------------------------------------------------------------------------
# MEMBER Functions
#--------------------------------------------------------------------------------

sub new {
    my ($class, %args) = @_;
    check_args( \%args,
		-valid    => [qw(* -debug)],
		-require  => [qw()], # look at parent
	);
    my ($intelyear, $intelww) = intelww(localtime(time));
    $intelyear =~ s/.*(\d\d)$/$1/;

    my $self = $class->SUPER::new(%args);
    
    $self->{intel_ww}=sprintf("%02.2dww%02.2d", $intelyear, $intelww);
    if (defined $ENV{CLUSTER_COVERAGE_ROOT} && -d $ENV{CLUSTER_COVERAGE_ROOT} && -w $ENV{CLUSTER_COVERAGE_ROOT}) {
        $self->{cluster_coverage_dir}=$ENV{CLUSTER_COVERAGE_ROOT}."/".$ENV{ACE_PROJECT}."/".$self->{intel_ww};
    }
    if (defined $self->{_options}{Ace::UDF::get_base_scope()}{-model_compile_results_dir}) {
	$self->{_eclipseDir}   = "$self->{_options}{Ace::UDF::get_base_scope()}{-model_compile_results_dir}/eclipse";
	$self->{_vcsCovDir}   = "$self->{_options}{Ace::UDF::get_base_scope()}{-model_compile_results_dir}/sim_cov";

#VR
## $self->get_option(-results)
#VR
    }
    else {
	$self->{_eclipseDir}   = "$self->{_options}{Ace::UDF::get_base_scope()}{-results}/eclipse";
	$self->{_vcsCovDir}   = "$self->{_options}{Ace::UDF::get_base_scope()}{-results}/sim_cov";

    }
    $self->{_Dlibs} = [];
    $self->{_Rlibs} = {};
    return $self;

}
#--------------------------------------------------------------------------------
sub get_super_scope {
  my ($self) = @_;
  return $self->{_super_scope};
}
#-------------------------------------------------------------------------------
# Switch for the various MODELSIM scrags
#-------------------------------------------------------------------------------
sub create_scrag {
  my ($self, %args) = @_;
  $_ = $args{-name};
  SWITCH: {
    /^eclipse$/ && do {
	my $exectest_scrag;
	$exectest_scrag .= $self->_create_eclipse_scrag(%args);
	return $exectest_scrag;
    };
    $self->_unknown_ace_command_error($_);
  }
}
#-------------------------------------------------------------------------------

sub _create_eclipse_scrag {
  my ($self, %args) = @_;
  my $hdlspec = $self->{_udf_man}->get_udf_ref(-catagory=>"HDLSpec");
  my $cur_scope = $self->{_cur_scope};
  my $model =  $self->{_options}->{$cur_scope}->{-model};
  my $model_libs = $hdlspec->{_datahash}->{$cur_scope}->{models}->{$model}->{libs};
  my $base_scope = Ace::UDF::get_base_scope();
  #my $lint_tool = $self->{_options}->{$base_scope}->{-lint_tool};
  #my $default_lint_top = $hdlspec->{_datahash}->{$cur_scope}->{models}->{$cur_scope}->{top};
  #$default_lint_top =~ s/\S+\.(\S+)/$1/;
  #my $model_top = $hdlspec->{_datahash}->{$cur_scope}->{models}->{$model}->{top}; 
  #my $lint_top = exists $self->{_options}->{$base_scope}->{-lint_top} && 
  #    defined $self->{_options}->{$base_scope}->{-lint_top} && 
  #    $self->{_options}->{$base_scope}->{-lint_top} =~ /\S+/ ?
  #    $self->{_options}->{$base_scope}->{-lint_top} : $default_lint_top ? $default_lint_top : $model_top;
  #my $lint_goal = exists $self->{_options}->{$base_scope}->{-lint_goal} && defined $self->{_options}->{$base_scope}->{-lint_goal} &&
  #  $self->{_options}->{$base_scope}->{-lint_goal} =~ /\S+/ ? 
  #  join(' ' ,@{$self->{_options}->{$base_scope}->{-lint_goal}}) : "initial_rtl/initial_rtl_audit";
  #if(exists $self->{_options}->{$base_scope}->{-lint_goal}[1] && defined $self->{_options}->{$base_scope}->{-lint_goal}[1]){
  #    $lint_goal = substr($lint_goal,30);
  #}
  #my $lint_gui = $self->{_options}->{$base_scope}->{-lint_gui} == 1 ? 1 : undef;
  #my $lint_elab_only = $self->{_options}->{$base_scope}->{-lint_elab_only} == 1 ? 1 : undef;
  #my $lint_build_only = $self->{_options}->{$base_scope}->{-lint_build_only} == 1 ? 1 : undef;
  #my $lint_license_check = $self->{_options}->{$base_scope}->{-lint_license_check} == 1 ? 1 : undef;
  #my $lint_project_mode = $self->{_options}->{$base_scope}->{-lint_project_mode} == 1 ? 1 : undef;
  #my $lint_default_max_warnings = $self->{_options}->{$base_scope}->{-lint_max_warnings};
  #my $max_warn_ref = $self->{_cli}->{_ivars}->{_datahash}->{$base_scope}->{-lint_max_warnings};
  #my $lint_error_exit = $self->{_options}->{$base_scope}->{-lint_error_exit};
#  my $lint_opts = join(' ', @{$self->{_options}->{$base_scope}->{-lint_tool_opts}});
  #my $lint_opts;
  #foreach my $t_opts (@{$self->{_options}->{$base_scope}->{-lint_spyglass_opts}}) {
  #    if(! $lint_gui) {
# 	  $lint_opts .= $t_opts.' ';
#       } else {
# 	  $lint_opts .= $t_opts.' ' unless($t_opts =~ /Audit\-RTL/);
#       }
#   }
#   my $lint_global_opts = join(' ', @{$self->{_options}->{$base_scope}->{-lint_global_opts}});
#   my @Lint_ignore = @{$self->{_options}->{$base_scope}->{-lint_ignore_libs}};
#  my %lint_ignore = ();
#  foreach my $ignore_lib (@Lint_ignore) { $ignore_lib =~ s/\'//g; $lint_ignore{$ignore_lib} = 1; } 
#  my $lint_max_warnings = exists $max_warn_ref->{$lint_top} ? $max_warn_ref->{$lint_top} : $lint_default_max_warnings;

  my $demo = $self->{_options}{$base_scope}{-turn_demo_mode_on};
  my $cmd = "";
  my $modelRoot;
  my $modelRootStr;
  if ($ENV{ACE_PROJECT} eq 'sc'){
  	$modelRoot = $ENV{SCIP_ROOT};
	$modelRootStr = 'SCIP_ROOT';
  }elsif ( $ENV{ACE_PROJECT} eq 'pcu' ){
  	$modelRoot = $ENV{PCU_ROOT};
	$modelRootStr = 'PCU_ROOT';
  }elsif ( $ENV{ACE_PROJECT} eq 'TNGSC' ){
  	$modelRoot = $ENV{SC_ROOT};
	$modelRootStr = 'SC_ROOT';
  }elsif ( $ENV{ACE_PROJECT} eq 'sec' ){
  	$modelRoot = $ENV{SEC_ROOT};
	$modelRootStr = 'SEC_ROOT';
  }elsif ( $ENV{ACE_PWA_DIR} eq 'pssa' ){
  	$modelRoot = $ENV{SOC_ROOT}."/".$ENV{ACE_PWA_DIR};
	$modelRootStr = 'SOC_ROOT';
  }elsif ( $ENV{ACE_PROJECT} eq 'tlm' ){
  	$modelRoot = $ENV{TLM1_ROOT};
	$modelRootStr = 'TLM1_ROOT';
  }else {
  	$modelRoot = $ENV{SOC_ROOT};
	$modelRootStr = 'SOC_ROOT';
  }
  $args{modelRootStr} = $modelRootStr;
  $args{hdlspec} = $hdlspec;
  $args{model_libs} = $model_libs;
  $args{cur_scope} = $cur_scope;
  $args{model} = $model; 
  $args{base_scope} = $base_scope;
#   $args{lint_tool} = $lint_tool;
#   $args{lint_top} = $lint_top;  
#   $args{lint_ignore} = \%lint_ignore;
#   $args{lint_elab_only} = $lint_elab_only;
#   $args{lint_build_only} = $lint_build_only;
#   $args{lint_error_exit} = $lint_error_exit;
#   $args{lint_project_mode} = $lint_project_mode;
#   $args{lint_goal} = $lint_goal;
#   $args{lint_gui} = $lint_gui;
  return $self->_usage(%args) if ($self->{_options}->{$base_scope}->{-eclipse_help} == 1);
  $self->_create_res_dir(%args) unless($self->{_options}->{$base_scope}->{-eclipse_show_libs});
  my @Tlibs = ();
  push(@Tlibs, @$model_libs);
  $args{run_libs} = \@Tlibs;

  foreach my $lib (@Tlibs) {
      $self->_find_dependent_libs(%args, $lib);
      my $t_lib = pop @{$self->{_Dlibs}};
      @{$super{$t_lib}{dep_libs}} = @{$self->{_Dlibs}} if(scalar @{$self->{_Dlibs}});
      $super{$t_lib}{compiled} = 0;
      if(exists $hdlspec->{_datahash}->{$cur_scope}->{libs}->{$lib}->{-sub_libs}) {
	$super{$t_lib}{super_lib} = 1;
	$self->_find_sub_libs(%args, orig_lib => $lib, $lib);
      } else {
	push(@{$super{Run_libs}}, $lib);
      }
      if(!$self->{_options}->{$base_scope}->{-eclipse_show_libs}) {
	  $self->_create_mapfile(%args, $lib);
      }
      $self->{_Dlibs} = [];
  }
  if($self->{_options}->{$base_scope}->{-eclipse_show_libs}) {
    $self->_show_libs_structure(%args);
    print "\n$model MODEL LIBS:\n---------------\n".join('  ', @$model_libs)."\n\n" ;
    print "total ".scalar @$model_libs." libs\n";
    return;
  } 
  $args{model_libs} = $model_libs;
  $self->_create_filelist(%args);
  $self->_create_prj_file(%args);
  $self->_create_elab_mapfile(%args);
#  $self->_check_mapfiles(%args);
  $cmd = Ace::GenericScrag::chdir("$self->{_eclipseDir}");
  #########################
  ##Create Spglass Scrag
  #########################
  if(1) {
      # my $show_goals = $self->{_options}->{$base_scope}->{-lint_show_goals} ? 1 : 0;
      $cmd .= "$ENV{DVT_HOME}/bin/dvt_cli.sh " .
	  "createSVProject  $self->{_eclipseDir}/projects " .
	  "-f $self->{_eclipseDir}/project.f " .
	  "-workspace  $self->{_eclipseDir}/workspace " .
	  "-eclipsespace $self->{_eclipseDir}/eclipsespace";
      return $cmd;
  } 
#   else {
# #	  $cmd .= "echo \"\nIgnore Libs: $lint_ignore\n\"\n";
#       my $actual_libs = undef;
#       my %actChkr = ();
#       # Compilation stage
#       foreach my $lib (@{$super{Run_libs}}) {
# 	  next if($super{$lib}{compiled} == 1 || (exists $lint_ignore{$lib} && $lint_ignore{$lib}) ); # || $lib eq $top_lib 
# 	  $super{$lib}{compiled} = 1;
# 	  foreach my $dlib ( @{$super{$lib}{dep_libs}} ) {
# 	      next if($super{$dlib}{compiled} == 1 || (exists $lint_ignore{$dlib} && $lint_ignore{$dlib}));
# 	      $super{$dlib}{compiled} = 1;
# 	      my $dcmd = "";
# 	      if(exists $super{$dlib}{super_lib}) {
# 		  foreach my $subLib (@{$hdlspec->{_datahash}->{$cur_scope}->{libs}->{$lib}->{-sub_libs}}) {
# 		      $super{$subLib}{compiled} = 1;
# 		      if($self->_check_filelist(%args, filelist => "$super{sub_libs}{$subLib}{super_lib}_S_${subLib}_filelist.f")) {
# 			  $dcmd = "spyglass -batch -f $super{sub_libs}{$subLib}{super_lib}_S_${subLib}_filelist.f".
# 			      ((-e "$self->{_eclipseDir}/$lint_tool/$super{sub_libs}{$subLib}{super_lib}_map.f")?" -f $super{sub_libs}{$subLib}{super_lib}_map.f":"").		      
# 			      ((-e "${dlib}_map.f")?" -f ${dlib}_map.f":"").
# 			      ((-e "${subLib}_map.f")?" -f ${subLib}_map.f":"").
# 			      " -hdlin_translate_off_skip_text -lib $super{sub_libs}{$subLib}{super_lib} $super{sub_libs}{$subLib}{super_lib} -enable_precompile_vlog -work $super{sub_libs}{$subLib}{super_lib}".
# 			      " -wdir $super{sub_libs}{$subLib}{super_lib}_S_${subLib}_wdir -mixed -noelab -norules $lint_global_opts";
# 			  if(!$lint_elab_only) {
# 			      $cmd .= "$dcmd > /dev/null 2>&1\n";
# 			      #$cmd .= "checkrc \"$dcmd\"\n\n" if($lint_error_exit);
# 			      my $post_cmd = "$modelRoot/bin/spyglass_post_comp.pl -lib $super{sub_libs}{$subLib}{super_lib}_S_${subLib} -res_path $self->{_results}";
# 			      $cmd .= "$post_cmd\n";
# 			      $cmd .= "checkrc \"$post_cmd\"\n" if($lint_error_exit);
# 			  }
# 			  $actual_libs .= "$super{sub_libs}{$subLib}{super_lib}_S_${subLib} ";
# 		      }
# 		  }
# 		  last;
# 	      } else {
# 		  if($self->_check_filelist(%args, filelist => "${dlib}_filelist.f")) {
# 		      $dcmd = "spyglass -batch -f ${dlib}_filelist.f".
# 			  ((-e "$self->{_eclipseDir}/$lint_tool/${lib}_map.f")?" -f ${lib}_map.f":"").
# 			  ((-e "$self->{_eclipseDir}/$lint_tool/${dlib}_map.f")?" -f ${dlib}_map.f":"").
# 			  "  -hdlin_translate_off_skip_text -lib $dlib $dlib -enable_precompile_vlog -work $dlib -wdir ${dlib}_wdir -mixed -noelab -norules $lint_global_opts";
# 		      unless(exists $actChkr{$dlib}) {
# 			  $actual_libs .= "$dlib ";
# 			  $actChkr{$dlib} = 1;
# 		      }
# 		  }
# 	      }
# 	      if(!$lint_elab_only and $self->_check_filelist(%args, filelist => "${dlib}_filelist.f")) {
# 		  $cmd .= "$dcmd > /dev/null 2>&1\n";
# 		  #$cmd .= "checkrc \"$dcmd\"\n\n" if($lint_error_exit);
# 		  my $post_cmd = "$modelRoot/bin/spyglass_post_comp.pl -lib $dlib -res_path $self->{_results}";
# 		  $cmd .= "$post_cmd\n";
# 		  $cmd .= "checkrc \"$post_cmd\"\n" if($lint_error_exit);		  
# 	      }
# 	  }
# 	  my $lcmd = "";
# 	  if(exists $super{sub_libs}{$lib} && exists $super{sub_libs}{$lib}{super_lib} && 
# 	     defined $super{sub_libs}{$lib}{super_lib} && $self->_check_filelist(%args, filelist => "$super{sub_libs}{$lib}{super_lib}_S_${lib}_filelist.f") ) {
# 	      $lcmd = "spyglass -batch -f $super{sub_libs}{$lib}{super_lib}_S_${lib}_filelist.f".
# 		  ((-e "$self->{_eclipseDir}/$lint_tool/$super{sub_libs}{$lib}{super_lib}_map.f")?" -f $super{sub_libs}{$lib}{super_lib}_map.f":"").
# 		  "  -hdlin_translate_off_skip_text -lib $super{sub_libs}{$lib}{super_lib} $super{sub_libs}{$lib}{super_lib} -enable_precompile_vlog -work $super{sub_libs}{$lib}{super_lib}".
# 		  " -wdir $super{sub_libs}{$lib}{super_lib}_S_${lib}_wdir -mixed -noelab -norules $lint_global_opts";
# 	      $actual_libs .= "$super{sub_libs}{$lib}{super_lib}_S_${lib} ";
# 	  } else {
# 	      if($self->_check_filelist(%args, filelist => "${lib}_filelist.f")) {
# 		  $lcmd = "spyglass -batch -f ${lib}_filelist.f".
# 		      ((-e "$self->{_eclipseDir}/$lint_tool/${lib}_map.f")?" -f ${lib}_map.f ":" ").
# 		      "-noelab -norules  -hdlin_translate_off_skip_text -lib $lib $lib -enable_precompile_vlog -work $lib -wdir ${lib}_wdir -mixed $lint_global_opts";
# 		  unless(exists $actChkr{$lib}) {
# 		      $actual_libs .= "$lib ";
# 		      $actChkr{$lib} = 1;
# 		  }
# 	      }
# 	  }
# 	  if(!$lint_elab_only and (exists $super{sub_libs}{$lib}{super_lib} ? 
# 				   $self->_check_filelist(%args, filelist => "$super{sub_libs}{$lib}{super_lib}_S_${lib}_filelist.f") :
# 				   $self->_check_filelist(%args, filelist => "${lib}_filelist.f")) ) {
# 	      $cmd .= "$lcmd > /dev/null 2>&1\n";
# 	      #$cmd .= "checkrc \"$lcmd\"\n\n" if($lint_error_exit);
# 	      my $post_cmd = "$modelRoot/bin/spyglass_post_comp.pl -lib ".(exists $super{sub_libs}{$lib}{super_lib}?"$super{sub_libs}{$lib}{super_lib}_S_${lib}" :$lib )." -res_path $self->{_results}";
# 	      $cmd .= "$post_cmd\n";
# 	      $cmd .= "checkrc \"$post_cmd\"\n" if($lint_error_exit);
# 	  }
#       }
# #	  $cmd .= "$modelRoot/bin/spyglass_post_comp.pl -res_path $self->{_results} -dc -run_libs \"@{$super{Run_libs}}\"\n";
#       # Elaboration stage
#       $actual_libs .= "${lint_top}_elab " if(!$lint_build_only);
#       my @Map_files = `/bin/ls $self->{_eclipseDir}/$lint_tool/\*map.f`;
#       foreach my $f (@Map_files) {
# 	  $f =~ s/\S+\/(\S+)/$1/;
# 	  chomp($f);
# 	  $f = "-f $f";
#       }
#       #my $orig = $,;
#       # setting default separator to -f for @Map_files array
#       #$, = " -f ";
#       my $opts_files = "";
#       if(exists $self->{_options}->{$base_scope}->{-lint_opts_file} && defined $self->{_options}->{$base_scope}->{-lint_opts_file}) {
# 	  foreach my $file (@{$self->{_options}->{$base_scope}->{-lint_opts_file}}) {
# 	      $opts_files .= "-f $file ";
# 	  }
#       }
#       my $waive_files = "";
#       if(exists $self->{_options}->{$base_scope}->{-lint_waive_file} && defined $self->{_options}->{$base_scope}->{-lint_waive_file}) {
# 	  foreach my $file (@{$self->{_options}->{$base_scope}->{-lint_waive_file}}) {
# 	      $waive_files .= " -waiver $file ";
# 	  }
#       }
#       # -gui=sde -run
#       my $tcmd = "spyglass  ".($lint_gui ? "" : "-batch")." -f ${lint_top}_elab_map.f ".
# 	  $waive_files.
# 	  $opts_files.
# 	  "-relax_hdl_parsing ".
# 	  "-top $lint_top ".
# 	  " -hdlin_translate_off_skip_text -lib ${lint_top}_elab ${lint_top}_elab  -work ${lint_top}_elab -wdir ${lint_top}_elab_wdir $lint_opts -mixed -hdllibdu -sort -DEBUG=elaborator ";
#       #$, = $orig;
#       if(!$lint_build_only) {
# 	  $cmd .= "$tcmd > /dev/null 2>&1\n";
# 	  #$cmd .= "checkrc \"$tcmd\"\n\n"  if($lint_error_exit);
# 	  my $post_cmd = "$modelRoot/bin/spyglass_post_comp.pl -lib ${lint_top}_elab -res_path $self->{_results}";
# 	  $cmd .= "$post_cmd\n";
# 	  $cmd .= "checkrc \"$post_cmd\"\n" if($lint_error_exit);
#       }
#       my $postCmd = "$modelRoot/bin/spyglass_post_comp.pl -summary -comp_libs ".
# 	  ($lint_elab_only ? "${lint_top}_elab" : "\"$actual_libs\"").
# 	  " -top_module $lint_top -res_path $self->{_results}".
# 	  ($lint_build_only ? "" : " -max_warnings $lint_max_warnings");
#       $cmd .= "$postCmd\n";
#       $cmd .= "checkrc \"$postCmd\"\n";
#       return $cmd;
#   }
}
#-------------------------------------------------------------------------------

sub _check_target_lib {
  my $target_lib = pop(@_);
  my ($self, %args) = @_;
  my $hdlspec = $args{hdlspec};
  my $base_scope = Ace::UDF::get_base_scope();
  my $cur_scope = $self->{_cur_scope};
  my $model =  $args{model};
  my $model_libs = $hdlspec->{_datahash}->{$cur_scope}->{models}->{$model}->{libs};
  foreach my $lib (@{$model_libs}) {
    return 1 if($lib eq $target_lib);
  }
  die "Error: $target_lib isn\'t found inside $model model libs\n";
  return 0;
}
#-------------------------------------------------------------------------------

sub _create_res_dir {
  my ($self, %args) = @_;
  my $demo = $self->{_options}{$args{base_scope}}{-turn_demo_mode_on};
  my $quiet = $self->{_options}{$args{base_scope}}{-quiet};
  my $cmd;
  if( -d "$self->{_eclipseDir}") {
      if(!$demo ) {
	  aprint "===> Removing existing eclipse results\n";
	  $cmd = "rm -Rf $self->{_eclipseDir}";
	  Utilities::System::execute_cmd($cmd, -quiet=>$quiet); #, -demo=>$demo);
      }
  }
  unless(-d "$self->{_eclipseDir}") {  
      $cmd = "mkdir -p $self->{_eclipseDir};";
      $cmd .= "mkdir -p $self->{_eclipseDir}/workspace;";
      $cmd .= "mkdir -p $self->{_eclipseDir}/eclipsespace;";
      Utilities::System::execute_cmd($cmd, -quiet=>$quiet);
  }
}
#-------------------------------------------------------------------------------

sub _find_dependent_libs {
  my $target_lib = pop(@_);
  my ($self, %args) = @_;
  my $dependent_libs = $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$target_lib}->{-dependent_libs};
  $self->{_Rlibs}->{$target_lib} = 1;
  if(exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$target_lib}->{-dependent_libs} &&
    defined $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$target_lib}->{-dependent_libs} ) {
    foreach my $lib (@$dependent_libs) {
      if(exists $self->{_Rlibs}->{$lib} && $self->{_Rlibs}->{$lib} == 1) {
	print "Error: circular dependency between $target_lib and $lib\n";
	$self->{_Rlibs}->{$target_lib} = 0;
	foreach my $lib (@{$self->{_Dlibs}}) {
	  return if($lib eq $target_lib);
	}
	push (@{$self->{_Dlibs}}, $target_lib);
	return;
      }
      $self->_find_dependent_libs(%args, $lib);
    }
  }
  $self->{_Rlibs}->{$target_lib} = 0;
  foreach my $lib (@{$self->{_Dlibs}}) {
    return if($lib eq $target_lib);
  }
  push (@{$self->{_Dlibs}}, $target_lib);
}
#-------------------------------------------------------------------------------

sub _find_sub_libs {
    my $lib = pop(@_);
    my ($self, %args) = @_;    
    foreach my $subLib (@{$args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-sub_libs}}) {
	push(@{$super{Run_libs}}, $subLib);
	$super{sub_libs}{$subLib}{super_lib} = $args{orig_lib};
	if(exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$subLib}->{-sub_libs}) {
	    #$super{$subLib}{super_lib} = 1;
	    $self->_find_sub_libs(%args, $subLib);
	}
    }
}
#-------------------------------------------------------------------------------

sub _create_filelist {
  my ($self, %args) = @_;
  my $fh = new FileHandle;
  my $hdl_fh = new FileHandle;
  my @src_types = (
		   '-vlog_files',
		   '-sverilog_files',
		   '-vlog_incdirs',
		   '-vlog_lib_dirs',
		   '-vlog_lib_files',
		   '-vlog_opts',
		  );
  push (@src_types, '-vhdl_files', '-vcom_opts') if($args{lint_tool} eq "spyglass");
  foreach my $lib (@{$super{Run_libs}}) {
      my %Files = ();
    if(exists $super{sub_libs}{$lib}) {
      $fh->open("> $self->{_eclipseDir}/$super{sub_libs}{$lib}{super_lib}_S_${lib}_filelist.f") ||
	die "can\'t open  $self->{_eclipseDir}/$super{sub_libs}{$lib}{super_lib}_S_${lib}_filelist.f for writing. $!";
    } else {
      $fh->open("> $self->{_eclipseDir}/${lib}_filelist.f") ||
	die "can\'t open  $self->{_eclipseDir}/${lib}_filelist.f for writing. $!";
    }
    my $filelist = {};
    # hdl handling
    ###############
    # getting all the hdl specs
    my $Hdl_Spec = [];
    my $Hdl_Spec_lib = $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-hdl_spec};
    my $srcScope = defined $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-srcScope} ?
      $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-srcScope} : $args{cur_scope};
    my $scope = exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-srcScope} ? 
	$args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-srcScope} : $args{cur_scope};
    (@{$Hdl_Spec}) = (@{$Hdl_Spec_lib}) if(ref ($Hdl_Spec_lib) eq 'ARRAY' && scalar @{$Hdl_Spec_lib});
    if(defined @{$Hdl_Spec_lib} && scalar @{$Hdl_Spec_lib}) {
      foreach my $hdl_file (@{$Hdl_Spec_lib}) {
	push(@{$Hdl_Spec}, @{$self->_find_hdlspec(%args, srcScope => $scope , $hdl_file)} );
      }
      foreach my $hdl (@$Hdl_Spec) {
	my $fp_hdl = $self->find_file($hdl, $srcScope);
	$hdl_fh->open("< $fp_hdl") || die "can\'t open $fp_hdl for reading. $!";
	my @lines = $hdl_fh->getlines;
	my %hdl_print = ();
	$hdl_spec = {};
	eval("@lines");
	foreach my $src (@src_types) {
	  my $prefix;
	  switch ($src) {
	    case '-vlog_incdirs' {$prefix = "+incdir+"}
	    case '-vlog_lib_files' {$prefix = "-v "}
	    case '-vlog_lib_dirs' {$prefix = "-y "}
	    else  {$prefix = ""}
	  }
	  my @KO = ();
	  (@KO) = (ref $hdl_spec->{$src} ne "ARRAY") ? split(/\s+/,$hdl_spec->{$src}) : (@{$hdl_spec->{$src}});
	  foreach my $str (@KO) {
	    if($src ne "-vlog_opts" and $src ne "-vcom_opts") {
	      next if($str =~ /\S+\:\:nonsynth/ || $str =~ /\S+\:\:nonemu/);
	      unless(exists $Files{$src}{$str}) {
		my $find = undef;
		if($src eq "-vlog_files" || $src eq "-sverilog_files" || $src eq "-vhdl_files" || $src eq "-vlog_lib_files") {
		  $find = $self->find_file($str, $srcScope);
		} else {
		  $find = $self->find_dir($str, $srcScope);	
		}
		unless( exists $hdl_print{$fp_hdl}{$src} ) {
		  push(@{$filelist->{$src}}, "# ".$fp_hdl."\n" );
		  $hdl_print{$fp_hdl}{$src} = 1;
		}
		push(@{$filelist->{$src}} , $prefix.$find."\n");
		$Files{$src}{$str} = 1;
	      }
	    } else {
	      $str =~ s/\-novopt//g;
	      $str =~ s/\-assert svaext//g;
	      $str =~ s/\-libmap\s+\S+//g;
	      $str =~ s/\-\S*_*timescale=\S+//g;
	      $str =~ s/\-nc//g;
	      $str =~ s/\-lca//g;
	      $str =~ s/\-sv(erilog)*//g;
	      $str =~ s/\-xlrm//g;
	      $str =~ s/\+v2k//g;
	      $str =~ s/\-ntb_opts\s+\S+\s*//g;
	      $str =~ s/^\s+$//g;
	      $str =~ s/^\s+(.*\S)\s*/$1/;
	      $str =~ s/.?ntb_opts//;
	      $str =~ s/-dtm+pcs//;
	      $str =~ s/^-liblist//;
	      $str =~ s/^\w+$//;
	      $str =~ s/dtm\+pcs//;
	      $str =~ s/-mda//;
	      $str =~ s/-mdb//;
	      $str =~ s/-93// if($src eq '-vcom_opts');
	      $str =~ s/-\S\S+\s+\S+//g;
	      $str =~ s/-\S\S+=\w+//g;
	      $str =~ s/\+verilog2001ext\+\.v//;
	      $str =~ s/\+verilog2001ext\+inc//;
	      $str =~ s/-ignore//;
	      $str =~ s/\+define\+MCP_ON//;
              $str =~ s/\+(define|DEFINE)\+ASSERT_ON//;
	      unless(exists $Files{$src}{$str}) {
		push(@{$filelist->{$src}}, "$str\n");
		$Files{$src}{$str} = 1;
	      }
	    }
	  }
	}
	$hdl_fh->close;
      }
      
    }
    # udf handling
    ###############
    foreach my $src (@src_types) {
      my $prefix;
      switch ($src) {
	case '-vlog_incdirs' {$prefix = "+incdir+"}
	case '-vlog_lib_files' {$prefix = "-v "}
	case '-vlog_lib_dirs' {$prefix = "-y "}
	else  {$prefix = ""}
      }
      if(exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{$src} &&
	 ref $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{$src} eq 'ARRAY') {
	foreach my $str (@{$args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{$src}}) {
	  if($src ne "-vlog_opts" and $src ne "-vcom_opts") {
	    next if($str =~ /\S+\:\:nonsynth/ || $str =~ /\S+\:\:nonemu/);	
	    unless(exists $Files{$src}{$str}) {
	      if($src eq "-vlog_files" || $src eq "-sverilog_files" || $src eq "-vhdl_files" || $src eq "-vlog_lib_files") {
		push(@{$filelist->{$src}} , $prefix.$self->find_file($str, $srcScope)."\n" );
	      } else {
		push(@{$filelist->{$src}} , $prefix.$self->find_dir($str, $srcScope)."\n" );	
	      }
	      $Files{$src}{$str} = 1;
	    }
	  } else {
	    $str =~ s/\-novopt//g;
	    $str =~ s/\-assert svaext//g;
	    $str =~ s/\-libmap\s+\S+//g;
	    $str =~ s/\-\S*_*timescale=\S+//g;
	    $str =~ s/\-nc//g;
	    $str =~ s/\-lca//g;
	    $str =~ s/\-sv(erilog)*//g;
	    $str =~ s/\-xlrm//g;
	    $str =~ s/\+v2k//g;
	    $str =~ s/^\s+$//g;
	    $str =~ s/^\s+(.*\S)\s*/$1/;
	    $str =~ s/.?ntb_opts//;
	    $str =~ s/-dtm+pcs//;
	    $str =~ s/^-liblist//;
	    $str =~ s/^\w+$//;
	    $str =~ s/dtm\+pcs//;
	    $str =~ s/-mda//;
	    $str =~ s/-mdb//;
	    $str =~ s/-93// if($src eq '-vcom_opts');
	    $str =~ s/-\S\S+\s+\S+//g;
	    $str =~ s/-\S\S+=\w+//g;
	    $str =~ s/\+verilog2001ext\+\.v//;
	    $str =~ s/\+verilog2001ext\+inc//;
	    $str =~ s/-ignore//;
	    $str =~ s/\+define\+MCP_ON//;
            $str =~ s/\+(define|DEFINE)\+ASSERT_ON//;
	    unless(exists $Files{$src}{$str}) {
	      push(@{$filelist->{$src}}, "$str\n");
	      $Files{$src}{$str} = 1;
	    }
	  }
	}
      }
    }
    push(@{$filelist->{-vlog_opts}}, "-sverilog\n");
    push(@{$filelist->{-vlog_opts}}, "+define+SVA_OFF\n");
    push(@{$filelist->{-vlog_opts}}, "+define+functional\n");
    push(@{$filelist->{-vlog_opts}}, "+libext+.v+.sv\n");
		push(@{$filelist->{-vlog_opts}}, "+define+ASSERT_OFF\n");
#		push(@{$filelist->{-vlog_opts}}, "+define+FL_SYNTHESIS_ON\n");
		
    # printing to $lib_listfile.f
    #############################
    foreach my $src (@src_types) {
      if(exists $filelist->{$src}) {
	print $fh "#${src}\n" if($src ne "-sverilog_files");
	print $fh @{$filelist->{$src}};
      }
    }
    $fh->close;
  }
}
#-------------------------------------------------------------------------------

sub _find_hdlspec{
  my $hdl_spec = pop(@_);
  my ($self, %args) = @_;
  my $hdl_spec_list = [];
  my $hdl_fh = new FileHandle;
  my $fp_hdl_spec = $self->find_file($hdl_spec, $args{srcScope});
  $hdl_fh->open("< $fp_hdl_spec") || die "can\'t open $fp_hdl_spec for reading. $!";
  my @lines = $hdl_fh->getlines;
  eval("@lines");
  if(ref $hdl_spec eq "HASH" && exists $hdl_spec->{-hdl_spec} && defined $hdl_spec->{-hdl_spec} && scalar @{$hdl_spec->{-hdl_spec}}) {
    push(@{$hdl_spec_list}, @{$hdl_spec->{-hdl_spec}});
    foreach my $hdl (@{$hdl_spec->{-hdl_spec}}) {
      push(@{$hdl_spec_list}, @{$self->_find_hdlspec(%args, $hdl)} );
    }
  }
  return $hdl_spec_list;
}
#-------------------------------------------------------------------------------

sub _create_mapfile {
  my $lib = pop(@_);
  my ($self, %args) = @_;
  return if(-e "$self->{_eclipseDir}/${lib}_map.f");
  my $fh = new FileHandle;
  return if(
      (! exists $super{$lib}{dep_libs} && ref $super{$lib}{dep_libs} ne 'ARRAY') &&
      (! exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-sub_libs} && 
       ref $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-sub_libs} ne 'ARRAY')
      );
  $fh->open("> $self->{_eclipseDir}/${lib}_map.f") ||
      die "can't open  $self->{_eclipseDir}/${lib}_map.f for writing ?: $!";
  if(exists $super{$lib}{dep_libs} && ref $super{$lib}{dep_libs} eq 'ARRAY') {
      foreach my $dlib (@{$super{$lib}{dep_libs}}) {
	  #next if(exists $args{lint_ignore}->{$dlib} && $args{lint_ignore}->{$dlib});
	  print $fh "-lib $dlib $self->{_eclipseDir}/$dlib\n";
      }
  }
  if(exists $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-sub_libs} && 
     ref $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$lib}->{-sub_libs} eq 'ARRAY') {
      foreach my $subLib (keys %{$super{sub_libs}}) {
	  next if($super{sub_libs}{$subLib}{super_lib} ne $lib);
	  #next if(exists $args{lint_ignore}->{$subLib} && $args{lint_ignore}->{$subLib});
	  print $fh "-lib $subLib $self->{_eclipseDir}/$lib\n";
      }
      print $fh "-lib $lib $self->{_eclipseDir}/$lib\n";
  }
  ##AG print $fh "-lib SPY_DW_WORK ".$self->{_eclipseDir}."/".$args{lint_tool}."/SPY_DW_WORK\n";
  $fh->close;
}
#--------------------------------------------------------------------------------

sub _create_elab_mapfile {
  my ($self, %args) = @_;
  my $fh = new FileHandle;
  #my $lint_top = exists $args{lint_top} ? $args{lint_top} : "/////.......NO LINT TOP ......//////";
  my $top = $args{hdlspec}->{_datahash}->{$self->{_cur_scope}}->{models}->{$args{model}}->{top}; 
  $fh->open("> $self->{_eclipseDir}/${top}_elab_map.f") ||
    die "can\'t open  $self->{_eclipseDir}/${top}_elab_map.f for writing ?: $!";
  foreach my $rlib (@{$args{run_libs}}) {
    #next if(exists $args{lint_ignore}->{$rlib} && $args{lint_ignore}->{$rlib});
    print $fh "-lib $rlib $self->{_eclipseDir}/$rlib\n";
  }
  ##AG print $fh "-lib SPY_DW_WORK ".$self->{_eclipseDir}."/".$args{lint_tool}."/SPY_DW_WORK\n";
  $fh->close;
}
#--------------------------------------------------------------------------------

sub _show_libs_structure {
  my ($self, %args) = @_;
  my $libs_ref = $args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs};
  print "\n$args{cur_scope} libs structure:\n";
  print   "-----------------------\n";
  foreach my $lib (@{$args{model_libs}}) {
    #next if(exists $args{lint_ignore}->{$lib} && $args{lint_ignore}->{$lib});
    if(exists ${libs_ref}->{$lib}->{-preCompiled}) {
      next;
#      print "$lib   ".(exists ${libs_ref}->{$lib}->{-srcScope} ? ${libs_ref}->{$lib}->{-srcScope} : $args{cur_scope})." - preCompiled (not participate in $args{lint_tool})\n";
    } elsif(exists ${libs_ref}->{$lib}->{-sub_libs}) {
      print "$lib   ".(exists ${libs_ref}->{$lib}->{-srcScope} ? ${libs_ref}->{$lib}->{-srcScope} : $args{cur_scope})." - superLib\n";
      foreach my $subLib (@{${libs_ref}->{$lib}->{-sub_libs}}) {
	print "\t| $subLib   ".(exists ${libs_ref}->{$lib}->{-srcScope} ? ${libs_ref}->{$lib}->{-srcScope} : $args{cur_scope})." - subLib\n";
      }
    } else {
      print "$lib   ".(exists ${libs_ref}->{$lib}->{-srcScope} ? ${libs_ref}->{$lib}->{-srcScope} : $args{cur_scope})."\n";
    }
    if(exists ${libs_ref}->{$lib}->{-dependent_libs}) {
      foreach my $depLib (@{$super{$lib}{dep_libs}}) {
	print "\t> $depLib   ".(exists ${libs_ref}->{$lib}->{-srcScope} ? ${libs_ref}->{$lib}->{-srcScope} : $args{cur_scope})." - depLib\n";
      }
    }
  }
}
#--------------------------------------------------------------------------------

sub _check_mapfiles {
    my ($self, %args) = @_;
    my $fh = new FileHandle; 
    my $fh_new = new FileHandle; 
    my @Map_files = `/bin/ls $self->{_eclipseDir}/\*_map.f`;
    foreach my $map_file (@Map_files) {
	my ($mlib) = ($map_file =~ /(\S+)_map\.f/);
	$fh->open("< $map_file") || die "can\'t open $map_file for reading. $!";
	$fh_new->open("> ${mlib}_map.f.new") || die "can\'t open ${mlib}_map.f.new for writing. $!";
	while(my $line = $fh->getline) {
	    my ($lib) = ($line =~ /\-lib\s+(\S+)\s+\S+/);
#	    print $fh_new $line if($self->_check_filelist(%args, filelist => "${lib}_filelist.f") );
	    print $fh_new $line;
	}
	##AGprint $fh_new "-lib SPY_DW_WORK ".$self->{_eclipseDir}."/".$args{lint_tool}."/SPY_DW_WORK\n";
	$fh->close;
	$fh_new->close;
	`/bin/cp ${mlib}_map.f.new $map_file`;
    }
}
#--------------------------------------------------------------------------------

sub _check_filelist {
  my ($self, %args) = @_;
  my $fh = new FileHandle;
  my $res = 0; # 0 fail, 1 - pass
  if(-e "$self->{_eclipseDir}/$args{filelist}") {
    $fh->open("< $self->{_eclipseDir}/$args{filelist}") || die "can\'t open $args{filelist} for reading. $!";
    while(my $line = $fh->getline) {
      if($line =~ /\#\-vlog_files/ or $line =~ /\#\-vhdl_files/) {
	$res = 1;
	last;
      }
    }
  }
  return $res;
}

#--------------------------------------------------------------------------------

sub _create_prj_file {
  my ($self, %args) = @_;
  my $fh = new FileHandle;
  my $supLibs = {};
  my $stacked = {};
  my @Src_prj = ();
  foreach my $lib (@{$super{Run_libs}}) {
      if(exists $super{sub_libs}{$lib}) {
	  unless(exists $supLibs->{$super{sub_libs}{$lib}{super_lib}}) {
	      $supLibs->{$super{sub_libs}{$lib}{super_lib}} = 1;
	  }
	  next;
      }
      next unless($self->_check_filelist(%args, filelist => "${lib}_filelist.f"));
      if(exists $super{$lib}{dep_libs} && ref $super{$lib}{dep_libs} eq 'ARRAY') {
	  foreach my $dlib (@{$super{$lib}{dep_libs}}) {
	      #next if(exists $args{lint_ignore}->{$dlib} && $args{lint_ignore}->{$dlib});
	      if($self->_check_filelist(%args, filelist => "${dlib}_filelist.f")) {
		  unless(exists $stacked->{$dlib}) {
		      unshift(@Src_prj, "+dvt_init+VCS.vlogan\n-sverilog\n -work $dlib\n-f $self->{_eclipseDir}/${dlib}_filelist.f\n");
		      $stacked->{$dlib} = 1;
		  }
	      }
	  }
      }
      unless(exists $stacked->{$lib}) {
	  unshift(@Src_prj, "+dvt_init+VCS.vlogan\n-sverilog\n -work $lib\n-f $self->{_eclipseDir}/${lib}_filelist.f\n");
	  $stacked->{$lib} = 1;
      }
  }
  # handle super libs
  my $superLibs_map_f = "";
  foreach my $supLib (keys %{$supLibs}) {
      $superLibs_map_f .= "read_file -type sourcelist ${supLib}_map.f\n" if(-e "$self->{_eclipseDir}/${supLib}_map.f");
#      foreach my $runLib (@{$args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$supLib}->{-sub_libs}}) {
      push(@Src_prj, "+dvt_init+VCS.vlogan\n-sverilog\n -work $supLib");

      foreach my $runLib (@{$super{Run_libs}}) {
	  if(exists $super{sub_libs}{$runLib}{super_lib} && $super{sub_libs}{$runLib}{super_lib} eq $supLib) {
	      # foreach my $subLib (@{$args{hdlspec}->{_datahash}->{$args{cur_scope}}->{libs}->{$supLib}->{-sub_libs}}) {
	      #next if(exists $args{lint_ignore}->{$runLib} && $args{lint_ignore}->{$runLib});
	      if($self->_check_filelist(%args, filelist => "$super{sub_libs}{$runLib}{super_lib}_S_${runLib}_filelist.f")) {
		  push(@Src_prj, "-f $self->{_eclipseDir}/$super{sub_libs}{$runLib}{super_lib}_S_${runLib}_filelist.f");
	      } else {
#	print "DEBUG Error Can\'t find the sublib $runLib $super{sub_libs}{$runLib}{super_lib}_S_${runLib}_filelist.f file \n";
	      }
	  }
      }
  }
  # create top prj file
  my $waivers = "";
  my $prj_tool_opts = "";
  my $prj_build_opts;
  my $prj_spp_files = "";
#   if(exists $self->{_options}->{$args{base_scope}}->{-lint_waive_file} && 
#      defined $self->{_options}->{$args{base_scope}}->{-lint_waive_file}) {
#     foreach my $file (@{$self->{_options}->{$args{base_scope}}->{-lint_waive_file}}) {
#       $waivers .= "read_file -type waiver $file\n";
#     }
#   }
#   if(exists $self->{_options}->{$args{base_scope}}->{-lint_prj_build_opts} && 
#      defined $self->{_options}->{$args{base_scope}}->{-lint_prj_build_opts}) {
#     foreach my $opt (@{$self->{_options}->{$args{base_scope}}->{-lint_prj_build_opts}}) {
#       $prj_build_opts .= "$opt\n";	  
#     }
#   }
  
#   if(exists $self->{_options}->{$args{base_scope}}->{-lint_prj_tool_opts} && 
#      defined $self->{_options}->{$args{base_scope}}->{-lint_prj_tool_opts}) {
#     foreach my $opt (@{$self->{_options}->{$args{base_scope}}->{-lint_prj_tool_opts}}) {
#       # Atrenta bug - Audit rpt switch can't be located in the prj file when running gui
#       next if($args{lint_gui} && $opt =~ /set_option\s+report\s+{\s+Audit\s+}/);
#       $prj_tool_opts .= "$opt\n";	  
#     }
#   }
#   if(exists $self->{_options}->{$args{base_scope}}->{-lint_prj_spp_files} && 
#      defined $self->{_options}->{$args{base_scope}}->{-lint_prj_spp_files}) {
#       foreach my $spp (@{$self->{_options}->{$args{base_scope}}->{-lint_prj_spp_files}}) {
# 	  $prj_spp_files .= "read_file -type sourcelist $spp\n";	  
#       }
#   }
  
  my $prj_file = "$self->{_eclipseDir}/project.f";
  $fh->open("> $prj_file") ||
      die "can\'t open $prj_file for writing ?: $!"; 
  my $src_str = join("\n",@Src_prj);
  my $topPrj_str = 
      "+disable_ext\n" .
      "+dvt_init+VCS.vlogan\n" .
      "-sverilog\n" .
      "$ENV{OVM_HOME}/src/ovm_pkg.sv\n" .
      "+incdir+$ENV{OVM_HOME}/src\n\n" .
      "$src_str";
 #  my $topPrj_str = "read_file -type sourcelist $args{lint_top}_elab_map.f\n".
#       $superLibs_map_f.
#       ($args{lint_elab_only} ? "" : "${src_str}\n").
#       (
#        $args{lint_build_only} ? "" : 
#        "set_option top $args{lint_top}\n".
#        "set_option gen_block_options { ". $args{lint_top}. "} \n".
#        $waivers.
#        "set_option current_methodology \$env($args{modelRootStr})/tools/lint/spyglass/intel-methodology\n".
#        $prj_spp_files.
#         "create_report summary\n". #create report file 
#        "create_report moresimple\n".
#        "create_report moresimple_sevclass\n".
#        "create_report waiver\n"
#       ).
#       "set_option dw yes\n".
#       "set_option allow_module_override  yes\n".
#       "set_option hdlin_translate_off_skip_text yes\n".
#       "set_option hdlin_synthesis_off_skip_text yes\n".
#        "set_option enable_pgnetlist yes\n".
#        $prj_build_opts.
#       "set_option hdllibdu yes\n".
#       (
#        $args{lint_build_only} ? "set_option projectwdir pre_comp_wdir" :
#        "set_option projectwdir $args{lint_top}_elab_wdir\n".
#        $prj_tool_opts.
#        "current_goal $args{lint_goal}\n"
#       );
  
  print $fh $topPrj_str;  
  $fh->close;
}
#--------------------------------------------------------------------------------

sub _usage {
  my ($self, %args) = @_;
  print <<EOT;

USAGE: ace -lint [-lint_target_libs \"<liblist separated by space>\"] [-lint_top <top module for elab stage>] 
\t[-lint_waive_file \"<path to waiver file>\"] [-lint_opts_file \"<path to opts file>\"] [-lint_gui] | [-lint_show_libs] | [-lint_help]

     -lint_show_libs     - prints model libs structure
     -lint_error_exit    - exit when spyglass finish with non 0 value.
     -lint_top           - specify the top module, fundemental argument for elaboration stage.
     -lint_target_libs   - processing a specific libs list, instead of all model\'s libs
     -lint_tool          - specify the lint tool will be used spyglass or lintra (not supported yet)
     -lint_tool_opts     - option list for lint elaboration stage
     -lint_global_opts   - option list for lint compilation stage
     -lint_ignore_libs   - lib list which not be part of the lint process
     -lint_waive_file    - lint waiver file pointer for elaboration stage
     -lint_opts_file     - lint option file pointer for elaboration stage
     -lint_gui           - load gui for elaboration stage, default is batch mode
     -lint_help          - lint usage
     -lint_build_only    - lint execute build commands only 
     -lint_elab_only     - lint execute elaboration commands only
     -lint_license_check - checking spyglass license for each precompile job
     -lint_max_warnings  - lint max allowed elaboration warnings

Examples:
\t1) ace -lint -lint_top psf -lint_gui
\t2) ace -lint -lint_top psf -lint_target_libs \"psf_rtl SBR_rtl lowpwrss_rtl secur_rtl_lib\"
\t3) ace -lint -lint_show_libs
EOT
return undef;
}
#--------------------------------------------------------------------------------

1;
