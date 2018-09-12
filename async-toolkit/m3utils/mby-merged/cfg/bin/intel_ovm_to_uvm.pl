eval 'exec perl -S $0 ${1+"$@"}'
if 0;
# -*- mode: cperl -*-
#----------------------------------------------------------------------
#   Copyright 2007-2010 Cadence Design Systems, Inc.
#   Copyright 2010 Synopsys Inc.
#   Copyright 2010 Mentor Graphics Corporation
#   All Rights Reserved Worldwide
#
#   Licensed under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in
#   compliance with the License.  You may obtain a copy of
#   the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in
#   writing, software distributed under the License is
#   distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#   CONDITIONS OF ANY KIND, either express or implied.  See
#   the License for the specific language governing
#   permissions and limitations under the License.
#----------------------------------------------------------------------

#use strict;
use warnings;
use Getopt::Long;
use File::Find;
use Cwd;
use Fcntl;
use File::Temp qw/tempfile tempdir/;
use File::Path;
use File::Copy;
use Data::Dumper;
use File::stat;
use Text::Balanced qw(extract_tagged);

# Tar module may not be available
if (eval {require Archive::Tar; 1;}) {
  $Tar_Pm = 1;
}

my @all_files=();
my %content=();
local $opt_marker="-*-";
local $opt_help;
local $opt_backup;
local $opt_write;
local $opt_all_text_files;
local $opt_tar_exec="tar";
local $opt_deprecated=1;
local $opt_exclude_filelist="";
local $opt_nested_class_on; #RahulM - assumes the files do no use class nesting.

# regexp to match sv files (can be overriden using --sv_ext)
local $opt_sv_ext="\.(s?vh?|inc)\$";
local $opt_ace_ext="\.(hdl|udf|pp)\$";

# regexp of mime types for files considered for a change with the --all option
local $text_file_mime_regexp="(text\/|application\/x-shellscript|regular file)";

# ignore pattern
local $file_ignore_pattern="(/(.hg|.git|INCA_libs|.daidir|.vdb|simv|csrc|DVEfiles)\/|[#~]\$|\.(zip|gz|bz2|orig|diff|patch)\$)";

my $VerID='-*- $Id: ovm-to-uvm10.pl,v d60c9fc172de 2010/10/13 14:58:52 accellera $ -*-';
my @Options=(
      ["help","this help screen"],
      ["top_dir=s","the top directory name containing the files to translate"],
      ["backup","if specified make a backup of all files handled by the script"],
      ["write","really write the changed files back to disk (by default it runs in dry mode)"],
      ["marker=s", "use the marker supplied instead of the default marker [$opt_marker]"],
      ["sv_ext=s","a regexp matching all sv files default:[$opt_sv_ext]"],
      ["ace_ext=s","a regexp matching all sv files default:[$opt_ace_ext]"],
      ["all_text_files","apply the simple \"o2u\" transformation to all files where the MIME-type of matches [$text_file_mime_regexp]"],
      ["tar_exec=s","the script assume a gtar compatible tar, if your gtar compatible tar is not avail point to it via --tar_exec"],
      ["deprecated","also replace still working but deprecated code"],
      ["exclude_filelist=s","specify a text file with names of files to skip completely (one file p/line):[$opt_exclude_filelist]"],
	  ["nested_class_on","Turn it on only if your files have nested classes"]
      );


if(!GetOptions(map ( @$_[0], @Options))) {
   ErrorMessage("Error during option parsing");
}
if (defined $opt_help) { PrintUsage(@Options); exit(1);}

if(!$opt_top_dir) { $opt_top_dir= getcwd;}

$opt_top_dir =~ s/\/$//;

$DUMMY='-' x 80;
NoteMessage("$DUMMY");
NoteMessage("$VerID");

NoteMessage("traversing directory [$opt_top_dir] to find files");
if (defined $opt_nested_class_on){print "Nesting class check enabled !\n" };
NoteMessage("-*- this script requires a gtar compatible tar to make backups -*-");

search_all_relevant_files($opt_top_dir);

#Parse exclude file list
my @exclude_files = ();
if ($opt_exclude_filelist ne "") {
        open(EXCFILE,$opt_exclude_filelist) || die("error opening exclude file $opt_exclude_filelist for read!");
	while (<EXCFILE>) {
	  chop($_);
	  print "EXCLUDE FILE  $_\n";
	   push @exclude_files, $_;
	}
        close(EXCFILE);
}


#
#
#
foreach my $file (@all_files) {
   if (check_exclude_file($file) eq 1) {
      print "skipping $file as it was found in the exclude file list $opt_exclude_filelist\n";
      next;
   }

    if($file =~ /${opt_sv_ext}/) {
        print "handling sv [$file]\n";
        $content{$file}=ReadFileAsText($file);
	
	if (($content{$file} =~ /extends\s+(.*?)(_ral_env|_ral_file|_ral_reg)\s*\;/)  && ($content{$file} !~ /ovm_sequence_utils/)) {
	    print "Preserving $file  as it is derived from sla_ral_* base class...\n\n";
            $content{$file}=handle_saola_xvm_types($content{$file},$file);	
	    
	    next;
	}
	if (($content{$file} =~ /extends\s+(.*?)(_tap_env|_scan_env|_tap_file|_tap_reg)\s*\;/)  && ($content{$file} !~ /ovm_sequence_utils/)) {
	    print "Preserving $file  as it is derived from sla_tap_* base class...\n\n";
            $content{$file}=handle_saola_xvm_types($content{$file},$file);	
	    
	    next;
	}
	if (($content{$file} =~ /extends\s+(.*?)(_stf_env|_stf_file|_stf_reg)\s*\;/)  && ($content{$file} !~ /ovm_sequence_utils/)) {
	    print "Preserving $file  as it is derived from sla_stf_* base class...\n\n";
            $content{$file}=handle_saola_xvm_types($content{$file},$file);	
	    
	    next;
	}



          $content{$file}=change_ovm_to_uvm($content{$file},$file);
          $content{$file}=change_uvm_to_uvm12($content{$file},$file);
          $content{$file}=change_ovm_phases($content{$file},$file);
          $content{$file}=handle_saola_xvm_types($content{$file},$file);	

	
    } elsif ($file =~ /${opt_ace_ext}/) {
        print "handling ace [$file]\n";
        $content{$file}=ReadFileAsText($file);
       #$content{$file}=simple_fix($content{$file});
       #only change ACE filenames containing "ovm" but do not modify pathnames
        $content{$file} =~ s/\/(.?)ovm(.?)\./{$1}uvm{$2}\./g;
        $content{$file} =~ s/\/(.?)OVM(.?)\./{$1}UVM{$2}\./g;
	if ($content{$file} =~ /SAOLA_HOME/) {
	  $content{$file} =~ s/\"\$ENV{SAOLA_HOME}\/verilog\",/\"\$ENV{SAOLA_HOME}\/verilog\", \"\$ENV{SAOLA_HOME}\/verilog_uvm\",/g;
	  $content{$file} =~ s/\"\$ENV{OVM_HOME}\/src\",/\"\$ENV{OVM_HOME}\/src\", \"\$ENV{UVM_HOME}\/src\",/g;
	}
    } elsif (defined($opt_all_text_files) && matches_mime_type($file)) {
        print "handling generic [$file]\n";
        $content{$file}=ReadFileAsText($file);
        $content{$file}=simple_fix($content{$file});
    } else {
        print "skipping $file ...\n";
    }
}

write_back_files(%content);

#check if the filename is in the exclude file list provided by user
sub check_exclude_file {
  my($file) = @_;
  foreach $f (@exclude_files)  {
      if ($file =~ /$f/) {
         return 1;
      }
  }
  return 0;
}


# determines if the file is one of requested mime types
sub matches_mime_type {
    my($file) = @_;
    my($mime)=qx{file -i $file};
    if($mime =~ /:\s+${text_file_mime_regexp}/) {
        return 1;
    } else {
        return 0;
    }
}

# writes back the contents of the files
sub write_back_files {
    my(%content)=@_;


# now write it back
    my $tempdir=tempdir(CLEANUP => 1);
    NoteMessage("writing file to new top_dir $tempdir");
    foreach $f (keys %content) {
        my($nfile)=$f;
        $nfile=~ s/^$opt_top_dir/$tempdir/;
        my ($volume,$directories,$file) = File::Spec->splitpath( $nfile );

        mkpath($directories, 0);

        open(OUT,">$nfile") || die("error opening $nfile fullfile:$f for write [$!]");
        print OUT $content{$f};
        close(OUT);
    }

    NoteMessage("the patch is avail as [ovm2uvm_$$.patch] for inspection");
    system("diff -r $opt_top_dir $tempdir > ovm2uvm_$$.patch");
    if($opt_backup) {
        NoteMessage("making backup of current files before writing back in [ovm2uvm_back_$$.tar.gz]");

        if ($Tar_Pm) {
          my $tar=Archive::Tar->new;
          $tar->add_files(keys(%content));
          $tar->write("ovm2uvm_back_$$.tar.gz",COMPRESS_GZIP);
        } else {
          my($fh,$fname) = tempfile();
          print $fh join("\n",keys(%content));
          system "$opt_tar_exec cf - -T $fname | gzip -9v > ovm2uvm_back_$$.tar.gz";
        }
    }

    if($opt_write) {
        NoteMessage("now writing back to changes (some files might be written back with changed filenames [*ovm* -> *uvm*]");

        foreach $f (keys %content) {
            my($nfile)=$f;
            $nfile=~ s/^$opt_top_dir//;
	    
	    #Remove path prefix from file name to determine ONLY the local file name and extension
	    #to prevent renaming a sub-directory that contains "ovm" in its name
            my ($toppath, $directories,$file) = File::Spec->splitpath( $nfile );
	    
	               # my($mod_nfile)=simple_fix($nfile);
	   my($mod_nfile)=simple_fix($file);
	   
            my($target)=$f;
            my($source)=$f;
            $source=~ s/^$opt_top_dir/$tempdir/;

            #if($mod_nfile ne $nfile) {
            if($mod_nfile ne $file) {
	    
                #WarnMessage("filename of [$f]=top_dir[$opt_top_dir][$nfile] got changed, its now [$opt_top_dir][$mod_nfile]");
                WarnMessage("filename of [$f] got changed, its now [$opt_top_dir][$directories][$mod_nfile]");
                unlink $f;
                #$target=$opt_top_dir . $mod_nfile;
		$target=$opt_top_dir . $directories . $file;
            }

            warn("target file $target is not writeable and in the way") if (-e $target && !(-w $target));
            #NoteMessage("moving [$source] to [$target]");
	    
	    $target =~ s/nebulon\/output\/uvm/nebulon\/output\/ovm/;  #Preserve the Nebulon"ovm" output directory name

            move($source,$target);
        }
    }
}

sub search_all_relevant_files {
    my ($dir) = @_;
    finddepth({ wanted => \&pattern, no_chdir => 1 }, $dir);
}


sub change_ovm_to_uvm {
    my($t,$fname) = @_;

    # FIX deprecated macros OVM_REPORT_*
    $t =~ s/\`OVM_REPORT_(WARNING|ERROR|FATAL)/sprintf("\`uvm_%s",lc($1))/ge;

    # FIX deprecated 'OVM_REPORT_INFO now needs a UVM_NONE in the end
    $t =~ s/\`OVM_REPORT_INFO(.*?)\)/\`uvm_info$1,UVM_MEDIUM)/g;

    # FIX somecomponent::global_stop_request() -> (global) global_stop_request()
	# FIX RahulM - add uvm_pkg in scope resolution to avoid conflict between OVM and UVM global functions
    $t =~ s/(\S+::|)global_stop_request/uvm_pkg::global_stop_request/g;

    # FIX instancehandle.global_stop_request() -> (global) global_stop_request()
   	# FIX RahulM - add uvm_pkg in scope resolution to avoid conflict between OVM and UVM global functions
	$t =~ s/(\S+\.)global_stop_request/uvm_pkg::global_stop_request/g;

 	 #FIX RahulM - scope resolve set_global_timeout
	 $t =~ s/(\S+::|)set_global_timeout/uvm_pkg::set_global_timeout/g;
   
	# FIX take remove arg2,arg3 from constructor ovm_sequence::new(,arg2,arg3)
    #Fixed JDF to handle ovm_sequence with parameters

	if(!defined $opt_nested_class_on){    
   		 while($t=~ /extends\s+ovm_sequence[;|\s+|#](.*?)endclass/s) {
        	($pre,$b,$post) =($`,$&, $');

       		 $b =~ s/super\.new\(([^,\n]+),(.*);/super.new($1); \/\/ NOTE: [$&]/;
	         $b =~ s/super\.new\((.*?),(.*);/super.new($1); \/\/ NOTE: [$&]/;
        	 $b =~ s/ovm_sequence/uvm_sequence/g;
	         $t = $pre . $b . $post;
    	 }
	}
#######################################################
#RahulM - Adding new fix for identifying nested classes
#RahulM-FIX for HSD id_1405170323

	else{
		#First check if multiple class declarations in `ifdef statments
		if ($t=~/`if[|n]def\s+.+?\n*class\s+(\w+).*?extends\s+ovm_sequence[;|\s+|#].*?`else\s+.*?\n*class\s+\1.*?extends\s+ovm_sequence[;|\s+|#]/s) {
			while($t=~ /extends\s+ovm_sequence[;|\s+|#](.*?)endclass/s) {
		        ($pre,$b,$post) =($`,$&, $');
 		      	 $b =~ s/super\.new\(([^,\n]+),(.*);/super.new($1); \/\/ NOTE: [$&]/;
 		      	 $b =~ s/super\.new\((.*?),(.*);/super.new($1); \/\/ NOTE: [$&]/;
   			     $b =~ s/ovm_sequence/uvm_sequence/g;
     		  	 $t = $pre . $b . $post;
    		}
  	    }

		else {
    		while($t =~ /[^\/\/]class\s+(.*?)\s+extends\s+ovm_sequence[;|\s+#]/) {
			   #print "\nEntered the OVM sequence class \n";
   			   ($pre,$b,$post) =($`,$&, $');
	 		   $nested_class=0;
		   	   my @flines = split /\n/, $post;
     		   foreach my $fileline (@flines) {
			   		#if inside nested class and endclass found, exit out of nested class
					if ($nested_class!=0 && $fileline =~/^\s*endclass/) {
						$nested_class-=1;
						#print "\nCame out of nested class\n";
					}

					#if in outermost class and endclass found, make changes and exit loop.
					elsif ($nested_class==0 && $fileline =~/^\s*?endclass/) {
						#print "\nDone : $fileline\n";
						$b=~s/ovm_sequence/uvm_sequence/g;
				 		$post=join("\n",@flines); 
				 		$t = $pre . $b . $post;
						last;
					}
					#Only if in outermost class and super.new found, make changes.
					if ($nested_class==0 && $fileline =~ s/super\.new\((.*?),(.*);/super.new($1); \/\/ NOTE: [$&]/) {
						#print "Found constructor !:";
					}
	
					#check for nested class and increment the nested class depth
        			if ($fileline =~ /[^\/\/](?<!\w)class\s+(.*?);/s) {
						$nested_class+=1;
						#print "\nEntered nested class: $1\n";
        			}
      			}
     
  			}
		}
	}
#######################################################################

    # FIX take remove arg2,arg3 from constructor ovm_sequence_item::new(,arg2,arg3)  within out-of-body/extern constructor 
    if ($t =~ /extends\s+uvm_sequence[;|\s+|#](.*?)|;/s) {
      my @lines = split /\n/, $t;
      my $classname = "";
      foreach my $fileline (@lines) {

        if ($fileline =~ /class\s+(.*?)\s+extends\s+uvm_sequence/s) {
         $classname = $1;
        }
      }    
      if (($t=~ /$classname\:\:new.*?super\.new.*?\;/s) && ($classname ne "")) {
        ($pre,$b,$post) =($`,$&, $');	
        $b =~ s/super\.new\(([^,\n]+),(.*);/super.new($1); \/\/ NOTE: [$&]/;
        $b =~ s/super\.new\((.*?),(.*);/super.new($1); \/\/ NOTE: [$&]/;
        $t = $pre . $b . $post;
      }
    }




    # FIX take remove arg2,arg3 from constructor ovm_sequence_item::new(,arg2,arg3)
    while($t=~ /extends\s+ovm_sequence_item\s*;.*?endclass/s) {
        ($pre,$b,$post) =($`,$&, $');

        $b =~ s/super\.new\(([^,\n]+),(.*);/super.new($1); \/\/ NOTE: [$&]/;
        $b =~ s/ovm_sequence_item/uvm_sequence_item/g;
        $t = $pre . $b . $post;
    }

    # FIX take remove arg2,arg3 from constructor ovm_sequence_item::new(,arg2,arg3)  within out-of-body/extern constructor       
    if ($t =~ /extends\s+uvm_sequence_item/s) { 
      my @lines = split /\n/, $t;
      my $classname = "";
      foreach my $fileline (@lines) {
        if ($fileline =~ /class\s+(.*)\s+extends\s+uvm_sequence_item/s) {
         $classname = $1;
        }
      }    
      if (($t=~ /$classname\:\:new.*?super\.new.*?\;/s) && ($classname ne "")) {
        ($pre,$b,$post) =($`,$&, $');	
        $b =~ s/super\.new\(([^,\n]+),(.*);/super.new($1); \/\/ NOTE: [$&]/;
        $t = $pre . $b . $post;
      }
    }


    # MARKER include of ovm private files
    $t =~ s/include\s+\"ovm_(?!macros).*?\.svh\".*/$& \/\/ $opt_marker FIXME include of ovm file other than ovm_macros.svh detected, you should move to an import based methodology/g;

    # FIX  `message(sev,(...)) -> uvm_info("FIXME","...",sev)
    $t =~ s|(?s)\`message\(([^,]+),\s*\(\s*\$psprintf\((.*?)\)\s*\)\s*\)|\`uvm_info("FIXME",\$sformatf($2),$1)|g;
    $t =~ s|(?s)\`message\(([^,]+),\s*\(\s*\$sformatf\((.*?)\)\s*\)\s*\)|\`uvm_info("FIXME",\$sformatf($2),$1)|g;
    $t =~ s|(?s)\`message\(([^,]+),\s*\((.*?)\)\s*\)|\`uvm_info("FIXME",\$sformatf($2),$1)|g;
    $t =~ s|(?s)\`message\(([^,]+),\s*(.*?)\)|\`uvm_info("FIXME",\$sformatf($2),$1)|g;

    # FIX ovm_factory::print() -> factory.print
    $t =~ s/ovm_factory::print\(\)/factory.print()/g;

    # FIX ovm_factory:: set_type_override  -> factory. set_type_override_by_name ()
    $t =~ s/ovm_factory::set_type_override_by_name/factory.set_type_override_by_name/g;
    $t =~ s/ovm_factory::set_type_override_by_type/factory.set_type_override_by_type/g;
    $t =~ s/ovm_factory::set_type_override\(/factory.set_type_override_by_name(/g;

    #FIX JDF    create_object  ->  create_object_by_name()
    #$t =~ s/ovm_factory::create_object/uvm_default_factory::create_object_by_name/g;
    $t =~ s/ovm_factory::create_object\(/uvm_coreservice_t::get().get_factory().create_object_by_name(/g;
    $t =~ s/\s+factory.create_object\(/uvm_coreservice_t::get().get_factory().create_object_by_name(/g;
    $t =~ s/\s+factory.create_object_by_name\(/uvm_coreservice_t::get().get_factory().create_object_by_name(/g;


    # FIX `dut_error(MSG) -> uvm_error
    $t =~ s/(?s)\`dut_error\(\((.*?)\)\s*\)/\`uvm_error(\"DUT\",\$psprintf($1))/g;
    $t =~ s/(?s)\`dut_error\((.*?)\)/\`uvm_error(\"DUT\",$1)/g;

    # FIX ovm_msg_detail(XX) -> uvm_report_enabled(MBY)
    $t =~ s/(?s)\`ovm_msg_detail\((.*?)\)/uvm_report_enabled($1)/g;

    # FIX set_global_verbosity(..) -> set_report_...
    $t =~ s/ovm_urm_report_server::set_global_verbosity\((.*)\);/uvm_pkg::uvm_top.set_report_verbosity_level_hier($1)/g;

    # MARKER _urm_
    $t =~ s/_urm_.*/$& \/\/ $opt_marker FIXME potential deprecated URM reference\n/g;

    # MARKER _avm_
    $t =~ s/_avm_.*/$& \/\/ $opt_marker FIXME potential deprecated AVM reference\n/g;

    # FIX+MARKER .ovm_enable_print_topology -> .enable_print_topology
    $t =~ s/\S+\.[ou]vm_enable_print_topology(.*)/uvm_top.enable_print_topology$1 \/\/ $opt_marker NOTE mapped from $& \n/g;
    $t =~ s/(?<!\.)[ou]vm_enable_print_topology(.*)/uvm_top.enable_print_topology$1 \/\/ $opt_marker NOTE mapped from $& \n/g;

    # FIX ovm_print_topology() -> uvm_top.print()
    $t =~ s/ovm_print_topology/uvm_top.print/g;

    # MARKER configure_ph -> end_of_elaboration
    $t =~ s/configure_ph.*/$& \/\/ $opt_marker FIXME potential usage of configure_ph, this should be mapped to end_of_elaboration_ph\n/g;

    # FIX ovm_reporter -> uvm_report_object
    $t =~ s/ovm_reporter/uvm_report_object/g;

    # FIX+MARKER ovm_template.svh is gone
    $t =~ s/\`include\s+\"ovm_templates\.svh\".*/\/\/ $opt_marker NOTE this doe not exist anymore in UVM $&\n/g;

    # FIX cdns_recording.svh has been replaced by package for uvm
    $t =~ s/\`include\s+\".*cdns_recording.*\".*/\/\/ $opt_marker NOTE please contact cadence support for an updated package $&\n/g;

    # MARKER OVM_FIELD_DATA is a private macro to the ovm library and deprecated, if its a must it might be replaced with M_UVM_FIELD_DATA
    $t =~ s/\`OVM_FIELD_DATA/\`M_UVM_FIELD_DATA/g;
    $t =~ s/M_UVM_FIELD_DATA.*/$&   \/\/ $opt_marker NOTE OVM_FIELD_DATA is a private macro to the ovm library and deprecated, in legacy code it might be replaced with M_UVM_FIELD_DATA\n/g;

    # FIX map raise|drop objection argument mix
    # first swap with 3 args
    $t =~ s/\.(drop|raise)_objection\(([^,;]+),([^,;]+),([^,;]+)\)\s*;/.$1\_objection($2,$4,$3);/g;

    # then swap those with two args
    $t =~ s/\.(drop|raise)_objection\(([^,;]+),([^,;]+)\)\s*;/.$1\_objection($2,,$3);/g;

    # FIX ovm_factory::print_all_overrides() -> factory.print()
    $t =~ s/ovm_factory::print_all_overrides\(\)/factory.print()/g;

    # FIX+MARKER seq_item_prod_if -> seq_item_port
    $t =~ s/seq_item_prod_if.*/$& \/\/ $opt_marker NOTE seq_item_prod_if has been replaced with seq_item_port \n/g;
    $t =~ s/seq_item_prod_if/seq_item_port/g;

    # FIX+MARKER seq_item_cons_if -> seq_item_export
    $t =~ s/seq_item_cons_if.*/$& \/\/ $opt_marker NOTE seq_item_cons_if has been replaced with seq_item_export \n/g;
    $t =~ s/seq_item_cons_if/seq_item_export/g;

    # FIX seq_item_port.connect_if -> seq_item_port.connect
    $t =~ s/(seq_item_port\.connect)_if/$1/g;

    # FIX ovm_threaded_component -> uvm_component
    $t =~ s/ovm_threaded_component/uvm_component/g;

    # FIX post_new->build, pre_run->start_of_simulation
    $t =~ s/post_new\s*\(\)/build()/g;
    $t =~ s/pre_run\s*\(\)/start_of_simulation()/g;

    # MARKER (import+export)_connections do not exist anymore and have beein mapped to connect()
    $t =~ s/(export|import)_connections.*/$& \/\/ $opt_marker NOTE $1 _connections has been deprecated and should be mapped into connect()/g;


    #replace legacy analysis_fifo instances with tlm_analysis_fifo
	#FIX - RahulM - Change only class name and not instance name.
    $t =~ s/(\s+)analysis_fifo(\s+#)/${1}tlm_analysis_fifo${2}/g; 

    # FIX the simple O -> U changes
    $t=simple_fix($t);
    $t;

}

sub change_uvm_to_uvm12 {
    my($t,$fname) = @_;
    no warnings "uninitialized";
    my($prefix)="automatic uvm_coreservice_t cs_=uvm_coreservice_t::get();\n";

    # FIX remove the protected keyword from phases
    $t =~ s/virtual\s+protected\s+(function|task)\s+(void\s+)?(((pre|post)_)?((reset|configure|main|shutdown)|run)_phase)/virtual $1 $2 $3/g;

    # FIX replace _global_reporter.get_report_server
    $t =~ s/_global_reporter\.get_report_server/uvm_report_server::get_server/g;

    # FIX replace _global_reporter.report_summarize
    $t =~ s/_global_reporter\.report_summarize\(\)\s*;/begin uvm_root r = uvm_root::get(); r.report_summarize(); end/g;

    # FIX replace _global_reporter.dump_report_state
    $t =~ s/_global_reporter\.dump_report_state\(\)\s*;/begin uvm_root r = uvm_root::get(); r.dump_report_state(); end/g;

    # FIX replace uvm_severity_type by uvm_severity
    $t =~ s/uvm_severity_type/uvm_severity/g if $opt_deprecated;

    # FIX extending uvm_report_server
    $t =~ s/extends\s+uvm_report_server/extends uvm_default_report_server/g;

    # FIX Mantis 4431 (starting_phase ==)
    $t =~ s/starting_phase\s*([!=]+)/get_starting_phase()$1/g;
    # FIX Mantis 4431 (starting_phase =)
    $t =~ s/starting_phase\s*=\s*(\w+)/set_starting_phase($1)/g;
    # FIX Mantis 4431 (starting_phase.)
    $prefix="uvm_phase phase_=get_starting_phase();\n";
    $t = coreservice_repl_fct($t,'starting_phase\.','phase_.',1,$prefix);

    # FIX Mantis 3472: set_config_*/get_config_* are deprecated
    # remove the clone=0 because that is the semantic now
    
    $t =~ s/([sg])(et_config_object\s+\([^\)]+),\s*0\s*\)/$1~XYZ~$2)/g if $opt_deprecated;
    $t =~ s/([sg]et_config_object\s+\(.*?)(\n)/$1 \/\/ $opt_marker semantic changed see mantis3472 (clone bit)$2/g if $opt_deprecated;
    $t =~ s/([sg])~XYZ~/$1/g if $opt_deprecated;

    $prefix="";
    foreach $o ('int','string','object') {
      #Remove spaces between set/get_config function and argument list - JDF
      $t =~ s/set_config_$o\s+\(/set_config_$o\(/g;
      $t =~ s/get_config_$o\s+\(/get_config_$o\(/g;
      
        $t = coreservice_repl_initial($t,"set_config_$o\\(","uvm_config_$o\::set(null, ",1,$prefix);
        $t = coreservice_repl_initial($t,"get_config_$o\\(","uvm_config_$o\::get(,\"\", ",1,$prefix);
        $t = coreservice_repl_fct($t,"set_config_$o\\(","uvm_config_$o\::set(this, ",1,$prefix);
        $t = coreservice_repl_fct($t,"get_config_$o\\(","uvm_config_$o\::get(this, \"\",",1,$prefix);
    }
    # NOTE set_config_object( ....., clone=1) => [sg]et_config_object() with the clone arg=1 (which is the default) doesnt map to the ::[sg]et

    # DISABLED reverse chained function calls
#    $prefix="uvm_coreservice_t cs_ = uvm_coreservice_t::get();\n";
#    $t = coreservice_repl_fct($t,'uvm_coreservice_t::get\(\)\.','cs_.',1,$prefix);
#    $t = coreservice_repl_initial($t,'uvm_coreservice_t::get\(\)\.','cs_.',1,$prefix);

    # FIX uvm_sequencer_utils -> uvm_component_utils
    $t =~ s/`uvm_sequencer_utils/`uvm_component_utils/g if $opt_deprecated;

    # FIX `uvm_sequence_utils_begin(foo, bar)       `uvm_object_utils_begin(foo)
    #         ...                               ->      ...
    #     `uvm_sequence_utils_end                   `uvm_object_utils_end
    #                                               `uvm_declare_p_sequencer(bar)
    $t =~ s/`uvm_sequence_utils_begin\s*\(\s*(\S+)\s*\,\s*(\S+)\s*\)(.*?)(\s+)`uvm_sequence_utils_end/`uvm_object_utils_begin($1)$3 $4`uvm_object_utils_end $4`uvm_declare_p_sequencer($2)/gs if $opt_deprecated;

    # FIX `uvm_sequence_utils(foo, bar)  -> `uvm_object_utils(foo)
    #                                       `uvm_declare_p_sequencer(bar)
    $t =~ s/(\s*)`uvm_sequence_utils\s*\(\s*(\S+)\s*\,\s*(\S+)\s*\)/$1`uvm_object_utils($2) $1`uvm_declare_p_sequencer($3)/g if $opt_deprecated;

    # MARKER sequence association seqr-seq needs an update
    
    #FIX JDF
    $t =~ s/`uvm_update_sequence_lib_and_item\((.*)\)/\/\/ $opt_marker association seqr-seq has changed `uvm_update_sequence_lib_and_item($1)/g if $opt_deprecated;
    $t =~ s/`uvm_update_sequence_lib/\/\/ $opt_marker association seqr-seq has changed `uvm_update_sequence_lib/g if $opt_deprecated;

    # FIX start_item new argument (sequencer)
    # FIX JDF
    
    $t =~ s/\.start_item\s*\(([^,\)]*),\n*([^\)]*)/\.start_item\($1, $2,  sequencer/g; #FIX for HSD id_1405170388

    $t =~ s/task(.*?)start_item\s*\(([^,\)]*),\n*([^\)]*)/task ${1}start_item\($2, $3, uvm_sequencer_base sequencer=null/g;
    
    
    # FIX ovm_report_global_server -> uvm_report_server AND global_report_server -> get_server()
    $t =~ s/ovm_report_global_server/uvm_report_server/g;
    $t =~ s/uvm_report_global_server/uvm_report_server/g;   #JDF
    
    $t =~ s/global_report_server/get_server()/g;
    
	 # FIX get_current_phase() -> m_current_phase
	 $t =~ s/get_current_phase\(\)/m_current_phase/g;

	#FIX member m_curr_phase -> m_current_phase 
	#FIX RahulM
	$t =~ s/m_curr_phase/m_current_phase/g;

	 # FIX ovm_test_top -> uvm_top.uvm_test_top
	 $t =~ s/ovm_test_top/uvm_top.uvm_test_top/g;

    # MARKER report_summarize() -> report
    $t;
}

sub coreservice_repl_fct {
  my ($text,$match,$repl,$depr,$prefix)=@_; #
  my($last)=0;
  my(@blks)=();
  my($q,$tf,$bd);

  return $text unless defined $depr;

  # Small perf optimization: before we go inside of the while loop, check if there is a match anywhere in the file
  return $text if ($text !~ /${match}/);

  # TODO: The pattern below can match words "task" or "function" inside of a comment block (or line)..

  while( $text =~ /((extern|virtual|protected|local|static)\s+)*((task|function)\s+[^;]+;)(.*?end\4)?/gsx ) {
    ($q,$tf,$bd,$pre,$post)=($1,$3,$5,$-[0],$+[0]);

    $q="" unless defined $q;

    next if($q =~ /extern/); # dont deal with extern decl
    next if($prefix ne "" && $bd =~ /\s*$prefix/); # dont insert again
    
    next if ($bd =~ /\.$match/);    ##Handle case of OVM set/get_config methods that were called on objects extending from ovm_component 

    # deal with the body
    if($bd =~ s{$match}{$repl}g ) {
      #print "found [$q|$tf|$bd]\n";
      
      #remove optional "clone" argument from original OVM set/get_config_object() call
#      $bd =~ s/uvm_config_object::set\((.*?),(.*?),(.*?),(.*?),(.*?)\)/uvm_config_object::set\($1,$2,$3,$4\)/g;
#      $bd =~ s/uvm_config_object::get\((.*?),(.*?),(.*?),(.*?),(.*?)\)/uvm_config_object::get\($1,$2,$3,$4\)/g;
      $bd =~ s/uvm_config_object::set\(([^;]*?),\s*?(0|1)\s*?\)/uvm_config_object::set\($1\)/gs;  #FIX-RahulM
      $bd =~ s/uvm_config_object::get\(([^;]*?),\s*?(0|1)\s*?\)/uvm_config_object::get\($1\)/gs;  #FIX-RahulM
      
	  push @blks,substr($text,$last,$pre-$last);
      push @blks,"$q$tf $prefix$bd";
      $last=$post;
    }
  
  }
  
  
  
#  foreach $f (@blks) {
#    if($f =~ /$match/) { print "LOOP failed with $match $repl [$f]" } ;
#  }

  # now recreate output
  push @blks,substr($text,$last);
  return join("",@blks);
}

sub coreservice_repl_initial {
  my ($text,$match,$repl,$depr,$prefix)=@_; #
  my($last)=0;
  my(@blks)=();
  my($bd);

  return $text unless defined $depr;

  while( $text =~ /initial\s+begin/gsx ) {
    ($pre,$post)=($-[0],$+[0]);
    $bd=substr($text,$pre+7);
    my (@set) = extract_tagged($bd, "begin", "end");
    $bd=$set[4];
    $post=length($text)-length($set[1]);

    $bd="" unless defined $bd;
    next if($prefix ne "" && $bd =~ /\s*$prefix/); # dont insert again

    # deal with the body
    if($bd =~ s{$match}{$repl}g ) {
      #remove optional "clone" argument from original OVM set/get_config_object() call
#      $bd =~ s/uvm_config_object::set\((.*?),(.*?),(.*?),(.*?),(.*?)\)/uvm_config_object::set\($1,$2,$3,$4\)/g;
#      $bd =~ s/uvm_config_object::get\((.*?),(.*?),(.*?),(.*?),(.*?)\)/uvm_config_object::get\($1,$2,$3,$4\)/g;
      $bd =~ s/uvm_config_object::set\(([^;]*?),\s*?(0|1)\s*?\)/uvm_config_object::set\($1\)/gs;  #FIX-RahulM
      $bd =~ s/uvm_config_object::get\(([^;]*?),\s*?(0|1)\s*?\)/uvm_config_object::get\($1\)/gs;  #FIX-RahulM


      push @blks,substr($text,$last,$pre-$last);
      push @blks,"initial begin $prefix$bd". "end";
      $last=$post;
    }
  }
  # now recreate output
  #push @blks,substr($text,$last);
    
  ##Handle calls to coreservice routines within module tasks/functions not inside initial begin/end code block
  
  while( $text =~ /module /gsx ) {
    ($pre,$post)=($-[0],$+[0]);
    $bd=substr($text,$pre);
    my (@set) = extract_tagged($bd, "module ", "initial ");
    $bd=$set[4];
    next unless defined $bd;
    
    
    $post=length($text)-length($set[1]);

    $bd="" unless defined $bd;
    next if($prefix ne "" && $bd =~ /\s*$prefix/); # dont insert again


    if ($bd =~ /initial\s+begin(.*?)${match}(.*?)end/) {  next; }

    # deal with the body
    if($bd =~ s{$match}{$repl}g ) {
      push @blks,substr($text,$last,$pre-$last);
      push @blks,"module $prefix$bd". "initial ";
      $last=$post;
    }
  }
  
  ##Handle calls to coreservice routines within tasks/functions but not inside initial begin/end code block within interface  
  while( $text =~ /interface /gsx ) {
  
    ($pre,$post)=($-[0],$+[0]);
    $bd=substr($text,$pre);
    
    $pretext = substr($text,0,$pre);    
   
    next if ($bd =~ /module|program|class (.*?)interface/);  #Do not convert if "interface" keyword is encountered within a module/program/class
    next if ($pretext =~ /module|program|class /);  #Do not convert if "interface" keyword was encountered within a module/program/class
    
    my (@set) = extract_tagged($bd, "interface ", "initial ");
    $bd=$set[4];
    next unless defined $bd;
    
    $post=length($text)-length($set[1]);

    $bd="" unless defined $bd;
    next if($prefix ne "" && $bd =~ /\s*$prefix/); # dont insert again

    if ($bd =~ /initial\s+begin(.*?)${match}(.*?)end/) {  next; }

    # deal with the body
    if($bd =~ s{$match}{$repl}g ) {
      push @blks,substr($text,$last,$pre-$last);
      push @blks,"interface $prefix$bd". "initial ";
      $last=$post;
    }
  }  

  ##Handle calls to coreservice routines within tasks/functions but not inside initial begin/end code block within program 
  while( $text =~ /program /gsx ) {
    ($pre,$post)=($-[0],$+[0]);
    $bd=substr($text,$pre);
    my (@set) = extract_tagged($bd, "program ", "initial ");
    $bd=$set[4];
    next unless defined $bd;
    
    $post=length($text)-length($set[1]);

    $bd="" unless defined $bd;
    next if($prefix ne "" && $bd =~ /\s*$prefix/); # dont insert again

    if ($bd =~ /initial\s+begin(.*?)${match}(.*?)end/) {  next; }

    # deal with the body
    if($bd =~ s{$match}{$repl}g ) {
      push @blks,substr($text,$last,$pre-$last);
      push @blks,"program $prefix$bd". "initial ";
      $last=$post;
    }
  }  

  
  # now recreate output
  push @blks,substr($text,$last);
  
 
  return join("",@blks);
}

#Added JDF
sub handle_saola_xvm_types {
    my($t,$fname) = @_;
     
    my $replace_xvm_code = "\n`ifdef XVM\n";
    $replace_xvm_code .= "   import ovm_pkg::*\;\n";
    $replace_xvm_code .= "   import xvm_pkg::*\;\n";
    $replace_xvm_code .= "   `include \"ovm_macros.svh\"\n";
    $replace_xvm_code .= "   `include \"sla_macros.svh\"\n";
    $replace_xvm_code .= "`endif\n";


    my $replace_runtest_code = "\n if (\$value\$plusargs(\"UVM_TESTNAME=\%s\", testname  ))\n";
    $replace_runtest_code   .= "\`ifndef XVM \n"; 
    $replace_runtest_code   .= "    uvm_pkg::run_test(testname)\;\n";
    $replace_runtest_code   .= "\`else\n"; 
    $replace_runtest_code   .= "    xvm_pkg::run_test(\"\", testname,"; 
    $replace_runtest_code   .= "   xvm::EOP_UVM)\;\n";
    $replace_runtest_code   .= "\`endif";
    
     
    $t =~ s/sla_/slu_/g;
    $t =~ s/`SLA_SM/`SLU_SM/g;
    $t =~ s/slu_pkg/sla_pkg/g;
    $t =~ s/slu_ral_env/sla_ral_env/g;    #Preserve sla_ral_env references
    $t =~ s/slu_ral_reg/sla_ral_reg/g;    #Preserve sla_ral_reg references
    $t =~ s/slu_ral_field/sla_ral_field/g;    #Preserve sla_ral_field references
    $t =~ s/slu_ral_file/sla_ral_file/g;    #Preserve sla_ral_file references
    $t =~ s/slu_tap_env/sla_scan_env/g;    #Preserve sla_scan_env references
    $t =~ s/slu_tap_env/sla_tap_env/g;    #Preserve sla_tap_env references
    $t =~ s/slu_tap_reg/sla_tap_reg/g;    #Preserve sla_tap_reg references
    $t =~ s/slu_tap_field/sla_tap_field/g;    #Preserve sla_tap_field references
    $t =~ s/slu_tap_file/sla_tap_file/g;    #Preserve sla_tap_file references
    $t =~ s/slu_stf_env/sla_stf_env/g;    #Preserve sla_stf_env references
    $t =~ s/slu_stf_reg/sla_stf_reg/g;    #Preserve sla_stf_reg references
    $t =~ s/slu_stf_field/sla_stf_field/g;    #Preserve sla_stf_field references
    $t =~ s/slu_stf_file/sla_stf_file/g;    #Preserve sla_stf_file references
    
    if ($t =~ /sla_ral_env\s+(.*?);/) {      #Revert uvm_config*::get  calls on pointers to sla_ral_env (such as within a sequence) to use OVM get_config* methods
        my $ralptr = $1;
        $t =~ s/${ralptr}\.uvm_config_int\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${ralptr}.get_config_int\($3,$4\)/g;    
        $t =~ s/${ralptr}\.uvm_config_string\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${ralptr}.get_config_string\($3,$4\)/g;    
        $t =~ s/${ralptr}\.uvm_config_object\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${ralptr}.get_config_object\($3,$4\)/g;      
    }
    if ($t =~ /sla_tap_env\s+(.*?);/) {      #Revert uvm_config*::get  calls on pointers to sla_tap_env (such as within a sequence) to use OVM get_config* methods
        my $tapptr = $1;
        $t =~ s/${tapptr}\.uvm_config_int\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${tapptr}.get_config_int\($3,$4\)/g;    
        $t =~ s/${tapptr}\.uvm_config_string\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${tapptr}.get_config_string\($3,$4\)/g;    
        $t =~ s/${tapptr}\.uvm_config_object\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${tapptr}.get_config_object\($3,$4\)/g;      
    }
    if ($t =~ /sla_stf_env\s+(.*?);/) {      #Revert uvm_config*::get  calls on pointers to sla_stf_env (such as within a sequence) to use OVM get_config* methods
        my $stfptr = $1;
        $t =~ s/${stfptr}\.uvm_config_int\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${stfptr}.get_config_int\($3,$4\)/g;    
        $t =~ s/${stfptr}\.uvm_config_string\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${stfptr}.get_config_string\($3,$4\)/g;    
        $t =~ s/${stfptr}\.uvm_config_object\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${stfptr}.get_config_object\($3,$4\)/g;      
    }
    if ($t =~ /sla_scan_env\s+(.*?);/) {      #Revert uvm_config*::get  calls on pointers to sla_scan_env (such as within a sequence) to use OVM get_config* methods
        my $scanptr = $1;
        $t =~ s/${scanptr}\.uvm_config_int\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${scanptr}.get_config_int\($3,$4\)/g;    
        $t =~ s/${scanptr}\.uvm_config_string\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${scanptr}.get_config_string\($3,$4\)/g;    
        $t =~ s/${scanptr}\.uvm_config_object\:\:get\((.*?),(.*?),(.*?),(.*?)\)/${scanptr}.get_config_object\($3,$4\)/g;      
    }
    
    if ($t =~ /extends slu_fuse/) {       #Update fuse get_config_int() calls
        $t =~ s/build\(/this\.build_phase\(/g;
	$t =~ s/uvm_phase phase/uvm_phase phase = null/g;
        $t =~ s/_env\.uvm_config_int\:\:get\(this/uvm_config_int\:\:get\(_env/g;
    }
    
   if ($t =~ /run_test/) {
   #  if ($t =~ /initial\s*begin\s*:(.*)\s*/)   {
   #    $t =~ s/initial\s*begin\s*:(.*)\s*/    string testname\;\n\n   initial begin : ${1}\n/;
   #  } else {
       $t =~ s/initial\s*begin/  string testname\;\n\n  initial begin\n/;
   #  }
   #  $t =~ s/initial(.*?)run_test/\n   string testname\;\n\n initial $(1)run_test/;
   }
   
   if($t =~ s/run_test\(\)\;/$replace_runtest_code/g) {
			$t =~ s/(import\s*)/$replace_xvm_code\n${1}/;
	}
    
    $t =~ s/\`include(.*?)slu_/\`include${1}sla_/g;   #Preserve  `include filenames containing "sla_"
    $t =~ s/sla_macros/slu_macros/g;
    
    $t =~ s/slu_fusegen_pkg/sla_fusegen_pkg/g;  #Preserve Nebulon generated fusegen package name
    #Add XVM ifdef code anywhere that sla_pkg is imported
    if ($t =~ /sla_pkg/) {
       $t =~ s/(import\s*sla_pkg)/$replace_xvm_code\n${1}/g;
    }
    
    #Ensure script doesn't modify include file path names from Nebulon with "ovm" in them
    $t =~ s/nebulon(.*?)uvm/nebulon${1}ovm/g;
    
    $t;

}


sub simple_fix {
    my($t)=@_;
    $t =~ s/ovm/uvm/g;
    $t =~ s/(?<!uvm_)tlm_[b_|analysis_fifo|event|fifo|get|nb|get|req]/uvm_$&/g;
    $t =~ s/OVM/UVM/g;
    $t =~ s/(?<!UVM_)TLM_/UVM_TLM_/g;
	#FIX RahulM - revert all filenames in "`include" statements to ovm
	$t =~ s/(include\s+\"\w*?)uvm(?!_macros)(\w*?\.svh\")/$1ovm$2/g;
	$t =~ s/(include\s+\"\w*?)UVM(\w*?\.svh\")/$1OVM$2/g;
	

    $t;
}

# change ovm phase names to uvm format by appending '_phase(uvm_phase phase)'
sub change_ovm_phases {
    my($t,$fname) = @_;

    # append '_phase(uvm_phase phase)' to all standard OVM phases except run(), which is handled in the next line
#    $t =~ s/(\W*function\s+.*(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\()/${1}_phase${3}uvm_phase phase/g;
#    $t =~ s/(\W*function\s+.*(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\;)/${1}_phase (uvm_phase phase)\;/g;   #FIX JDF
    $t =~ s/(\W*function\s+.*::(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\()/${1}_phase${3}uvm_phase phase/g;
    $t =~ s/(\W*function\s+.*::(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\;)/${1}_phase (uvm_phase phase)\;/g;   #FIX JDF / Kishor
    $t =~ s/(\W*function\s+.*\s+(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\()/${1}_phase${3}uvm_phase phase/g;
    $t =~ s/(\W*function\s+.*\s+(build|connect|end_of_elaboration|start_of_simulation|extract|check|report))(\s*\;)/${1}_phase (uvm_phase phase)\;/g;   #FIX JDF / Kishor




    # append '_phase(uvm_phase phase)' to the run() task
    $t =~ s/(\W*task\s+.*run)(\s*\()/${1}_phase ${2}uvm_phase phase/g;
    $t =~ s/(\W*task\s+.*run)(\s*)\;/${1}_phase ${2}(uvm_phase phase)\;/g;

    # change super.<phase>() to super.<phase>(uvm_phase phase)
    $t =~ s/(super.(build|connect|end_of_elaboration|start_of_simulation|extract|check|report|run))(\s*\()/${1}_phase${3}phase/g;

    # replace 'endtask/endfunction : <ovm phase name>' -> 'endtask/endfunction : <ovm phase name>_phase
    $t =~ s/endfunction\s*:\s*(build|connect|end_of_elaboration|start_of_simulation|extract|check|report)(\s+)/endfunction : ${1}_phase${2}/g;
    
    
    #$t =~ s/endtask\s*:\s*run(\s|\n)(.*?)/endtask : run_phase\n${2}/g;
    
    #avoid VCS mysterious endtask label parsing bug with run_phase  - temporary fix until root-caused 
    #$t =~  s/endtask\s*:\s*run(\s|\n)(.*?)/endtask\n${2}/g;
    $t =~ s/endtask\s*:\s*run(\s|\n)(.*?)/endtask\n${2}/g;
	 
    
    $t;
}

sub pattern {
    my($filename)= $File::Find::name;
    my($st) =stat($filename) || die "error: $filename: $!";

#    print "[$filename][$st]";
# print Dumper($filename);
#    print Dumper($st),"\n";

# NOTE directories are not handled (a directory ovm_bla has to be renamed manually)
    return 0 if (-d $filename);
    warn("file $filename is a link and may lead to strange results") if -l $filename;
    return 0 if $filename =~ /${file_ignore_pattern}/;
#    return unless $filename =~ /\.s?vh?$/;

#    replace_string($filename);
    push @all_files,$filename;
}

sub PrintUsage {
  my(@Options)=@_;


  while(<DATA>){
    print $_;
  }

  my($txt);
  print "supported transformations\n";

  $txt = ReadFileAsText($0);
  while($txt =~ /#\s+(FIX|MARKER|TODO)(.*?)\n/gs) {
    print "action:$1\t\tdescription: $2\n";
  }

  print "\n\noptions:\n\n";

  foreach $i (@Options) {
    printf("\t--%-20s\t\t@$i[1]\n",@$i[0]);
  }
}

sub ReadFileAsText {
  my($FILENAME)=@_;
  my(@FILES)=();
  my($FILE,$TEXT);
  local(*INFILE);

#  DebugMessage("1trying to read text-file [$FILENAME]");
  $TEXT="";
  foreach $FILE (glob($FILENAME)) {
#    DebugMessage("2trying to read text-file [$FILE]");
    open(INFILE,$FILE) || WarnMessage("can't open file [$FILE]");
    undef $/;
    $TEXT .= <INFILE>;
    $/ = "\n";
    close(INFILE);
  }
  return ($TEXT);
}

sub WarnMessage{
  my($Line)=@_;

  Message("warning",$Line);
}

sub ErrorMessage{
    my($Line)=@_;
    Message("error",$Line);
    die "$0  stopped\n";
}

sub NoteMessage{
    my($Line)=@_;

    Message("note",$Line);
}

sub Message {
  my($Level,$Line)=@_;

  print STDERR "$Level $Line\n";
}

__DATA__

This scripts walks through all files under the --top_dir hierarchy and makes modifications
so that the OVM code fragments are changed to their UVM counterparts. As it is based on perl/regexps rather
then a full parsing some of the replacements might be inaccurate or it might not find all occurences required
to change. however it is expected that ~90%+ of the changes required in a conversion are completed by the script.

standard usage model sv files only:

1. run the script with the --top_dir option

2. inspect the changes made in the produced *.patch file

3. enable automatic write-back by supplying --write to the command invocation

(under some circumstances filenames get changed (when they contain either *ovm* or *OVM*))

4. inspect the MARKERS


usage: ovm-to-uvm10.pl options* args*

example: ovm-to-uvm10.pl --top_dir /xyz/abc/src


all_text_files use model:

If the all_text_files use model is enabled using the --all_text_files switch all files recognized
as text files are additionally considered for replacements. The simple o-to-u replacements will be performed
in all files (textual + sv) while the SV/OVM specific changes will be only applied to files identified as SV.

example: example: ovm-to-uvm10.pl --top_dir /xyz/abc/src --all

