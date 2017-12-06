#!/usr/bin/env perl
use strict;
use warnings;
use diagnostics;
use Carp;
use Data::Dumper;
use Getopt::Long;
use Cwd 'abs_path';
use File::Basename;

#############################################################################
# Globals
my %options;
my $debug = 0;
my $cwd = `pwd`;
chomp($cwd);
my $repo_root;

my $SAOLA_HOME = "/p/hdk/rtl/cad/x86-64_linux30/intel/saola/v20150417p4";
my $NATURAL_DOCS = $SAOLA_HOME . "/tools/natural_docs";

#############################################################################
# Env vars

$ENV{MODEL_ROOT} = $cwd;
$ENV{ACE_ENV_ROOT} = $cwd;
$ENV{SAOLA_HOME} = $SAOLA_HOME;
$ENV{NATURAL_DOCS} = $NATURAL_DOCS;

#############################################################################
# Custom "die" routine

sub mydie {
    my ($error) = @_;
    $error = "ip_create.pl: $error";
    confess($error) if $debug;
    die($error);
}

#############################################################################

Getopt::Long::Configure( "no_ignore_case",
    "prefix_pattern=(--|-)" );

my @optionSpec = (
   'debug|d',
   'help|h',
   'noiosf',
   'nochassis',
   'ovm',
   'docs',
   'nebulon');

GetOptions( \%options, @optionSpec );
$debug = $options{debug} if ( defined $options{debug} );


usage() if ( defined $options{'help'} );

foreach my $ip_name (@ARGV) {
    my $ip_name_uc = uc($ip_name);
    my $ip_name_lc = lc($ip_name);

    mydie "File/dir $ip_name_uc already exists" if (-e $ip_name_uc);

    print "Creating IP for $ip_name_uc\n";
    print defined $options{ovm}       ? "  - Generating OVM version\n" : "  - Generating UVM version\n";
    print defined $options{noiosf}    ? "  - Removing IOSF\n"    : "  - Including IOSF\n";
    print defined $options{nochassis} ? "  - Removing Chassis\n" : "  - Including Chassis\n";

    my $repo = dirname(abs_path(__FILE__));
    print "Cloning template from $repo...\n";
    system("git clone --quiet $repo $ip_name_uc");
    print "Cloning complete.\n" if $debug;

    chdir $ip_name_uc;
    $repo_root = `pwd`;
    chomp($repo_root);
    system("cp  .git/config .git_config");
    system("rm -rf .git ./README.release_notes ./replace_cte.pl ./build_me.sh");

    process_tb();
    remove_chassis() if defined $options{'nochassis'};
    remove_iosf()    if defined $options{'noiosf'};

    my ( $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst ) = localtime time;
    $year += 1900;
    $mon  += 1;
    my $date = "$mday/$mon/$year";

    # Replace 'mby' strings in files with ip name
    print "Replacing strings in files...\n" if $debug;
    &replace_string( "mby", "$ip_name_lc", $ENV{USER}, $date );

    # Replace 'mby' strings in directories with ip name
    print "Replacing strings in directories...\n" if $debug;
    &replace_string_dir( "mby", "$ip_name_lc" );

    print "Initializing Git repo\n" if $debug;
    system("git init ; mv .git_config .git/config ; git add . ; git commit -a -m \"Initial $ip_name_uc repo\"");

    run_NaturalDocs() if defined $options{docs};
    run_Nebulon()     if defined $options{nebulon};

    print "IP $ip_name_uc creation completed.\n";

}


#############################################################################

sub special_file_processing {
    my ($filename, $pattern, $mode) = @_;
    my $newfilename   = $filename . ".chassis_free";
    my $no_print_flag = 0;

    open( SPECIAL_FILES, $filename )
      || die "Error: couldn't open file $filename\n";
    open( SPECIAL_FILES_OUT, ">", $newfilename )
      || die "Error: couldn't open file $newfilename\n";

    while (<SPECIAL_FILES>) {
        if (m/\/\/ START $pattern/) {
          if ( $mode == 0 ) {
             $no_print_flag = 1;
          } else {
             $no_print_flag = 0;
          }
        }
        elsif (m/\/\/ END $pattern/) {
            $no_print_flag = 0;
        }
        elsif (m/\/\/ ELSE $pattern/) {
          if ( $mode == 0 ) {
             $no_print_flag = 0;
          } else {
             $no_print_flag = 1;
          }
        }
        else {
            if ( $no_print_flag == 0 ) {
                print SPECIAL_FILES_OUT $_;
            }
        }
    }
    close(SPECIAL_FILES);
    close(SPECIAL_FILES_OUT);
    system("mv $newfilename $filename");
}

#############################################################################

sub replace_string_dir {
    my ($from_str, $to_str)  = @_;
    my @changed_dir = `find ./ -type d -name "*mby*"`;

    foreach my $ch_dir (@changed_dir) {
        print "dir = $ch_dir" if $debug;
        chomp $ch_dir;
        my $new_dir = $ch_dir;
        $new_dir =~ s/$from_str/$to_str/g;
        system("mv $ch_dir $new_dir");
    }
    # dbenita: ugly hack
    system("mv verif/tests/${to_str}_test_pack/${from_str}_alive_test verif/tests/${to_str}_test_pack/${to_str}_alive_test/");
 
    my @changed_files_name = `find ./ -type f -name "*mby*"`;

    foreach my $ch_file_name (@changed_files_name) {
        print "file name  = $ch_file_name" if $debug;
        chomp $ch_file_name;
        my $new_file_name = $ch_file_name;
        $new_file_name =~ s/$from_str/$to_str/g;
        system("mv $ch_file_name $new_file_name");
    }
}


#############################################################################

sub replace_string {
    my ($from_str, $to_str, $user_id, $data_id) = @_;

    my $from_str_uc = uc($from_str);
    my $to_str_uc   = uc($to_str);

    #print "$from_str  $to_str\n\n" if $debug;

    my @changed_files = `find ./ -type f`;

    foreach my $ch_file (@changed_files) {

        my $full_str = "";
        my $out_str;

        if ( $ch_file !~ /^\.\/subIP/ ) {

            chomp $ch_file;
            open( my $INFO, "$ch_file" )
              || mydie "couldn't open info file $ch_file";
            while (<$INFO>) {
                $out_str = $_;
                $out_str =~ s/$from_str/$to_str/g;
                $out_str =~ s/$from_str_uc/$to_str_uc/g;
                $out_str =~ s/geitan/$user_id/g;
                $out_str =~ s/20/11/2017/$data_id/g;
                $full_str .= $out_str;
            }

            close($INFO);

            open( my $OUT, ">$ch_file" )
              || mydie "couldn't open output file $ch_file\n";
            print $OUT $full_str;
            close($OUT);
        }
    }
}

#############################################################################

sub process_tb {

   my $uvm_present = (defined $options{ovm}) ? 0 : 1;

   &special_file_processing("./cfg/ace/HDL/verif/mby_tb.hdl","UVM_NOT_PRESENT",$uvm_present);
   &special_file_processing("./cfg/ace/HDL/verif/mby_fusegen.hdl","UVM_NOT_PRESENT",$uvm_present);
   &special_file_processing("./cfg/ace/HDL/verif/mby_env.hdl","UVM_NOT_PRESENT",$uvm_present);
   #&special_file_processing("./cfg/ace/mby.env","UVM_NOT_PRESENT",$uvm_present);
   &special_file_processing("./cfg/ace/mby_hdl.udf.uvm","UVM_NOT_PRESENT",$uvm_present);
   system("mv ./cfg/ace/mby_hdl.udf.uvm ./cfg/ace/mby_hdl.udf") if ($uvm_present);
   &special_file_processing("./cfg/ace/mby_local_ivars.udf.uvm","UVM_NOT_PRESENT",$uvm_present);
   system("mv ./cfg/ace/mby_local_ivars.udf.uvm ./cfg/ace/mby_local_ivars.udf") if ($uvm_present);
   &special_file_processing("./cfg/LocalToolData.pm","UVM_NOT_PRESENT",$uvm_present);

   if ( $uvm_present ) {
     #&special_file_processing("./verif/tests/mby_alive_test/mby_alive_test.sv.uvm","UVM_NOT_PRESENT",$uvm_present);
     system("mv ./verif/tests/mby_alive_test/mby_alive_test.sv.uvm ./verif/tests/mby_alive_test/mby_alive_test.sv");
     system("mv ./verif/lib/mby/ti/mby_ti_low.sv.uvm ./verif/lib/mby/ti/mby_ti_low.sv");
     system("mv ./verif/lib/mby/env/mby_env.sv.uvm ./verif/lib/mby/env/mby_env.sv");
   }
   if ( $uvm_present ) {
     my $cmd = "cd verif; $repo_root/cfg/bin/intel_ovm_to_uvm.pl --write";
     print "Running OVM to UVM conversion. Command: $cmd\n";
     system ($cmd);
   }
}

#############################################################################

sub remove_chassis {
    &special_file_processing( "./verif/lib/mby/env/mby_env.sv", "CHASSIS_NOT_PRESENT",0);
    &special_file_processing( "./verif/lib/mby/mby_env_pkg.sv", "CHASSIS_NOT_PRESENT",0);
    &special_file_processing( "./cfg/ace/mby_hdl.udf", "CHASSIS_NOT_PRESENT",0);
    &special_file_processing( "./verif/lib/mby/ti/mby_ti_high.sv", "CHASSIS_NOT_PRESENT",0);
    &special_file_processing( "./verif/lib/mby/ti/mby_ti_low.sv", "CHASSIS_NOT_PRESENT",0);
    &special_file_processing( "./verif/lib/mby/env/mby_base_env.sv", "CHASSIS_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_config.sv", "CHASSIS_NOT_PRESENT",0 );
    &special_file_processing( "./verif/tb/mby_tb.sv", "CHASSIS_NOT_PRESENT",0);
}

#############################################################################
sub remove_iosf {
    &special_file_processing( "./cfg/ace/mby_hdl.udf",     "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./src/mby/rtl/dummy_dut.v", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_env.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_base_env.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_config.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_env_monitor.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_types.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_ral_env.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/env/mby_sm_env.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/ti/mby_ti_high.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/ti/mby_ti_low.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/seqlib/mby_seqlib.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/lib/mby/mby_env_pkg.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/tb/mby_tb.sv", "IOSF_NOT_PRESENT",0 );
    &special_file_processing( "./verif/tests/mby_test_pack/mby_alive_test/mby_alive_test.svh", "IOSF_NOT_PRESENT",0 );
}

#############################################################################

sub run_NaturalDocs {
  print "Running NaturalDocs\n";
  my $cmd = "$NATURAL_DOCS/NaturalDocs -I verif/ -p .natural_docs_proj -o html doc/NaturalDocs -r";
  print "Command: $cmd\n";
  system ($cmd);
}

#############################################################################

sub run_Nebulon {
  print "Running Nebulon\n" if $debug;
}

#############################################################################
sub usage {
    print "Usage: ip_create.pl [options] <ip-name>...\n";
    print "  - For each <ip-name> specified, will create new IP in directory <ip-name>\n";
    print "    in current working directory\n\n";
    print "Options:\n";
    print "   -help | -h  : display this information\n";
    print "   -ovm        : generate OVM-based IP (default is UVM)\n";
    print "   -noiosf     : generate IP without IOSF\n";
    print "   -nochassis  : generate IP without chassis\n";
    print "   -docs       : run NaturalDocs\n";
    print "   -nebulon    : run Nebulon on RDL\n";
    print "   -debug | -d : turn on script debugging\n";
    exit(0);
}
#############################################################################

