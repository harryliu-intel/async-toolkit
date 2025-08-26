#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use File::Spec::Functions qw/:ALL/;
use Data::Dump qw(dump);
use FindBin;
use Cwd qw/abs_path/;

my ($cast_path, $spar_dir, $gls_dir, $cell, $env, $cosim, @cast_defines);
my ($beh, $fpga_path, @c2v_args, @vcs_args, $kdb, $sdf, $perf, @power, $axi, $nolint, $help, $nodebug);
my $width = 300;
my $mem = '16G';
my $reset_duration = '10ns';
GetOptions("cast-path=s" => \$cast_path,
           "spar-dir=s"  => \$spar_dir,
           "gls-dir=s"   => \$gls_dir,
           "cell=s"      => \$cell,
           "env=s"       => \$env,
           "cosim=s"     => \$cosim,
           "width=i"     => \$width,
           "fpga-path=s" => \$fpga_path,
           "define=s"    => \@cast_defines,
           "c2v-args=s"  => \@c2v_args,
           "vcs-args=s"  => \@vcs_args,
           "mem=s"       => \$mem,
           "beh!"        => \$beh,
           "kdb!"        => \$kdb,
           "no-debug!"   => \$nodebug,
           "sdf=s"       => \$sdf,
           "perf!"       => \$perf,
           "power=s"     => \@power,
           "axi!"        => \$axi,
           "no-lint!"    => \$nolint,
           "help!"       => \$help) || pod2usage(2);

pod2usage(-verbose => 1) if $help;

if (defined($cell) and defined($env) and !defined($cosim)) {
    $cosim = "$cell\{verilog.gate\}:$env" unless $cosim
}
pod2usage("Either --cosim, or --cell and --env must be specified") unless $cosim;

my @netlists = map { abs_path($_) } @ARGV;
pod2usage("No Verilog specified") unless @netlists or $beh;

if (!defined($cast_path)) {
    my $hw_dir = catdir($FindBin::Bin, updir(), updir(), updir());
    $cast_path = join(':', abs_path(catdir($hw_dir, 'cast')),
                           abs_path(catdir($hw_dir, 'spec')));
}

my $flist = "testbench.f";

my $cmd = ("cast2verilog --cast-path=\Q$cast_path\E " .
           "--register-width=$width " .
           "--max-heap-size=$mem --generate-testbench --enable-orphan-bdc --ifdef " .
           "--structure-declarations=structs.txt " .
           "--behavior-report=beh.rpt " .
           "--cell=\Q$cosim\E " .
           "--file-list=$flist " .
           join('', map { "--define=\Q$_\E " } @cast_defines) .
           join('', map { "\Q$_\E " } @c2v_args) .
           "--output-file=testbench.v 2> testbench.err");

system($cmd) == 0 || die "Error executing cast2verilog: $!";

#   
sub get_spar_from_verilog_string {
  my ($vlog_file) = @_;
  my @vlog_split = split(/\//, $vlog_file);
  shift @vlog_split; #Remove GLS DIR
  my $subtype = pop @vlog_split;
  $subtype =~ s/\.vg//;
  my $fqcn = join('.', @vlog_split, $subtype);
  $fqcn = `echo '$fqcn' | fulcrum rename --type=cell --from=cast --to=mw 2>/dev/null`;
  $fqcn =~ s/\n//; #chomp($fqcn); doesnt work
  my $cell_name =  pop @vlog_split;
  $cell_name = `echo '$cell_name' | fulcrum rename --type=cell --from=cast --to=cadence 2>/dev/null`;
  $cell_name =~ s/\n//; #chomp($cell_name); doesnt work
  push @vlog_split, $cell_name;
  unshift @vlog_split, $spar_dir;
  return join('/', @vlog_split), $fqcn;
}

sub get_spar_for_macro {
  my ($macro) = @_;
  my @macro_split = split(/_D_/, $macro);
  return "" if (scalar @macro_split <= 1); 
  my $subtype = pop @macro_split;
  my $cell_name =  pop @macro_split;
  $cell_name= `echo '$cell_name' | fulcrum rename --type=cell --from=mw --to=cadence 2>/dev/null`;
  $cell_name =~ s/\n//; #chomp($cell_name); doesnt work
  push @macro_split, $cell_name;
  unshift @macro_split, $spar_dir;
  return join('/', @macro_split);
}

sub get_verilog_path_for_macro {
  my ($macro) = @_;
  my @macro_split = split(/_D_/, $macro);
  return "" if (scalar @macro_split <= 1); 
  my $subtype = pop @macro_split;
  my $cell_name =  pop @macro_split;
  $cell_name= `echo '$cell_name' | fulcrum rename --type=cell --from=mw --to=cast 2>/dev/null`;
  $cell_name =~ s/\n//; #chomp($cell_name); doesnt work
  push @macro_split, $cell_name;
  push @macro_split, $subtype.".vg";
  unshift @macro_split, "\$GLS_DIR";
  return "-v ".join('/', @macro_split);
}

sub get_macros {
  my ($file) = @_;
  die "ERROR: can't read $file\n" if (!-e $file);
  my $grep_output = qx{grep -v '\\/\\/' $file | grep '_D_' $file};
  return split(/\n/,$grep_output);
}

sub get_verilog_paths_for_macros {
  my ($vlog_file) = @_;
  $vlog_file =~ s/^\s*-v\s*//;
  my ($spar_macro_dir,$file_prefix) = get_spar_from_verilog_string($vlog_file);
  my $macro_list = "$spar_macro_dir/$file_prefix.macro.list";
  return "" if (!-e $macro_list);
  my @queue = get_macros($macro_list);
  my @verilog_paths;
  while (@queue) {
    my $macro = pop @queue;
    my $verilog_path = get_verilog_path_for_macro($macro);
    if ($verilog_path ne "") {
      push @verilog_paths, $verilog_path;
      $spar_macro_dir =  get_spar_for_macro($macro);
      $macro_list = "$spar_macro_dir/$macro.macro.list";
      push @queue, get_macros($macro_list) if (-e $macro_list);
    }
  }
  return join('\n', @verilog_paths);
}

# escape special characters in the filelist
my $content;
if (open(my $lh, $flist)) {
  local $/;
  $content = <$lh>;
  close $lh;
  open($lh, '>', $flist);
  foreach my $line (split(/\n/, $content)) {
    chomp($line);
    print $lh "$line\n";
    my $verilog_path = get_verilog_paths_for_macros($line);
    print $lh $verilog_path."\n" if ($verilog_path ne "");
  }
  close $lh;
}

if ($perf) {
  unshift @vcs_args, "-f \"\$CAST2VERILOG_RUNTIME/perf.vcfg\"";
}

if (@power) {
  #iterate over @power
  my $power_mon = "power_mons.sv";
  open my $fh_power, ">$power_mon" || die "Can't open $power_mon: $!";
  print $fh_power "module power_mons ();\n\n";
  foreach my $power (@power) {
    #split $power into name and path
    my ($power_name, $power_path) = split(/:/, $power);
    print $fh_power "power_mon #(\"$power_name\") mon_$power_name($power_path);\n"
  }
  print $fh_power "\nendmodule\n";
  close $fh_power;
  push @vcs_args, $power_mon;
  push @vcs_args, "\"\$CAST2VERILOG_RUNTIME/power_mon.sv\"";
}

my $instdir = $ENV{'FULCRUM_PACKAGE_ROOT'};
my $runtime = "$instdir/share/cast2verilog";

my $vcfg = $beh ? 'beh.vcfg' : 'fpga.vcfg';

my @defines = ();

push @defines, "+define+FPGA_HIER_PATH=.\Q$fpga_path\E." if $fpga_path;

my @args = ();
if (-s $flist) {
  push @args, '-f', $flist;
}

my @sdf_args = ();
my @crit_args = ();
if (defined($gls_dir)) {
  if ($sdf) {
    push @args, '-f', '$CAST2VERILOG_RUNTIME/sdf.vcfg';

    #Get SDF file corresponding to every netlist in the filelist
    open(my $lh, $flist) || die "Can't open $flist: $!\n";
    while (my $vlog_file = <$lh>) {
      chomp $vlog_file;
      $vlog_file =~ s/^\s*-v\s*//;
      my ($spar_macro_dir,$file_prefix) = get_spar_from_verilog_string($vlog_file);
      my $sdf_dir = "$spar_macro_dir/vcs";
      #Split vlog_file to get design FQN
      my $sdf_file = (glob "$sdf_dir/$file_prefix*$sdf*.sdf.gz")[0];
      #Check if glob returned any files
      if (!-e $sdf_file) {
        print "WARNING: No SDF file found for $vlog_file in directory $sdf_dir\n";
        next;
      }
      #Get block name directly from SDF file
      my $block = `zgrep '(DESIGN ' $sdf_file`;
      $block = substr((split(/ /, substr($block, 1, -2)))[1], 1, -1);
      my $arg = "max:$block:$sdf_file";
      push @sdf_args, "-sdf $arg";
      #If performance is enabled, also include the history monitors
      if ($perf) {
        my $bind_file = "$sdf_dir/${block}.bind_hist_mon.v";
        if (!-e $bind_file) {
          print "WARNING: No history monitors found for $block in directory $sdf_dir\n";
          next;
        }
        push @sdf_args, "$bind_file";
        my $conn_file = "$sdf_dir/${block}.bd_conn.rpt";
        if (!-e $conn_file) {
          print "WARNING: No connectivity file found for $block in directory $sdf_dir\n";
          next;
        }
        push @crit_args, "$conn_file";
      }
    }
    close $lh;

    #foreach my $sdf (@sdf) {
    #    my $arg = "max:$block:$sdf";
    #    push(@sdf_args, ("-sdf $arg"));
    #    print $fh_tcl "run_workarounds $block $sdf_dir/${block}_${minmax}\n";
    #    if (open(my $fh1, '<', "$sdf_dir/$block.$minmax.bind_notifiers.sv")) {
    #        while (<$fh1>) {
    #            chomp;
    #            $binds{$_} = 1;
    #        }
    #        close $fh1;
    #    }
    #}

  } else {
    push @args, '-f', '$CAST2VERILOG_RUNTIME/gls.vcfg';
  }
  push @args, '-f', '$GLS_DIR/gls.vcfg';
}
else { $gls_dir=""; }

push @vcs_args, '-kdb' if $kdb;
push @vcs_args, '-debug_access+all', '-debug_region+cell', '+vcs+initreg+random' unless $nodebug;
push @vcs_args, '+lint=PCWM +lint=TFIPC-L' unless $nolint;

#If using AXI VIP
if ($axi) {
    push @vcs_args, '+vpi -LDFLAGS "-L ${QUESTA_MVC_GCC_LIB} -Wl,-V,-rpath ${QUESTA_MVC_GCC_LIB} " -laxi4_IN_SystemVerilog_VCS_full_DVC -cpp /usr/intel/pkgs/gcc/6.2.0/bin/g++';
    #append to top of flist
    open (my $fh_flist, "<$flist") || die "Can't open $flist: $!\n";
    my @content = <$fh_flist>;
    close $fh_flist;
    open ($fh_flist, ">$flist") || die "Can't open $flist: $!\n";
    #These lines go first
    print $fh_flist "\$MENTOR_VIP_AE/common/questa_mvc_svapi.svh\n";
    print $fh_flist "\$MENTOR_VIP_AE/axi4/bfm/mgc_common_axi4.sv\n";
    print $fh_flist @content;
    close $fh_flist;
}

#Create the scripts to run vcs and verdi commands
my $runvcs   = "run-vcs.sh";
my $runverdi = "run-verdi.sh";
my $runcrit = "run-crit.sh";

open my $fh_vcs, ">$runvcs" || die "Can't open $runvcs: $!";
open my $fh_verdi, ">$runverdi" || die "Can't open $runverdi: $!";
open my $fh_crit, ">$runcrit" || die "Can't open $runcrit: $!";

#Print the header
print $fh_vcs <<'EOF';
#!/bin/bash
EOF
print $fh_verdi <<'EOF';
#!/bin/bash
EOF
if ($perf) {
    print $fh_crit <<'EOF';
#!/bin/bash
EOF
}

if (defined($gls_dir)) {
    print $fh_vcs <<EOF;
[[ -z "\$STDCELL_DIR" ]] && export STDCELL_DIR=$ENV{"NCL_DIR"}/stdcells
[[ -z "\$GPIO_DIR" ]] && export GPIO_DIR=$ENV{"NCL_DIR"}/gpio/latest
[[ -z "\$CMO_DIR" ]] && export CMO_DIR=$ENV{"NCL_DIR"}/sram/cmo/latest
[[ -z "\$PUF_DIR" ]] && export PUF_DIR=$ENV{"NCL_DIR"}/puf/latest
EOF
    print $fh_verdi <<EOF;
[[ -z "\$STDCELL_DIR" ]] && export STDCELL_DIR=$ENV{"NCL_DIR"}/stdcells
[[ -z "\$GPIO_DIR" ]] && export GPIO_DIR=$ENV{"NCL_DIR"}/gpio/latest
[[ -z "\$CMO_DIR" ]] && export CMO_DIR=$ENV{"NCL_DIR"}/sram/cmo/latest
[[ -z "\$PUF_DIR" ]] && export PUF_DIR=$ENV{"NCL_DIR"}/puf/latest
EOF
}

print $fh_vcs <<EOF;
export SPAR="$spar_dir"
export CAST2VERILOG_RUNTIME="$runtime"
export GLS_DIR="$gls_dir"
export VCS_PRINT_INITREG_INITIALIZATION="1"
export VCS_LIB=\$VCS_HOME/linux64/lib
EOF
if ($axi) {
    print $fh_vcs <<EOF;
export MENTOR_VIP_AE="/p/hdk/rtl/cad/x86-64_linux26/altera/quartus_prime/22.1.1/ip/altera/mentor_vip_ae/"
export QUESTA_MVC_GCC_LIB=\$MENTOR_VIP_AE/common/questa_mvc_core/linux_x86_64_gcc-6.2.0_vcs
export LD_LIBRARY_PATH={\$QUESTA_MVC_GCC_LIB:\$VCS_LIB}
EOF
}
print $fh_vcs "verdi3 vcs -V -assert svaext -licqueue -full64 -j4 -fgp -lrt @defines -f \"\$CAST2VERILOG_RUNTIME/$vcfg\" @args @vcs_args testbench.v \"\$CAST2VERILOG_RUNTIME/readhexint.c\" @sdf_args @netlists";
close $fh_vcs;
chmod 0755, $runvcs;
print $fh_verdi <<EOF;
export SPAR="$spar_dir"
export CAST2VERILOG_RUNTIME="$runtime"
export GLS_DIR="$gls_dir"
export GPIO_DIR=$ENV{"NCL_DIR"}/gpio/latest
vcs verdi3 verdi -nologo -ssf trace.fsdb -dbdir simv.daidir &
EOF
close $fh_verdi;
chmod 0755, $runverdi;

if ($perf) {
    print $fh_crit <<EOF;
\$MY_FULCRUM --latest critical bd_hist.log @crit_args \$@
EOF
    close $fh_crit;
    chmod 0755, $runcrit;
}

__END__
=head1 NAME

gen_model.pl - Create collateral for VCS simulation

=head1 SYNOPSIS

gen_model.pl [options] [verilog files...]

 Options:
   --cast-path     Specify the CAST path
   --gls-dir       Specify GLS top directory
   --cell          Name of the CAST cell
   --env           Name of the CAST environment
   --cosim         Or, specify a cosim spec instead of --cell and --env
   --width         Specify the maximum bitwidth for temporary variables
   --fpga-path     Specify the instance path of the FPGA relative to the top
   --define        Override CAST variables; can be specified any number of times
   --mem           Specify the Java heap size (defaults to 16G)
   --beh           If specified, generate a behavior model
   --kdb           If specified, generate Verdi Elaboration Database
   --no-debug      Disable default VCS debugging options
   --c2v-arg       Flags to cast2verilog; can be specified any number of times
   --vcs-arg       Flags to VCS; can be specified any number of times
   --sdf           Specify SDF args as <path_to_file.sdf>
   --perf          Enables performance monitoring of BD controllers
   --axi           Include AXI libraries and files
   --nolint          Enables linting during compile
=cut
