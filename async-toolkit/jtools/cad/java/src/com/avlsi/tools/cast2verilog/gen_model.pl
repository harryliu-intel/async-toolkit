#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use File::Spec::Functions qw/:ALL/;
use FindBin;
use Cwd qw/abs_path/;

my ($cast_path, $spar_dir, $gls_dir, $cell, $env, $cosim, @cast_defines);
my ($beh, $fpga_path, @c2v_args, @vcs_args, $kdb, @sdf, $perf, $help, $nodebug);
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
           "defines=s"   => \@cast_defines,
           "c2v-args=s"  => \@c2v_args,
           "vcs-args=s"  => \@vcs_args,
           "mem=s"       => \$mem,
           "beh!"        => \$beh,
           "kdb!"        => \$kdb,
           "no-debug!"   => \$nodebug,
           "sdf=s"       => \@sdf,
           "perf!"       => \$perf,
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

my $runvcs   = "run-vcs";
my $runverdi = "run-verdi";

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

# escape special characters in the filelist
my $content;
if (open(my $lh, $flist)) {
    local $/;
    $content = <$lh>;
    close $lh;
    if ($content =~ /[()]/) {
        #print "Experiment-Not Escaping: \n$content\n";
        #escape special characters in the filelist
        $content =~ s/([()])/\\$1/g;
        #print "Experiment-Escaped: \n$content\n";
        #$content =~ s/([()])/\\\\\\$1/g;
        #open($lh, '>', $flist);
        #print $lh $content;
        #close $lh;
    }
}

# Collect list of cells to bind performance monitor
my @bd_ctrls;
my $search_ctrl = "^...bd";
my @bd_divs;
my $search_div = "^...divert";
if ($perf) {
    unshift @vcs_args, "-f \"\$CAST2VERILOG_RUNTIME/perf.vcfg\"";
    if (open(my $lh, $flist)) {
        local $/;
        my @content2 = split(/\n/, $content);
        foreach my $line (@content2) {
            chomp($line);
            $line = (split(/ /, $line))[-1];
            $line =~ s/\$GLS_DIR/$gls_dir/g;
            $line =~ s/[\r\n]+$//;
            #print "VLOG: $line\n";
            my @ctrl_match = split(/\n/, `grep -oh '$search_ctrl\\w*' $line \| sort \| uniq`);
            my @div_match = split(/\n/, `grep -oh '$search_div\\w*' $line \| sort \| uniq`);
            chomp(@ctrl_match);
            chomp(@div_match);
            foreach my $match (@ctrl_match) {
                push(@bd_ctrls, $match) unless grep{$_ =~ $match} @bd_ctrls;
            }
            foreach my $match (@div_match) {
                push(@bd_divs, $match) unless grep{$_ =~ $match} @bd_divs;
            }
        }
        close $lh;
        # Generate verilog file to bind performance monitors to controllers
        open my $fh_bind, ">bind_perf_mon.v" || die "Can't open bind_perf_mon.v: $!";
        foreach my $ctrl (@bd_ctrls) {
            print $fh_bind "bind $ctrl bd_ctrl_perf_mon ctrl_mon(.*);\n";
        }
        foreach my $div (@bd_divs) {
            print $fh_bind "bind $div bd_div_perf_mon div_mon(.*);\n";
        }
        close $fh_bind;
        push @vcs_args, "bind_perf_mon.v";
    }
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

push @vcs_args, '-kdb' if $kdb;

open my $fh_vcs, ">$runvcs" || die "Can't open $runvcs: $!";
open my $fh_verdi, ">$runverdi" || die "Can't open $runverdi: $!";

if (defined($gls_dir)) {
    print $fh_vcs <<'EOF';
[[ -z "$STDCELL_DIR" ]] && export STDCELL_DIR=$NCL_DIR/stdcells
[[ -z "$GPIO_DIR" ]] && export GPIO_DIR=$NCL_DIR/gpio/latest
[[ -z "$CMO_DIR" ]] && export CMO_DIR=$NCL_DIR/sram/cmo/latest
[[ -z "$PUF_DIR" ]] && export PUF_DIR=$NCL_DIR/puf/latest
EOF
print $fh_verdi <<'EOF';
[[ -z "$STDCELL_DIR" ]] && export STDCELL_DIR=$NCL_DIR/stdcells
[[ -z "$GPIO_DIR" ]] && export GPIO_DIR=$NCL_DIR/gpio/latest
[[ -z "$CMO_DIR" ]] && export CMO_DIR=$NCL_DIR/sram/cmo/latest
[[ -z "$PUF_DIR" ]] && export PUF_DIR=$NCL_DIR/puf/latest
EOF
}

if (defined($gls_dir)) {
    if (@sdf) {
        my %binds = ();

        my $reset_tcl = "sdf_reset.tcl";
        open my $fh_tcl, ">$reset_tcl" || die "Can't open $reset_tcl: $!";
        print $fh_tcl <<"EOF";
source $runtime/sdf_workarounds.tcl
reset_tcheck $reset_duration
EOF
        foreach my $sdf (@sdf) {
            #Get design name directly from SDF file
            my $block = `zgrep '(DESIGN ' $sdf`;
            $block = substr((split(/ /, substr($block, 1, -2)))[1], 1, -1);
            my @sdf_split = split(/\//, $sdf);
            my $basename = pop @sdf_split;
            my $sdf_dir = join('/', @sdf_split);
            my $subtype = (split(/\./, $basename))[0];
            my $minmax = (split(/\./, $basename))[1];

            my $arg = "max:$block:$sdf";
            push(@sdf_args, ("-sdf $arg"));
            print $fh_tcl "run_workarounds $block $sdf_dir/${block}_${minmax}\n";
            if (open(my $fh1, '<', "$sdf_dir/$block.$minmax.bind_notifiers.sv")) {
                while (<$fh1>) {
                    chomp;
                    $binds{$_} = 1;
                }
                close $fh1;
            }
        }
        close $fh_tcl;

        push @args, '-f', '$CAST2VERILOG_RUNTIME/sdf.vcfg';
        if (%binds) {
            my $nsv = 'bind_notifiers.sv';
            open my $fh_nsv, '>', $nsv || die "Can't open $nsv: $!";
            print $fh_nsv join('', map { "$_\n" } keys %binds);
            close $fh_nsv;
            push @args, $nsv;
        }

    } else {
        push @args, '-f', '$CAST2VERILOG_RUNTIME/gls.vcfg';
    }
    push @args, '-f', '$GLS_DIR/gls.vcfg';
}
else { $gls_dir=""; }

push @vcs_args, '-debug_access+all', '-debug_region+cell', '+vcs+initreg+random' unless $nodebug;

print $fh_vcs <<EOF;
export SPAR="$spar_dir"
export CAST2VERILOG_RUNTIME="$runtime"
export GLS_DIR="$gls_dir"
export VCS_PRINT_INITREG_INITIALIZATION="1"
verdi3 vcs  -assert svaext -licqueue -full64 -j4 -fgp -lrt @defines -f "\$CAST2VERILOG_RUNTIME/$vcfg" @args @vcs_args testbench.v "\$CAST2VERILOG_RUNTIME/readhexint.c" @sdf_args @netlists
EOF
print $fh_verdi <<EOF;
export SPAR="$spar_dir"
export CAST2VERILOG_RUNTIME="$runtime"
export GLS_DIR="$gls_dir"
verdi3 verdi -nologo -sv -top TESTBENCH -ssf trace.fsdb @defines -f "\$CAST2VERILOG_RUNTIME/$vcfg" @args testbench.v  @netlists &
EOF
close $fh_vcs;
close $fh_verdi;
chmod 0755, $runvcs;
chmod 0755, $runverdi;

__END__
=head1 NAME

gen_model.pl - Create collateral for VCS simulation from fpga.vg

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
=cut
