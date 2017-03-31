#!/usr/intel/bin/perl
use strict;
use warnings;
use Cwd qw/abs_path/;
use Getopt::Long;
use IPC::Open2;
use File::Path qw/make_path/;
use File::Spec;
use File::Spec::Functions;
use File::Temp qw/tempdir/;

my ($castpath, $cdswd, $cast_cell) = ('', '', '', '');
my @bboxes = ();
my $workdir = '.';
my @deletes = ();
my $runset = '/nfs/sc/proj/ctg/mrl108/mrl/tools/rdt/kits/p1273_14.2.1/runsets/icvalidator/verification_runsets/latest';

sub usage {
    local $, = " ";
    print <<EOF;
Usage: $0
	--cast-path=[$castpath] (CAST path)
	--cdswd=[$cdswd] (Cadence working directory)
	--top=[$cast_cell] (Top level CAST cell name)
	--runset=[$runset] (Location of LVS runsets)
	--work-dir=[$workdir] (Working directory)
	--delete=[@deletes] (Deletes the CAST cell from layout; specify any number of times)
	[@bboxes] (List of CAST names of blackbox cells)
EOF
    exit 1;
}

GetOptions("cast-path=s" => \$castpath,
           "cdswd=s" => \$cdswd,
           "top=s" => \$cast_cell,
           "runset=s" => \$runset,
           "delete=s" => \@deletes,
           "work-dir=s" => \$workdir) || usage();

@bboxes = @ARGV;

usage() unless $cdswd and $cast_cell;

$workdir = File::Spec->rel2abs($workdir);
make_path($workdir);
die "$workdir is not an existing directory and cannot be created" unless -d $workdir;

my $pid = open2(my $rename_out, my $rename_in, "rename --type=all");

sub translate {
    my ($type, $from, $to, $name) = @_;
    print $rename_in "$type $from $to $name\n";
    chomp (my $result = <$rename_out>);
    return $result;
}

sub export_gds {
    my ($cell, $view, $cdswd, $workdir) = @_;
    my $replay = catfile($workdir, 'replay.il');
    my $gds = catfile($workdir, "$cell.gds");
    open(my $fh, '>', $replay) || die "Can't write to replay.il: $!";
    print $fh <<EOF;
(ExportGDS ?CV (nrOpenCellViewReadableByName \"$cell\" \"$view\") ?gdsFile \"$gds\" ?directory \"$workdir\")
EOF
    close $fh;
    my $cmd = "cd \Q$cdswd\E && layout ".
              "-log \Q$workdir/CDS.log\E " .
              "-nograph " .
              "-replay \Q$replay\E < /dev/null >\Q$workdir/strm.out\E";
    system($cmd) == 0 or die "Failed to execute $cmd: $?\n";
    
    my $cellmap = catfile($workdir, 'xStrmOut_cellMap.txt');
    my %mapping = ();
    die "Stream file not generated: $gds" unless -e $gds;
    if (open(my $mh, '<', $cellmap)) {
        while(<$mh>) {
            next if (/^#/ || /^\s*$/);
            my @fields = split;
            my $cast = translate('cell', 'cadence', 'cast', $fields[1]);
            $mapping{$cast} = $fields[3];
        }
        close $mh;
    } else {
        die "Can't open Cell map file $cellmap: $!";
    }

    return ($gds, \%mapping);
}

sub export_cdl {
    my ($castpath, $cell, $cadence, $mapping, $workdir) = @_;
    my $cdlmap = catfile($workdir, 'cdl.map');
    open(my $fh, '>', $cdlmap) or die "Can't write to $cdlmap: $!";
    {
        local $, = "\n";
        local $\ = "\n";
        print $fh map { "cell $_ $mapping->{$_}" } sort keys %{$mapping};
    }
    close $fh;
    my $cdl = catfile($workdir, "$cadence.cdl");
    my $cmd = "jflat --cast-path=\Q$castpath\E --tool=cdl " .
              "--cdl-translate=cadence " .
              "--cdl-name-map=\Q$cdlmap\E " .
              "--cdl-mos-parameter=m " .
              "--cdl-call-delimiter= " .
              "--cell=\Q$cell\E " .
              "--max-heap-size=8G " .
              "--output-file=\Q$cdl\E";
    system($cmd) == 0 or die "Failed to execute $cmd: $?\n";

    return $cdl;
}

sub run_nettran {
    my ($cdl, $cell, $workdir) = @_;
    my $nettran = catfile($workdir, "nettran.cdl");
    my $tempdir = tempdir(CLEANUP => 1);
    my $cellcdl = catfile($tempdir, "cell.cdl");
    symlink abs_path($cdl), $cellcdl;
    open(my $nh, '>', $nettran) or die "Can't write to $nettran: $!";
    print $nh <<EOF;
*.SCALE meter
.include '$cellcdl'
EOF
    close($nh);

    my $logFile = catfile($workdir, "nettran.log");
    my $outFile = catfile($workdir, "nettran.out");
    my $schFile = catfile($workdir, "$cell.sch_out");
    my $cmd = $ENV{ICV_SCRIPT} .
              " icv_nettran -sp \Q$nettran\E " .
              "-logFile \Q$logFile\E " .
              "-outName \Q$schFile\E > \Q$outFile\E";
    system($cmd) == 0 or die "Failed to run nettran $cmd: $?\n";
    return $schFile;
}

sub run_lvs {
    my ($gds, $sch, $cell, $bboxes, $deletes, $workdir) = @_;
    my $pdk = $ENV{FULCRUM_PDK_ROOT};

    my $clf = catfile($workdir, 'lvs.clf');
    open(my $fh, '>', $clf) or die "Can't write to $clf: $!";
    print $fh <<EOF;
-I .
-I $pdk/share/Fulcrum/icv/lvs
-I $runset/PXL_ovrd
-I $runset/PXL
-I $runset/StandAlone/dotOne
-I $runset/util/dot1/HIP
-I $runset/util/Cadnav
-I $runset/util/denplot
-D _drIncludePort
-D NOCLD
-vue
-dp8
-turbo
-D _drMaxError=100000000
-D _drUSENDG=_drNO
-D _drUSERDEFINESUIN
-D _drMSR
-D _drCaseSensitive
-D _drTOPCHECK=_drmixed
-D _drDONTCMPCAPS
-i $gds
-s $sch
-sf ICV
-stc $cell
-c $cell
$runset/StandAlone/dotOne/trclvs.1.rs
EOF
    close($fh);

    my $supplies = catfile($workdir, 'lve_supplies.rs');
    open(my $ph, '>', $supplies) or die "Can't write to $supplies: $!";
    print $ph <<EOF;
lve_ground = {"vss", "GND"};
lve_power = {"vccneuro_0p75", "vccneuro_1p0", "vccneuro_1p8", "Vdd", "VDDIO", "VDDM", "vcc"};
EOF
    close($ph);

    my $bbox = catfile($workdir, 'userProject_blackbox');
    open(my $bh, '>', $bbox) or die "Can't write to $bbox: $!";
    {
        local $, = "\n";
        local $\ = "\n";
        print $bh map { "lvs_black_box_options(equiv_cells={{schematic_cell=\"$_\", layout_cell=\"$_\"}});" } @{$bboxes};
    }
    close($bh);

    if (@{$deletes}) {
        local $" = ", ";
        my @quoted_deletes =  map { "\"$_\"" } @{$deletes};
        my $userdef = catfile($workdir, 'userProject_dr_cell_lists');
        open(my $uh, '>', $userdef) or die "Can't write to $userdef: $!";
        print $uh "ProjectDeleteList = { @quoted_deletes };\n";
        close($uh);

        my $compare = catfile($workdir, 'user_LVScompare_options');
        open(my $ch, '>', $compare) or die "Can't write to $compare: $!";
        print $ch <<EOF;
delete_schematic_cells = { @quoted_deletes },
#include "$runset/PXL/user_LVScompare_options"
EOF
        close($ch);
    }

    my $cmd = "cd \Q$workdir\E && $ENV{ICV_SCRIPT} icv -clf lvs.clf";
    system($cmd) == 0 or die "Failed to run ICV $cmd: $?";
}

my $cadence = translate('cell', 'cast', 'cadence', $cast_cell);

print "Exporting GDS...";
my ($gds, $mapping) = export_gds($cadence, 'layout', $cdswd, $workdir);
print "done\n";

print "Exporting CDL...";
my $cdl = export_cdl($castpath, $cast_cell, $cadence, $mapping, $workdir);
print "done\n";

print "Running nettran...";
my $icv = run_nettran($cdl, $cadence, $workdir);
print "done\n";

print "Starting LVS...\n";
run_lvs($gds, $icv, $mapping->{$cast_cell},
        [ grep { defined($_) } map { $mapping->{$_} } @bboxes ],
        [ grep { defined($_) } map { $mapping->{$_} } @deletes ],
        $workdir);
