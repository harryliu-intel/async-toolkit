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
my $spar_dir = ('');
my @bboxes = ();
my $workdir = '.';
my @deletes = ();
my $runset = '/nfs/sc/proj/ctg/mrl108/mrl/tools/rdt/kits/p1273_14.2.1/runsets/icvalidator/verification_runsets/latest';
my $user_equiv = '';
my $icv_options = '';
my $verbose = 0;
my $layout = '';
my ($layout_lib, $layout_cell, $layout_view) = ('', '', 'layout');
my $schematic = '';
my @default_sp = ('lvs/d04.sp');
my $edtext = '';

sub usage {
    local $, = " ";
    print <<EOF;
Usage: $0
	--cast-path=[$castpath] (CAST path)
	--spar-dir=[$spar_dir] (SPAR directory)
	--cdswd=[$cdswd] (Cadence working directory)
	--top=[$cast_cell] (Top level CAST cell name)
	--runset=[$runset] (Location of LVS runsets)
	--work-dir=[$workdir] (Working directory)
	--delete=[@deletes] (Deletes the CAST cell from layout; specify any number of times)
	--generate-user-equiv=[$user_equiv] (How to generate user-intended equiv: FULL_NAME_CASE_SENSITIVE | NONE)
	--icv-options=[$icv_options] (Additional ICV options)
	--layout=[$layout] (Use specified layout in GDS2 or OASIS format)
	--layout-lib=[$layout_lib] (Layout library name)
	--layout-cell=[$layout_cell] (Layout cell name)
	--layout-view=[$layout_view] (Layout view name)
	--schematic=[$schematic] (Use specified schematic netlist in ICV format)
	--edtext=[$edtext] (Use additional text from specified file)
	[@bboxes] (List of CAST names of blackbox cells)
EOF
    exit 1;
}

GetOptions("cast-path=s" => \$castpath,
           "spar-dir=s" => \$spar_dir,
           "cdswd=s" => \$cdswd,
           "top=s" => \$cast_cell,
           "runset=s" => \$runset,
           "delete=s" => \@deletes,
           "generate-user-equiv=s" => \$user_equiv,
           "icv-options=s" => \$icv_options,
           "work-dir=s" => \$workdir,
           "layout=s" => \$layout,
           "layout-lib=s" => \$layout_lib,
           "layout-cell=s" => \$layout_cell,
           "layout-view=s" => \$layout_view,
           "schematic=s" => \$schematic,
           "edtext=s" => \$edtext,
           "verbose!" => \$verbose) || usage();

@bboxes = @ARGV;

usage() unless ($layout ne '' || $cdswd) and $cast_cell;

$workdir = File::Spec->rel2abs($workdir);
make_path($workdir);
die "$workdir is not an existing directory and cannot be created" unless -d $workdir;

die "Layout file $layout does not exist" unless $layout eq '' || -f $layout;

die "Schematic file $schematic does not exist" unless $schematic eq '' || -f $schematic;
 
die "edText file $edtext does not exist" unless $edtext eq '' || -f $edtext;

my $pid = open2(my $rename_out, my $rename_in, "rename --type=all");

sub detect_file {
    my ($file) = @_;
    my $result;
    open my $fh, $file or die "Can't open file: $file";
    read $fh, my $b4, 4;
    if ($b4 eq "\x00\x06\x00\x02") {
        $result = 'GDSII';
    } else {
        read $fh, my $b7, 7;
        if ("$b4$b7" eq '%SEMI-OASIS') {
            $result = 'OASIS';
        }
    }
    close $file;
    return $result;
}

sub translate {
    my ($type, $from, $to, $name) = @_;
    print $rename_in "$type $from $to $name\n";
    chomp (my $result = <$rename_out>);
    return $result;
}

sub export_gds {
    my ($lib, $cell, $view, $cdswd, $workdir) = @_;
    my $replay = catfile($workdir, 'replay.il');
    my $gds = catfile($workdir, "$cell.gds");
    open(my $fh, '>', $replay) || die "Can't write to replay.il: $!";
    if ($lib ne '') {
        print $fh <<EOF;
(ExportGDS ?CV (dbOpenCellViewByType \"$lib\" \"$cell\" \"$view\") ?gdsFile \"$gds\" ?directory \"$workdir\")
EOF
    } else {
        print $fh <<EOF;
(ExportGDS ?CV (nrOpenCellViewReadableByName \"$cell\" \"$view\") ?gdsFile \"$gds\" ?directory \"$workdir\")
EOF
    }
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

sub export_netlist {
    my ($castpath, $spar_dir, $cell, $workdir) = @_;
    my $vdir = catdir($workdir, "verilog");
    mkdir $vdir;
    my %types = ();
    my %done = ();
    my @worklist = ($cell);
    while (my $curr = shift @worklist) {
        if (exists($done{$curr})) {
            next;
        } else {
            $done{$curr} = 1;
        }
        my $filelist = catfile($vdir, "$curr.filelist");
        my @cmd = ('prs2verilog',
                   "--cast-path=$castpath",
                   "--outdir=$vdir",
                   "--file-list=$filelist",
                   "--cell=$curr\{1\{verilog.lvs,subcells,spice\}\}",
                   '--converter=lvs',
                   '--translate=mw',
                   '--max-heap-size=32G');
        print "RUNNING:@cmd\n" if $verbose;
        system(@cmd) == 0 or die "Failed to execute @cmd: $?\n";
        open(my $fh, $filelist) or die "Can't open $filelist: $!\n";
        while (<$fh>) {
            chomp;
            my ($key, $val) = ('standard', $_);
            if (/^(cdl|spice|cast|verilog) (\S+)/) {
                ($key, $val) = ($1, $2);
                if ($key eq "cast") {
                    push @worklist, $val;
                }
            }
            $types{$key}->{$val} = 1;
        }
        close ($fh);
    }

    my @cdlcells = sort keys %{$types{'cdl'}};
    my $cdl = catfile($workdir, "$cell.cdl");
    if (@cdlcells) {
        my $first = shift @cdlcells;
        my @cmd = ('jflat',
                   '--max-heap-size=4G',
                   "--cast-path=$castpath",
                   "--output-file=$cdl",
                   '--tool=cdl',
                   "--cell=$first",
                   '--cdl-translate=mw',
                   '--cdl-mos-parameters=m',
                   '--cdl-call-delimiter=');
        if (@cdlcells) {
            push @cmd, "--cdl-cells=" . join(':', @cdlcells);
        }
        system(@cmd) == 0 or die "Failed to execute @cmd: $?\n";
    }

    my @missing = ();
    my @sp = ($cdl);
    foreach my $sp (sort (keys %{$types{'spice'}}, @default_sp)) {
        my $file = catfile($spar_dir, $sp);
        if (-f $file) {
            push @sp, $file;
        } else {
            push @missing, $sp;
        }
    }

    my @vg = keys %{$types{'standard'}};
    foreach my $v (sort keys %{$types{'verilog'}}) {
        my $file = catfile($spar_dir, $v);
        if (-f $file) {
            push @vg, $file;
        } else {
            push @missing, $v;
        }
    }

    if (@missing) {
        print STDERR "Can't find the following in SPAR ($spar_dir): \n" .
                     join('', map { "$_\n" } @missing);
    }
    return ([sort @sp], [sort @vg]);
}

sub run_nettran {
    my ($sp, $vg, $cell, $workdir) = @_;
    my $nettran = catfile($workdir, "nettran.cdl");
    open(my $nh, '>', $nettran) or die "Can't write to $nettran: $!";
    my $includes = join("\n", map { ".include '$_'" } @{$sp});
    print $nh <<EOF;
*.SCALE meter
$includes
EOF
    close($nh);

    my $clf = catfile($workdir, "nettran.clf");
    open(my $ch, '>', $clf) or die "Can't write to $clf: $!";
    print $ch "-sp $nettran\n";
    print $ch join('', map { "-verilog $_\n" } @{$vg});
    close($ch);

    my $logFile = catfile($workdir, "nettran.log");
    my $outFile = catfile($workdir, "nettran.out");
    my $schFile = catfile($workdir, "$cell.py");
    my $cmd = $ENV{ICV_SCRIPT} .
              " icv_nettran -clf \Q$clf\E " .
              "-logFile \Q$logFile\E " .
              "-outName \Q$schFile\E > \Q$outFile\E";
    system($cmd) == 0 or die "Failed to run nettran $cmd: $?\n";
    return $schFile;
}

sub run_lvs {
    my ($gds, $sch, $sch_cell, $lay_cell, $bboxes, $deletes, $user_equiv,
		$edtext, $workdir) = @_;
    my $pdk = $ENV{FULCRUM_PDK_ROOT};

    my $format = detect_file($gds);
    if (!defined($format)) {
        print "Can't detect format of $gds, assuming GDS2\n";
        $format = "GDSII";
    }

	my $edtextopt = '';
	if ($edtext ne '') {
		$edtextopt = '-D _drEdTxt';
		symlink $edtext, 'edTextFile';
	}

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
$edtextopt
$icv_options
-i $gds
-s $sch
-sf ICV
-stc $sch_cell
-c $lay_cell
-f $format
$runset/StandAlone/dotOne/trclvs.1.rs
EOF
    close($fh);

    my $supplies = catfile($workdir, 'lve_supplies.rs');
    open(my $ph, '>', $supplies) or die "Can't write to $supplies: $!";
    print $ph <<EOF;
// empty, rely on default lve_ground/lve_power definition
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

    if ($user_equiv ne '') {
        my $lvsopt = catfile($workdir, 'user_LVSoptions');
        open(my $fh, '>', $lvsopt) or die "Can't write to $lvsopt: $!";
        print $fh <<EOF;
generate_user_equivs = $user_equiv,
generate_system_equivs = true
EOF
        close $fh;
    }

    my $cmd = "cd \Q$workdir\E && $ENV{ICV_SCRIPT} icv -clf lvs.clf";
    system($cmd) == 0 or die "Failed to run ICV $cmd: $?";
}

my $cadence = translate('cell', 'cast', 'cadence', $cast_cell);
my $mw = translate('cell', 'cast', 'mw', $cast_cell);

my $gds;
if ($layout ne '') {
    print "Using layout $layout...\n";
    $gds = $layout;
} else {
    print "Exporting GDS...";
    my $mapping;
    ($gds, $mapping) = export_gds($layout_lib,
                                  $layout_cell ne '' ? $layout_cell : $cadence,
                                  $layout_view,
                                  $cdswd,
                                  $workdir);
    print "done\n";
}

my $icv;
if ($schematic ne '') {
    print "Using schematic $schematic...\n";
    $icv = $schematic;
} else {
    print "Exporting netlist...";
    my ($sp, $vg) = export_netlist($castpath, $spar_dir, $cast_cell, $workdir);
    print "done\n";

    print "Running nettran...";
    $icv = run_nettran($sp, $vg, $mw, $workdir);
    print "done\n";
}

print "Starting LVS...\n";
run_lvs($gds, $icv, $mw, $layout_cell ne '' ? $layout_cell : $mw,
        [ grep { defined($_) } map { translate('cell', 'cast', 'mw', $_) } @bboxes ],
        [ grep { defined($_) } map { translate('cell', 'cast', 'mw', $_) } @deletes ],
        $user_equiv, $edtext,
        $workdir);

