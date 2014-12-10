#!/usr/intel/bin/perl -w
use strict;
use Getopt::Long;
use File::Temp qw/tempfile/;
use File::Path qw/mkpath/;
use IPC::Open2;

my $package_root = $0;
my $exe = $package_root;
$exe =~ s:.*/::;
if (! ($package_root =~ m:^/:)) {
    my $pwd = `pwd`;
    chomp $pwd;
    $package_root = $pwd;
    $package_root .= "/$0";
    $package_root =~ s:$exe$::;
    $package_root =~ s://:/:g;
    chdir $package_root;
    $package_root = `pwd`;
    chomp $package_root;
    chdir $pwd;
}
else {
    $package_root =~ s:/bin/$exe::;
}

my $rename = "rename";

my $layoutPlus = 'layout';
my $outdir = '.';
my $defView = 'layout';
my $pdkroot;
my $cadenceLog;

sub usage {
    return <<USAGE;
Usage: $0
    [ --cadence-log=<log file> ]
    [ --command=<layout command> (defaults to $layoutPlus) ]
    [ --view=<default view name> (defaults to $defView) ]
    [ --outdir=<output directory> (defaults to $outdir) ]
    --fulcrum-pdk-root=<PDK directory>
USAGE
}

my $renamePid;
my ($renameIn, $renameOut);
sub renameCell {
	my $cell = shift;
	if (!$renamePid) {
		$renamePid = open2($renameIn, $renameOut, "$rename --type=cell --from=cast --to=cadence");
	}
	print $renameOut "$cell\n";
	my $renamed = <$renameIn>;
	chomp($renamed);
	return $renamed;
}

sub getLibrary {
	my $cell = shift;
	if ($cell =~ /(.+)\.[^.]+\.[^.]+/) {
		return $1;
	}
	return undef;
}

my $result = GetOptions("command=s"          => \$layoutPlus,
                        "view=s"             => \$defView,
                        "outdir=s"           => \$outdir,
                        "fulcrum-pdk-root=s" => \$pdkroot,
						"cadence-log=s"      => \$cadenceLog);

die usage() . "Unable to parse command line arguments" if (!$result);
die usage() . "You must specify --fulcrum-pdk-root=" if(!$pdkroot);
die "$outdir is not a directory" if (-e $outdir && !-d $outdir);
-e $outdir || mkpath($outdir);
die "Output directory $outdir not writeable" unless (-w $outdir);

my @todo = ();

while(<STDIN>) {
    chomp;
	my ($cell, $view, $library) = split;
	$view = $defView unless defined($view);
	$library = getLibrary($cell) unless defined($library);
	die "Cannot get library for cell $cell" unless defined($library);
	my $renamedCell = renameCell($cell);
	die "Cannot rename cell $cell from CAST to Cadence"
		unless defined($renamedCell);
	push @todo, [ $library, $renamedCell, $view ];
}

my ($skillFh, $skillName) = tempfile(UNLINK => 1);

print $skillFh <<HEADER;
(load "$package_root/share/skill/autoload.il")
(load "$pdkroot/share/Fulcrum/pdkinfo.il")
HEADER

foreach my $triple (@todo) {
	my ($library, $cell, $view) = @{$triple};
print $skillFh <<BODY;
(WriteInstancesFile "$outdir" (dbOpenCellViewByType "$library" "$cell" "$view") ?LibCellPairRegExsToExclude `(,\@WiringCellLibCellPairRegExs ,\@GateLibCellPairRegExs ,\@ChainLibCellPairRegExs ,\@SuperStackLibCellPairRegExs (,( strcat "^" TechLibName "\$" ) ".*" ) ) )
BODY
}

print $skillFh <<FOOTER;
(exit)
FOOTER
close($skillFh);

my ($logFh, $logName);
if ($cadenceLog) {
	$logName = $cadenceLog;
} else {
	($logFh, $logName) = tempfile(UNLINK => 1);
	close($logFh);
}

my $wrapper = $ENV{'CADENCE_WRAPPER_SCRIPT'} || "/usr/local/cadence/bin/ic";
$wrapper = $ENV{IC_SCRIPT} if defined $ENV{IC_SCRIPT};
my $command = "$wrapper $layoutPlus -nograph -replay \"$skillName\" -log \"$logName\" </dev/null &>/dev/null";
print "Running $command...\n";
my $status = system($command);

if ($status != 0) {
	open($logFh, $logName);
	while(<$logFh>) {
		print STDERR $_;
	}
}
close($renameIn) if ($renameIn);
close($renameOut) if ($renameOut);
waitpid $renamePid, 0 if ($renamePid);
exit $status >> 8;
