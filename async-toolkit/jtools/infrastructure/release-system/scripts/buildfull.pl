#!/usr/intel/bin/perl
# AAG
# $Id$
# $DateTime$

use strict;
use FindBin;

BEGIN {
    my $correctitools="$FindBin::Bin/.itools";
    $correctitools="/p/rrc/tools/.itools" unless -e $correctitools;
    if ( $ENV{USER_ITOOLS} ne $correctitools) {
        $ENV{USER_ITOOLS}=$correctitools;
        exec "$0",@ARGV;
    }
}

# make sure important stuff is in the PATH
my @path=split(/:/,$ENV{PATH});
die "Cannot run in a directory to which you cannot write\n".
    "Change directories and run again\n" if ! -w ".";

$ENV{PATH}="/usr/intel/bin:/bin:/usr/ucb:/usr/bin";
$ENV{'FULCRUM_WRAPPER_DIR'} = "/nfs/sc/proj/ctg/mrl108/mrl/tools/bin"
    unless defined $ENV{'FULCRUM_WRAPPER_DIR'};
# specific paths
my $binhome=$0;
# name of this executable
my $exec=$0;
$exec =~ s:.*/::;
# find cononical path to this executable
if ( $binhome =~ m:^/:) {
    $binhome =~ s:/[^/]+$::;
}
else {
    my $pwd = `pwd`;
    chomp $pwd;
    if ( $binhome =~ m:/: ) {
        $binhome =~ s:/[^/]+$::;
        chdir $binhome;
    }
    $binhome=`pwd`;
    chomp $binhome;
    chdir $pwd;
}

# point to specific sub-tools
my $buildfulcrum="$binhome/$exec";
my $updatefmdb="$binhome/updatefmdb.pl";
my $packageinstall="$binhome/package-inst.pl";

use Getopt::Long;
use File::Temp qw(tempfile tempdir);
my $verbose=0;
my $strip=1;

my %pdknobuild = (
    "fulcrum-nv90-pdk" => 1,
    "fulcrum-tsmc130hs-pdk" => 1,
    "fulcrum-tsmc13-pdk" => 1,
    "fulcrum-tsmc13_lowK-pdk" => 1,
    "fulcrum-tsmc55-pdk" => 1,
    "fulcrum-tsmc65-pdk" => 1,
);

my $user = `whoami`;
chomp $user;
if ($user eq "") {
    if (-x "/usr/ucb/whoami") {
        $user = `/usr/ucb/whoami`;
        chomp $user;
    }
    elsif (-x "/usr/bin/whoami") {
        $user = `/usr/bin/whoami`;
        chomp $user;
    }
}
my $p4user = $user;
$p4user = "sys_system" if ($user eq "johndoe" or $user eq "tsbuild");
my $p4port=$ENV{P4PORT} if defined $ENV{P4PORT};
$p4port = "ssl:p4proxy19.devtools.intel.com:2510" if $p4port !~ /^ssl/;

my $hostname=`hostname`;
chomp $hostname;

sub wrap {
    my $str = $_[0];
    my $len = 80;
    my $line = 0;
    my $toc = index ($str,':')+1;
    while (length ($str) > $len) {
        my $l = substr($str,0,80);
        my $x=length ($l)-1;
        while (substr($str,$x,1) ne ' ' and $x > 0) {
            $x--;
        }
        print STDERR substr($str,0,$x)."\n";
        $str = substr($str,$x);
        $str = sprintf "%*.*s$str", $toc,$toc," ";
    }
    print STDERR "$str\n" if length($str);
}

my $template=
"Client:	CLIENTNAME

Update:	2004/05/18 15:40:42

Access:	2004/05/19 08:25:15

Description:
	Created by $user.

Root:	BUILD_PACKAGE_ROOT

Options:	noallwrite noclobber nocompress unlocked modtime rmdir unlocked

LineEnd:	local

View:
	//mrl/sw/intel/cad/external-tools-integration/cadence/virtuoso/... //CLIENTNAME/cad/external-tools-integration/cadence/virtuoso/...
	-//mrl/sw/intel/cad/external-tools-integration/cadence/virtuoso65/... //CLIENTNAME/cad/external-tools-integration/cadence/virtuoso65/...
	-//mrl/sw/intel/cad/external-tools-integration/cadence/virtuoso/2.1/... //CLIENTNAME/cad/external-tools-integration/cadence/virtuoso/2.1/...
	-//mrl/sw/intel/cad/external-tools-integration/cadence/virtuoso65/2.1/... //CLIENTNAME/cad/external-tools-integration/cadence/virtuoso65/2.1/...
	//mrl/sw/intel/cad/all/... //CLIENTNAME/cad/all/...
	//mrl/sw/intel/cad/c/... //CLIENTNAME/cad/c/...
	//mrl/sw/intel/cad/java/... //CLIENTNAME/cad/java/...
	//mrl/sw/intel/cad/java/src/com/avlsi/tools/tsim/... //CLIENTNAME/cad/java/src/com/avlsi/tools/tsim/...
	//mrl/sw/intel/cad/oldC/... //CLIENTNAME/cad/oldC/...
	//mrl/sw/intel/cad/perl/... //CLIENTNAME/cad/perl/...
	//mrl/sw/intel/cad/lad_quake/... //CLIENTNAME/cad/lad_quake/...
	//mrl/sw/intel/cad/rdt/... //CLIENTNAME/cad/rdt/...
	//mrl/sw/intel/cad/proteus/... //CLIENTNAME/cad/proteus/...
	//mrl/sw/intel/cad/vendor-release/... //CLIENTNAME/cad/vendor-release/...
	-//mrl/sw/intel/cad/java/Makefile //CLIENTNAME/cad/java/Makefile
	-//mrl/sw/intel/cad/java/projects/congestion/doc/... //CLIENTNAME/cad/java/projects/congestion/doc/...
	-//mrl/sw/intel/cad/java/projects/jauto/doc/... //CLIENTNAME/cad/java/projects/jauto/doc/...
	-//mrl/sw/intel/cad/java/projects/mgn/doc/... //CLIENTNAME/cad/java/projects/mgn/doc/...
	-//mrl/sw/intel/cad/java/projects/subtyping/doc/... //CLIENTNAME/cad/java/projects/subtyping/doc/...
	-//mrl/sw/intel/cad/java/tests/... //CLIENTNAME/cad/java/tests/...
	//mrl/sw/intel/cad/lve/... //CLIENTNAME/cad/lve/...
	//mrl/sw/intel/cad/ubersize/... //CLIENTNAME/cad/ubersize/...
	//mrl/sw/intel/cad/scripts/... //CLIENTNAME/cad/scripts/...
	//mrl/sw/intel/infrastructure/sh-lib/... //CLIENTNAME/infrastructure/sh-lib/...
	//mrl/sw/intel/infrastructure/build-system/... //CLIENTNAME/infrastructure/build-system/...
";

my $pdktemplate =
"Client:	CLIENTNAME

Update:	2004/05/20 17:20:30

Access:	2004/05/20 17:20:34

Description:
	Created by $user.

Root:	BUILD_PACKAGE_ROOT

Options:	noallwrite noclobber nocompress unlocked modtime rmdir unlocked

LineEnd:	local

View:
	//mrl/sw/intel/infrastructure/sh-lib/... //CLIENTNAME/sw/infrastructure/sh-lib/...
	//mrl/sw/intel/infrastructure/build-system/... //CLIENTNAME/sw/infrastructure/build-system/...
PDKDIRS
	-//mrl/sw/intel/infrastructure/build-system/doc/... //CLIENTNAME/sw/infrastructure/build-system/doc/...
";

# command line args
my $targetarch="all";    # default build all arch
my $ROOT_PROJECT_DIR;
my $ROOT_TARGET_DIR;
my $TOOLS_HOME_DEFAULT="/p/rrc/tools/fulcrum";
my $PACKAGE_STORAGE_DEFAULT="$TOOLS_HOME_DEFAULT/packages";
my $BUILD_SYSTEM_ROOT="$TOOLS_HOME_DEFAULT/build-system";
my $PACKAGE_STORAGE=$PACKAGE_STORAGE_DEFAULT;
my $toolhome="$TOOLS_HOME_DEFAULT/tools";
my $package_storage_set=0;
my $TOOLS_TARGET;
my $headchange;
my $changename;
my $argchange;
my $argchangedate;
my $log;
my $force=0;
my $overwrite=0;
my $packages_to_make;
my $fixed_target=0;
my $fixed_project=0;
my $keep_target=0;
my $keep_project=0;
my $keepall=0;
my $dosync=1;
my $pdkbuild=0;
my $pdksg='sh -c';
my $p4cmd;
my $isqrsh=0;
my @qrshargs=();
my $branch="";
my $links=0;

my %options = (
    "links" => sub { $links = 1;
                    push @qrshargs, "--links";
                },
    "branch=s" => \$branch,
    "qrsh" => \$isqrsh,
    "target-arch=s" => \$targetarch, # do not pass targetarch to qrshargs
    "build-system=s" =>
            sub {
                $BUILD_SYSTEM_ROOT=@_[1];
                push @qrshargs, "--build-system";
                push @qrshargs, "@_[1]";
            },
    "root-project-dir=s" =>
            sub {
                $ROOT_PROJECT_DIR = @_[1];
                $fixed_project=1; $keep_project=1;
                push @qrshargs, "--root-project-dir";
                push @qrshargs, "@_[1]";
            },
    "root-target-dir=s" =>
            sub {
                $ROOT_TARGET_DIR = @_[1];
                $fixed_target=1;
                $keep_target=1;
                push @qrshargs, "--root-target-dir";
                push @qrshargs, "@_[1]";
            },
    "package-storage=s" =>
            sub {
                $PACKAGE_STORAGE = @_[1];
                push @qrshargs, "--package-storage";
                push @qrshargs, @_[1];
                $package_storage_set=1;
            }, 
    "toolhome=s" =>
            sub {
                $PACKAGE_STORAGE = "@_[1]";
                $toolhome = "@_[1]";
                push @qrshargs, "--toolhome";
                push @qrshargs, "$_[1]";
            },
    "verbose" =>
            sub {
                $verbose=1;
                push @qrshargs, "--verbose";
            },
    "log-file=s" =>
            sub {
                $log=@_[1];
                push @qrshargs, "--log-file";
                push @qrshargs, "$_[1]";
            },
    "change=n" =>
            sub {
                $argchange=@_[1];
                push @qrshargs, "--change";
                push @qrshargs, "$_[1]";
            },
    "force" =>
            sub {
                $force=1;
                push @qrshargs, "--force";
            },
    "overwrite" =>
            sub {
                $overwrite=1;
                push @qrshargs, "--overwrite";
            },
    "pdk" =>
        sub {
            $pdkbuild=1;
            push @qrshargs, "--pdk";
        }, 
    "pdk-group=s" =>
        sub {
            $pdksg="sg $_[1]";
            push @qrshargs, "--pdk-group";
            push @qrshargs, $_[1];
        }, 
    "keep" =>
        sub {
            $keepall = $keep_project = $keep_target = 1;
            push @qrshargs, "--keep";
        },
    "nostrip" => sub { $strip = 0; },
    "nop4sync" =>
        sub {
            $dosync = 0;
            push @qrshargs, "--nop4sync";
        },
    "p4user=s" =>
        sub {
            $p4user = $_[1];
            push @qrshargs, "--p4user";
            push @qrshargs, "$_[1]";
        },
    "help" => \&usage,
);

sub usage {
    my ($msg)=@_;
    if (defined ($msg) and $msg ne "help") {
        print STDERR "Error: $msg\n\n";
    }
    my @help=(
   "--build-system <dir>     : Where the build system is, if you want to use ".
   "your own. Defaults to /home/local/common/fulcrum/build-system ".
   "The build system always is p4 sync'd to the latest change nr ".
   "regardless of the specified change number if the root is the ".
   "default root above.",
   "--root-project-dir <dir> : Used if you want to keep the project dir, otherwise ".
   "it is temp. Not recommended.",
   "--root-target-dir <dir>  : Used if you want to keep the target dir, otherwise it ".
   "is temp.",
   "--package-storage <dir>  : default is $PACKAGE_STORAGE_DEFAULT. Use this to set ".
   "a local.",
   "--toolhome <dir>         : default is $TOOLS_HOME_DEFAULT. Use this to set ".
   "a local packages area.",
   "--verbose                : a little more detail",
   "--log-file               : a place for the log file, default is a log....log ".
   "name. The log file has more data than you typically want.",
   "--change #               : specify the specific change number",
   "--pdk                    : build a pdk instead of tool. If no packages are ".
   "specified, then all pdks are built.",
   "--keep                   : Keeps the temp dirs for project and target rather ".
   "than deleting them.",
   "--force                  : does p4 sync -f instead of p4 sync",
   "--overwrite              : will overwrite existing change numbers",
   "--help                   : This help message.",
   "--p4user <name>          : define p4 user for doing p4sync",
   "--qrsh                   : [hidden] for internal use only, do not use on command line",
   "--target-arch <list>     : one or more of Linux-i686, Linux-x86_64 ".
   "or all",
   "--nop4sync               : will not run p4 sync, use only when specifying project ",
   "--nostrip                : don't strip executable of debugging symbols when installing ".
   "directory",
    );
    print STDERR <<U;
Usage: $0 [options]
U
    foreach my $opt (sort keys %options) {
        my $found=0;
        $opt =~ s/[=:].*//;
        foreach my $str (@help) {
            my ($op,$str1) = split (/ /,$str, 2);
            $op =~ s/^--//;
            $op =~ s/[=:].*//;
            if ($op eq $opt) {
                wrap ($str) if ! ($str =~ /\[hidden\]/);
                $found = 1;
                last;
            }
        }
        if (! $found) {
            printf STDERR "   --%-24.24s: Unknown\n", $opt;
        }
    }
    exit 1;
}

sub cleanup {
    my ($code)=@_;
    if ( ! $keep_project ) {
        print STDERR "Cleaning up project directory\n" if $verbose;
	system "rm -rf $ROOT_PROJECT_DIR";
    }
    if ( ! $keep_target ) {
        print STDERR "Cleaning up target directory\n" if $verbose;
	system "rm -rf $ROOT_TARGET_DIR";
    }
    $code = 1 unless defined($code);
}

my $waitclean=0;

END {
    if (! $waitclean) {
        print "Waiting..." if $verbose;
        while (wait != -1) {};
        cleanup(0);
        print "\n" if $verbose;
    }
    exit 0;
}

sub getheadchange {
    my ($p4cmd)=@_;
    local (*P);
    local ($_);
    open (P, "$p4cmd changes -s submitted -m 1 |") or usage ("cannot open pipe to p4 changes");
    $_=<P>;
    close P;
    my ($f,$headchange)=split;
    return $headchange;
}

sub getchangefromdate {
    my ($date)=@_;
    local (*P);
    my ($year, $month, $day, $hour, $minute, $second) = split(/[- :\/]/, $date);
    my $change;
    usage unless defined ($day);
    usage unless $year > 2001;
    usage unless $month >= 1 and $month <= 12;
    usage unless $day >= 1 and $day <= 31;
    $hour = 0 unless $hour;
    $minute = 0 unless $minute;
    $second = 0 unless $second;
    my $datetime = sprintf "%04d/%02d/%02d %02d:%02d:%02d",
        $year, $month, $day, $hour, $minute, $second;
    open (P, "$p4cmd changes -t |");
    while (<P>) {
        chomp;
        my @f=split;
        $change = $f[1];
        my $dt = "$f[3] $f[4]";
        if ("$dt" le "$datetime" ) {
            last;
        }
    }
    close P;
    $change;
}

my $logitwarn = 0;

sub logit {
    my ($msg)=@_;
    local (*P);
    if (! open (P, ">>$log")) {
        if (! $logitwarn ) {
            print "Warning: Cannot open $log, no log generated.\n";
            $logitwarn = 1;
        }
    }
    else {
        print P $msg . "\n";
        close P;
    }
    print $msg . "\n" if $verbose;
}

$verbose=0;

my %legalplatforms = ();

my @allarches=("Linux-x86_64");
my @arches=@allarches;
my @allqarch=("lx24-x86","lx24-amd64");
my $mem512="512";
my $mem48="48";
$mem512="512M";
$mem48="48M";
@allqarch=("lx24-amd64");
my @qarch=@allqarch;
my %arches=();

foreach my $a (0..$#arches) {
    $arches{$qarch[$a]}=$arches[$a];
    $legalplatforms{$arches[$a]}=$qarch[$a];
}

my $ostype=`uname -s`;
chomp $ostype;
my $archtype=`uname -m`;
chomp $archtype;
my $aname="${ostype}-${archtype}";

GetOptions (
    %options,
) or usage;

undef $targetarch if $isqrsh;

if ($targetarch =~ /^all$/i ) {
    my @t=keys %legalplatforms;
    $targetarch="$t[0]";
    foreach my $t (1..$#t) {
        $targetarch .= ",$t[$t]";
    }
}
my $definedargchange=1;
$p4cmd = "P4CONFIG= P4USER=$p4user P4PORT=$p4port /usr/intel/bin/p4";
if ($p4user ne "system") {
    my $xxy= `$p4cmd client -o 2>&1 1>/dev/null`;
    if ( "$xxy" =~ m/P4PASSWD/ ) {
        print STDERR
            "Warning: p4 user $p4user requires password, changing to system user\n";
        $p4user = "system";
        $p4cmd = "P4CONFIG= P4USER=$p4user P4PORT=$p4port /usr/intel/bin/p4";
    }
}
if (! defined ($argchange)) {
    $definedargchange=0;
    my $x=`$p4cmd changes -s submitted -m 1`;
    my $f;
    ($f,$argchange)=split(/ /,$x);
}
my %packages_to_make;
my $buildtype=0;
push @qrshargs, @ARGV;
foreach my $pkg (@ARGV) {
    if (!defined ($packages_to_make)) {
        $packages_to_make = $pkg;
    }
    else {
        $packages_to_make .= ",$pkg";
    }
}
$buildtype = $pdkbuild;
my %donotbuild;
if (defined ($packages_to_make)) {
    my @pkg = split(/[, ]/,$packages_to_make);
    foreach my $pkg (sort @pkg) {
        if ($pkg =~ /-pdk$/) {
            if ($buildtype & 2) {
                print STDERR "Attempt to build PDK in same script as standard package\n";
                print STDERR "Will skip $pkg\n";
            }
            else {
                $packages_to_make{$pkg}=1;
                $buildtype |= 1;
            }
        }
        else {
            if ($buildtype & 1) {
                print STDERR "Attempt to build standard package same script pdk\n";
                print STDERR "Will skip $pkg\n";
            }
            else {
                $packages_to_make{$pkg}=1;
                $buildtype |= 2;
            }
        }
    }
}
else {
    local (*P);
    local ($_);
    open (P, "/home/local/common/fulcrum/config/Linux-i686/donotbuild");
    while (<P>) {
        chomp;
        $donotbuild{$_}=1;
    }
    close P;
}
$pdkbuild = ( $buildtype == 1 );
$targetarch="Linux-x86_64" if $pdkbuild; # override for all pdk's
my @targetarch = split(/,/,$targetarch);
foreach my $targetarch (@targetarch) {
    usage("Illegal platform $targetarch")
       if (defined ($targetarch) and ! defined ($legalplatforms{$targetarch}));
}
my $continue=0;
$continue = 1 if ! defined $targetarch;
$ENV{QB_LOCAL}=0;
undef $ENV{QB_LOCAL};
$ENV{QRSH_FLAGS}="";
$ENV{QB_RUN_NAME}="buildfull";
foreach my $targetarch (@targetarch) {
    if (defined ($targetarch) and $targetarch ne $aname and
            defined ($legalplatforms{$targetarch})) {
        my @qrsh=();
        $ENV{QRSH_FLAGS}="-P build-fulcrum -cwd -now n -l a=$legalplatforms{$targetarch},mem=$mem512";
        push @qrsh, ("$buildfulcrum","--change","$argchange");
        push @qrsh, ("--branch", "$branch") if $branch ne "";
        push @qrsh, "--qrsh"
            if $#targetarch > 0; # do not install single targets
        push @qrsh, @qrshargs;
        if ( ! fork()) {
            print STDERR join(" ",@qrsh)."\n" if $verbose;
            print "@qrsh\n" if $verbose;
            exec @qrsh;
            exit 0;
        }
    }
    else {
        print "Continue $targetarch\n" if $verbose;
        $continue=1;
    }
}
if ( ! $continue ) {
    exit 0;
}    
print STDERR "Continuing...\n" if $verbose;

$ROOT_PROJECT_DIR =~ s:/$::;
$ROOT_TARGET_DIR =~ s:/$::;
$PACKAGE_STORAGE =~ s:/$::;
$PACKAGE_STORAGE =~ s:/(tools|packages)$::;
$PACKAGE_STORAGE .= "/packages";
if ($user ne "tsbuild" or "$PACKAGE_STORAGE" ne "$PACKAGE_STORAGE_DEFAULT") {
    system "mkdir -p '$PACKAGE_STORAGE' >/dev/null 2>&1";
    if ( ! ( -d "$PACKAGE_STORAGE" ) or ! ( -w "$PACKAGE_STORAGE" ) ) {
        usage "Cannot create specified $PACKAGE_STORAGE directory";
    }
}
if (! -w "$PACKAGE_STORAGE" or ! -d "$PACKAGE_STORAGE" or
        ($user ne "tsbuild" and
            "$PACKAGE_STORAGE" eq "$PACKAGE_STORAGE_DEFAULT") ) {
    usage "You do not have permission to write to $PACKAGE_STORAGE";
}
#here
my $scratchdir="/scratch";
#prevent pdk build from doing strange things in updatefmdb
if ($pdkbuild) {
    if ($ostype ne "Linux") {
        print STDERR "Please build PDK only on Linux\n";
        exit 1;
    }
    $scratchdir="/scratch";
    $template=$pdktemplate;
}
if ($ENV{TMP} =~ /^\/scratch/) {
    $scratchdir=$ENV{TMP};
}
else {
    $scratchdir .= "/$user";
}
if ($fixed_target ) {
    if ($branch eq "") {
        $ROOT_TARGET_DIR .= "/$aname";
    }
    else {
        $ROOT_TARGET_DIR .= "/$branch/$aname";
    }
}
else {
    my $cleanup = ( ! $keep_target );
    mkdir "$scratchdir" if ( ! -d "$scratchdir" );
    $ROOT_TARGET_DIR=tempdir("$scratchdir/target.XXXXXX", CLEANUP => $cleanup);
}
if (! $fixed_project ) {
    my $cleanup = ( ! $keep_project );
    `mkdir -p "$scratchdir"` if ( ! -d "$scratchdir" );
    $ROOT_PROJECT_DIR=tempdir("$scratchdir/project.XXXXXX", CLEANUP => $cleanup);
    $force = 1; # always force removal and build when temporaries used
}
if ( "$BUILD_SYSTEM_ROOT" eq "/p/rrc/tools/fulcrum/build-system" and $user eq "tsbuild") {
    # make sure the build system is up to date
    system "P4CLIENT=tsbuild-build-system $p4cmd sync >/dev/null 2>\&1";
}
$ENV{PATH} = "/usr/intel/bin:/bin:/usr/bin:/p/rrc/tools/bin:$ENV{PATH}";
my $makelinks="";
$makelinks="--links" if $links and ( ! ($ROOT_PROJECT_DIR =~ m:^/scratch:));
my $makecmd="make -f '$BUILD_SYSTEM_ROOT/Makefile' 'ROOT_PROJECT_DIR=$ROOT_PROJECT_DIR' 'ROOT_TARGET_DIR=$ROOT_TARGET_DIR' 'BUILD_SYSTEM_ROOT=$BUILD_SYSTEM_ROOT' MAKELINKS=$makelinks";
# arg checking
if ( ! -d "$BUILD_SYSTEM_ROOT" ) {
    usage ("build-system-root $BUILD_SYSTEM_ROOT does not exist");
}
if ( ! -d "$ROOT_PROJECT_DIR" and ! ( mkdir "$ROOT_PROJECT_DIR" ) ) {
    system "mkdir -p '$ROOT_PROJECT_DIR'";
}
if (! -d "$ROOT_PROJECT_DIR" ) {
    print STDERR "$ROOT_PROJECT_DIR does not exist and cannot be created.\n";
    exit 1;
}
if ($pdkbuild && $dosync) {
    # this seems silly, but the %1 syntax is really really slow
    local(*X);
    local($_);
    my $dir;
    my $dirlist="";
    open (X, "$p4cmd dirs //mrl/pdk/\\\* |");
    while (<X>) {
        chomp;
        s:.*/::;
        $dir = $_;
	$dirlist .=
            sprintf "\t//mrl/pdk/%s/... //CLIENTNAME/pdk/%s/...\n",
                $dir,$dir;
    }
    close X;
    $template =~ s:PDKDIRS:$dirlist:;
}
my $build_client="build-temp-$hostname-$$";
$build_client="build-temp-$branch-$hostname-$$" if $branch ne "";
if ($dosync) {
    # create a temporary client
    $template =~ s:BUILD_PACKAGE_ROOT:$ROOT_PROJECT_DIR:;
    $template =~ s:CLIENTNAME:$build_client:g;
    $template =~ s:/sw/intel/:/sw/$branch/:g if $branch ne "";
    open (P, "| $p4cmd client -i");
    print P "$template\n";
    close P;
}
if (defined ($argchange) and $dosync and ! $overwrite) {
    $headchange = getheadchange ($p4cmd) unless defined ($headchange);
    if ($argchange > $headchange) {
        usage "specified change, $argchange, greater then head change $headchange";
    }
}
if (defined ($argchange)) {
    $headchange = $argchange;
}
if (defined ($argchangedate)) {
    $headchange = getchangefromdate ($argchangedate);
}
if (! defined ($headchange)) {
    $headchange = getheadchange ($p4cmd);
}
my $buildid="$aname-$headchange-official";
$buildid="$aname-${branch}_$headchange-official" if $branch ne "";
if ($branch eq "") {
    $makecmd .= " INTEL=1 BUILD_CHANGE_NUMBER=$headchange FULCRUM_BUILD_ID=$buildid FULCRUM_RESULTS_DIR=$PACKAGE_STORAGE";
}
else {
    $makecmd .= " INTEL=1 BUILD_CHANGE_NUMBER=${branch}_$headchange FULCRUM_BUILD_ID=$buildid FULCRUM_RESULTS_DIR=$PACKAGE_STORAGE";
}
if ( ! -d "$ROOT_TARGET_DIR") {
    if ( ! mkdir "$ROOT_TARGET_DIR" ) {
        system "mkdir -p '$ROOT_TARGET_DIR' >/dev/null 2>\&1";
    }
}
if ( ! -d "$ROOT_TARGET_DIR") {
    die "Failed to create $ROOT_TARGET_DIR.\n";
}
my $logname="log.$headchange.$$.$aname-$$.log";
$logname="log.${branch}_$headchange.$$.$aname-$$.log" if $branch ne "";
if (defined ($log) and -d "$log" and -r "$log") {
    $log = "$log/$logname";
    $log =~ s://:/:g;
}
$log=$logname
    unless defined ($log);
system "touch $log";
if ( ! -w "$log" ) {
    warn "Warning: Logfile $log cannot be opened, changing log to /dev/null";
    $log = "/dev/null";
}
logit $log;
if ($force and ! $keep_project) {
    logit "Removing project tree at $ROOT_PROJECT_DIR";
    system "rm -rf $ROOT_PROJECT_DIR/* >/dev/null 2>\&1";
}
$p4cmd = "P4CLIENT=$build_client $p4cmd";
if ($dosync) {
    logit "Doing p4 sync \@$headchange";
    if ($force and $fixed_project) {
        system "$p4cmd sync -f \@$headchange >> $log 2>&1";
    }
    elsif ($force) {
        system "$p4cmd sync -p \@$headchange >> $log 2>&1";
    }
    elsif ($keepall) {
        system "$p4cmd sync \@$headchange >> $log 2>&1";
    }
    else {
        system "$p4cmd sync -p \@$headchange >> $log 2>&1";
    }
}
else {
logit "skipping p4 sync \@$headchange";
}
# remove the temporary client
if ( $dosync and ! $keepall ) {
    logit "Removing temporary client";
    system "$p4cmd client -d $build_client";
}
if ( ! $keep_target ) {
    logit "Removing target files";
    system "rm -rf $ROOT_TARGET_DIR/* >/dev/null 2>\&1";
}
logit "Making target packages";
my $package_make_count=0;
my $package_found_count=0;
my %packages_found;
open (P, "find '$ROOT_PROJECT_DIR' -follow -name '*.package' |");
while (<P>) {
    chomp;
    s/\.package$/.tar.gz/;
    s:$ROOT_PROJECT_DIR:$ROOT_TARGET_DIR:;
    my $target=$_;
    my $target_package = $target;
    $target_package =~ s/.*\///;
    $target_package =~ s/.tar.gz$//;
    if ($target_package eq "build-system") {
        next; # for whatever reason, the build system does not build this way, for now
    }
    if (defined ($donotbuild{$target_package}) && $donotbuild{$target_package}) {
        next; # list of do not build from config file
    }
    if ( $pdkbuild and ! ( $target_package =~ m/^fulcrum-[^-\/]+-pdk$/)) {
        next;
    }
    if ($target_package =~ /-safe$/) {
        next; # no sense in building -safe packages now
    }
    if ((!defined ($packages_to_make) and !defined ($pdknobuild{$target_package}))
            or defined ($packages_to_make{$target_package})) {
        unlink $target;
        my @f=split (/\//,$target);
        my $file=$f[$#f];
        my $targetdir=$target;
        $targetdir =~ s/\/[^\/]+$//;
        if ($branch eq "") {
            $file =~ s/.tar.gz/-$aname-$headchange-official.tar.gz/;
        }
        else {
            $file =~ s/.tar.gz/-$aname-${branch}_$headchange-official.tar.gz/;
        }
        $packages_found{$target_package} = 0;
        if ( -f "$PACKAGE_STORAGE/$file" and ! $overwrite) {
            print STDERR "$PACKAGE_STORAGE/$file already exists\nOverwriting prohibitted.\n";
        }
        else {
            my $target_spec = $target;
            $target_spec =~ s/.tar.gz/-package/;
            logit "Making $target_spec";
            if ($pdkbuild) {
                print STDERR "$pdksg \"$makecmd $target_spec\" >> $log 2>&1\n" if ($verbose);
                system "$pdksg \"$makecmd $target_spec\" >> $log 2>&1";
            }
            else {
                print STDERR "$makecmd $target_spec >> $log 2>&1\n" if ($verbose);
                system "$makecmd $target_spec >> $log 2>&1";
            }
            if ( -f "$target" ) {
                if (! -d "$PACKAGE_STORAGE" ) {
                    mkdir "$PACKAGE_STORAGE";
                }
                if (! -d "$PACKAGE_STORAGE" or ! -r "$PACKAGE_STORAGE" ) {
                    print STDERR "Package Storage location $PACKAGE_STORAGE does not exist.\n";
                    exit 1;
                }
                if ( -f "$PACKAGE_STORAGE/$file") {
                    $packages_found{$target_package}=1;
                    $package_make_count++;
                }
                else {
                    print STDERR "Copy of $target to $PACKAGE_STORAGE/$file failed.\n";
                }
            }
            else {
                logit "Failed $target";
            }
        }
        $package_found_count++;
    }
}
close P;
if ($package_found_count == 0) {
    print STDERR "No packages found to build\n";
    cleanup;
    exit 1;
}
if ($package_make_count == 0) {
    print STDERR "No packages successfully built.\n";
    cleanup;
    exit 1;
}
if ($package_make_count != $package_found_count) {
    foreach my $tgt (sort keys %packages_found) {
        if ($packages_found{$tgt} == 0) {
            print STDERR "$tgt failed to build.\n";
        }
    }
    cleanup;
    exit 1;
}
cleanup(0);
if ( ! $isqrsh ) {
    print "Waiting..." if $verbose;
    while (wait != -1) {};
    print "\n" if $verbose;
}
$waitclean = 1;

# do not install from explicit qrsh
exit 0 if $isqrsh;

#newinstall.pl
my $newpreload = <<EP;
if [ "\$arch_type" = "x86_64" ]; then
    preload_libs=/usr/lib64/libncurses.so
else
    preload_libs=/usr/lib/libncurses.so
fi
EP

sub fixjava {
    my ($script)=@_;
    local(*P,*O,$_);
    open (P, "<$script");
    my $fix=0;
    my @lines=();
    while (<P>) {
        chomp;
        push @lines, $_;
        $fix=1 if /^preload_libs/;
    }
    close P;
    if ($fix) {
        open (O, ">$script.tmp");
        select O;
        foreach my $line (@lines) {
            $_=$line;
            if ($fix and /^arch_type=/) {
                print "$_\n";
                print "\n";
                print "$newpreload\n";
                next;
            }
            print "\n";
        }
        select STDOUT;
        close O;
        system "touch -r $script $script.tmp; chmod 775 $script.tmp";
        unlink ("$script");
        link ("$script.tmp", "$script");
        unlink ("$script.tmp");
    }
    undef @lines;
}

##
print STDERR "Installing...\n" if $verbose;
foreach my $toolname (sort {$b cmp $a} keys %packages_found) {
    print STDERR "Installing $toolname...\n" if $verbose;
    $toolhome =~ s:/$::;
    $toolhome =~ s:/(tools|packages|pdk)$::;
    $toolhome .= "/tools" if ( ! ( $toolhome =~ m:/tools$: ));
    system "mkdir -p $toolhome" if ! -d $toolhome;
    my $PACKAGE_STORAGE=$toolhome if ! $package_storage_set;
    $PACKAGE_STORAGE =~ s:/$::;
    $PACKAGE_STORAGE =~ s:/(tools|packages|pdk)$::;
    $PACKAGE_STORAGE .= "/packages" if ( ! ( $PACKAGE_STORAGE =~ m:/packages$: ));
    my $myarch=`uname -sm | sed -e 's/ /-/'`;
    chomp $myarch;
    my $pkgcnt=0;
    my $pdk=0;
    $pdk = 1 if $toolname =~ /-pdk$/;
    $toolhome =~ s:/tools$:/pdk: if $pdk;
    if (defined ($targetarch) and ! $pdk ) {
        my @tmparch = split(/,/,$targetarch);
        foreach my $a (@tmparch) {
            if ( ! defined ($legalplatforms{$a})) {
                usage "Illegal Architecture $a";
            }
        }
        @arches=sort { $b cmp $a } @tmparch;
        @qarch=();
        foreach my $a (@arches) {
            push @qarch, $legalplatforms{$a};
        }
    }
    foreach my $a (@arches) {
        my $f="$PACKAGE_STORAGE/${toolname}-${a}-${headchange}-official.tar.gz";
        $f="$PACKAGE_STORAGE/${toolname}-${a}-${branch}_${headchange}-official.tar.gz"
            if ($branch ne "");
        if ( ! -f "$f") {
            print STDERR "No install file found! $f\n" if !$pdk;
        }
        else {
            $pkgcnt++;
        }
    }
    if ( ($pkgcnt != $#arches+1 and !$pdk) or ($pkgcnt != 1 and $pdk) ) {
        exit 1
    }
    my $installdir="$toolhome/$toolname/$headchange";
    $installdir="$toolhome/$toolname/${branch}_$headchange"
        if $branch ne "";
    if ( -d "$installdir") {
        if ( $overwrite ) {
            system "/bin/rm -rf '$installdir'";
        }
        else {
            print STDERR "$installdir exists, overwrite not specified\n";
            exit 1;
        }
    }
    # DON'T USE OVERWRITE HERE!
    my $installcmd="$packageinstall";
    $installcmd .= " --strip" if $strip;
    $installcmd .= " --verbose" if $verbose;
    foreach my $n (0..$#arches) {
        my $arch=$arches[$n];
        my $file="${toolname}-${arch}-${headchange}-official.tar.gz";
        $file="${toolname}-${arch}-${branch}_${headchange}-official.tar.gz"
            if $branch ne "";
        my $package = "$PACKAGE_STORAGE/$file";
        if ( -f "$package") {
            print STDERR "installing $package for $arch\n" if $verbose;
            if ($pdk) {
                print "$installcmd $package $toolhome\n" if $verbose;
                if ($pdkbuild) {
                    system "$pdksg \"$installcmd $package $toolhome\"";
                }
                else {
                    system "$installcmd $package $toolhome";
                }
            }
            else {
                if ( "$myarch" eq "$arch" ) {
                    print STDERR "$installcmd $package $toolhome\n"
                        if $verbose;
                    if ($pdkbuild) {
                        system "unset -v DISPLAY; $pdksg \"$installcmd $package $toolhome\"";
                    }
                    else {
                        system "unset -v DISPLAY; $installcmd $package $toolhome";
                    }
                }
                else {
                    $ENV{QRSH_FLAGS}="-l a=$qarch[$n],mem=$mem48";
                    $ENV{QB_RUN_NAME}="package-inst";
                    print STDERR "$installcmd $package $toolhome\n"
                        if $verbose;
                    if ($pdkbuild) {
                        system "unset -v DISPLAY; $pdksg \"$installcmd $package $toolhome\"";
                    }
                    else {
                        system "unset -v DISPLAY; $installcmd $package $toolhome";
                    }
                }
            }
            unlink "$toolhome/$file";
        }
    }
    sleep 1;
    # temporary fixups while co-existing old and new fulcrum
    my $rootdir="$toolhome/$toolname/$headchange";
    $rootdir="$toolhome/$toolname/${branch}_$headchange"
        if $branch ne "";
    my $bindir = "$rootdir/bin";
    opendir (D, "$bindir");
    my @files=grep (/(jdsim|client|server)/, readdir (D));
    closedir D;
    foreach my $file (@files) {
        fixjava "$bindir/$file";
    }
    if ( open (P, "<$bindir/cdsp4sync") ) {
        my @lines=();
        my $redo=0;
        while (<P>) {
            chomp;
            if (/^package_root.*dirname.*package_root/) {
                $_ = "#$_";
                $redo=1;
            }
            push @lines, $_;
        }
        close P;
        if ($redo) {
            open (P, ">$bindir/cdsp4sync.tmp");
            foreach my $l (@lines) {
                print P "$l\n";
            }
            close P;
            system "touch -r '$bindir/cdsp4sync' '$bindir/cdsp4sync.tmp'; chmod 775 '$bindir/cdsp4sync.tmp';/bin/mv '$bindir/cdsp4sync.tmp' '$bindir/cdsp4sync'";
        }
    }
    if ( ! $pdk) {
        foreach my $arch (@arches) {
            system "touch '$rootdir/.installed-$arch'"
                if ( -d "$rootdir/$arch/bin");
        }
    }
    system "touch '$rootdir/.installed'";
}

# done installing, finally re-index the stuff

if ($toolhome =~ m:^$TOOLS_HOME_DEFAULT: ) {
    foreach my $arch ( "fedora","solaris8","fedora-x86_64") {
        system "P4USER=$p4user P4CONFIG= P4CLIENT=system-$arch-fulcrum-config p4 sync 2>/dev/null";
        system "P4USER=$p4user P4CONFIG= P4CLIENT=system-$arch-fulcrum p4 sync 2>/dev/null";
    }
}
foreach my $arch (@allqarch) {
    print STDERR "$arch\n" if $verbose;
    $toolhome =~ s:/(pdk|tools)$::;
    my $cmd="$updatefmdb --toolhome $toolhome >/dev/null";
    $cmd="$updatefmdb --toolhome $toolhome --branch $branch >/dev/null"
        if $branch ne "";
    if (! fork ) {
        if ($arches{$arch} ne "$aname") {
            $ENV{QRSH_FLAGS}="-cwd -l a=$arch,mem=$mem48";
            $ENV{QB_RUN_NAME}="updatefmdb";
            $cmd="unset -v DISPLAY; $cmd";
        }
        print STDERR "$cmd\n" if $verbose;
        exec "$cmd";
    }
}
while (wait != -1) {};
