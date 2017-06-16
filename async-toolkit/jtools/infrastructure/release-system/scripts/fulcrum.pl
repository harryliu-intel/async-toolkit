#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use FindBin;

my $top = '/nfs/sc/proj/ctg/mrl108/mrl/tools';
BEGIN {
    my $correctitools="$FindBin::Bin/.itools";
    $correctitools="$top/bin/.itools" unless -e $correctitools;
    if ( $ENV{USER_ITOOLS} ne $correctitools) {
        $ENV{USER_ITOOLS}=$correctitools;
        exec "$0",@ARGV;
    }
}

use Getopt::Long qw(:config require_order); # allows tool options not to be decoded
use DB_File;

if(not defined $ENV{FULCRUM_NB_CONFIG}) {
  if ( (not defined  $ENV{NBPOOL}) || (not defined $ENV{NBQSLOT}) || (not defined $ENV{NBCLASS}) ) {
      $ENV{FULCRUM_NB_CONFIG}="/nfs/sc/proj/ctg/mrl108/mrl/tools/local/fulcrum_nb.config";
  }
}

# config file
my $configfile = $ENV{FULCRUMCONFIG};
$configfile = ".fulcrum.config" unless defined $configfile;
my $configpath;
my %config;
my %execonfig;
my %fconfig;
my $argfindpackage;
my $pedantic=-1;
my $argverbose=0;
my $isOA=0;
my %projectversions=();
sub setNBPOOL {
  return if not defined $ENV{FULCRUM_NB_CONFIG};
  return if not -e $ENV{FULCRUM_NB_CONFIG};
  if (open (P, "<$ENV{FULCRUM_NB_CONFIG}")) {
    while(<P>){
      chomp;
      next if (/^#/);      
      if(/NBPOOL=(\S+)/){
        $ENV{NBPOOL}=$1;
        last;
      }
      if(/(CLASS_DEFAULT|NBCLASS)=(\S+)/){
        $ENV{NBCLASS}=$2;
        last;
      }
    }
  }
}



sub gettmp {
    return if defined $ENV{TMP} and -d $ENV{TMP} and -w $ENV{TMP};
    my $fd;
    opendir ($fd, "/scratch/");
    my @list=sort ( grep (/\.q$/, readdir ($fd)));
    closedir $fd;
    my $tmp=undef;
    my $c=0;
    foreach my $dir (sort @list) {
        if ( -d "/scratch/$dir" and -w "/scratch/$dir") {
            $tmp="/scratch/$dir";
            $c++;
        }
    }
    # only use this if there is no ambiguity
    if (defined ($tmp) and $c == 1) {
        $ENV{TMP}=$tmp;
    }
}

sub sortcmpchg {
    my $na = $a;
    my $nb = $b;
    $na =~ s/.*://;
    $nb =~ s/.*://;
    $na =~ s/.*_//;
    $nb =~ s/.*_//;
    $na - $nb;
}

sub cmpchg {
    my ($a, $b) = @_;
    $a =~ s/.*_//;
    $b =~ s/.*_//;
    $a - $b;
}

sub fixargs {
    my (@f)=@_;
    my $arg="";
    foreach my $f (@f) {
        if ($f =~ m/"/) {
            $arg .= " '$f'";
        }
        else {
            $arg .= " \"$f\"";
        }
    }
    substr($arg,1);
}

sub findconfig {
    my ($configfile) = @_;
    my $configpath;
    local ($");
    if ($configfile =~ m:/:) {
        $configpath = $configfile;
    }
    else {
        my $pwd = `pwd`;
        chomp $pwd;
        my @pwd = split(/\//, $pwd);
        $" = "/";
        while ($#pwd > 0) {
            $pwd = "@pwd";
            if (($pwd =~ m:^/mnt: and $#pwd <= 3)) {
                last;
            }
            if ( -r "$pwd/$configfile" && -f "$pwd/$configfile") {
                $configpath="$pwd/$configfile";
                last;
            }
            pop @pwd;
        }
    }
    $configpath = "$ENV{HOME}/$configfile"
        if (! defined ($configpath) and 
            -r "$ENV{HOME}/$configfile" and -f "$ENV{HOME}/$configfile");
    $configpath;
}

my $argexe;
my %exelist;

sub readconfig {
    my ($configpath) = @_;
    if (!defined ($configpath)) {
        return;
    }
    local (*P);
    getexelist();
    my @f;
    my $found=0;
    my $foundfulcrum=0;
    my $block=0;
    my $blocklvl=0;
    my $ln=0;
    if (open (P, "<$configpath")) {
        while (<P>) {
            chomp;
            $ln++;
            # comments
            s/^\s*//;
            s/\s*$//;
            if (/^[;#]/) {
                next;
            }
            # blank lines
            if (/^$/) {
                next;
            }
            if (/\x7b$/) {
                my $neg=0;
                if (/^-/) {
                    $neg=1;
                    s/^-\s*//;
                }
                @f=split (/[^-_a-zA-Z0-9]/,$_);
                if ($#f < 0) {
                    print STDERR "Warning: No exe's specified in config line $ln";
                }
                $block=$ln;
                $blocklvl++;
                $found=$neg;
                foreach my $f (@f) {
                    if ($f eq $argexe) {
                        $found = ! $neg;
                    }
                    if ($f eq "fulcrum") {
                        $foundfulcrum=1;
                    }
                }
                next;
            }
            if (/\x7d$/) {
                $block=0;
                $blocklvl--;
                $found=0;
                if ($blocklvl < 0) {
                    print STDERR "Warning: Config file block not correctly formed line $ln in $configpath";
                }
                next;
            }
            @f = split(/[ =]/,$_, 2);
            if (defined ($f[1])) {
                # remove trailing white space
                $f[1] =~ s/[ \t\r\n]*$//;
            }
            $f[1] = '=' if (($f[1] eq "") and /=/);
            if (! $block ) {
                $config{$f[0]}=$f[1];
            }
            if ($block and $found) {
                push @{$execonfig{$f[0]}},$f[1];
            }
            if ($block and $foundfulcrum) {
                $fconfig{$f[0]}=$f[1];
            }
        }
        close P;
    }
    if ($blocklvl != 0) {
        print STDERR "Warning: Config file block(s) not correctly formed in $configpath";
    }
    if ($block != 0) {
        print STDERR "Config file block at line $block of $configpath not closed";
    }
}

my $cadencewrapperdir=$ENV{'FULCRUM_WRAPPER_DIR'};
$cadencewrapperdir="/nfs/sc/proj/ctg/mrl108/mrl/tools/bin" unless $cadencewrapperdir;
$ENV{'FULCRUM_WRAPPER_DIR'}=$cadencewrapperdir;
my $javapath = "/usr/intel/pkgs/java/1.6.0.10-64/bin";
warn "No JAVA found $javapath" if (! -d $javapath);
$ENV{PATH}="/usr/intel/bin:$cadencewrapperdir:/usr/ucb:/bin:/usr/bin:$javapath";

my %cadencewrappers = ();

my %notwrapper=(
    "license" => 1,
    "waveview" => 1,
    "deprecated" => 1,
    "SpaceCruiser" => 1,
);

opendir (D, $cadencewrapperdir);
my @c=readdir(D);
closedir D;
foreach my $c (@c) {
    if (! ($c =~ /\./) and -x "$cadencewrapperdir/$c" and -l "$cadencewrapperdir/$c" ) {
        $cadencewrappers{$c}=1 if ! $notwrapper{$c};
    }
}

my @cadence_wrappers;

sub setwrapperenvs {
    my %wf=();
    foreach my $cad (@cadence_wrappers) {
        my $name = $cad;
        $name =~ s:$cadencewrapperdir::;
        $name =~ s:/::;
        $name =~ s/\..*//;
        next if (($name =~ /_oa$/) and ! $isOA);
        $name =~ s/_oa$//;
        $wf{$name}=1;
        $name = uc $name;
        $ENV{$name."_SCRIPT"}=$cad;
        print "${name}_SCRIPT=$cad" if $argverbose;
    }
    foreach my $name (sort keys %cadencewrappers) {
        next if (($name =~ /_oa$/) and ! $isOA);
        $name =~ s/_oa$//;
        if (! $wf{$name}) {
            $ENV{uc($name)."_SCRIPT"}="$cadencewrapperdir/$name";
            print uc($name)."_SCRIPT=$cadencewrapperdir/$name" if $argverbose;
        }
    }
}

sub iAmCadence {
    my ($arg,@args) = @_;
    my $abrarg = $arg;
    $abrarg =~ s/\..*//;
    (defined ($cadencewrappers{$abrarg}) and $cadencewrappers{$abrarg} == 1 and
        $#args >= 0 and -x "$cadencewrapperdir/$arg" );
}

# directories
my $fulcrum="$top/fulcrum";
my $defaultfulcrum = $fulcrum;
my $argtoolhome = "$fulcrum/tools";
my $defaulttoolhome = $argtoolhome;
my $argpdkhome = "$fulcrum/pdk";
my $defaultpdkhome = $argpdkhome;
my $argafter="";
my $argpdkafter="";
my $argbefore="";
my $argpdkbefore="";
my $argpdkrelease=0;
my $argpdkset=0;
# architecture directory name
my $sname=`uname -s`;
chomp $sname;
my $mname=`uname -m`;
chomp $mname;
my $archname="$sname-$mname";
# misc hashes, including for databases above
my %all;
my %pdkall;
my %prefs;
my %project;
my %pdkproject;
my %exe;
# args decoded values
my $arglist;
my $arglatest=0;
my $argpdklatest=0;
my $argdevelop=0;
my $argrelease=1;
my $argbeta=0;
my $branch="";
my $argpdkbeta=0;
my $argall = 0;
my $argdoc=0;
my @f;
my @list;
my $argproject="";
my $argpdkproject="";
my $argpdk;
my $argwhich;
my $argwhatis=0;
my $argman=0;

# detects the presence of the fulcrum-pdk-root argument in the tool
sub usespdk {
    my ($file)=@_;
    local($/,$_);
    my $pdkroot = 0;
    local(*P);
    open (P, "<$file");
    $_=<P>;
    close P;
    if (/fulcrum-pdk-root/) {
        $pdkroot=1;
    }
    $pdkroot;
}

sub usesarg {
    my ($file,$arg)=@_;
    local ($/,$_,*P);
    open (P,"<$file");
    $_=<P>;
    close P;
    if (/$arg/) {
        return 1;
    }
    0;
}

# result of --version
sub printversion {
    my $vs='$Id$';
    my $dt='$DateTime$';
    $vs =~ s/.*#//;
    $vs =~ s/ .*$//;
    my @dt = split (/ /,$dt);
    my $ex = $0;
    $ex =~ s/\/+/\//g;
    $ex =~ s/.*\///;
    print STDERR <<EV;
$ex
    Version #$vs $dt[1] $dt[2]
EV
exit 0;
}

# need to specify %options so that usage can use the list
# but the actual options are specified later.
my %options;
my %exclusive = (
   "after" => "\$argafter",
   "before" => "\$argbefore",
   "beta" => "\$argbeta",
   "latest" => "\$arglatest",
   "develop" => "\$argdevelop",
   "project" => "\$argproject",
   "release" => "\$argrelease",
);

# prints Usage message. Incorporates automatically all specified
# options even if I forgot to include them in this message.
sub usage {
    my($msg)=@_;
    if (defined ($msg) and $msg ne "help") {
        print STDERR "Error: ".$msg."\n";
        exit 1;
    }
    my @usage=(
"    --all              : used in conjunction with --list to see all changes",
"    --beta             : use latest beta package",
"    --branch <branch>  : specify sw or pdk branch from which pkg built",
"    --cadence <list>   : list of versions to use (e.g. assura.30.8)",
"    --wrapper <list>   : identical to --cadence",
"    --config file      : fulcrum config file",
"    --develop          : run bypassing regression checking",
"    --doc              : web pages of documentation",
"    --help             : print this message",
"    --whatis           : search a fulcrum 'whatis'",
"    --wiki             : brings up wiki page for command'",
"    --latest           : run the latest build, nightly or triggered",
"    --list             : list available tools and packages",
"    --package name     : Finds the released or latest package path",
"    --path             : Finds the path to the specified tool or pdk",
"    --pedantic[=0|1|2] : Forces some Java programs to reject illegal args",
"    --pdk name         : Finds the pdk for the specified process",
"    --pdk-project name : overrides --project for pdk",
"    --pdk-release      : overides --release for pdk",
"    --pdk-after <chg>  : overides --after for pdk",
"    --pdk-before <chg> : overides --before for pdk",
"    --pdk-beta         : overides for --beta for pdk",
"    --pdk-latest       : overides for --latest for pdk",
"    --pdkhome dir      : overrides default $defaultfulcrum for pdk",
"    --project name     : Uses the project defaults rather than release defaults",
"    --release          : use latest released package",
"    --toolhome dir     : default $defaultfulcrum",
"    --verbose          : a little detail is given",
"    --version          : Shows the version and date info of fulcrum",
"    --which            : tells path to the selected executable",
"    --after <chg>      : Tells fulcrum to find change or date immediately after or equal chg\n".
"                         <chg> is either change nr or datetime",
"    --before <chg>     : Tells fulcrum to find change or date immediately before or equal chg",
);
    printf STDERR <<EU;
Usage: fulcrum [script args] [package:]tool [tool args]
    script args:
EU
    foreach my $opt (sort keys %options) {
        $opt =~ s/[|=:].*//;
        my $found=0;
        foreach my $n (0..$#usage) {
            my @f=split(/[ \[]+/,$usage[$n]);
            if (defined ($f[1]) and "--$opt" eq "$f[1]" ) {
                printf STDERR "$usage[$n]\n";
                $found=1;
                $usage[$n]="";
            }
        }
        if (!$found) {
            printf STDERR "    --%-17.17s: unknown\n", $opt;
        }
    }
    printf STDERR "    ";
    foreach my $opt (sort keys %exclusive) {
        printf STDERR "--$opt ";
    }
    print STDERR "";
print STDERR <<EP;
       Are all mutually exclusive.
   Exception: pdkafter and pdkbefore can be used in conjunction with any of the above.
   Command line arguments override personal fulcrum config file entries.
EP
    exit 1;
}

# for invokation of Cadence tools

my %cadence;
my %notcadence = (
"lic_error" => 1,
"lmdiag" => 1,
"lmdown" => 1,
"lmgrd" => 1,
"lmhostid" => 1,
"lmremove" => 1,
"lmreread" => 1,
"lmstat" => 1,
"lmswitchr" => 1,
"lmver" => 1,
"lmutil" => 1,
"cdsbd" => 1,
);


# convert a date-time to a change number

sub p4d2c {
    my($date,$before)=@_;
    local(*P);
    my($year,$month,$day,$hour,$min,$sec)=split(/[-\/:]/, $date);
    if (!defined ($hour)) {$hour = 0;}
    if (!defined ($min)) {$min = 0;}
    if (!defined ($sec)) {$sec = 0;}
    my $lastchange;
    $before = 0 if ! defined ($before);

    if (!defined ($year)) {
        $year = 3000; #result is head change
    }
    my $datetime=sprintf ("%04d/%02d/%02d %02d:%02d:%02d",
        $year, $month,$day,$hour,$min,$sec);

    open (P, "p4 changes |");
    # reads changes in reverse order
    while (<P>) {
        chomp;
        my @f=split;
        my $change=$f[1];
        my $cdate="$f[3] $f[4]";
        if ($cdate le $datetime) {
            system "pkill p4 -u `whoami` -P $$";
            close P;
            if ($before or $cdate eq $datetime or !defined ($lastchange)) {
                return $change;
            }
            else {
                return $lastchange;
            }
            last;
        }
        $lastchange = $change;
    }
    close P;
    0; # earlier than any change
}

my $settoolhome = 0;
my $setpdkhome = 0;
my $setpath = 0;
my $argpath=0;
my $argcadence="";
my %fulcrumchangeargs = (
    "main" => "",
    "pdk" => "",
    "cadence" => "",
    "toolhome" => "",
    "pdkhome" => "",
    "branch" => "",
    "config" => "",
);

sub clearmain {
   $argafter="";
   $argbefore="";
   $argbeta=0;
   $arglatest=0;
   $argdevelop=0;
   $argproject="";
   $argrelease=0;
}

sub clearpdk {
   $argpdkafter="";
   $argpdkbefore="";
   $argpdkbeta=0;
   $argpdklatest=0;
   $argpdkproject="";
   $argpdkrelease=0;
}

my $pdkset=0;
my $branchset=0;
my $cmdline=1;

%options = (
    "beta" => sub { clearmain; $argbeta=1; $fulcrumchangeargs{main} = "--beta";},
    "after=s" => sub { clearmain; $argafter=$_[1]; $fulcrumchangeargs{main} = "--after=$_[1]";},
    "before=s" => sub { clearmain; $argbefore=$_[1]; $fulcrumchangeargs{main} = "--before=$_[1]";},
    "branch=s" => sub {
        $branch=$_[1];
        if (($cmdline or ! $branchset) and ( $_[1] eq "main" )) {
            $branch="";
        } else {
            $fulcrumchangeargs{branch} = "--branch=$_[1]";
        };
        $branchset=$cmdline;},
    "doc" => \$argdoc,
    "cadence=s" => sub { $argcadence=$_[1]; $fulcrumchangeargs{cadence} = "--cadence=$_[1]";},
    "wrapper=s" => sub { $argcadence=$_[1]; $fulcrumchangeargs{cadence} = "--cadence=$_[1]";},
    "config=s" => sub { $configfile=$_[1]; $fulcrumchangeargs{config} = "--config=$_[1]";},
    "help" => \&usage,
    "latest" => sub { clearmain; $arglatest = 1; $fulcrumchangeargs{main} = "--latest";},
    "wiki" => \$argman,
    "develop" => sub { clearmain; $argdevelop = 1; $fulcrumchangeargs{main} = "--develop";},
    "all" => \$argall,
    "list" => \$arglist,
    "path" => sub { $argpath = 1; $argwhich = 1; $setpath = 1;},
    "pedantic:1" => \$pedantic,
    "pdk=s" => sub {
        $argpdk=$_[1];
        if (($cmdline or ! $branchset) and ($_[1] =~ /tsmc[56]5/)) {
            $branch="alta";
            $fulcrumchangeargs{branch} = "--branch=alta";
        }
        $pdkset=$cmdline;},
    "pdk-after=s" => sub { clearpdk; $argpdkset=1; $argpdkafter = $_[1]; $fulcrumchangeargs{pdk} = "--pdk-after=$_[1]";},
    "pdk-before=s" => sub { clearpdk; $argpdkset=1; $argpdkbefore = $_[1]; $fulcrumchangeargs{pdk} = "--pdk-before=$_[1]";},
    "pdk-latest" => sub { clearpdk; $argpdkset=1; $argpdklatest = 1; $fulcrumchangeargs{pdk} = "--pdk-latest";},
    "pdk-project=s" => sub { clearpdk; $argpdkset=1; $argpdkproject = $_[1]; $fulcrumchangeargs{pdk} = "--pdk-project=$_[1]";},
    "pdk-release" => sub { clearpdk; $argpdkset=1; $argpdkrelease = 1; $fulcrumchangeargs{pdk} = "--pdk-release";},
    "pdk-beta" => sub { clearpdk; $argpdkset=1; $argpdkbeta = 1; $fulcrumchangeargs{pdk} = "--pdk-beta";},
    "project=s" => sub { clearmain; $argproject = $_[1]; $fulcrumchangeargs{main} = "--project=$_[1]";},
    "release" => sub { clearmain; $argrelease = 1; $fulcrumchangeargs{main} = "--release";},
    "toolhome=s" => sub {
        $argtoolhome = $_[1];
        $settoolhome=1;
        if (! $setpdkhome ) {
            $argpdkhome = $argtoolhome;
        }
        $fulcrumchangeargs{toolhome} = "--toolhome=$_[1]";
    },
    "pdkhome=s" => sub {
        $argpdkhome = $_[1];
        $setpdkhome=1;
        $fulcrumchangeargs{pdkhome} = "--pdkhome=$_[1]";
    },
    "verbose|v!" => \$argverbose,
    "version" => \&printversion,
    "package=s" => \$argfindpackage,
    "whatis" => \$argwhatis,
    "which" => \$argwhich,
);

my @envoptions=split(/\|/,$ENV{FULCRUM_CHANGE_ARGS});
unshift @ARGV, @envoptions if @envoptions;

GetOptions (
    %options
) or usage;

if ($argpdk =~ /^fulcrum-(.*)-pdk$/) {
    $argpdk = $1;
}
if ($argman and defined $ARGV[0]) {
    my $bad = `wget -O - 'http://fddp/index.php/$ARGV[0]' 2>\&1 | grep -c 'There is currently no text in this page,'`;
    chomp $bad;
    if ($bad) {
        print STDERR "No Wiki exists for $ARGV[0]";
    }
    else {
        `firefox 'http://fddp/index.php/$ARGV[0]'`;
    }
    exit 1;
}
if ($argwhatis) {
    my $whatis = "$argtoolhome/../config/fulcrum.whatis";
    my $search = $ARGV[0];
    if (! -r "$whatis") {
        $whatis = "$defaulttoolhome/../config/fulcrum.whatis";
    }
    open (W, "<$whatis") or usage ("Cannot open whatis file $whatis");
    my $found=0;
    my $maxlen=0;
    my @what;
    while (<W>) {
        chomp;
        my ($tool,$descr)=split(/[\t ]/,$_, 2);
        if (/\Q$search\Q/i) {    
            $maxlen = length($tool) if $maxlen < length($tool);
            push @what, $_;
        }
    }
    close W;
    foreach my $line (@what) {
        my ($tool,$desc) = split(/\t/,$line);
        my $mark='';
        $mark='*' if $tool eq $search;
        my $istty=1;
        my $len = $maxlen;
        $len += 5 if (($tool eq $search) and $istty);
        printf "%s%-*.*s $desc\n",
        (($tool eq $search) and $istty) ? "\x1b[31m" : '',
        $len+2, $len+2,
        $tool.((($tool eq $search) and $istty) ? "\x1b[00m" : '').$mark;
    }
    exit 0;
}

if ($pedantic >= 0 and $pedantic <= 2) {
    $ENV{FULCRUM_PEDANTIC}=$pedantic;
    print STDERR "FULCRUM_PEDANTIC=$pedantic" if $argverbose;
}
$"="|";
my @env=();
foreach my $key (sort keys %fulcrumchangeargs) {
    if ($fulcrumchangeargs{$key} ne "") {
        push @env, "$fulcrumchangeargs{$key}";
    }
}
$ENV{FULCRUM_CHANGE_ARGS}="@env";
print STDERR "FULCRUM_CHANGE_ARGS=@env" if $argverbose;
$branch = "" if $branch eq "main";

my %default_wrappers = ("ic"=>0, "assura"=>1);

my $wrapper_key=10;
my %addedwrappers=();
$argexe = $ARGV[0];
$argexe =~ s/.*://;
$configpath = findconfig ($configfile);
readconfig ($configpath);
# hack to pre-read pdk and branch args
$cmdline=0;
foreach my $key (keys %config) { # stuff outside of any block
    if ($key =~ /^--/) {
        my $found = 0;
        my $kopt = $key;
        $kopt =~ s/^--//;
        next if $kopt ne "pdk" and $kopt ne "branch";
        next if $branchset and $kopt eq "branch";
        next if $pdkset and $kopt eq "pdk";
        foreach my $opt (keys %options) {
            my @f=split(/[|=:!]/, $opt);
            if ($f[0] eq $kopt) {
                my $arg;
                my $set;
                $found = 1;
                eval "\$arg = \$arg$f[0]";
                $config{$key} = 1 unless defined $config{$key};
                if ($options{$opt} =~ /CODE/) {
                    eval "\$set = \$set$f[0]";
                    eval "\$arg = \$arg$f[0]";
                    if (!$set) {
                        &{$options{$opt}}($kopt,$config{$key});
                        print "config sets $f[0] to $config{$key}" if $argverbose;
                    }
                    elsif ($arg ne $config{$key}) {
                        print "command line overrides config of $f[0] to $config{$key}" if $argverbose;
                    }
                }
            }
        }
    }
}
$cmdline=1;
if (defined ($config{"--toolhome"}) and ! $settoolhome and
        -d $config{"--toolhome"} ) {
    &{$options{"toolhome=s"}}("toolhome",$config{"--toolhome"});
    if ($config{"--pdkhome"} and ! $setpdkhome and -d $config{"--pdkhome"}) {
        &{$options{"pdkhome=s"}}("pdkhome",$config{"--pdkhome"});
    }
}
$argtoolhome =~ s:/$::;
$argtoolhome .= "/tools" unless $argtoolhome =~ m:/tools$:;
usage "toolhome $argtoolhome does not exist" unless -d "$argtoolhome";
$argpdkhome =~ s:/$::;
$argpdkhome .= "/pdk" unless $argpdkhome =~ m:/pdk$:;
usage "pdkhome $argpdkhome does not exist" unless -d "$argpdkhome";
$fulcrum = $argtoolhome;
$fulcrum =~ s:/tools$::;
$argtoolhome = "$fulcrum/tools";
my $configtop = "$fulcrum/config";
my $configdir = "$configtop/$archname";
$configdir = "$configtop/$branch/$archname" if $branch ne "";
usage "config $configdir does not exist" unless -d "$configdir";
my $defaultconfigdir = $configdir;
my $pdkconfigtop = $argpdkhome;
$pdkconfigtop =~ s:/$::;
$pdkconfigtop =~ s:/tools$::;
$pdkconfigtop =~ s:/pdk$::;
my $dottools="";
if ($argproject eq "") {
    if ($arglatest) {
        $dottools="$configdir/.latest";
    }
    else {
        $dottools="$configdir/.release";
    }
}
else {
    $dottools="$configdir/.$argproject";
}
if ( -s $dottools) {
    open (PV, "<$dottools");
    while (<PV>) {
        chomp;
        if (/^P:(\S+)\s+(\S+)/) {
            my $name=$1;
            my $version=$2;
            next if $version !~ /^$name\Q/;
            if ( -x "$cadencewrapperdir/$version") {
                $projectversions{$name}="$version";
            }
        }
    }
    my %ac=();
    my @ac=();
    foreach my $ac (split(/,/, $argcadence)) {
        push @ac, $ac;
        $ac =~ s/\..*//;
        $ac{$ac}=1;
    }
    foreach my $key (keys %projectversions) {
        if (! defined ($ac{$key})) {
            push @ac, $projectversions{$key};
        }
    }
    $argcadence=join(",", @ac);
    undef @ac;
    undef %ac;
}
my $pdkconfigdir = $pdkconfigtop;
# this should be more robust if a branched pdk exists, but for now it does not.
$pdkconfigdir .= "/config"; # handle legacy pdk develop dirs
if ( -d "$pdkconfigdir/$archname" ) {
    $pdkconfigdir .= "/$archname";
}
$pdkconfigtop .= "/config";
usage "pdkconfig $pdkconfigdir does not exist" unless -d "$pdkconfigdir";
# databases, compiled by updatefmdb
my $reldb = "$configdir/releasedb";
my $betadb = "$configdir/betadb";
my $latdb = "$configdir/latestdb";
my $alldb = "$configdir/alldb";
my $prefdb = "$configdir/prefdb";
 # there is some risk in this because some versions
 # of dbm do not support multiple open files at the
 # same time. It appears that both Solaris and Linux
 # are ok at this time 5/26/04 AAG
$alldb = "$configdir/develdb" if $argdevelop == 1;
my $pdkreldb = "$pdkconfigdir/releasedb";
my $pdkbetadb = "$pdkconfigdir/betadb";
my $pdklatdb = "$pdkconfigdir/latestdb";
my $pdkalldb = "$pdkconfigdir/alldb";
$pdkalldb = "$pdkconfigdir/develdb" if $argdevelop == 1;
print "config=$configpath" if $argverbose and $configpath;
print "configdir=$configdir" if $argverbose;
print "pdkconfig=$pdkconfigdir" if $argverbose and $configdir ne $pdkconfigdir;
my %configchange;

# deal with the config entries
if ($argproject ne "") {
    readconfig ("$configdir/$argproject");
}
foreach my $key (keys %config) { # stuff outside of any block
    if ($key =~ /^--/) {
        my $found = 0;
        my $kopt = $key;
        $kopt =~ s/^--//;
        foreach my $opt (keys %options) {
            my @f=split(/[|=:!]/, $opt);
            if ($f[0] eq $kopt) {
                my $arg;
                my $set;
                $found = 1;
                eval "\$arg = \$arg$f[0]";
                $config{$key} = 1 unless defined $config{$key};
                if ($options{$opt} =~ /SCALAR/) {
                    if (!defined ($arg) or $arg eq "") {
                        if ($exclusive{$f[0]} ) {
                            my $mask = 0;
                            foreach my $opt (sort keys %exclusive) {
                                if ($opt ne $f[0]) {
                                    eval "\$mask++ if $exclusive{$opt} ne ''";
                                }
                                if ($mask) {
                                    last;
                                }
                            }
                            if ($mask) {
                                print "command line overrides config of $f[0]";
                                next;
                            }
                        }
                        eval "\$arg$f[0] = \"$config{$key}\"";
                        print "config sets $f[0] to $config{$key}" if $argverbose;
                    }
                    elsif ($arg ne $config{$key}) {
                        print "command line overrides config of $f[0] to $config{$key}" if $argverbose;
                    }
                }
                elsif ($options{$opt} =~ /CODE/) {
                    eval "\$set = \$set$f[0]";
                    eval "\$arg = \$arg$f[0]";
                    if (!$set) {
                        &{$options{$opt}}($kopt,$config{$key});
                        print "config sets $f[0] to $config{$key}" if $argverbose;
                    }
                    elsif ($arg ne $config{$key}) {
                        print "command line overrides config of $f[0] to $config{$key}" if $argverbose;
                    }
                }
            }
        }
        if (! $found) {
            # make it an arg to the executable
            my $exe = shift @ARGV;
            if (defined ($exe)) {
                if ($config{$key} ne "") {
                    if ($config{$key} eq "=") {
                        unshift (@ARGV, "$key=");
                    }
                    else {
                        unshift (@ARGV, "$key=$config{$key}");
                    }
                }
                else {
                    unshift (@ARGV, "$key");
                }
                print "adding $ARGV[0] to exec args" if $argverbose;
                unshift (@ARGV, $exe);
            }
            else {
                print "ignoring $key=$config{$key} in config as no exec specified" if $argverbose;
            }
        }
    }
    elsif ($key =~ /^[a-z][-a-z0-9]+$/) { # and $config{$key} =~ /^\d+$/)
        # a change number specification for a package
        $configchange{$key}=$config{$key};
        $configchange{$key}="${branch}_$config{$key}"
            if ($branch ne "" and ! ($config{$key} =~ /_/) and ! ($key =~ /^fulcrum-.*-pdk/));
    }
    else {
        $ENV{uc $key} = $config{$key};
    }
}
foreach my $key (keys %fconfig) { # stuff from the fulcrum block
    if ($key =~ /^--/) {
        my $kopt = $key;
        $kopt =~ s/^--//;
        foreach my $opt (keys %options) {
            if ($options{$opt} =~ /SCALAR/) {
                my @f=split(/[|=:!]/, $opt);
                my $arg;
                if ($f[0] eq $kopt) {
                    eval "\$arg = \$arg$f[0]";
                    $config{$key} = 1 unless defined $config{$key};
                    if (!defined ($arg) or $arg eq "") {
                        if ($exclusive{$f[0]} ) {
                            my $mask = 0;
                            foreach my $opt (sort keys %exclusive) {
                                if ($opt ne $f[0]) {
                                    eval "\$mask++ if $exclusive{$opt} ne ''";
                                }
                                if ($mask) {
                                    last;
                                }
                            }
                            if ($mask) {
                                print "command line overrides config of $f[0]";
                                next;
                            }
                        }
                        eval "\$arg$f[0] = \"$config{$key}\"";
                        print "config sets $f[0] to $config{$key}" if $argverbose;
                    }
                    elsif ($arg ne $config{$key}) {
                        print "command line overrides config of $f[0] to $config{$key}" if $argverbose;
                    }
                }
            }
        }
    }
}
foreach my $key (keys %execonfig) { # stuff inside of exe blocks
    if ($key =~ /^-/) {
        # make it an arg to the executable
        my $exe = shift @ARGV;
        if (defined ($exe)) {
            foreach my $configarg (@{$execonfig{$key}}) {
                if ($configarg ne "") {
                    if ($configarg eq "=") {
                        unshift (@ARGV, "$key=");
                    }
                    else {
                        unshift (@ARGV, "$key=$configarg");
                    }
                }
                else {
                    unshift (@ARGV, "$key");
                }
                print "adding $ARGV[0] to exec args" if $argverbose;
            }
            unshift (@ARGV, $exe);
        }
        else {
            print "ignoring $key=$execonfig{$key} in config as no exec specified" if $argverbose;
        }
    }
    elsif ($key =~ /^[a-z][-a-z0-9]+$/ and $execonfig{$key} =~ /^\d+$/) {
        # a change number specification for a package
        $configchange{$key}=$execonfig{$key};
        $configchange{$key}="${branch}_$execonfig{$key}"
            if ($branch ne "" and ! ($execonfig{$key} =~ /_/) and ! ($key =~ /^fulcrum-.*-pdk/));
    }
    else {
        $ENV{uc $key} = $execonfig{$key};
    }
}
# end dealing with config
$argpdkafter = $argafter if ( ! $argpdkset );
$argpdkbefore = $argbefore if ( ! $argpdkset );

dbmopen (%all, "$alldb", 0) or die "Cannot open $alldb $!";
dbmopen (%pdkall, "$pdkalldb", 0) or die "Cannot open $pdkalldb";
if (!$setpdkhome and !($pdkalldb =~ m:$defaultconfigdir: ) ) {
    my $ok = 0;
    foreach my $key (keys %pdkall) {
        if ($key =~ /-pdk/) {
            $ok = 1;
            last;
        }
    }
    if (! $ok ) {
        print STDERR "Note: Switching to global fulcrum PDK $pdkalldb";
        dbmclose %pdkall;
        $pdkconfigdir = "$defaultconfigdir";
        $pdkreldb = "$pdkconfigdir/releasedb";
        $pdkbetadb = "$pdkconfigdir/betadb";
        $pdklatdb = "$pdkconfigdir/latestdb";
        $pdkalldb = "$pdkconfigdir/alldb";
        $pdkalldb = "$pdkconfigdir/develdb" if $argdevelop == 1;
        $argpdkhome = "$defaultfulcrum/pdk";
        dbmopen (%pdkall, "$pdkalldb", 0) or die "Cannot open $pdkalldb";
    }
}
dbmopen (%prefs, "$prefdb", 0) or die "Cannot open $prefdb";

my $mask = 0;
$mask++ if $argpdklatest == 1;
$mask++ if $argpdkbeta == 1;
$mask++ if $argpdkrelease == 1;
$mask++ if $argpdkafter ne "";
$mask++ if $argpdkbefore ne "";
$mask++ if $argpdkproject ne "";
if ($mask > 1) {
    usage "do not use more than one of the following\n".
       " --[pdk-after|pdk-before],".
       " --pdk-beta,".
       " --pdk-latest,".
       " --pdk-project,".
       " --pdk-release".
       "";
}
if ($mask == 0) {
    $argpdklatest = $arglatest;
    $argpdkbeta = $argbeta;
    $argpdkrelease = $argrelease;
    $argpdkafter = $argafter;
    $argpdkbefore = $argbefore;
    $argpdkproject = $argproject;
}

my $datechange;

usage if ($#ARGV == -1) and ! $arglist and ! $argdoc and ! $argfindpackage;
if ($argbeta and ! -f "$configdir/beta") {
    usage "No beta packages defined.";
}
if ($argrelease and ! -f "$configdir/release" and ! -f "$configtop/release") {
    usage "No release packages defined.";
}

if ($argpdkbeta and ! -f "$pdkconfigdir/beta") {
    usage "No beta pdk packages defined.";
}
if ($argpdkrelease and ! -f "$pdkconfigdir/release" and ! -f "$pdkconfigtop/release") {
    usage "No release pdk packages defined.";
}

my $pdkchange;
my $change;
if ($argpdkproject ne "") {
    local (*P);
    local ($_);
    my $pdkcnt=0;
    my $tmppdk="";
    foreach my $x (keys %configchange) {
        if ($x =~ /-pdk$/) {
            $pdkcnt++;
            $tmppdk=$x;
            $pdkproject{$x} = $configchange{$x};
        }
    }
    if ($pdkcnt == 1) {
        $argpdk = $tmppdk;
    }
}
elsif ($argpdkbeta) {
    open (P, "$pdkconfigdir/beta");
    while (<P>) {
        chomp;
        my ($package,$change)=split;
        $pdkproject{$package}=$change;
        $pdkproject{$package}="${branch}_$change"
            if ($branch ne "" and ! ($change =~ /_/) and ! ($package =~ /^fulcrum-.*-pdk/));
    }
    close P;
}
elsif ($argpdklatest) {
    dbmopen (%pdkproject, "$pdklatdb", 0) or die "Cannot open $pdklatdb";
}
elsif ($argpdkafter or $argpdkbefore) {
    dbmopen (%pdkproject, "$pdkalldb", 0) or die "Cannot open $pdkalldb";
}
else {
    dbmopen (%pdkproject, "$pdkreldb", 0) or die "Cannot open $pdkreldb";
}
if ($argproject ne "") {
    local (*P);
    local ($_);
    my $pdkcnt=0;
    my $tmppdk="";
    foreach my $x (keys %configchange) {
        $project{$x} = $configchange{$x};
    }
}
elsif ($argbeta) {
    open (P, "$configdir/beta");
    while (<P>) {
        chomp;
        my ($package,$change)=split;
        $project{$package}=$change;
    }
    close P;
}
elsif ($arglatest) {
    dbmopen (%project, "$latdb", 0) or die "Cannot open $latdb";
}
elsif ($arglist and $argall and ! $argrelease ) {
    dbmopen (%project, "$alldb", 0) or die "Cannot open $alldb";
    dbmopen (%pdkproject, "$alldb", 0) or die "Cannot open $pdkalldb";
}
else {
    dbmopen (%project, "$reldb", 0) or die "Cannot open $reldb";
}
my $hasall=0;
$hasall=1 if defined($project{"all:$argexe"}) and $arglatest;
# this whole pdk thing seems to work but
# could have issues, needs oversight and/or more work
my $pdkpath="";
my $pdkchg = 0;
my $pdkcnt = 0;
my $tmpargpdk;
if ($argpdkbefore eq "" and $argpdkafter eq "" and ! $argpdklatest) {
    $argpdkbefore = $argbefore if $argbefore ne "";
    $argpdkafter = $argafter if $argafter ne "";
}
if ($argpdkbefore ne "") {
    if ( ! ($argpdkbefore =~ /^\d+$/) ) {
        $argpdkbefore = p4d2c ($argpdkbefore, 0);
    }
}
if ($argpdkafter ne "") {
    if ( ! ($argpdkafter =~ /^\d+$/) ) {
        $argpdkafter = p4d2c ($argpdkafter, 0);
    }
}
if ($argpdkbefore ne "" and defined($pdkproject{"fulcrum-$argpdk-pdk::$argpdkbefore"})) {
    $pdkpath="$argpdkhome/fulcrum-$argpdk-pdk";
    $pdkchg=$argpdkbefore;
    $pdkcnt=1;
}
elsif ($argpdkafter ne "" and defined($pdkproject{"fulcrum-$argpdk-pdk::$argpdkafter"})) {
    $pdkpath="$argpdkhome/fulcrum-$argpdk-pdk";
    $pdkchg=$argpdkafter;
    $pdkcnt=1;
}
else {
    foreach my $pdk (sort keys %pdkproject) {
        my $px = $pdk;
        $px =~ s/:.*//;
        my $abbr = $px;
        $abbr =~ s/^fulcrum-//;
        $abbr =~ s/-pdk$//;
        if ($argpdk eq "" or $abbr eq $argpdk or $px eq $argpdk) {
            if ($argpdk ne "") {
                $tmpargpdk = $px;
                $pdkpath = "$argpdkhome/$px";
                $pdkchg = $pdkproject{$pdk};
                $pdkcnt++;
                last;
            }
            elsif ($px =~ /-pdk$/) {
                $tmpargpdk = $pdk;
                $pdkpath = "$argpdkhome/$pdk";
                $pdkchg = $pdkproject{$pdk};
                $pdkcnt++;
            }
        }
    }
}
if ($pdkcnt == 1) {
    $argpdk = $tmpargpdk;
}
else {
    undef $argpdk;
    undef $pdkpath;
}
if ($argpdkafter ne "") {
    if ($argpdkafter ne $pdkchg) {
        $pdkchg="";
        $pdkcnt=0;
        foreach my $key (sort sortcmpchg keys %pdkall) {
            if ("$key" =~ /^$argpdk\:\:/) {
                if (cmpchg($pdkall{$key}, $argpdkafter) >= 0) {
                    $pdkchg = $pdkall{$key};
                    $pdkcnt++;
                    last;
                }
            }
        }
    }
}
elsif ($argpdkbefore ne "") {
    if ($argpdkbefore ne $pdkchg) {
        $pdkchg="";
        $pdkcnt=0;
        foreach my $key (reverse sort sortcmpchg keys %pdkall) {
            if ($key =~ m/^$argpdk\:\:/) {
                if (cmpchg ($pdkall{$key}, $argpdkbefore) <= 0) {
                    $pdkchg = $pdkall{$key};
                    $pdkcnt++;
                    last;
                }
            }
        }
    }
}
if ($pdkchg eq "") {
    undef $pdkpath;
    undef $argpdk;
    $pdkcnt=0;
}
else {
    $pdkpath .= "/$pdkchg";
}
if ( ! -d $pdkpath ) {
    undef $pdkpath;
    $pdkcnt=0;
}
if ( -e "$pdkpath/share/Fulcrum/OA") {
    $isOA=1;
    print "Using OA" if $argverbose;
}
# setup cadence arg handling
foreach my $key (keys %default_wrappers) {
    if (defined $cadencewrappers{$key."_oa"}) {
        $default_wrappers{$key."_oa"}=$default_wrappers{$key};
        undef $default_wrappers{$key};
    }
}
my %cadence_ver;
foreach my $key (keys %cadencewrappers) {
    if ($isOA and defined($cadencewrappers{$key."_oa"})) {
        $cadence_ver{$key}=$key."_oa";
    }
    else {
        $cadence_ver{$key}=$key;
    }
}

foreach my $cad (split(/,/,$argcadence)) {
    if (iAmCadence($cad,["x"])) {
        my $cv = $cad;
        $cv =~ s/\..*//;
        $cadence_ver{$cv}=$cad if ( -x "$cadencewrapperdir/$cad" );
        $addedwrappers{$cv}=$wrapper_key if ! defined $default_wrappers{$cv};
        $default_wrappers{$cv} = $wrapper_key++;
    }
    else {
        print STDERR "cadence version $cad does not exist, ignorning!";
    }
}

@cadence_wrappers = sort ( { $default_wrappers{$a}-$default_wrappers{$b} }
   (keys %default_wrappers));

foreach my $cad (0..$#cadence_wrappers) {
    $cadence_wrappers[$cad] =
        "$cadencewrapperdir/$cadence_ver{$cadence_wrappers[$cad]}";
    $cadence_wrappers[$cad] = ""
        if (! $isOA and $cadence_wrappers[$cad] =~ /_oa$/);
    $cadence_wrappers[$cad] = ""
        if ($isOA and defined($cadence_ver{$cadence_wrappers[$cad]."_oa"}));
}

# remove duplicates and empty elements
my @temp=();
my %temp=("" => 1);
foreach my $ptr (reverse @cadence_wrappers) {
    unshift @temp, $ptr if ! defined $temp{$ptr};
    $temp{$ptr}=1;
}

@cadence_wrappers=@temp;
undef @temp;
undef %temp;
print "pdk=$pdkpath" if $argverbose;
$argexe=shift @ARGV;
my $argpackage;
($argpackage,$argexe)=split(/:/,$argexe,2);
if (! defined ($argexe)) {
    $argexe = $argpackage;
    undef $argpackage;
}
my $iAmCadence=iAmCadence($argexe,@ARGV);
if ($iAmCadence) {
    print "Cadence wrapper found." if $argverbose;
    undef $argpackage;
}

print "Preferred pkg for $argexe is $prefs{$argexe}" if $argverbose and defined ($prefs{$argexe});

# the following loops are so similar they should be combined..

if ($iAmCadence) {
    $exe{$argexe}="/";
}
elsif ($argproject ne "" or $argrelease or $arglatest or $argbeta) {
    my ($pkg,$exe,$chg,@f);
    if ($arglist) {
        foreach my $kv (keys %project) {
            my ($pkg,$exe)=split(/:/,$kv);
            my $chg = $project{$kv};
            if (! defined ($exe)) { # probably project db
                local(*D);
                my $dir="$argtoolhome/$pkg/$chg/bin";
                if (!defined ($argexe) or $argexe eq "") {
                    opendir (D, "$dir");
                    while ($_=readdir(D)) {
                        if ( -f "$dir/$_" and -x "$dir/$_") {
                            $exe = $_;
                            if ($exe eq $argexe or $argexe eq "") {
                                if (! defined ($argpackage) or $argpackage eq $pkg) {
                                    $exe{$exe} .= "$pkg/$chg/$exe ";
                                }
                            }
                        }
                    }
                    closedir D;
                }
                elsif ( -f "$dir/$argexe" and -x "$dir/$argexe") {
                    if (! defined ($argpackage) or $argpackage eq $pkg) {
                        $exe{$argexe} .= "$pkg/$chg/$argexe ";
                    }
                }
            }
            elsif ($exe eq $argexe or $argexe eq "") {
                if (! defined ($argpackage) or $argpackage eq $pkg) {
                    $exe{$exe} .= "$pkg/$chg/$exe ";
                }
            }
        }
    }
    elsif (defined ($argpackage)) {
        foreach my $kv (keys %project) {
            ($pkg,$exe)=split(/:/,$kv);
            if ( -x "$argtoolhome/$pkg/$project{$kv}/bin/$argexe" and
                    $argpackage eq $pkg) {
                if ($arglist) {
                    $exe{$argexe} .= "$pkg/$project{$kv}/$argexe ";
                }
                else {
                    $exe{$argexe} .= "$pkg/$project{$kv} ";
                }
            }
        }
    }
    elsif ($arglatest and $hasall) {
        $pkg = "all";
        if (defined ($project{$pkg})) {
            $chg= $project{$pkg};
            $exe{$argexe}="$pkg/$chg";
        }
        elsif (defined ($project{"$pkg:$argexe"})) {
            $chg= $project{"$pkg:$argexe"};
            $exe{$argexe}="$pkg/$chg";
        }
    }
    elsif (defined ($prefs{$argexe})) {
        $pkg = $prefs{$argexe};
        if (defined ($project{$pkg})) {
            $chg= $project{$pkg};
            $exe{$argexe}="$pkg/$chg";
        }
        elsif (defined ($project{"$pkg:$argexe"})) {
            $chg= $project{"$pkg:$argexe"};
            $exe{$argexe}="$pkg/$chg";
        }
    }
    else {
        foreach my $kv (keys %project) {
            ($pkg,$exe)=split(/:/,$kv);
            if ( -x "$argtoolhome/$pkg/$project{$kv}/bin/$argexe" and
                    ($argpackage eq $pkg or !defined ($argpackage) or $argpackage eq "")) {
                if ($arglist) {
                    $exe{$argexe} .= "$pkg/$project{$kv}/$argexe ";
                }
                else {
                    $exe{$argexe} .= "$pkg/$project{$kv} ";
                }
            }
        }
    }
}
elsif ($argafter ne "") {
    if ( ! ($argafter =~ /^\d+$/) ) {
        $argafter = p4d2c ($argafter, 0);
    }
    if (defined ($argpackage) and defined ($all{"$argpackage:$argexe:$argafter"})) {
        $exe{$argexe} = "$argpackage/$argafter";
    }
    elsif (defined ($all{"$prefs{$argexe}:$argexe:$argafter"})) {
        $exe{$argexe} = "$prefs{$argexe}/$argafter";
    }
    else {
    #    $argpackage = $prefs{$argexe} if ! defined $argpackage;
        foreach my $key (sort sortcmpchg keys %all) {
            if ("$key" =~ /\:$argexe\:/ and (! defined ($argpackage) or
                    "$key" =~ /^$argpackage:/)) {
                my ($pkg,$exe)=split(/:/,$key);
                if (cmpchg ($all{$key}, $argafter) >= 0) {
                    if (defined ($argpackage)) {
                        if ($pkg eq $argpackage) {
                            $exe{$exe} = "$pkg/$all{$key}";
                            last;
                        }
                    }
                    else {
                        if (defined ($all{"$prefs{$exe}:$exe:$all{$key}"})) {
                            $exe{$exe} = "$prefs{$exe}/$all{$key}";
                        }
                        else {
                            $exe{$exe} = "$pkg/$all{$key}";
                        }
                        last;
                    }
                }
            }
        }
    }
}
elsif ($argbefore ne "") {
    if ( ! ($argbefore =~ /^\d+$/) ) {
        $argbefore = p4d2c ($argbefore, 1);
    }
    if (defined ($argpackage) and defined ($all{"$argpackage:$argexe:$argbefore"})) {
        $exe{$argexe} = "$argpackage/$argbefore";
    }
    elsif (defined ($all{"$prefs{$argexe}:$argexe:$argbefore"})) {
        $exe{$argexe} = "$prefs{$argexe}/$argbefore";
    }
    else {
        foreach my $key (reverse sort sortcmpchg keys %all) {
            if ("$key" =~ /\:$argexe\:/ and (! defined ($argpackage) or
                    "$key" =~ /^$argpackage:/)) {
                my ($pkg,$exe)=split(/:/,$key);
                if (cmpchg ($all{$key}, $argbefore) <= 0) {
                    if (defined ($argpackage)) {
                        if ($pkg eq $argpackage) {
                            $exe{$exe} = "$pkg/$all{$key}";
                            last;
                        }
                    }
                    else {
                        if (defined ($all{"$prefs{$exe}:$exe:$all{$key}"})) {
                            $exe{$exe} = "$prefs{$exe}/$all{$key}";
                        }
                        else {
                            $exe{$exe} = "$pkg/$all{$key}";
                        }
                        last;
                    }
                }
            }
        }
    }
}
else {
    die "Internal error, none of latest, beta, release defined.";
}
undef @f;
if (defined ($arglist)) {
    foreach my $fx (sort keys %exe) {
        push (@f, split(/ /, $exe{$fx}));
    }
}
elsif (defined ($exe{$argexe})) {
    @f=split(/ /,$exe{$argexe});
}
elsif (defined ($arglist)) {
    foreach my $fx (sort keys %exe) {
        push (@f, split(/ /, $exe{$fx}));
    }
}
else {
    undef @f;
}
if ($argexe eq "pdk" and $argpath ) {
    print "$pdkpath";
    exit 0;
}
if (!defined ($arglist)) {
    if ($#f == 0) {
        $list[0] = $f[0];
    }
    else {
        my %list;
        foreach my $f (@f) {
            my ($pkg,$chg)=split(/\//,$f);
            if ($argrelease) {
                $list{$f}=1;
            }
            elsif ($chg eq $project{"$pkg:$argexe"}) {
                $list{$f}=1;
            }
        }
        @list=(reverse sort keys %list);
        undef %list;
    }
    if ($#list > 0 and $argrelease) {
        # just pick the highest change number of the released versions
        my $hn = 0;
        my $hv = "";
        foreach my $n (0..$#list) {
            my ($x,$y)=split(/\//,$list[$n]);
            if ($y > $hn) {
                $hn = $y;
                $hv = $list[$n];
            }
        }
        undef @list;
        $list[0] = $hv;
    }
    my @args;
    if (! $argdoc and ! $arglist) { # always set the environment in case the tool uses Cadence
        # make the expansion of arrays be separated by spaces
        $" = " ";
        # look for pdk specified on the command line after the tool name
        my $pdk = "";
        foreach my $f (@ARGV) {
            if ($f =~ /^--fulcrum-pdk-root=/) {
                $pdk = $f;
                $pdk =~ s/--fulcrum-pdk-root=//;
                if ( -d "$pdk" ) {
                    $pdkpath = $pdk;
                }
            }
            else {
                push (@args,$f);
            }
        }
        if (defined ($pdkpath) and $pdkpath ne "" and -d "$pdkpath") {
            $ENV{FULCRUM_PDK_ROOT}="$pdkpath";
        }
        elsif ($pdkcnt > 1) {
            warn "Warning: Too many PDKs found, use --pdk";
        }
        else {
            warn "Warning: No appropriate PDK found." if $argverbose;
        }
        my $virtuosohome;
        if ( -d "$argtoolhome/$list[0]/share/skill/ui/ui") {
            $virtuosohome = "$argtoolhome/$list[0]";
        }
        else {
            $virtuosohome=locatepackage ("all");
            if ($virtuosohome eq "" or ! -d "$virtuosohome/share/skill/ui/ui") {
                $virtuosohome=locatepackage("virtuoso-integration");
            }
        }
        if ( $virtuosohome ne "" and -d "$virtuosohome/share/skill/ui/ui" ) {
            my $ui_dirs=`find $virtuosohome/share/skill/ui/ui -maxdepth 1 -mindepth 1 -type d -printf "%p/share/Fulcrum:"`;
            chomp $ui_dirs;
            my $autoload_dirs=`find $virtuosohome/share/skill/ui/ui -maxdepth 1 -mindepth 1 -type d -printf "%p/share/autoload:"`;
            chomp $autoload_dirs;
            $ENV{AUTOLOAD_DIRS}=$autoload_dirs;
            $ENV{UI_DIRS}=$ui_dirs;
            $ENV{VIRTUOSO_HOME}=$virtuosohome;
            $ENV{VIRTUOSO_BIN}="$virtuosohome/bin";
            $ENV{FULCRUM_PACKAGE_ROOT}="$virtuosohome";
            print "AUTOLOAD_DIRS=$autoload_dirs" if $argverbose;
            print "UI_DIRS=$ui_dirs" if $argverbose;
            print "VIRTUOSO_HOME=$virtuosohome" if $argverbose;
            print "VIRTUOSO_BIN=$ENV{VIRTUOSO_BIN}" if $argverbose;
            print "FULCRUM_PACKAGE_ROOT=$ENV{FULCRUM_PACKAGE_ROOT}" if $argverbose;
        }
        else {
            warn "Warning: ui package in virtuoso-integration not found in $argtoolhome";
        }
    }
    if ($iAmCadence) {
        if ( $argwhich ) {
            print "$cadencewrapperdir/$cadence_ver{$argexe}";
        }
        elsif ($argdoc) {
            $argexe =~ s/ .*//;
            if (defined ($argexe) && $argexe ne "") {
                print "Starting cdsdoc";
                exec "($cadencewrapperdir/$cadence_ver{$argexe} cdsdoc &)";
                exit 0;
            }
        }
        else {
            print "Executing $argexe @args" if $argverbose;
            $ENV{PATH} = "$ENV{VIRTUOSO_BIN}:".$ENV{PATH};
            print "PATH=$ENV{PATH}" if $argverbose;
            gettmp;
            print "TMP=$ENV{TMP}" if $argverbose;
            exec "$cadencewrapperdir/$cadence_ver{$argexe}",@args;
        }
    }
    elsif ($#list == 0) {
        # add the package bin dir to the path
        $ENV{PATH} = "$ENV{VIRTUOSO_BIN}:".$ENV{PATH}
            if $list[0] =~ m:^lve/:;
        $ENV{PATH} = "$argtoolhome/$list[0]/bin:".$ENV{PATH};
        $ENV{FULCRUM_PACKAGE_ROOT}="$argtoolhome/$list[0]";
        print STDERR "FULCRUM_PACKAGE_ROOT=$ENV{FULCRUM_PACKAGE_ROOT}" if $argverbose;
        print "PATH=$ENV{PATH}" if $argverbose;
        gettmp;
        print "TMP=$ENV{TMP}" if $argverbose;
        # make the expansion of arrays be separated by spaces
        $" = " ";
        # look for pdk specified on the command line after the tool name
        my $pdk = "";
        foreach my $f (@ARGV) {
            if ($f =~ /^--fulcrum-pdk-root=/) {
                $pdk = $f;
                $pdk =~ s/--fulcrum-pdk-root=//;
            }
        }
        # check to see if the tool uses a pdk
        my $usespdk=usespdk ("$argtoolhome/$list[0]/bin/$argexe");
        if (defined ($pdkpath) and $pdkpath ne "" and -d "$pdkpath" and $usespdk) {
            unshift @ARGV, "--fulcrum-pdk-root=$pdkpath";
            if ( -d "$pdk" and $pdk ne "" and $pdk ne "$pdkpath") {
                warn "Warning: User is overriding installed PDK";
            }
        }
        elsif ($usespdk) {
            if ($pdkcnt > 1) {
                warn "Warning: Too many PDKs found, use --pdk";
            }
            else {
                warn "Warning: No appropriate PDK found." if $argverbose;
            }
        }
        if ( $argwhich ) {
            if ($argpath) {
                print "$argtoolhome/$list[0]/bin/$argexe";
            }
            else {
                unshift @ARGV,"$argtoolhome/$list[0]/bin/$argexe";
                setwrapperenvs();
                unshift @ARGV, @cadence_wrappers;
                my $args = fixargs (@ARGV);
                print "@ARGV";
            }
        }
        elsif ($argdoc or $argman) {
            $argexe =~ s/ .*//;
            my $pageroot=$argtoolhome;
            $pageroot = "https://internal.ts.intel.com/eng/fulcrum-tools/"
                if $argtoolhome eq "$defaulttoolhome";
            $pageroot = "https://internal.ts.intel.com/eng/fulcrum-pdk/"
                if $argtoolhome eq "$defaultpdkhome";
            if (defined ($argexe) && $argexe ne "") {
                if ( -f "$argtoolhome/$list[0]/doc/$argexe.html") {
                    print "Starting firefox";
                    exec "(firefox $pageroot/$list[0]/doc/$argexe.html 2>/dev/null 1>/dev/null &)";
                    exit 0;
                }
                elsif ( -f "$argtoolhome/$list[0]/share/html/$argexe.html") {
                    print "Starting firefox";
                    exec "(firefox $pageroot/$list[0]/share/html/$argexe.html 2>/dev/null 1>/dev/null &)";
                    exit 0;
                }
                elsif ( -d "$argtoolhome/$list[0]/doc/") {
                    local(*D);
                    opendir (D, "$argtoolhome/$list[0]/doc");
                    my @files=grep (/^index/,readdir(D));
                    if (!defined ($files[0])) {
                        $files[0]="";
                    }
                    closedir (D);
                    print "Starting firefox";
                    exec "(firefox $pageroot/$list[0]/doc/$files[0] 2>/dev/null 1>/dev/null &)";
                    exit 0;
                }
                elsif ( -d "$argtoolhome/$list[0]/share/html/") {
                    local(*D);
                    opendir (D, "$argtoolhome/$list[0]/share/html");
                    my @files=grep (/^index/,readdir(D));
                    if (!defined ($files[0])) {
                        $files[0]="";
                    }
                    closedir (D);
                    print "Starting firefox";
                    exec "(firefox $pageroot/$list[0]/share/html/ 2>/dev/null 1>/dev/null &)";
                    exit 0;
                }
                else {
                    my $pkg=$list[0];
                    $pkg =~ s:/.*::;
                    print STDERR "No documentaion found for $argexe or for the package $pkg.";
                    exit 1;
                }
            }
            else {
                print "Starting firefox";
                exec "firefox https://internal.ts.intel.com/eng/cad/fulcrum/fulcrumscript.shtml 2>/dev/null 1>/dev/null";
                exit 0;
            }
        }
        else {
            unshift @ARGV,"$argtoolhome/$list[0]/bin/$argexe";
            setwrapperenvs();
# now with all package, need the wrappers
            unshift @ARGV, @cadence_wrappers;
            print "Executing @ARGV" if $argverbose;
            exec @ARGV;
        }
    }
    elsif ($#list < 0) {
        if ($argdoc) {
            exec "firefox https://internal.ts.intel.com/eng/cad/fulcrum/fulcrumscript.shtml 2>/dev/null 1>/dev/null";
            exit 0;
        }
        my $found = 0;
        my $noherc=1;
        my $noicv=1;
        my @ARGLAST=@ARGV;
        @ARGV=();
        my @ARGEXE=();
        foreach my $wrapper (keys %cadencewrappers) {
            if ( $isOA and defined ($cadencewrappers{$wrapper.".oa"})) {
                $wrapper=$wrapper.".oa";
            }
            my $x=`$cadencewrapperdir/$cadence_ver{$wrapper} which $argexe 2>/dev/null`;
            if ( "$x" =~ m:/nfs/site/eda/tools/: and ! ( "$x" =~ /no $argexe in/ ) and ! ( "$x" =~ /does not exist/) ) {
                if ((($argexe =~ /^layout/) or ($argexe =~ /^icfb/) or ($argexe =~ /virtuoso/))
                        and ($wrapper =~ /^ic/)) {
                    unshift @ARGEXE, $argexe if $found == 0;
                    foreach my $arg (@ARGV) {
                        $noherc=0 if $arg eq "$cadencewrapperdir/$cadence_ver{herc}";
                        $noicv=0 if $arg eq "$cadencewrapperdir/$cadence_ver{icv}";
                    }
                    if ($noherc) {
                        unshift @ARGV, "$cadencewrapperdir/$cadence_ver{herc}";
                        $noherc=0;
                    }
                    if ($noicv) {
                        unshift @ARGV, "$cadencewrapperdir/$cadence_ver{icv}";
                        $noicv=0;
                    }
                    unshift @ARGV,
                        "$cadencewrapperdir/$cadence_ver{assura}",
                        "$cadencewrapperdir/$cadence_ver{$wrapper}";
                    setwrapperenvs();
                }
                else {
                    unshift @ARGEXE, $argexe if $found == 0;
                    unshift @ARGV, "$cadencewrapperdir/$cadence_ver{$wrapper}";
                }
                $ENV{PATH} = "$ENV{VIRTUOSO_BIN}:".$ENV{PATH};
                print "PATH=$ENV{PATH}" if $argverbose;
                gettmp;
                print "TMP=$ENV{TMP}" if $argverbose;
                $found = 1;
                last;
            }
        }
        if ($found) {
            foreach my $w (sort keys %addedwrappers) {
                if (defined ($cadence_ver{$w})) {
                    push @ARGV, "$cadencewrapperdir/$cadence_ver{$w}";
                }
                elsif (defined ($cadence_ver{$w})) {
                    if ($w ne "herc" or $noherc) {
                        push @ARGV, "$cadencewrapperdir/$cadence_ver{$w}";
                        $noherc=0 if $w eq "herc";
                    }
                    elsif ($w ne "icv" or $noicv) {
                        push @ARGV, "$cadencewrapperdir/$cadence_ver{$w}";
                        $noicv=0 if $w eq "icv";
                    }
                }
            }
        }
        if ( ! $found) {
            unshift @ARGV, $argexe;
            if (defined ($argpackage)) {
                print STDERR "No $argexe executable found in package $argpackage";
            }
            else {
                printf STDERR "No $argexe executable found in any%s package\n",
                    $argbeta ? " beta" : ($argrelease ? " release" : "");
            }
        }
        $" = " ";
        if ($ARGEXE[0] eq "ncsim" or $ARGEXE[0] eq "ncverilog") {
            unshift @ARGLAST, "+licq_vxl";
        }
        @ARGV=(@ARGV, @ARGEXE, @ARGLAST);
        if ($argwhich) {
            if ($argpath) {
                print "$ARGV[0]";
            }
            elsif ($found) {
                print "@ARGV";
            }
            else {
                exec "which",$ARGV[0];
            }
        }
        else {
            print "Executing @ARGV" if $argverbose;
            exec @ARGV;
            die "Unable to execute @ARGV";
        }
    }
    else {
        print STDERR "Multiple packages contain the executable $argexe";
        foreach my $list (sort @list) {
            my ($x,$y)=split(/\//,$list);
            my %pr;
            if (!defined ($pr{$x})) {
                my $y;
                $y = $project{"$x:$argexe"};
                print "$x:$argexe $y";
                $pr{$x}=1;
            }
        }
    }
}
else {
    @list=@f;
    if ($#list == -1) {
        if (defined ($argpackage)) {
            print STDERR "No $argexe executable found in package $argpackage";
        }
        else {
            print STDERR "No $argexe executable found in any package";
        }
    }
    else {
        my %chglist;
        if ($#list > 0) {
            print STDERR "These packages contain the executable $argexe";
        }
        else {
            print STDERR "This package contains the executable $argexe";
        }
        if ($argverbose) {
            my $minchg = 0;
            local ($_,*P);
            foreach my $list (sort @list) {
                my ($pkg,$chg,$fl)=split(/\//,$list);
                if ($minchg == 0 || $chg < $minchg) {
                    $minchg = $chg;
                }
                $chglist{$chg}="";
            }
            if ($minchg > 0) {
                open (P, "p4 changes -t |");
                while (<P>) {
                    chomp;
                    my @f=split;
                    if (defined ($chglist{$f[1]})) {
                        $chglist{$f[1]}="$f[3] $f[4]";
                    }
                    if ($f[1] == $minchg) {
                        system "pkill p4 -u `whoami` -P $$";
                        close P;
                        last;
                    }
                }
                close P;
            }
        }
        my %rel;
        dbmopen (%rel, "$reldb", 0) or die "Cannot open $reldb";
        foreach my $list (sort @list) {
            my ($pkg,$chg,$fl)=split(/\//,$list);
            if ($argverbose) {
                print "Change $chg on $chglist{$chg} $pkg:$fl";
            }
            else {
                printf "--change $chg $pkg:$fl%s\n", $rel{"$pkg:$fl"} eq $chg ? '*' : '';
            }
        }
    }
}

sub locatepackage {
    my ($argfindpackage)=@_;
    my $packagechange = 0;
    if (defined ($argfindpackage)) {
        if ($argrelease or $argbeta or $arglatest or $argproject ne "") {
            foreach my $p (keys %project) {
                if ($p =~ m/^$argfindpackage\:/ or $p eq $argfindpackage) {
                    if (cmpchg ($project{$p}, $packagechange) > 0 and
                        ($argfindpackage ne "virtuoso-integration" or
                            -d "$argtoolhome/$argfindpackage/$project{$p}/share/skill/ui/ui")) {
                        $packagechange=$project{$p};
                    }
                }
            }
        }
    }
    if ($packagechange eq "0") {
        if ($argafter ne "") {
            foreach my $p (sort sortcmpchg keys %all) {
                if ( $p =~ m/^$argfindpackage\:/ and cmpchg ($all{$p}, $argafter) >= 0) {
                    $packagechange = $all{$p}
                        if ($packagechange eq "0");
                }
            }
        }
        elsif ($argbefore ne "") {
            foreach my $p (reverse sort sortcmpchg keys %all) {
                if ( $p =~ m/^$argfindpackage\:/ and cmpchg ($all{$p}, $argbefore) <= 0) {
                    $packagechange = $all{$p}
                        if ($packagechange eq "0");
                }
            }
        }
    }
    if ($packagechange eq "0") {
        warn "Warning: Change # for $argfindpackage not located." if $argverbose;
        return "";
    }
    "$argtoolhome/$argfindpackage/$packagechange";
}

sub getexelist {
    my %a;
    dbmopen (%a, "$defaultconfigdir/latestdb", 0);
    foreach my $k (keys %a) {
        my($pkg,$exe)=split(/:/,$k);
        if (defined ($exe)) {
            $exelist{$exe}=1;
        }
    }
    dbmclose %a;
    if ("$argtoolhome" ne "$defaultfulcrum") {
        dbmopen (%a, "$configdir/latestdb", 0);
        foreach my $k (keys %a) {
            my($pkg,$exe)=split(/:/,$k);
            if (defined ($exe)) {
                $exelist{$exe}=1;
            }
        }
        dbmclose %a;
    }
}
