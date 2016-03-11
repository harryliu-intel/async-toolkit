#!/usr/intel/bin/perl -w

use strict;
use Socket;
use Getopt::Long;
use DB_File;

sub doshut {
   shutdown (SOCK, 2);
   close (SOCK);
   exit 0;
}

sub usage {
    print STDERR "Usage: trigger [-v] [--toolhome toolhome] [--branch bali] <package list>\n";
    print STDERR "   <package list> is one of\n";
    print STDERR "      list of tool packages\n";
    print STDERR "      list of pdk packages\n";
    print STDERR "      a testbench package file\n";
    print STDERR "      the literal 'reindex'\n";
    print STDERR " IMPORTANT: use of branch disabled. Only rrc branch may be built and it is treated as main.\n";
    exit 1;
}

my $package="";
my $port=6777;
my $rhost="mprcs055";
my $verbose=0;
my $user=`whoami`;
my $branch="";
my $change;
my $cc;
my $os;
my %donotbuild;
chomp $user;
my $to=2;
my $aname=`uname -sm`;
chomp $aname;
$aname =~ s/ /-/;
my $toolhome = "/nfs/sc/proj/ctg/mrl108/mrl/tools/fulcrum";

my ($argport, $arghost, $cfgport, $cfghost);
GetOptions (
	"verbose|v" => \$verbose,
    "toolhome=s" => \$toolhome,
	"port=n" => \$argport,
	"host=s" => \$arghost,
	"timeout|t=n" => \$to,
        "os=s" => \$os,
#        "branch=s" => \$branch,
        "change=i" => \$change,
        "cc=s" => \$cc,
) or usage;

# ignore branch for donotbuild and for alldb
my $config = "$toolhome/config";
if (open (P, "<$config/$aname/donotbuild")) {
    while (<P>) {
        chomp;
        $donotbuild{$_}=1;
    }
    close P;
}
my %db;
my %all;
if (dbmopen (%db, "$config/$aname/alldb", 0)) {
    foreach my $key (keys %db) {
        my ($pkg)=split(/:/,$key);
        $all{$pkg}=1;
    }
    dbmclose %db;
}
$all{all}=1; # just in case it is not in alldb
if (open (P, "<$config/trigger")) {
    while (<P>) {
        chomp;
        my ($name,$value)=split;
        if (defined ($value)) {
            if ($name eq "port" and $value =~ m/^\d+$/) {
                $cfgport = $value;
            }
            if ($name eq "host" and $value =~ m/^[a-z][a-z0-9]+$/) {
                $cfghost = $value;
            }
        }
    }
    close P;
}

$port = $argport || $cfgport || $port;
$rhost = $arghost || $cfghost || $rhost;

$package="";
foreach my $f (@ARGV) {
    if ($f ne "") {
        $package .= ",$f";
    }
}
$package =~ s/^,//;
$package =~ s/ //g;
if ($package eq "") {
    usage;
}
my @packages=split(/,/,$package);
my $pkgtype=0;
my $pkgok=0;
my $pkgnok=0;
$package = "";
select STDERR;
$|=1;
select STDOUT;
foreach my $p (@packages) {
    if ($p =~ /-pdk/) {
        $pkgtype |= 1;
    }
    elsif ($p =~ /\/tb-/) {
        $pkgtype |= 4;
    }
    elsif ($p eq "reindex") {
        $pkgtype |= 8;
    }
    else {
        $pkgtype |= 2;
    }
    if ($donotbuild{$p}) {
        print STDERR "You cannot use trigger to build $p, discarded.\n";
        $pkgnok++;
    }
    elsif ($p =~ /\/tb-/) {
        if ( -f "$p") {
            $p =~ m:/(tb-[^-]+):;
            my $tb = $1;
            my $tbchange = $p;
            $tbchange =~ s/-official.tar.gz//;
            $tbchange =~ s/.*-//;
            if ( -d "$toolhome/tools/$tb/$tbchange") {
                print STDERR "$tb change $tbchange already installed.\n";
                $pkgnok++;
            }
            else {
                $pkgok++;
                $package .= ",$p";
            }
        }
        else {
            $pkgnok++;
        }
    }
    elsif (!$all{$p} and $p ne "reindex") {
        print STDERR "Warning: $p probably does not exist, continue? <y>";
        my $ans=<STDIN>;
        $ans = lc $ans;
        chomp $ans;
        if ($ans =~ /^n/) {
            exit 1;
        }
        if ( defined ($change) and $change =~ /^\d+$/ and -d "$toolhome/tools/$p/$change") {
            die "$toolhome/tools/$p/$change already exists, cannot trigger build a duplicate change\n";
        }
        $package .= ",$p";
        $pkgok++;
    }
    else {
        if ( defined ($change) and $change =~ /^\d+$/ and -d "$toolhome/tools/$p/$change") {
            die "$toolhome/tools/$p/$change already exists, cannot trigger build a duplicate change\n";
        }
        $package .= ",$p";
        $pkgok++;
    }
}
$package =~ s/^,//;
if ($pkgtype != 1 and $pkgtype != 2 and $pkgtype != 4 and $pkgtype != 8) {
    print STDERR "Error: Please do not mix pdk, non-pdk, reindex, and tb-??? packages\n";
    exit 1;
}
if ($pkgtype == 2 and $package ne "all") {
    print STDERR "You will building the 'all' package.\n";
    print STDERR "See bug 12971 for details.\n";
    $package="all";
}
if ($pkgok == 0) {
    print STDERR "No packages to build, aborting.\n";
    exit 1;
}
printf STDERR "Why are you doing this %s? (^D or . to finish)\n",
    $package eq "reindex" ? "reindex" : "build of $package";
my $why="";
while (<STDIN>) {
    if ($_ =~ m/^\.$/) {
        last;
    }
    $why .= $_;
}
$why =~ s/[ \n\t\r]+/ /g;
my $thishost=`hostname`;
chomp $thishost;
my ($ta,$b,$c,$d,@te)=gethostbyname("$thishost");
if ( ! defined ($ta) ) {
   print STDERR "Cannot get my host's address!\n";
   exit 1;
}
my $myaddr=$te[0];
my ($a,$bw,$proto)=getprotobyname('tcp');
($a,$bw,$c,$d,my @e)=gethostbyname("$rhost");
if ( defined ($a) ) {
   my $theiraddr=$e[0];
   my ($f,$g,$h,$i)=unpack('C4',$theiraddr);
   printf STDERR "$a %d.%d.%d.%d\n",$f,$g,$h,$i if $verbose;
   my $sockpack='S n a4 x8';
   my $this=pack("$sockpack", AF_INET, 0, $myaddr);
   my $they=pack("$sockpack", AF_INET, $port, $theiraddr);
   if ( ! socket (SOCK, AF_INET, SOCK_STREAM, $proto) ) {
      print STDERR "Cannot create socket.\n";
      exit 1;
   }
   if (! bind(SOCK, $this) ) {
      close (SOCK);
      print STDERR "Cannot do bind.\n";
      exit 1;
   }
   if ( ! connect (SOCK, $they) ) {
      close (SOCK);
      print STDERR "Cannot connect to $a:$port\n";
      exit 1;
   }
   select(SOCK); $|=1;
   $SIG{'INT'}='doshut';
   alarm($to);
   $SIG{'ALRM'}='doshut';
   if (defined ($why) and $why ne "") {
      print "why=$why\n";
   }
   if (defined ($user) and $user ne "") {
      print "user=$user\n";
   }
   if (defined ($os) and $os ne "") {
      print "os=$os\n";
   }
   if (defined ($branch) and $branch ne "") {
      print "branch=$branch\n";
   }
   if (defined ($change) and $change =~ /^\d+$/) {
      print "change=$change\n";
   }
   if (defined($cc) and ($cc =~ /^[a-z,]+$/)) {
      print "cc=$cc\n";
   }
   print "$package\n" or print STDERR "Cannot send $package\n";
   select STDOUT;
   shutdown (SOCK, 2);
   close (SOCK);
   print "Submitted build of $package, you should receive mail shortly\n";
}
else {
   print "Unknown host $rhost\n";
}
