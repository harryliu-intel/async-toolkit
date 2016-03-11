#!/usr/intel/bin/perl -lw

use strict;

use File::Spec::Functions;

use FindBin qw($Bin);

use Getopt::Long;

sub WNOHANG {1;}

sub usage {
    print <<EF;
Usage: triggerd [--toolhome dir] [--verbose] [--port <port>] [--host <host>] [build args...]
EF
exit 1;
}

$ENV{USER_ITOOLS}=catfile($Bin, '.itools');
my $buildhome="/nfs/sc/proj/ctg/mrl108/mrln1adm/trigger";
my $osbuild = "lx24-amd64";
my $fulcrum = "/nfs/sc/proj/ctg/mrl108/mrl/tools/fulcrum";
my $toolhome= $fulcrum;
my $buildcmd = "$Bin/buildfull.pl --target-arch Linux-x86_64";
my $indexcmd = "$Bin/updateall.pl --target-arch Linux-x86_64";
my $addr;
my $child0;
my $child1;
my %chldpack;
my @packages;
my $packages;
my %packages;
my $verbose=0;
my $user="";
my $cc="";
my $why="";
my $branch="";
my $change = "";
my $port=6777;
my $rhost="mprcs055";
my $test=0;
my $rv;
my @queue=();

sub mailto {
    my ($user,$packages,$osbuild,$rv,$log,$cc)=@_;
    if ($user ne "") {
        local (*P);
        local (*X);
        my $cmd ="| /bin/mail -s 'trigger build' $user";
        $cmd .= " -c $cc" if $cc =~ /^[a-z,]+$/;
        open (P, $cmd);
        print P " trigger install of $packages completed on $osbuild\nwith return code $rv";
        print P "   Return code of $rv != 0 means failure" if $rv;
        if ( defined($log) and -s "$log") {
            open (X, "<$log");
            while (<X>) {
                chomp;
                print P;
            }
            close X;
        }
        close P;
    }
}

sub donotbuild {
    my (@packages)=@_;
    local(*P,$_);
    my %donotbuild;
    undef %donotbuild;
    # just use the common one
    if (open (P, "<$fulcrum/config/Linux-x86_64/donotbuild")) {
        while (<P>) {
            chomp;
            $donotbuild{$_}=1;
        }
        close P;
    }
    my @out;
    foreach my $out (@packages) {
        if (! $donotbuild{$out}) {
            push @out,$out;
        }
        else {
            print "do not build $out" if $verbose;
        }
    }
    @out;
}

sub done {
    print STDERR "done" if $verbose;
    shutdown (NS, 2);
    shutdown (S, 2);
    close(NS);
    close (S);
    exit (0);
}

my $jobs = 0;

sub enqueue {
    if (@queue and !$jobs) {
        my $entry=shift @queue;
        my ($cmd,$user,$packages,$osbuild,$log,$cc)=@{$entry};
        $SIG{CHLD}="catchchild";
        $jobs++;
        if (!fork) {
            my $rv = system("$cmd");
            $rv &= 0xff;
            mailto($user,$packages,$osbuild,$rv,$log,$cc);
            unlink $log if -e $log;
            exit 0;
        }
    }
}

sub catchchild {
    while (waitpid (-1,WNOHANG) > 0) {$jobs--;}
    enqueue;
    $SIG{ALRM}="catchchild";
}

$ENV{PATH}="/usr/intel/bin:$Bin:/bin:/usr/bin";

my ($argport, $arghost, $cfgport, $cfghost);
GetOptions (
    "toolhome=s" => \$toolhome,
    "verbose|v" => \$verbose,
    "port=n" => \$argport,
    "host=s" => \$arghost,
    "test" => \$test,
) or usage;

if (open (P, "<$toolhome/config/trigger")) {
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

usage unless ($port =~ m/^\d+$/);
usage unless $rhost;
usage unless -d $toolhome;

$toolhome =~ s:/(tools|pdk|packages)$::;
$buildcmd .= " --toolhome $toolhome" . join('', map { " \Q$_\E" } @ARGV);

$verbose=1 if $test;
my $sockpack;
my $thishost;
my $proto;
my $a;
my $bw;
my $this;
use Socket;
$thishost=`hostname`;
chomp $thishost;
$thishost =~ s/\..*//;
if ($rhost ne $thishost) {
    print STDERR "Must run triggerd on $rhost not $thishost\n" if $verbose;
    exit 1;
}
my ($ta,$b,$c,$d,@te)=gethostbyname("$thishost");
if ( ! defined ($ta) ) {
   print STDERR "Cannot get my host's address!" if $verbose;
   exit 1;
}
$sockpack='S n a4 x8';
($a,$bw,$proto)=getprotobyname('tcp');
$this=pack($sockpack,AF_INET, $port, $te[0]);
# attempt to connect to triggerd, allows attempt to start without conflict
if (socket (S, AF_INET, SOCK_STREAM, $proto)) {
    if (!bind (S, $this)) {
        print STDERR "triggerd already running" if $verbose;
        exit 1;
    }
}
# connect failed, that is a good thing, no server running
shutdown (S, 2);
close S;
# now fork a child to do the actual daemon. This should have prevented
# a terminal from hanging, but it is actually just a convenience.
$child0 = fork;
if ($child0 != 0) {
    close STDIN;
    close STDOUT;
    close STDERR;
    exit 0;
}
# set up listening in the child
if (! $verbose) {
    close STDOUT;
    close STDERR;
    open (STDOUT, ">/dev/null");
    open (STDERR, ">/dev/null");
}
else {
    select(STDOUT);
    $|=1;
}
socket(S, AF_INET, SOCK_STREAM, $proto) or die "socket: $!";
while (! bind(S,$this) ) {
   print STDERR "Waiting for bind $!" if $verbose;
   sleep (1);
}
listen(S,5) or die "listen: $!";
select(S); $|=1;select(STDOUT);
select(NS); $|=1; select(STDOUT);
# catch SIGINT to allow cleanup of the sockets. Otherwise, restart
# can be painful.
$SIG{'INT'}='done';
# now the core loop
while (1) {
    # this checks for zombies every 120 seconds and removes them
    $SIG{'ALRM'}='catchchild';
    alarm 120;
    $addr=accept(NS,S);
    warn "Accept Failure $!" if (! $addr and $verbose);
    next if ! $addr;
    # turn off the timer and do a quick check for zombies
    # after an incoming message received.
    alarm 0;
    $SIG{'INT'}='DEFAULT';
    $SIG{'ALRM'}='DEFAULT';
    next if ! $addr;
    my ($af,$port,$inetaddr)=unpack($sockpack,$addr);
    $af=$af;
    my ($a,$b,$c,$d)=unpack('C4',$inetaddr);
    printf STDERR "Accepted %d.%d.%d.%d:%d\n", $a, $b, $c, $d, $port if $verbose;
    # only a two second timeout for receiving a message
    alarm(2);
    undef @packages;
    $why="";
    $user="";
    $cc="";
    $branch="";
    my $localbuildcmd=$buildcmd;
    my $localindexcmd=$indexcmd;
    # read the message from the trigger user message is like this:
    %packages=();
    $change="";
    while (<NS>) {
        chomp;
        print "Received $_" if $verbose;
        if (m/^user=(.*)/) {
            $user=$1;
        }
        elsif (m/^why=(.*)/) {
            $why = $1;
        }
        elsif (m/^os=/) {
            # optionally specify os to build, but ignore it
        }
        elsif (m/^branch=(.*)/) {
            $branch = $1;
        }
        elsif (m/^change=(\d+)$/) {
            $change = "--change=$1";
        }
        elsif (m/^cc=(.*)/) {
            $cc = "$1";
        }
        elsif ( -f "$_" or /^[a-zA-Z][-a-zA-Z0-9_,]+$/) {
            # this will be used to count up the packages being built
            if (!defined ($packages{$_}) or $packages{$_} == 0) {
                push (@packages,split(/,/,$_));
                print "PACKAGES @packages" if $verbose;
                $packages{$_}=-1;
            }
            print STDERR ":$_:" if $verbose;
        }
        else {
            print STDERR "$_ has illegal characters" if $verbose;
        }
    }
    alarm 0;
    @packages = donotbuild (@packages);
    $packages=join(',',@packages);
    $localbuildcmd .= " --branch $branch" if $branch ne "";
    $localindexcmd .= " --branch $branch" if $branch ne "" and $branch ne "main";
    print STDERR "$localbuildcmd $packages" if $verbose and $packages ne "";
    $SIG{'ALRM'}='DEFAULT';
    shutdown (NS, 2);
    close (NS);
    $child1=0;
    if ($packages =~ /\/tb-/ and $#packages == 0) {
        $packages =~ m:/(tb-[^-]+):;
        my $tb = $1;
        my $tbchange = $packages;
        $tbchange =~ s/-official.tar.gz//;
        $tbchange =~ s/.*-//;
        my $localbranch="";
        $localbranch="--branch $branch" if $branch ne "";
        push @queue, ["$Bin/tb-install.pl $packages $toolhome/tools ; $Bin/updateall.pl $localbranch --toolhome $toolhome",$user,$packages,$osbuild,"/dev/null",$cc];
    }
    elsif ($packages eq "reindex") {
        push @queue, ["$localindexcmd > $buildhome/buildLo$$ 2>&1",$user,"reindex",$osbuild,"/tmp/buildLo$$",$cc];
    }
    elsif ($packages ne "") {
        push @queue, ["$localbuildcmd $change --log-file $buildhome/buildLf$$ $packages >/tmp/buildLo$$ 2>&1", $user, $packages, $osbuild, "/tmp/buildLo$$",$cc];
    }
    enqueue;
    my $date;
    $date = localtime (time);
    mkdir "$toolhome/trigger";
    if (open (L, ">>$toolhome/trigger/trigger.log")) {
        if ($branch eq "") {
            print L "User $user triggered build of $packages on $date because $why";
        }
        else {
            print L "User $user triggered build of $packages in $branch branch on $date because $why";
        }
        close L;
    }
    close (NS);
}
1;
