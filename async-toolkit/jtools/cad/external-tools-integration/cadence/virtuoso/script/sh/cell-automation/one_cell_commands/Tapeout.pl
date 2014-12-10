#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use IPC::Open2;
use Getopt::Long;
use IPC::Open2;
my $user=$ENV{USER};
$user="nobody" if ($user eq "");

my $libName;
my $dfIIdir;
my $castpath;
my $cell;
my $output="";
my $outgds="";
my $bindfile="/dev/null";
my $use_tag=0;
my $pdkroot;
my $rootcellname="";
my $debug=0;
my $flattenpcells=1;
my $bit64=0;
my $maxheapsize = "4G";
my $jreargs = "-server";
my $qsubextras="";
my $workingDir=`pwd`;
chomp $workingDir;
my $viewName="layout";
my $layers="";
my $layertable="";
my $cellmaptable="";
my $verbose=0;
my %env=();
my $reducedgds;
my $prevgds;
my $assura_technology;
my $tasks;
my $maxnamelen=127;
my $checkonly=0;
my $grayboxlist="";
my $blackboxhlvs=0;
my $hdrc_threads=4;
my $hlvs_threads=4;
my $hdrc_bin="";
my $hdrc_select_window="";
my %jid=();
my %setmaxheap=();
my $tapeoutid=$$;
my $summary=0;
my $fullchip="";

my %tasks = (
    "fullgds" => 0,
    "subgds" => 0,
    "fullcdl" => 0,
    "fullerc" => 0,
    "gdsxor1" => 0,
    "gdsxor2" => 0,
    "fullhdrc" => 0,
    "fullhlvs" => 0,
    "fullant" => 0,
    "fullbump" => 0,
);

my %maxheapsize=();
foreach my $task (sort keys %tasks) {
    $maxheapsize{$task}=$maxheapsize;
}

# Env list for qsub (see also qb for original code
foreach my $e (keys %ENV) {
    if (! ($e =~ /^SSH_/) or ( $e =~ /^LD_/) or ( $e =~ /^TERM/ ) ) {
        $env{$e} = 1;
    }
}

my $env_vars=join(",", keys %env );

sub qrsh {
    my ($task, $cmd, $holdid,$flags)=@_;
    my $pwd=`pwd`;
    chomp $pwd;
    print STDERR "$cmd" if $verbose;
    unlink "$task.out";
    unlink "$task.err";
    my $taskscript=`mktemp qb.$task.XXXXXX`;
    chomp $taskscript;
    my $fs;
    open ($fs, ">$taskscript");
    print $fs "#!/bin/bash";
    print $fs "cd $pwd";
    print $fs "export CDS_AUTO_64BIT=ALL";
    print $fs "$cmd";
    close $fs;
    chmod 0700, $taskscript;
    my @holdid=();
    if (defined ($holdid) and $holdid ne "") {
        foreach my $tk (split(/,/, $holdid)) {
            if ($tasks{$tk} > 1 and defined ($jid{$tk})) {
                push @holdid, $jid{$tk};
            }
        }
        $holdid=join ",", @holdid;
        if (defined ($holdid) and length($holdid)) {
            $holdid="-hold_jid=$holdid";
        }
        else {
            $holdid="";
        }
    }
    else {
        $holdid="";
    }
    $ENV{QB_DIAG_FILE}="/dev/null";
    $ENV{QB_LOCAL}=0;
    $ENV{QB_RUN_NAME}="$task$tapeoutid";
    $ENV{QRSH_FLAGS}="$holdid -cwd -l mem=$maxheapsize{$task} $qsubextras -v '$env_vars' $flags";
    if (my $pid = fork()) {
        my $trys=0;
        my $jid=0;
        while (1) {
            $trys++;
            if ($trys > 15) {
                print STDERR "Could not find submitted jid for task $task";
                last;
            }
            open ($fs, "nbstatus jobs --fields 'Jobid,Task,StartTime,User' --format csv \"Task='$task$tapeoutid'\" |");
            my $header=<$fs>;
            chomp $header;
            my @header=split(/,/, $header);
            my %ndx=();
            foreach my $n (0..$#header) {
                $ndx{$header[$n]}=$n;
            }
            while (<$fs>) {
                chomp;
                my @data=split(/,/, $_);
                if (($data[$ndx{User}] eq $user) and ($data[$ndx{Task}] eq "$task$tapeoutid")) {
                    $jid=$data[$ndx{Jobid}] if $data[$ndx{Jobid}] > $jid;
                }
            }
            sleep 1;
            last if $jid > 0;
        }
        $jid{$task}=$jid;
        return $jid;
    }
    else {
        system("qb ./$taskscript 1> $task.out 2> $task.err");
        exit 0;
    }
}

sub expandlayers {
    my %mask;
    my %lpp;
    open (MO, "<$pdkroot/share/Fulcrum/stream/maskops.txt") or die "Warning: Cannot open $pdkroot/share/Fulcrum/stream/maskops.txt";
    my $df2=0;
    my $gds=0;
    while (<MO>) {
        chomp;
        s/\t/ /g;
        s/  */ /g;
        s/^ //;
        s/ $//;
        next if /^$/;
        s/[\(\)]/ /g;
        s/  */ /g;
        my @f=split;
        my $mask=shift @f;
        foreach my $f (@f) {
            if ($f =~ /;/) {
                my ($l,$p)=split(/;/,$f);
                $mask{$mask} .= "$l;$p ";
                $lpp{"$l;$p"} .= "$mask ";
            }
        }
    }
    close MO;

    my %layers=();
    foreach my $layer (split(/,/, $layers)) {
        if ($layer =~ /^m/) {
            $layer =~ s/^m//;
            foreach my $lpp (split(/ /,$mask{$layer})) {
                my ($l,$p) = split(/;/, $lpp);
                $layers{"$l;$p"}=1;
            }
        }
        else {
            $layers{$layer}=1;
        }
    }
    $layers = join(",", sort keys %layers);
}

sub fullgds {
    my $task="fullgds";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $ut= $use_tag ? " --use-tag" : "";
    my $cmd="gdsIIWrite --64 --fulcrum-pdk-root '$pdkroot' --view-name '$viewName' --dfII-dir '$dfIIdir' --cast-path '$castpath' --working-dir '$workingDir' --output-root-cell-name '$rootcellname' --tapeout --flatten-pcells --suppress-pins --max-heap-size '$maxheapsize{$task}' --jre-args '$jreargs' --cell '$cell' --output-bind-file '$rootcellname.bind' --pipo-log=strmOutFull.log --max-name-length=$maxnamelen$ut";
    $cmd .= " --debug" if $debug;
    $cmd .= " --output '$outgds'" if $outgds ne "";
    unlink $outgds if $outgds ne "";
    qrsh($task,$cmd,"","");
}

sub subgds {
    my $task="subgds";
    if (! defined($reducedgds) or $layers eq "*" or $layers eq "") {
        print STDERR "Skipping $task because of layers='$layers'" if $verbose;
        return 0;
    }
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $ut= $use_tag ? " --use-tag" : "";
    my $cmd="gdsIIWrite --fulcrum-pdk-root '$pdkroot' --view-name '$viewName' --dfII-dir '$dfIIdir' --cast-path '$castpath' --working-dir '$workingDir' --output '$reducedgds' --output-root-cell-name '$rootcellname' --tapeout --flatten-pcells --suppress-pins --max-heap-size '$maxheapsize{$task}' --jre-args '$jreargs' --cell '$cell' --layers '$layers' --max-name-length=$maxnamelen$ut --pipo-log=strmOutSub.log";
    $cmd .= " --debug" if $debug;
    unlink $reducedgds;
    qrsh ($task,$cmd,"","");
}

sub fullcdl {
    my $task="fullcdl";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
#    my $cmd = "mmonitor \&\n";
    my $cmd = "cast2cdl --fulcrum-pdk-root='$pdkroot' --process-dependent-name --max-heap-size='$maxheapsize{$task}' --cast-path='$castpath' --cell='$cell' --output='$rootcellname.cdl'\n";
#    $cmd .= "pkill -HUP -s 0 mmonitor\npkill -s 0 mmonitor";
    return qrsh($task,$cmd,"","");
}

sub gdsxor1 {
    # xor of the full gds with the reduced gds expects no errors in the reduced layers
    my $task="gdsxor1";
    if (! $tasks{$task} or ! defined ($reducedgds)) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    # notes on qsub
    # for holdid use --trigger with nbjob run
    # see qb 
    my $cmd="gdsxor --large --layerlist='$layers' '$outgds' '$reducedgds'";
    qrsh($task,$cmd,"fullgds,subgds","-l drc=1");
}

sub gdsxor2 {
    # xor of the new full gds with the old full gds if any
    my $task="gdsxor2";
    if ( ! defined $prevgds ) {
        print STDERR "Skipping $task because prevgds not defined." if $verbose;
        return 0;
    }
    if ( ! -s $prevgds ) {
        print STDERR "Skipping $task because $prevgds does not exist." if $verbose;
        return 0;
    }
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $cmd="gdsxor --large '$outgds' '$prevgds'";
    qrsh($task,$cmd,"fullgds","-l drc=1");
}


sub fullant {
    my $task="fullant";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $rules="$pdkroot/share/Fulcrum/icv/antenna.rul";
    my $cmd = "/bin/rm -rf hant; mkdir hant; cd hant; $ENV{ICV_SCRIPT} icv -c '$rootcellname' -i '../$outgds' '$rules'";
    qrsh($task,$cmd,"fullgds","-l hdrc=1,cc=$hdrc_threads");
}

sub fullhdrc {
    my $task="fullhdrc";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $rules="$pdkroot/share/Fulcrum/icv/drc.rul";
    # the following has NOT been tried with ICV
    if (defined ($hdrc_select_window) and $hdrc_select_window ne "") {
        my $fr;
        my $fro;
        open ($fr, "<$rules") or die "Cannot open '$rules'";
        $hdrc_bin=0;
        while ( -f "hdrc-$hdrc_bin.rul" ) { $hdrc_bin++;}
        my $newrules="hdrc-$hdrc_bin.rul";
        open ($fro, ">$newrules") or die "Cannot open '$newrules'";
        while (<$fr>) {
            chomp;
            if (/MAXIMUM_CELLNAME_LENGTH/) {
                print $fro $hdrc_select_window;
            }
            print $fro $_;
        }
        close $fro;
        close $fr;
        $rules="../$newrules";
    }
    my $cmd = "/bin/rm -rf hdrc$hdrc_bin; mkdir hdrc$hdrc_bin; cd hdrc$hdrc_bin; $ENV{ICV_SCRIPT} icv $fullchip -c '$rootcellname' -i '$outgds' '$rules'";
    qrsh($task,$cmd,"fullgds","-l hdrc=1,cc=$hdrc_threads");
}

# FIXME
sub fullbump {
    my $task="fullbump";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $cmd = "/bin/rm -rf hbump; mkdir hbump; cd hbump; $ENV{HERC_SCRIPT} hercules -threads $hdrc_threads -b '$rootcellname' -i '$outgds' '$pdkroot/share/Fulcrum/hdrc/hbump.rul'";
    qrsh($task,$cmd,"fullgds","-l hdrc=1,cc=$hdrc_threads");
}

sub fullhlvs {
    my $task="fullhlvs";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my $pwd=`pwd`;
    chomp $pwd;
    my $cdlname=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    my $grayarg="";
    if (-s $grayboxlist) {
        $grayarg="--gray-cell-list='$grayboxlist'";
    }
    my $hlvsmode="";
    if ( -s $grayboxlist ) {
        if ( $blackboxhlvs ) {
            $hlvsmode="--hlvs-mode=blackbox";
        }
        else {
            $hlvsmode="--hlvs-mode=graybox";
        }
    }
    chomp $cdlname;
    my $cmd = "/bin/rm -rf hlvs; mkdir hlvs; cd hlvs; hlvs --threads=$hlvs_threads $grayarg $hlvsmode --gds2-file '$outgds' --working-dir='$pwd/hlvs' -64bit=1 --cdl-file '../$rootcellname.cdl' --cdl-cell-name '$cdlname' --priority=0 --mem '$maxheapsize{$task}' --fulcrum-pdk-root '$pdkroot' --plug-wells=0 --merge-paths=0 --tapeout '$rootcellname'";
    qrsh($task,$cmd,"fullgds,fullcdl","-l herc=1,cc=$hlvs_threads");
}

# FIXME
sub fullerc {
    my $task="fullerc";
    if (! $tasks{$task}) {
        print STDERR "Skipping $task because it was not specified." if $verbose;
        return 0;
    }
    my %hold;
    $hold{$tasks{fullgds}}=1 if $tasks{fullgds} > 1;
    $hold{$tasks{fullcdl}}=1 if $tasks{fullcdl} > 1;
    my $holdid=join ",", keys %hold;
    my $pwd=`pwd`;
    chomp $pwd;
    $holdid = "-hold_jid '$holdid'" if length($holdid);
    my $cdlname=`echo '$cell' | rename --type=cell --from=cast --to=cadence`;
    my $grayarg="";
    if (-s $grayboxlist) {
        $grayarg="--gray-cell-list='$grayboxlist'";
    }
    my $hlvsmode="";
    if ( -s $grayboxlist ) {
        if ( $blackboxhlvs ) {
            $hlvsmode="--hlvs-mode=blackbox";
        }
        else {
            $hlvsmode="--hlvs-mode=graybox";
        }
    }
    chomp $cdlname;
    my $cmd = "hlvs --threads=$hlvs_threads $grayarg $hlvsmode --erc --gds2-file '$outgds' --working-dir='$pwd/herc' -64bit=1 --cdl-file '../$rootcellname.cdl' --cdl-cell-name '$cdlname' --priority=0 --mem '$maxheapsize{$task}' --fulcrum-pdk-root '$pdkroot' --plug-wells=0 --merge-paths=0 --tapeout '$rootcellname'";
    unlink "$task.out";
    unlink "$task.err";
    print STDERR "fullerc";
    my $pid=open2 (\*RD, \*WR, "qsub $holdid -N $task -l a=lx24-amd64,mem=$maxheapsize{$task},herc=1,cc=$hlvs_threads $qsubextras -cwd -o $task.out -e $task.err -v '$env_vars'");
    print WR "#!/bin/bash";
    print WR "/bin/rm -rf herc";
    print WR "mkdir herc";
    print WR "cd herc";
    print WR "$cmd";
    close WR;
    my $result = <RD>;
    chomp $result;
    if ($result =~ /Your job (\d+)/) {
        $jid{$task}=$1;
    }
    close RD;
    waitpid($pid,0);
    print STDERR $result if $verbose;
    my @f=split(/ /,$result);
    $f[2];
}

my %options = (
    "64" => \$bit64,
    "bind-rul=s" => \$bindfile,
    "cast-path=s" => \$castpath,
    "cell=s" => \$cell,
    "use-tag" => \$use_tag,
    "check-only" => \$checkonly,
    "debug" => \$debug,
    "dfII-dir=s" => \$dfIIdir,
    "flatten-pcells" => \$flattenpcells,
    "fulcrum-pdk-root=s" => \$pdkroot,
    "graybox-list=s" => \$grayboxlist,
    "hdrc-threads=i" => \$hdrc_threads,
    "hlvs-threads=i" => \$hlvs_threads,
    "hdrc-select-window=s" => sub {
        my ($x1,$y1,$x2,$y2)=split(/,/, $_[1]);
        if (defined ($y2) and $y2 > $y1 and $x2 > $x1) {
            $hdrc_select_window = "SELECT_WINDOW = [$x1,$y1, $x2,$y2]";
        }
        else {
            print STDERR "Error: select_window is --hdrc-select_window=x1,y1,x2,y2";
            exit 1;
        }
    },
    "hlvs-blackbox" => \$blackboxhlvs,
    "jre-args=s" => sub { $jreargs .= " $_[1]"; },
    "layers=s" => \$layers,
    "max-heap-size=s" => sub { $maxheapsize=$_[1];
        foreach my $task (keys %tasks) {
            $maxheapsize{$task}=$maxheapsize if ! $setmaxheap{$task};
        }
    },
    "max-name-length=i" => \$maxnamelen,
    "output-root-cell-name=s" => \$rootcellname,
    "output=s" => \$output,
    "previous-gds=s" => \$prevgds,
    "qsub-extras=s" => \$qsubextras,
    "task=s" => \$tasks,
    "verbose" => \$verbose,
    "view-name=s" => \$viewName,
    "working-dir=s" => \$workingDir,
    "full-chip" => sub { $fullchip="-D FULL_CHIP=1"; }
);

foreach my $task (sort keys %tasks) {
    eval "\$options{\"max-heap-size-$task=s\"}=sub { \$maxheapsize{$task}=\$_[1]; \$setmaxheap{$task}=1;}";
}

my %help = (
    "64" => "[$bit64] run in 64 bit mode (mostly irrelevant)",
    "bind-rul" => "[$bindfile] binding file, if specified",
    "cast-path" => "[$castpath] normal cast path",
    "cell" => "[$cell] cell cast name",
    "check-only" => "[$checkonly] Just check results",
    "debug" => "[$debug] progress info plus keep temporary files",
    "dfII-dir" => "[$dfIIdir] normal dfII-dir",
    "flatten-pcells" => "[$flattenpcells] stream out pcell flattening",
    "fulcrum-pdk-root" => "[$pdkroot] fulcrum pdk root path",
    "full-chip" => "do full chip DRC rules",
    "graybox-list" => "[$grayboxlist] list of cells for black/graybox (cast names)",
    "hdrc-threads" => "[$hdrc_threads] threads to use for hdrc",
    "hlvs-threads" => "[$hlvs_threads] threads to use for hlvs and erc",
    "hdrc-select-window" => "[$hdrc_select_window] restrict area for fullhdrc only",
    "hlvs-blackbox" => "[$blackboxhlvs] blackbox (0=graybox if list provided)",
    "jre-args" => "[$jreargs] for java sub-tasks",
    "layers" => "[$layers] comma separated list of:\n".
                "                ### (all purposes for this layer)\n".
                "                ###;### (just this layer,purpose)\n".
                "                m### (all lpp for this mask)\n".
                "                * (all lpp's required for tapeout)\n".
                "                blank/missing (all lpp in tech file)",
    "max-heap-size" => "[$maxheapsize] for java sub-tasks and for qsub memory",
    "max-heap-size-{task}" => "for java sub-tasks and for qsub memory for specific task",
    "max-name-length" => "[$maxnamelen] longest allowed gds name",
    "output" => "[$output] output gds file name",
    "output-root-cell-name" => "[$rootcellname] rename of top cell from default",
    "previous-gds" => "[$prevgds] The gds file from the last tapeout",
    "qsub-extras" => "[$qsubextras] longest allowed gds name",
    "task" => "[$tasks] list of tasks",
    "use-tag" => "[$use_tag] gdsIIWrite generates tag cell",
    "verbose" => "[$verbose] more progress info",
    "view-name" => "[$viewName] the dfII view name",
    "working-dir" => "[$workingDir] usually the current directory",
);

sub usage {
    my ($msg)=@_;
    my $name=$0;
    $name =~ s:.*/::;
    print STDERR $msg if defined $msg;
    printf STDERR <<EU;
Usage: $name [options]
  --cell=[${$options{"cell=s"}}]
  --dfII-dir=[${$options{"dfII-dir=s"}}]
  --fulcrum-pdk-root=[${$options{"fulcrum-pdk-root=s"}}]
Optional...
EU
    my %mandatory = (
        "cast-path" => 1, "cell" => 1, "fulcrum-pdk-root" => 1, "dfII-dir" => 1 );
    foreach my $option (sort keys %options) {
        my $opt = $option;
        $opt =~ s/=.*//;
        if ( ! defined ($mandatory{$opt})) {
            if ($opt eq "max-heap-size") {
                print "  --$opt $help{$opt}";
                print "  --max-heap-size-<task>=mem overrides above";
            }
            elsif ( defined ($help{$opt})) {
                print "  --$opt $help{$opt}";
            }
            elsif ($opt !~ /max-heap-size/) {
                print "  --$opt UNDEFINED HELP";
            }
        }
    }
    exit 1;
}

my $err=0;
GetOptions ( %options ) or usage;
`mkdir -p "$workingDir"`;
usage("invalid working-dir [$workingDir]") if ! -d $workingDir;
chdir $workingDir;
$output="$rootcellname.gds" if (! defined ($output) or $output eq "");
$outgds=$output;
my $pwd=`pwd`;
chomp $pwd;
$outgds = "$pwd/$output" if ($output !~ /\//) and $output ne "";
usage("output-root-cell-name not defined") if $rootcellname eq "";
usage("view-name not specified") if $viewName eq "";
usage("cell name not specified") if $cell eq "";
usage("invalid dfII-dir [$dfIIdir]") if ! -f "$dfIIdir/cds.lib.generated";
usage("invalid pdk root [$pdkroot]") if ! -d $pdkroot;
my $err=0;
foreach my $dir (split(/:/,$castpath)) {
    if ( ! -d $dir ) {
        $err++;
    }
}
usage("Invalid cast-path [$castpath]") if $err;

expandlayers if $layers =~ /m/;

my $taskerr=0;

$summary = 1 if $tasks =~ /summary/;
if (! defined($tasks) or $tasks eq "") {
    foreach my $key (keys %tasks) {
        $tasks{$key}=1;
    }
}
elsif (! ($tasks =~ /summary/)) {
    foreach my $key (split(/,/, $tasks)) {
        if (defined ($tasks{$key})) {
            $tasks{$key} = 1;
        }
        else {
            print STDERR "Illegal task $key";
            $taskerr++;
        }
    }
}

usage "Task error" if ($taskerr);
usage "dfII-dir improperly specified" if ! -d "$dfIIdir" or ! -s "$dfIIdir/cds.lib.generated";

if (! -s "cds.lib" ) {
    my $pwd=`pwd`;
    chomp $pwd;
    system "mkcdswd --target-dir='$pwd' --dfII-dir='$dfIIdir' --fulcrum-pdk-root='$pdkroot'";
}

if ( ! -s "cds.lib" or ! -s "assura_tech.lib") {
    print STDERR "Fatal: files cds.lib and assura_tech.lib not found.";
    exit 1;
}

$assura_technology = `awk '/^DEFINE Assura_/ {print \$2}' assura_tech.lib`;
chomp $assura_technology;

sub checkfullgds {
    if ( ! -s "$output") {
        print STDERR "Error: fullgds failed to generate a file";
        return 1;
    }
    else {
        print STDERR "Fullgds Run without Errors";
    }
    0;
}

sub checksubgds {
    if ( defined $reducedgds ) {
        if ( ! -s "$reducedgds" ) {
            print STDERR "Error: subgds had no output $reducedgds";
            return 1;
        }
    }
    0
}

sub checkfullcdl {
    if ( ! -s "$rootcellname.cdl") {
        print STDERR "Error: fullcdl failed to generate a file";
        return 1;
    }
    print STDERR "Fullcdl Run without Errors";
    0;
}

sub checkfullerc {
    if (! -f "herc/$rootcellname.LVS_ERRORS") {
        print STDERR "Error: fullerc did not run";
        return 1;
    }
    my $hercerr1=1;
    if (open (P, "<herc/$rootcellname.LAYOUT_ERRORS")) {
        $hercerr1=0;
        while (<P>) {
            chomp;
            last if /ERROR DETAILS/;
            $hercerr1=1 if /violations/ and ! /ERR_DUP_PLCMT/;
        }
    }
    print STDERR "Herc ERC Run without Errors" if ! $hercerr1;
    print STDERR "Herc ERC Run with Errors" if $hercerr1;
    if (! -f "herc/$rootcellname.LAYOUT_ERRORS") {
        print STDERR "Error: fullerc did not finish";
        return 1;
    }
    print STDERR "ERC Run without Errors" if ! $hercerr1;
    $hercerr1;
}

sub checkfullhdrc {
    if (! -f "hdrc$hdrc_bin/$rootcellname.LAYOUT_ERRORS") {
        print STDERR "Error: hdrc did not run";
        return 1;
    }
    my $hdrcerr=`grep -c ' violation.*found' 'hdrc$hdrc_bin/$rootcellname.LAYOUT_ERRORS'`;
    chomp $hdrcerr;
    print STDERR "Error: Hercules drc errors in hdrc$hdrc_bin/$rootcellname.LAYOUT_ERRORS" if ($hdrcerr);
    print STDERR "HDRC Run without Errors" if ! $hdrcerr;
    $hdrcerr;
}

sub checkfullant {
    if (! -f "hant/$rootcellname.LAYOUT_ERRORS") {
        print STDERR "Error: hant did not run";
        return 1;
    }
    my $hanterr=`grep -c ' violation.*found' 'hant/$rootcellname.LAYOUT_ERRORS'`;
    chomp $hanterr;
    print STDERR "Error: Hercules antenna drc errors in hant/$rootcellname.LAYOUT_ERRORS" if ($hanterr);
    print STDERR "Herc Antenna Run without Errors" if ! $hanterr;
    $hanterr;
}

sub checkfullbump {
    if (! -f "hbump/$rootcellname.LAYOUT_ERRORS") {
        print STDERR "Error: fullbump did not run";
        return 1;
    }
    my $hdrcerr=`grep -c ' violation.*found' 'hbump/$rootcellname.LAYOUT_ERRORS'`;
    chomp $hdrcerr;
    print STDERR "Error: Hercules drc errors in hbump/$rootcellname.LAYOUT_ERRORS" if ($hdrcerr);
    print STDERR "HBUMP Run without Errors" if ! $hdrcerr;
    $hdrcerr;
}

sub checkgdsxor1 {
    if ( ! -s "gdsxor1.out") {
        print STDERR "Error: gdsxor1 did not run";
        return 1;
    }
    my $match=`grep -c 'No shape was written to the output file' gdsxor1.out`;
    chomp $match;
    if ($match != 1) {
        print STDERR "Error: Mismatches in gdsxor1 ($match)";
        return 1;
    }
    print STDERR "XOR1 Run without Errors";
    0;
}

sub checkgdsxor2 {
    if ( ! -s "gdsxor2.out") {
        print STDERR "Error: gdsxor2 did not run";
        return 1;
    }
    my $match=`grep -c 'No shape was written to the output file' gdsxor2.out`;
    chomp $match;
    if ($match == 1) {
        print STDERR "Error: gdsxor2 shows no differences, quite odd";
        return 1;
    }
    my @egreplist=();
    foreach my $lpp (split(/,/, $layers)) {
        my ($l,$p) = split(/;/, $lpp);
        push @egreplist, "Layer\\( $l $p \\)";
    }
    my $egreplist=join("|", @egreplist);
    my $invalid = `egrep -cv '$egreplist' gdsxor2.out`;
    print STDERR "grep -cv '$egreplist' gdsxor2.out" if $verbose;
    chomp $invalid;
    if ($invalid > 0) {
        system "egrep -v '$egreplist' gdsxor2.out";
        return $invalid;
    }
    print STDERR "XOR2 Run without Errors";
    0;
}

sub checkfullhlvs {
    if (! -f "hlvs/$rootcellname.LVS_ERRORS") {
        print STDERR "Error: fullhlvs did not run";
        return 1;
    }
    my $hlvserr=1;
    open (P, "<hlvs/$rootcellname.LVS_ERRORS");
    while (<P>) {
        chomp;
        $hlvserr=0
            if(/TOP BLOCK COMPARE RESULT.*: PASS/i);
    }
    print STDERR "Herc LVS Run without Errors" if ! $hlvserr;
    print STDERR "Herc LVS Run with Errors" if $hlvserr;
    $hlvserr;
}

sub summarize {
    my @holdid=();
    foreach my $task (keys %tasks) {
        if ($tasks{$task} > 1) {
            if (defined ($jid{$task})) {
                push @holdid, $jid{$task};
            }
        }
    }
    my $holdid="-hold_jid=".join ",", @holdid;
    if (@holdid) {
        open (S, ">waitscript");
        print S "#!/bin/bash";
        print S "exit 0";
        close S;
        chmod 0700, "waitscript";
        sleep 1;
        $ENV{QB_DIAG_FILE}="/dev/null";
        $ENV{QB_LOCAL}=0;
        $ENV{QB_RUN_NAME}="wait$tapeoutid";
        $ENV{QRSH_FLAGS}="$holdid";
        system("qb ./waitscript 1>/dev/null 2>/dev/null");
    }
    my $err=0;
    $err += checkfullgds if $tasks{fullgds} or $summary;
    $err += checksubgds if $tasks{subgds} or $summary;
    $err += checkfullcdl if $tasks{fullcdl} or $summary;
    $err += checkfullhdrc if $tasks{fullhdrc} or $summary;
    $err += checkfullant if $tasks{fullant} or $summary;
    $err += checkfullbump if $tasks{fullbump} or $summary;
    $err += checkgdsxor1 if $tasks{gdsxor1} or $summary;
    $err += checkgdsxor2 if $tasks{gdsxor2} or $summary;
    $err += checkfullhlvs if $tasks{fullhlvs} or $summary;
    $err += checkfullerc if $tasks{fullerc} or $summary;
    $err;
}

if (defined ($layers) and $layers ne "" and $layers ne "*") {
    $reducedgds = $output;
    $reducedgds =~ s/\.g[^\.]*$//;
    $reducedgds .= "_${layers}_only.gds";
    $reducedgds =~ s/,//g;
    $reducedgds =~ s/;//g;
}

# run the tasks
if (! $checkonly) {
    $tasks{fullgds}  = fullgds;
    $tasks{subgds}   = subgds;
    $tasks{fullcdl}  = fullcdl;
    $tasks{gdsxor1}  = gdsxor1;
    $tasks{gdsxor2}  = gdsxor2;
    $tasks{fullbump} = fullbump;
    $tasks{fullhlvs} = fullhlvs;
    $tasks{fullerc} = fullerc;
    $tasks{fullant}  = fullant;
    $tasks{fullhdrc} = fullhdrc;
}

my $err=summarize;
print STDERR "--------------------";
if ($err) {
    print STDERR "Tapeout finished with errors";
    exit $err;
}
print STDERR "Tapeout finished with NO errors";
if (! $debug ) {
    `/bin/rm -f qb.* waitscript xStrmOut_cellMap.txt strmOut*.log`;
}
exit 0;
