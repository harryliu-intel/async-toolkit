#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$
# this is the new fulcrum updatefmdb

use Getopt::Long;
use DB_File;
use strict;

sub usage {
    my $prog=$0;
    $prog =~ s:.*/::;
    print STDERR <<H;
Usage: $prog [--help] [--verbose] [--toolhome toolhome-dir]
    updates all of the fulcrum databases, if you do not specify --toolhome,
    you must be the build user.
H
exit 1;
}

my $mvcmd="/bin/mv -fv";
sub cmpchg {
    my ($a, $b) = @_;
    $a =~ s/.*_//;
    $b =~ s/.*_//;
    $a - $b;
}

sub getpaths {
    my ($dir)=@_;
    local (*D);
    opendir (D, "$dir");
    my @tools=sort (grep (/^[a-z]/, readdir (D)));
    my @list=();
    closedir D;
    foreach my $tool (@tools) {
        opendir (D, "$dir/$tool");
        my @vers=sort (grep (/^[1-9a-zA-Z]/, readdir (D)));
        closedir D;
        foreach my $ver (@vers) {
            push @list, "$dir/$tool/$ver" if -d "$dir/$tool/$ver";
        }
    }
    @list;
}

$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";
my $fulcrumdefault="/p/rrc/tools/fulcrum";
my $fulcrum=$fulcrumdefault;
my $tmp = "tmp";
my $aname=`uname -sm`;
chomp $aname;
$aname =~ s/ /-/;
my %release;
my %beta;
my %latest;
my %all;
my %devel;
my %prefs;
my %temp;
my $toolhome="";
my $verbose=0;
my $branch="";
# latest and all
my %options = (
    "toolhome=s" => \$toolhome,
    "help" => sub { usage; },
    "verbose" => \$verbose,
    "branch=s" => \$branch,
);

GetOptions ( %options ) or usage;
$branch = "" if $branch eq "main";

my $pdkdir;
my $tooldir;

sub readfile {
    my ($file)=@_;
    local (*P);
    local ($_);
    my %list;
    open (P, "<$file");
    while (<P>) {
        chomp;
        my ($pkg,$chg)=split;
        $chg = "${branch}_$chg" if ($branch ne "" and $chg =~ /^\d+$/);
        if ($pkg =~ /^ *#/) {
            next;
        }
        if ($pkg eq "include") {
            if ( ! ( $chg =~ m:^/: ) ) {
                my $x = $file;
                $x =~ s:/[^/]+$::;
                $chg = "$x/$chg";
            }
            my %t = readfile ("$chg");
            foreach my $key (keys %t) {
                $list{$key}=$t{$key};
            }
        }
        elsif ($pkg =~ /pdk$/) {
            if ( ! -f "$pdkdir/$pkg/$chg/.installed") {
                print "$pkg $chg does not exist";
            }
            else {
                $list{"$pkg"}=$chg;
            }
        }
        elsif ( ! -f "$tooldir/$pkg/$chg/.installed-$aname") {
            print "$pkg $chg does not exist";
        }
        else {
            $list{"$pkg"}=$chg;
        }
    }
    %list;
}

if ( $toolhome ne "") {
    $fulcrum = $toolhome;
    $fulcrum =~ s:/(tools|pdk|packages|config)$::;
    mkdir $fulcrum;
}
system "mkdir -p '$fulcrum'" if ! -d $fulcrum;
$pdkdir = "$fulcrum/pdk";
mkdir $pdkdir if ! -d "$pdkdir";
$tooldir = "$fulcrum/tools";
mkdir $tooldir if ! -d "$tooldir";
my $configtop = "$fulcrum/config";
mkdir "$configtop" if ! -d "$configtop";
$configtop .= "/$branch" if $branch ne "";
mkdir "$configtop" if ! -d "$configtop";
my $configdir = "$configtop/$aname";
mkdir $configdir if ! -d $configdir;
system "/bin/rm -f '$configdir'/__db.* 2>/dev/null";
my $reldb = "$configdir/releasedb";
my $latdb = "$configdir/latestdb";
my $alldb = "$configdir/alldb";
my $betadb = "$configdir/betadb";
my $prefdb = "$configdir/prefdb";
my $develdb = "$configdir/develdb";
print "Creating Latest and All DBs in $configdir";
my $bindir="$tooldir/*/*/bin";
system "/bin/rm -f $latdb$tmp* $alldb$tmp* >/dev/null 2>&1";
dbmopen (%latest, "$latdb$tmp", 0644) || warn "Cannot open $latdb$tmp";
dbmopen (%all, "$alldb$tmp", 0644) || warn "Cannot open $alldb$tmp";
dbmopen (%devel, "$develdb$tmp", 0644) || warn "Cannot open $develdb$tmp";
my %exists=();
my @pathlist=getpaths($tooldir);
$|=1;
my $installed;
foreach my $path (@pathlist) {
    next if ( ! -d "$path");
    $_ = $path;
    s/^\.\///;
    $installed=0;
    $installed = 1 if ( -f "$path/.installed-$aname" and -d "$path/bin");
#    print "$path ". ($installed ? "Installed $aname" : "Not installed $aname") if $verbose;
    if ( -d "$path/bin") {
        opendir (X, "$path/bin");
        my @files = sort (grep (/^[^\.]/, readdir (X)));
        closedir X;
        foreach my $file (@files) {
            my $tpath = "$path/bin/$file";
            next if ( ! -f "$tpath" or ! -x "$tpath");
            my @f=split( /\//,$tpath);
            my $pkg=$f[$#f-3];
            my $chg = $f[$#f-2];
            my $tool=$f[$#f];
            if (!(($chg =~ /^${branch}_/ and $branch ne "") or
                ($chg =~ /^\d+$/ and $branch eq ""))) {
                    next;
            }
            $devel{"$pkg:$tool:$chg"}=$chg;
            if ($installed) {
                $all{"$pkg:$tool:$chg"}=$chg;
                if (cmpchg ($chg,$latest{"$pkg:$tool"}) > 0) {
                    if ( defined ($latest{"$pkg:$tool"})) {
                        delete $latest{"$pkg:$tool"};
                    }
                    $latest{"$pkg:$tool"}=$chg;
                    $exists{$tool}=$pkg;
                }
            }
        }
    }
}
@pathlist=getpaths($pdkdir);
$|=1;
my $installed;
my $tool="";
foreach my $path (@pathlist) {
    next if ( ! -d "$path");
    $_ = $path;
    s/^\.\///;
    $installed = 0;
    $installed = 1 if ( -f "$path/.installed" );
    my @f=split(/\//,$_);
    my $chg = $f[$#f];
    my $pkg = $f[$#f-1];
    if (!(($chg =~ /^${branch}_/ and $branch ne "") or
        ($chg =~ /^\d+$/ and $branch eq ""))) {
            next;
    }
    $devel{"$pkg:$tool:$chg"}=$chg;
#    print "$path ". ($installed ? "Installed" : "Not installed") if $verbose;
    if ($installed) {
        $all{"$pkg:$tool:$chg"}=$chg;
        if (cmpchg ($chg,$latest{"$pkg"}) > 0) {
            if ( defined ($latest{"$pkg"})) {
                delete $latest{"$pkg"};
            }
            $latest{"$pkg"}=$chg;
        }
    }
}
dbmclose %latest;
dbmclose %all;
dbmclose %devel;
print "Creating Release DB";
if ( -f "$configdir/release" and -r "$configdir/release" ) {
    print "Using $configdir/release" if $verbose;
    %release = readfile ("$configdir/release");
}
elsif ( -f "$configtop/release" and -r "$configtop/release") {
    print "Using $configtop/release" if $verbose;
    %release = readfile ("$configtop/release");
}
else {
    print "Using latest in lieu of release" if $verbose;
    dbmopen (%release, "$latdb$tmp", undef) || warn "Cannot open $latdb$tmp";
}
system "/bin/rm -f $reldb$tmp* 2>/dev/null";
dbmopen (%temp, "$reldb$tmp", 0644) or warn "Cannot open $reldb$tmp";
my %done=();
foreach my $pkg (sort keys %release) {
    my $pkgname=$pkg;
    $pkgname =~ s/:.*//;
    if (! $done{$pkgname}) {
        if (! ( $pkgname =~ /pdk$/) ) {
            opendir (P, "$tooldir/$pkgname/$release{$pkg}/bin");
            my @files = sort ( grep (/^[^\/]/, readdir (P)));
            closedir P;
            foreach my $file (@files) {
                my $path = "$tooldir/$pkgname/$release{$pkg}/bin/$file";
                next if ( ! -f $path  or ! -x $path );
                if ($path =~ m:/bin/[^/]+$:) {
                    my @f=split( /\//,$path);
                    my $pkg=$f[$#f-3];
                    my $chg = $f[$#f-2];
                    my $tool=$f[$#f];
                    $temp{"$pkg:$tool"}=$chg;
                }
            }
            close P;
        }
        else {
            if ( -f "$pdkdir/$pkgname/$release{$pkg}/.installed" ) {
                my $chg = $release{$pkg};
                $temp{"$pkg"}=$chg;
            }
        }
    }
    $done{$pkgname}=1;
}
undef %done;
dbmclose %temp;
print "Creating Beta DB";
if ( -f "$configdir/beta" and -r "$configdir/beta") {
    %beta = readfile ("$configdir/beta");
}
else {
    %beta = readfile ("$configtop/beta");
}
system "/bin/rm -f $betadb$tmp* >/dev/null 2>&1";
dbmopen (%temp, "$betadb$tmp", 0644) or warn "Cannot open $betadb$tmp";
foreach my $pkg (sort keys %beta) {
    if (! ( $pkg =~ /pdk$/) ) {
        opendir (P, "$tooldir/$pkg/$beta{$pkg}/bin");
        my @files = sort ( readdir (P));
        closedir P;
        foreach my $file (@files) {
            my $path="$tooldir/$pkg/$beta{$pkg}/bin/$file";
            next if ( ! -f "$path" or ! -x "$path");
            if ($path =~ m:/bin/[^/]+$:) {
                my @f=split( /\//,$path);
                my $pkg=$f[$#f-3];
                my $chg = $f[$#f-2];
                my $tool=$f[$#f];
                $temp{"$pkg:$tool"}= $chg;
            }
        }
    }
    else {
        if ( -f "$pdkdir/$pkg/$beta{$pkg}/.installed" ) {
            my $chg = $release{$pkg};
            $temp{"$pkg"}=$chg;
        }
    }
}
dbmclose %temp;
print "Creating Prefs DB";
dbmopen (%latest, "$latdb$tmp", undef) || warn "Cannot open $latdb$tmp";
dbmopen (%prefs, "$prefdb$tmp", 0644);
my @preferred=(
    "$configdir/preferred",
    "$configtop/preferred",
    "$fulcrumdefault/config/$aname/preferred",
    "$fulcrumdefault/config/preferred",
);

my $pf=0;
foreach my $p (@preferred) {
    if (open (P, "<$p")) {
        print "Using $p" if $verbose;
        $pf=1;
        last;
    }
}
warn "Cannot open any preferred file" if ! $pf;
while (<P>) {
    chomp;
    my ($pkg,$exe)=split(/:/,$_);
    if (defined ($latest{"$pkg:$exe"})) {
        $prefs{$exe} = $pkg;
    }
    elsif (defined ($exists{$exe})) {
        $prefs{$exe}=$exists{$exe};
    }
}
close P;
dbmclose %prefs;
dbmclose %latest;
print "Cleanup";
system "/bin/chmod +w ${reldb}bak* >/dev/null 2>&1; /bin/rm -f ${reldb}bak* >/dev/null 2>&1";
system "/bin/chmod +w ${latdb}bak* >/dev/null 2>&1; /bin/rm -f ${latdb}bak* >/dev/null 2>&1";
system "/bin/chmod +w ${alldb}bak* >/dev/null 2>&1; /bin/rm -f ${alldb}bak* >/dev/null 2>&1";
system "/bin/chmod +w ${prefdb}bak* >/dev/null 2>&1; /bin/rm -f ${prefdb}bak* >/dev/null 2>&1";
system "/bin/chmod +w ${betadb}bak* >/dev/null 2>&1; /bin/rm -f ${betadb}bak* >/dev/null 2>&1";
system "/bin/chmod +w ${develdb}bak* >/dev/null 2>&1; /bin/rm -f ${develdb}bak* >/dev/null 2>&1";
opendir (P, "$configdir");
my @files=grep (/db$tmp/, readdir (P));
closedir P;
foreach my $file (@files) {
    my @f=split(/\./,$file);
    my $ext;
    if (defined ($f[1])) {
        $ext = ".".$f[1];
    }
    else {
        $ext = "";
    }
    if ( "$configdir/$file" =~ m:$latdb$tmp:) {
        system "$mvcmd $latdb$ext ${latdb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $latdb$tmp$ext $latdb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $latdb$ext >/dev/null 2>&1";
    }
    elsif ("$configdir/$file" =~ m:$reldb$tmp:) {
        system "$mvcmd $reldb$ext ${reldb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $reldb$tmp$ext $reldb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $reldb$ext >/dev/null 2>&1";
    }
    elsif ("$configdir/$file" =~ m:$alldb$tmp:) {
        system "$mvcmd $alldb$ext ${alldb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $alldb$tmp$ext $alldb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $alldb$ext >/dev/null 2>&1";
    }
    elsif ("$configdir/$file" =~ m:$prefdb$tmp:) {
        system "$mvcmd $prefdb$ext ${prefdb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $prefdb$tmp$ext $prefdb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $prefdb$ext >/dev/null 2>&1";
    }
    elsif ("$configdir/$file" =~ m:$betadb$tmp:) {
        system "$mvcmd $betadb$ext ${betadb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $betadb$tmp$ext $betadb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $betadb$ext >/dev/null 2>&1";
    }
    elsif ("$configdir/$file" =~ m:$develdb$tmp:) {
        system "$mvcmd $develdb$ext ${develdb}bak$ext >/dev/null 2>&1";
        system "$mvcmd $develdb$tmp$ext $develdb$ext >/dev/null 2>&1";
        system "/bin/chmod 444 $develdb$ext >/dev/null 2>&1";
    }
    else {
        print "Unknown file $file";
    }
}
1;
