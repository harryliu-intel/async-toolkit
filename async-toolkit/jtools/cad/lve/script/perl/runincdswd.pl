#!/usr/intel/bin/perl -w

sub cleanUp {
    if ( defined $cds_wd && -d $cds_wd ) {
        if (@_) { print "Deleting $cds_wd\n"; }
        `rm -rf $cds_wd`;
    }
}

$SIG{'INT'} = \&cleanUp;
$SIG{__DIE__} = \&cleanUp;

while ( !defined $dfII_dir || !defined $fulcrum_pdk_root ) {
    $ARGV[0] =~ /^--(.*)/ or die "ERROR: arguments\n";
    ($flag, $value) = split("=",$1);
    if    ($flag eq "dfII-dir")          { $dfII_dir = $value; }
    elsif ($flag eq "fulcrum-pdk-root")  { $fulcrum_pdk_root = $value; }
    else { die "ERROR: bad arg $flag\n"; }
    shift @ARGV;
}

my $pwd = $ENV{'PWD'};
my $mkcdswd = "mkcdswd";
$cds_wd = `mktemp -d '$pwd/cdswd.XXXXXX'`;
chomp $cds_wd;
system("chmod 755 \"$cds_wd\"");
( -x $mkcdswd ) or die "Can't find executable $mkcdswd";

system("$mkcdswd --dfII-dir=$dfII_dir --fulcrum-pdk-root=$fulcrum_pdk_root --target-dir=$cds_wd --force --temp");
( $? == 0 ) or die "mkcdswd failed";

chdir $cds_wd;
my $shell = $ENV{SHELL} or die "You must set the SHELL environment variable\n";
my $esc   = join(" ",map {"\Q$_\E"} @ARGV);
my $cmd   = "$shell -ci \'$esc\'";
system("$cmd");
( $? == 0 ) or die "$@ failed";
cleanUp();
