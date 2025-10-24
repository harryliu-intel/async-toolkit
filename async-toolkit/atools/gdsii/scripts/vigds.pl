#!/usr/intel/bin/perl
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $doemacs=0;

sub usage {
    print <<EU;
Usage: vigds [--emacs] <gdsfile>
EU
exit 1;
}

GetOptions ( "emacs" => \$doemacs, ) or usage;

my $gvim=`which gvim 2>/dev/null`;
chomp $gvim;
my $vim=`which vim 2>/dev/null`;
chomp $vim;
my $emacs=`which emacs 2>/dev/null`;
chomp $emacs;
my $rdgds = `which rdgds 2>/dev/null`;
chomp $rdgds;
if ( ! -x $rdgds ) {
    $rdgds =`fulcrum --path rdgds`;
    chomp $rdgds;
}
my $wrgds = $rdgds;
$wrgds =~ s/rdgds/wrgds/;
if ( ! -x $rdgds or ! -x $wrgds) {
    print STDERR "Cannot find required rdgds and wrgds";
    usage;
}

sub doit {
    my $rdgfile=`mktemp '/scratch/temp.gds.XXXXXX'`;
    chomp $rdgfile;
    if ( -r "$ARGV[0]" ) {
        `$rdgds "$ARGV[0]" > "$rdgfile"`;
    }
    my @stat=stat($rdgfile);
    system "$gvim '$rdgfile'";
    my @nstat=stat($rdgfile);
    if ($nstat[9] != $stat[9]) {
        `$wrgds "$rdgfile" "$ARGV[0]"`;
    }
    unlink $rdgfile;
}

usage if !@ARGV;
usage if ! -s $ARGV[0];
if ( $doemacs) {
    if ( -x $emacs) {
        $gvim = $emacs;
        doit;
    }
    else {
        print STDERR "No emacs executable found.";
        usage;
    }
}
elsif ( -x $gvim) {
    $gvim .= " -geom 60x50 --nofork -i NONE -n";
    doit if ! fork();
}
elsif ( -x $vim) {
    $gvim = "$vim -i NONE -n";
    doit;
}
else {
    print STDERR "NO gvim nor vim executable found!\n";
}
