#!/usr/bin/perl -w
###############################################################################
#
# Print out a design's hierarchy
#  Usage:
#   hier_print.pl [+incdir+DIR] [-y LIBDIR] [+libext+LIBEXT] verilog_files...
#
# As usual there is no warranty!
#
# Costas
# $Header: /home/cc/v2html/rvp_scripts/RCS/hier_print.pl,v 1.2 2005/12/20 15:50:04 cc Exp $
###############################################################################

require rvp;
#use strict;

use vars qw($debug @files @inc_dirs @lib_dirs $lib_ext $vdb $m );

# defaults
$debug=0; 
@inc_dirs=('.');
@lib_dirs=();
$lib_ext='';

# process args
while ($_ = $ARGV[0]) {
    shift(@ARGV);
    if ( /^-y$/ ) { 
	&usage("$_ needs an arguement") if ($#ARGV < 0);
	push(@lib_dirs,shift(@ARGV));
	next; 
    }
    elsif ( /^\+incdir\+(.+)$/ ) { 
	push(@inc_dirs,$1);
	next; 
    }
    elsif ( /^\+libext\+(.+)$/ ) { 
	$lib_ext=$1;
	next; 
    }
    else {
	push(@files,$_);
    }
}

die "Usage $0 [+incdir+DIR] [-y LIBDIR] [+libext+LIBEXT] verilog_files..." unless @files;

# Parse the verilog
# pass in: list of files on command line
#          empty list of global includes []
#          empty hash of predefined defines {}
#          and set quiet to be quiet (1)
#          then incdirs, libdirs, libext
$vdb = rvp->read_verilog(\@files,[],{},1,\@inc_dirs,\@lib_dirs,$lib_ext);

my @problems = $vdb->get_problems();
if (@problems) {
    foreach my $problem ($vdb->get_problems()) {
	print STDERR "$problem.\n";
    }
    # die "Warnings parsing files!";
}


print "#  Source files scanned:\n";
print "#  ".join("\n#  ",$vdb->get_files()) . "\n\n";

# find modules at the top of the hierarchy (ie have no instantiators)
foreach $m ($vdb->get_modules()) {
    if  (! $vdb->get_first_instantiator($m)) {
        # now find all the files working down the hierarchy
	hierPrint($vdb,0,$m);
	print "\n";
    }
}

exit 0;

#################################################################################
#
# Subroutines

sub hierPrint {
    my ($vdb,$indent,$m) = @_;
    my ($imod,$mf,$iname,$l,@subMods);

    print "" . (" " x $indent) . "$m\n";

    # get a list of sub modules
    for (($imod,$mf,$iname,$l) = $vdb->get_first_instantiation($m );
	 $imod;
	 ($imod,$mf,$iname,$l) = $vdb->get_next_instantiation()) {
	push(@subMods,$imod);
    }
    
    # recurse down hierarchy
    foreach $imod (@subMods) {
	hierPrint($vdb,$indent+2,$imod);
    }

}


