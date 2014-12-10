#!/usr/bin/perl -w -I ..
###############################################################################
#
# File:         rvp_test.pl
# RCS:          $Header: /home/cc/v2html/rvp_scripts/RCS/rvp_test.pl,v 1.6 2006/01/09 20:51:23 cc Exp $
# Description:  Test RVP functions not used by v2html
# Author:       Costas Calamvokis
# Created:      Fri May  1 06:53:02 1998
# Modified:     Mon Jan  9 20:51:16 2006
# Language:     Perl
#
# Copyright (c) 1998-2006 Costas Calamvokis, all rights reserved.
#
###############################################################################

require rvp;


@files            = @ARGV;
%cmd_line_defines = ();
$quiet            = 1;
@inc_dirs         = ();
@lib_dirs         = ();
@lib_exts         = ();


$vdb = rvp->read_verilog(\@files,[],\%cmd_line_defines,
			  $quiet,\@inc_dirs,\@lib_dirs,\@lib_exts);
my @problems = $vdb->get_problems();
if (@problems) {
    foreach my $problem ($vdb->get_problems()) {
	print STDERR "$problem.\n";
    }
    # die "Warnings parsing files!";
}




foreach $module (sort $vdb->get_modules()) {
    print "Module $module\n\n";

    %parameters = $vdb->get_modules_parameters($module);
    foreach my $p (sort keys %parameters) {
	my $v = $parameters{$p};
	$v =~ s/[ \n]//gs;
	print "   parameter: $p defaults to \"$v\"\n";
    }


    foreach $sig (sort $vdb->get_modules_signals($module)) {
	($line,$a_line,$i_line,$type,$file,$posedge,$negedge,
	 $type2,$s_file,$s_line,$range,$a_file,$i_file,$dims) = 
	   $vdb->get_module_signal($module,$sig);

	print "   signal: $sig $type $type2 |$range|\n";
	print "      defined  $file:$line\n";
	print "      assigned $a_file:$a_line\n";
	print "      source   $s_file:$s_line\n";
	print "      input    $i_file:$i_line\n";
	print "      edges: $posedge,$negedge\n";
	foreach $dim (@$dims) {
	    print "        dimension: $dim\n";
	}


	for (($imod,$iname,$port,$l) = 
	     $vdb->get_first_signal_port_con($module,$sig );
	     $imod;
	     ($imod,$iname,$port,$l) = 
	     $vdb->get_next_signal_port_con()) {
	    print "     connected to: port $port of instance $iname of $imod\n";
	}
    }

    print "\n";
    for (($imod,$f,$iname,$l) = $vdb->get_first_instantiation($module );
	 $imod;
         ($imod,$f,$iname,$l) = $vdb->get_next_instantiation()) {

	print "     instance: $iname of $imod\n";
	%port_con = $vdb->get_current_instantiations_port_con();
	foreach $port (sort keys %port_con) {
	    my $p = $port_con{$port};
	    $p =~ s/[ \n]//gs;
	    print "        $port connected to \"$p\"\n";
	}


	%parameters = $vdb->get_current_instantiations_parameters();
	foreach my $p (sort keys %parameters) {
	    my $v = $parameters{$p};
	    $v =~ s/[ \n]//gs;
	    print "        parameter $p set to to \"$v\"\n";
	}


    }
    print "\n";

}

