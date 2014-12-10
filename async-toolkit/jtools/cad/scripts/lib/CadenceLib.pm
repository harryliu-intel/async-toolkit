#!/usr/intel/bin/perl
package CadenceLib;
require Exporter;

use strict;

sub conv_reg_to_hash
{
    my ($oldfile) = @_;
    $_ = $oldfile;
	s/\//#2d/g;
    s/\-/#2d/g;
    s/\./#2e/g;
    s/\(/##28/g;
    s/\)/##29/g;
    s/\,/##2c/g;
    return $_;
}

sub conv_percent_to_reg
{
    my ($oldfile) = @_;
    $_ = $oldfile;

    s/\%40/(/g;
    s/\%41/)/g;
    s/\%44/,/g;
    return $_;
}

sub conv_to_reg
{
	my ($oldfile) = @_;
	$_ = $oldfile;

	s/-/\//;
    s/\%40/(/g;
    s/\%41/)/g;
    s/\%44/,/g;
    s/##28/(/g;
    s/##29/)/g;
    s/##2c/,/g;
	return $_;
}

sub recurse_pcdb
{
    my $library_path = shift;
    my $cell = shift;
    my $view = shift;
    my $pre = shift;

    my %instances;

    my $curcell = "$library_path/$cell/$view/pc.db";
    if( -e $curcell )
    {
        open PCDB, "<$curcell" or die "Could not open $curcell (used in $pre)";
        my @lines = <PCDB>;
        close PCDB;
        while(@lines)
        {
        	$_ = shift @lines;
        	chomp;
            if( /\#/ )
            {
               	$_ = shift @lines; 
                chomp;
               	my ($libname,$cellname,$viewname) = split ' ';
        		if( not $libname eq "ADD8M" and not /pcell/ )
        		{
        			my $printcell = conv_to_reg $cellname;
                	while( @lines )
                	{
        				$cellname = conv_percent_to_reg $cellname;
        				$cellname = conv_reg_to_hash $cellname;
        				$_ = shift @lines;
        				unshift @lines, $_ if( /^\#/ );
        	            last if( /^\#/ );
        	            chomp;
        				my $inst = $pre . $_;
        				$inst =~ s/\(/[/g;
        				$inst =~ s/\)/]/g;
                        $instances{$inst} = $printcell if( not /\#/ );
#        	            print "$inst $printcell\n" if( not /\#/ );
        				my $moreinstances = recurse_pcdb ($library_path,$cellname,$viewname,$inst . ".");
                        foreach $inst(keys %{$moreinstances})
                        {
                            $instances{$inst} = $moreinstances->{$inst};
                        }
                	}
        		}
            }
        }
    }
    else
    {
    	print STDERR "Could not open $curcell (used in $pre)\n";
    }
    return \%instances;
}

sub list_instances
{
    my $cdslib = shift;
    my $library = shift;
    my $cell = shift;
    my $view = shift;

    my $library_path = CadenceLib::get_cdslib_library_path($cdslib,$library);

    if( not defined $library_path )
    {
        print STDERR "Error locating $library in $cdslib\n\n";
        exit;
    }
    print STDERR "Path to library $library: $library_path\n";

    if( not -d $library_path )
    {
        print "ERROR! Could not locate path $library_path\n";
        exit;
    }

    $cell = conv_percent_to_reg $cell;
    $cell = conv_reg_to_hash $cell;
    my $instances = recurse_pcdb($library_path,$cell,$view);
    return $instances;
}

sub get_cdslib_library_path
{
    my $cdslib = shift;
    my $library = shift;

    open CDSLIB, "<$cdslib" or die "Could not open $cdslib";
    while(<CDSLIB>)
    {
        if( /^DEFINE\s+([^ ]+)\s+(.*)$/ )
        {
            my $lib = $1;
            my $path = $2;
            return $path if($lib eq $library);
        }
    }
    return;
}

1;
