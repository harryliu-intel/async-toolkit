#!/usr/intel/bin/perl
# vim:ts=2:sw=2

# Jeremy Boulton, Apr 12 2002
# Input is a text file which describes a serial split tree or serial
# merge tree.  Output is a diagram in PostScript format.
# Each line of the input file has the format: bit-string label-text
# where bit-string is a string of ones and zeros with the leftmost
# bit being the one closest to the root of the tree.
#
# Example:
#   011 A
#   010 B
#   00 C
#   111 D
#   110 E
#   100 F
#   101 G


use strict 'vars';

use Getopt::Std;

# Configuration: measurements in points (1/72 inch)

my $labelheight = 12;
my $slope_max_x = 30;
my $hline_len = 10;
my $vspace = 5;
my $fontsize = 10;
my $merge_tree = 0;
my $line_width = 1;

##################################


sub show_usage
{
	print STDERR "Usage: serialtree2ps [options] filename\n";
  print STDERR "Command line options:\n";
  print STDERR "\t-m     Generate a merge tree instead of a split tree\n";
  print STDERR "\t-h     Show this help message\n";
  print STDERR "\n";
  exit;
}

if( not getopts('mh') ) {
  print STDERR "Error processing command line flags\n";
  show_usage;
}

if( $main::opt_h ) {
  show_usage;
}

if( $main::opt_m ) {
	$merge_tree = 1;
}

my $treelistref = []; # anonymous list
my $maxdepth = 0;

sub add_tree
{
	my ($treeref, $depth, $name, @bit_list) = @_;
	$depth++;
	if( $maxdepth < $depth ) {
		$maxdepth = $depth;
	}

	my $bit = shift @bit_list;
	if( $bit ne "0" and $bit ne "1" ) {
		print STDERR "Invalid bit: \"$bit\"\n";
		exit;
	}

	if( defined $treeref->[$bit] ) {
		if( @bit_list != 0 and ref($treeref->[$bit]) eq "ARRAY" ) {
			add_tree($treeref->[$bit], $depth, $name, @bit_list);
		}
		else {
			print STDERR "Could not add \"$name\": node in use.\n";
			exit;
		}
	}
	else {
		if( @bit_list != 0 ) {
			$treeref->[$bit] = []; # anonymous list
			$treeref->[$bit]->[2] = []; # anonymous list for metainfo
			add_tree($treeref->[$bit], $depth, $name, @bit_list);
		}
		else {
			$treeref->[$bit] = $name;
		}
	}
}


# compute_boxes

sub compute_boxes
{
	my ($treeref, $tonow) = @_;
	my (@u, @d, @voffset, @vheight);

	foreach my $bit (0,1) {
		if( defined $treeref->[$bit] ) {
			if( ref($treeref->[$bit]) eq "ARRAY" ) {
				($u[$bit], $d[$bit]) = compute_boxes($treeref->[$bit], "$tonow$bit");
			}
			else {
				($u[$bit], $d[$bit]) = ($labelheight/2, $labelheight/2);
			}
		}
		else {
			print STDERR "UNDEFINED value in tree at $tonow$bit.\n";
			exit;
		}
	}

	$voffset[0] = $d[0] + ($vspace/2);
	$voffset[1] = $u[1] + ($vspace/2);
	$vheight[0] = ($u[0] + $d[0] + $vspace/2);
	$vheight[1] = ($u[1] + $d[1] + $vspace/2);
	$treeref->[2] = [@voffset, @vheight];

	return @vheight;
}


sub ps_line
{
	my ($x, $y, $bit, $ydelta) = @_;

	print "newpath\n";
	print "$x $y moveto\n";

	my $dx = $slope_max_x;
	my $dy = $ydelta;
	my $dx2 = $hline_len;

	if( $dx > $dy ) {
		$dx = $dy;
	}# elsif ( $dy > (3*$dx) ) {
	#	$dx = $dy/3;
	#}
	if( $bit eq 1 ) {
		$dy = -$dy;
	}
	if( $merge_tree ) {
		$dx = -$dx;
		$dx2 = -$dx2;
	}
	print "$dx $dy rlineto\n";
	print "$dx2 0 rlineto\n";
	print "stroke\n";
	return ($x+$dx+$dx2, $y+$dy);
}


# ps_output

sub ps_output
{
	my ($treeref, $x, $y) = @_;
	my ($hres, $vres, $offres);

	my $ref = $treeref->[2];
	my ($upper_y, $lower_y, $upper_tot, $lower_tot) = @$ref;

	if( not defined $y ) {
		my $dx = 10;
		if( $merge_tree ) {
			$dx = -$dx;
		}
		# find root coordinate and draw root line
		$y = $lower_tot + 72;
		print "newpath\n";
		print "$x $y moveto\n";
		print "$dx 0 rlineto\n";
		print "stroke\n";
		$x += $dx;
	}

	foreach my $bit (0,1) {
		# Draw a line from the current point to each of its children
		my ($newx, $newy) = ps_line($x, $y, $bit, $ref->[$bit]);
		if( defined $treeref->[$bit] ) {
			if( ref($treeref->[$bit]) eq "ARRAY" ) {
				ps_output($treeref->[$bit], $newx, $newy);
			}
			else {
				# print name
				my $name = $treeref->[$bit];
				$newy -= ($fontsize/2);
				if( $merge_tree ) {
					$newx -= 10;
					print "$newx $newy moveto\n";
					print "($name) dup stringwidth pop neg 0 rmoveto show stroke\n";
				} else {
					$newx += 10;
					print "$newx $newy moveto\n";
					print "($name) show stroke\n";
				}
			}
		}
		else {
			print STDERR "UNDEFINED value in tree.\n";
			exit;
		}
	}
}


sub ps
{
	my ($treeref) = @_;

	compute_boxes($treeref,"");

	print "%!PS-Adobe-1.0\n";
	if( $merge_tree ) {
		print "%%Title: Merge Tree\n";
	} else {
		print "%%Title: Split Tree\n";
	}
	print "%%Pages: 1\n";
	print "%%EndComments\n";
	print "\n";
	print "0 setgray\n";
	print "$line_width setlinewidth\n";
  print "/Helvetica findfont\n";
  print "$fontsize scalefont\n";
  print "setfont\n";
	print "\n";
	if( $merge_tree ) {
		ps_output($treeref,7.5*72,undef);
	} else {
		ps_output($treeref,72,undef);
	}
	print "showpage\n";
}


sub print_tree
{
	my ($treeref, $depth) = @_;
	$depth++;

	foreach my $bit (0,1) {
		if( defined $treeref->[$bit] ) {
			if( ref($treeref->[$bit]) eq "ARRAY" ) {
				print_tree($treeref->[$bit], $depth);
			}
			else {
				print " "x$depth . $bit . $treeref->[$bit] . "\n";
			}
		}
		else {
			print " "x$depth . $bit . "UNDEFINED\n";
		}
	}
}

show_usage() if (@ARGV < 1);

open INPUT, "<$ARGV[0]" or die "Unable to read file \"${ARGV[0]}\"";

while (<INPUT>) {
	chomp;
	my ($bits, $name) = split ' ', $_, 2;
	my @bitlist = split //, $bits;
	add_tree($treelistref, 0, $name, @bitlist);
}

close INPUT;

#print_tree($treelistref, 0);
ps($treelistref);
