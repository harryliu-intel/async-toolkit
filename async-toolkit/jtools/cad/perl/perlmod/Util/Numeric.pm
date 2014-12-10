# $Id$
# $DateTime$

package Util::Numeric;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &round_float &is_numeric
    );
}

use strict;

#
# Rounds num to the nearest 10^digit.
# e.g. (11.25,-1) -> 11.3
#      (11.25,0)  -> 11
#      (11.21,1)  -> 10
#
sub round_float {
    my ($num,$digit) = @_;
    my $shifted_num = $num * 10**(-$digit);
    my $shifted_int = int($shifted_num);
    my $leftover = $shifted_num - $shifted_int;
    $shifted_int++ if ($leftover >= 0.5);
    return $shifted_int * 10**$digit;
}


# check if a string is a legal real number
sub is_numeric {
    my ($parm) = @_;
    if ($parm =~ /^[-+]?[\d]+(\.[\d]+)?([eE]?[-+]?[\d]+)?$/ ) { return 1; }
    else { return 0; }
}


1;
