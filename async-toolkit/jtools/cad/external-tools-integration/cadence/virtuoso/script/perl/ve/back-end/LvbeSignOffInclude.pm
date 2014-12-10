# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/script/perl/ve/back-end/LvbeAssuraDrc.pm#2 $
# $DateTime: 2002/04/16 16:04:07 $
# $Author: chrisb $

package LvbeSignOffInclude;

use Lvbe;
use LvbeAssura;

#
# Package name (used internally)
#

$PackageName = "AssuraSignOffInclude";

sub verify {
    #
    # Run Assura Verification Back-end
    #

    $wd = Lvbe::get_workingdir ();
    $rn = Lvfe::get_nvpair ("RunName");

    #
    # Start Assura RSF (run-specific file) with an optional inclusion
    #
        
    open RSF,">$wd/$rn\.rsf" or die "Can't write to $wd/$rn\.rsf";
    LvbeAssura::writeSignoffInclude(\*RSF);
    close(RSF);
}

#
# Perl 'require' requires that a module return a true value.
#

1;
