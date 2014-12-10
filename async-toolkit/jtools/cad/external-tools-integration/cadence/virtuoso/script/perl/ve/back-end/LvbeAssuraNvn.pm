# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/script/perl/ve/back-end/LvbeAssuraNvn.pm#2 $
# $DateTime: 2002/04/16 16:04:07 $
# $Author: chrisb $

# Verification back-end LvbeAssuraNvn
#
# This back-end performs Assura LVS verification by calling the Assura 
# back-end with AssuraType=Nvn.
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeAssuraNvn;
# LvbeAssuraNvn::verify();

package LvbeAssuraNvn;

use Lvbe;
use LvbeAssura;

#
# Package name (used internally)
#

$PackageName = "AssuraNvn";

#
# Perform LVS verification
#

sub verify {
	#
	# Print header for assignment to Assura variables
	#

	Lvfe::header ("SET ASSURA VERIFICATION TYPE");

	#
	# Set AssuraType variable for LVS
	#

	Lvfe::add_nvpair ("AssuraType", "Nvn", "set", $PackageName);
	
	#
	# Run Assura Verification Back-end
	#

	LvbeAssura::verify();

}

#
# Perl 'require' requires that a module return a true value.
#

1;
