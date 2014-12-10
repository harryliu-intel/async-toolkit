# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification back-end LvbeAssuraDrc
#
# This back-end performs Assura DRC verification by calling the Assura 
# back-end with AssuraType=Drc.
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeAssuraDrc;
# LvbeAssuraDrc::verify();

package LvbeAssuraDrc;

use Lvbe;
use LvbeAssura;

#
# Package name (used internally)
#

$PackageName = "AssuraDrc";

#
# Perform DRC verification
#

sub verify {
	#
	# Print header for assignment to Assura variables
	#

	Lvfe::header ("SET ASSURA VERIFICATION TYPE");

	#
	# Set AssuraType variable for DRC
	#

	Lvfe::add_nvpair ("AssuraType", "Drc", "set", $PackageName);
	
	#
	# Run Assura Verification Back-end
	#

	LvbeAssura::verify();

}

#
# Perl 'require' requires that a module return a true value.
#

1;
