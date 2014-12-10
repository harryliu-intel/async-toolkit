# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification back-end LvbeAssuraLvs
#
# This back-end performs Assura LVS verification by calling the Assura 
# back-end with AssuraType=Lvs.
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeAssuraLvs;
# LvbeAssuraLvs::verify();

package LvbeAssuraLvs;

use Lvbe;
use LvbeAssura;

#
# Package name (used internally)
#

$PackageName = "AssuraLvs";

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

	Lvfe::add_nvpair ("AssuraType", "Lvs", "set", $PackageName);
	
	#
	# Run Assura Verification Back-end
	#

	LvbeAssura::verify();

}

#
# Perl 'require' requires that a module return a true value.
#

1;
