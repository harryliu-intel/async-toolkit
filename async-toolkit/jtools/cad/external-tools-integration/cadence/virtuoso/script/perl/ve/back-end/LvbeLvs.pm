# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# XXX: this back-end is not used for stage one.

# Verification back-end LvbeLvs
#
# This back-end performs LVS verification.  The nvpair used by this back-end
# is LVS tool independent.  The following nvpair is used:
#
#	Name			Description
#
#	CastObject		Cast object for top-level cell
#	OpenOK			The string "true" if open connections are OK.
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeLvs;
# LvbeLvs::verify();

package LvbeLvs;

use LvbeAssuraLvs;

#
# Perform LVS verification
#

sub verify {

	# XXX: Flag an error for stage one use.

	Lvfe::error("Use back-end AssuraLvs instead of Lvs for stage one.");

	#
	# Ensure that required name-value pairs are defined.
	#

	Lvfe::assert_defined ("CastObject");

	#
	# Run Assura LVS verification
	#

	LvbeAssuraLvs::verify();
}

#
# Perl 'require' requires that a module return a true value.
#

1;
