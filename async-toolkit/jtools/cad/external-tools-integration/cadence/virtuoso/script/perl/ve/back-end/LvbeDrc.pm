# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# XXX: this back-end is not used for stage one.

# Verification back-end LvbeDrc
#
# This back-end performs DRC verification.  The nvpair used by this back-end
# is LVS tool independent.  The following nvpair is used:
#
#	Name			Description
#
#	CastObject		Cast object for the top-level cell.
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeDrc;
# LvbeDrc::verify();

package LvbeDrc;

use LvbeAssuraDrc;

#
# Perform DRC verification
#

sub verify {

	# XXX: Flag an error for stage one use.

	Lvfe::error("Use back-end AssuraDrc instead of Drc for stage one.");

	#
	# Ensure that required name-value pairs are defined.
	#

	Lvfe::assert_defined ("CastObject");

	#
	# Run Assura DRC verification
	#

	LvbeAssuraDrc::verify();
}

#
# Perl 'require' requires that a module return a true value.
#

1;
