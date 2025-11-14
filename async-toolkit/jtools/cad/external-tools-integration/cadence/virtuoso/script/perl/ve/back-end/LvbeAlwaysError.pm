# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Verification back-end LvbeAlwaysError
#
# This back-end always flags an error.  No nvpairs are used by this back-end.
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeAlwaysError;
# LvbeAlwaysError::verify();

package LvbeAlwaysError;

sub verify {
	Lvfe::error ("This back-end always returns an error");
}

#
# Perl 'require' requires that a module return a true value.
#

1;
