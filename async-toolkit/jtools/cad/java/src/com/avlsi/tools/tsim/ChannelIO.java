// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2000,2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Interface representing a class that can act as both an input
 * channel and an output channel. </p>
 *
 * @history Modified from <code>HChannelInputOutput</code> for the new TSim
 * timing model.
 *
 * @author Aaron Denney
 * @version $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public interface ChannelIO extends ChannelInput, ChannelOutput {

    // purposefully empty; just a tagging interface

} // end of interface ChannelIO

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
