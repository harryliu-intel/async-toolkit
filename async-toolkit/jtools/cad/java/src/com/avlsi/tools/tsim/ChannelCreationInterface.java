// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

/**
 * Class for being called back when a new BufferedChannel is created
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface ChannelCreationInterface {
    
    void newChannel(BufferedChannel channel); 
}

