/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.floorplanning;

import com.avlsi.floorplanning.PCellTypeInfo;


interface TransistorTypeInfo extends PCellTypeInfo {

    String getModelName();
    String getGateTerminalName();
    String getSourceTerminalName();
    String getDrainTerminalName();
    String getBulkTerminalName();

}
