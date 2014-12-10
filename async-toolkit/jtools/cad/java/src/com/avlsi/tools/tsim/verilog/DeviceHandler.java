/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

/**
 * Class for Handling when a new device is created in Verilog
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface DeviceHandler {

    void deviceInstantiated(DeviceInfo info);
}

