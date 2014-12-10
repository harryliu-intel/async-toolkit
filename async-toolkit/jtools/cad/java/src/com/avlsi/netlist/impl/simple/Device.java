/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl.simple;

import com.avlsi.file.common.HierName;

import com.avlsi.netlist.AbstractDevice;

import com.avlsi.netlist.impl.simple.Net;

abstract class Device implements AbstractDevice {

    private final HierName m_Name;

    Device( final HierName name ) { 
        m_Name = name;
    }

    public HierName getName() {
        return m_Name;
    }

    public abstract void replaceNet( final Net oldNet, final Net newNet );
}
