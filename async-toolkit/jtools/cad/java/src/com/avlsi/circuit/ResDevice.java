/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.circuit;

import com.avlsi.file.common.HierName;
import com.avlsi.file.aspice.Resistor;

/**********************************************************************
 * Resistor device class, for use as an edge in a circuit graph.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
final class ResDevice {

    final AbstractNode n0,n1;
    private final float res;
    private boolean mark;

    /** Constructs a ResDevice from two node terminals and a resistance. **/
    ResDevice(final AbstractNode a, final AbstractNode b, float r) {
        n0 = a; 
        n1 = b;
        res = r;
    }

    /** Sets the resistor's mark state. **/
    void setMark() { mark = true; }

    /** Clears the resistor's mark state. **/
    void clearMark() { mark = false; }

    /** Returns the resistor's mark state. **/
    boolean getMark() { return mark; }

    /** Returns the resistor's value. **/
    float getValue() { return res; }

    /** Returns the node terminal that isn't specified. **/
    AbstractNode nextAfter(final AbstractNode a) {
        if (a==n0) return n1;
        else return n0;
    }

    public Resistor getResistor() {
        return new Resistor(n0.name,n1.name,1/res);
    }
}
