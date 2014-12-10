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
import com.avlsi.file.common.Capacitor;

/**********************************************************************
 * Capacitor device class, for use as an edge in a circuit graph.
 * CapDevice is identical to a {@link ResDevice}, but omits marking
 * functionality.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public final class CapDevice {

    final AbstractNode n0,n1;
    private final float cap;

    /** Constructs a CapDevice from two node terminals and a resistance. **/
    public CapDevice(final AbstractNode a, final AbstractNode b, float c) {
        n0 = a; 
        n1 = b;
        cap = c;
    }

    /** Returns the capacitor's value. **/
    public float getValue() { return cap; }

    /** Returns the node terminal that isn't specified. **/
    public AbstractNode nextAfter(final AbstractNode a) {
        if (a==n0) return n1;
        else return n0;
    }

    /*****************************************************************
     * Returns true if the two capacitor terminals belong to the same
     * resistive subnetwork.  (Commonly such capacitance will not be
     * included when lumping over the net.)
     *****************************************************************/
    public boolean isDegenerate() {
        return n0.sameResNet(n1);
    }

    /*****************************************************************
     * Returns true if the capacitance is considered "good", i.e. if
     * it is known that one terminal is always fixed whenever the 
     * other switches.  Conditions for capacitance being good:
     * <ol>
     *  <li> Either terminal is Vdd or GND.
     *  <li> Terminal node names belong to different signals of an
     *       apparent 1-of-N channel.
     *  <li> Terminal node names belong to the same resistive net.
     * </ol>
     * Criterion (2) unfortunately relies on consistent alias naming
     * in the extract file (if "channelname.0" is used, then this
     * method assumes that "channelname.1", "channelname.e", etc.
     * also is used).  It also must assume that nodes named 
     * "channelname.N" (where N is a number) and "channelname.e" 
     * belong to an asynchronous hanshake channel.  
     *****************************************************************/
    public boolean isGood() {
        if (n0.isGND() || n0.isVdd() || n1.isGND() || n1.isVdd() ||
            n0.channelBaseName().equals(n1.channelBaseName()) ||
            n0.sameResNet(n1))
            return true;
        else return false;
    }

    /*****************************************************************
     * Returns the Capacitor corresponding to this CapDevice.
     *****************************************************************/
    public Capacitor getCapacitor() {
        return new Capacitor(n0.name,n1.name,cap);
    }

    /*****************************************************************
     * Returns the Capacitor corresponding to this CapDevice, lumping
     * the specified node to its resistive subnet name.  If lumpedNode
     * isn't actually connected to the capacitor, then the lumping
     * is undefined.
     *****************************************************************/
    public Capacitor getCapacitorLumpedToSubnet(AbstractNode lumpedNode) {
        if (n0 == lumpedNode)
            return new Capacitor(n0.name.getResistiveSubnetName(),n1.name,cap);
        else
            return new Capacitor(n0.name,n1.name.getResistiveSubnetName(),cap);
    }
}
