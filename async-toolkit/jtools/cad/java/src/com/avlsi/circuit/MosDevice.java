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
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.aspice.Transistor;

/**********************************************************************
 * Three-terminal Mosfet transistor device class, for use as an edge 
 * in a circuit graph.  Substrate terminal is not considered.  Gate
 * width and length parameters are stored, as well as 
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public final class MosDevice {

    //
    // Constants
    //

    /** A W:L ratio below this value is considered to be "weak" **/
    public static final float WEAK_DRIVE_RATIO = 2.0f;

    /** NFET gate oxide capacitance per m^2(?) (TSMC 0.18um) **/
    public static final float COX_N = 8.46e-3f;

    /** PFET gate oxide capacitance per m^2(?) (TSMC 0.18um) **/
    public static final float COX_P = 8.16e-3f;

    final AbstractNode source, drain, gate;
    private final float width, length;
    private final byte type;

    /** Constructs a ResDevice from two node terminals and a resistance. **/
    public MosDevice(final AbstractNode d, final AbstractNode g, 
                     final AbstractNode s, int t, double w, double l) {
        gate=g; source=s; drain=d;
        type=(byte)t; width=(float)w; length=(float)l;
    }

    public float gateCap() {
        return (type==DeviceTypes.N_TYPE ? COX_N:COX_P) * width * length;
    }

    /** Returns the AbstractNode attached to the gate terminal. **/
    public AbstractNode gateNode() { return gate; }

    /** Returns the AbstractNode attached to the source terminal. **/
    public AbstractNode sourceNode() { return source; }

    /** Returns the AbstractNode attached to the drain terminal. **/
    public AbstractNode drainNode() { return drain; }

    /** Returns the device type (P_TYPE or N_TYPE) **/
    public int getType() { return type; }

    /** Returns an equivalent Transistor type **/
    public Transistor getTransistor() {
        HierName bulk = (type == DeviceTypes.N_TYPE) ? 
                        HierName.makeHierName("GND!") : 
                        HierName.makeHierName("Vdd!");
        return new Transistor((int)type,source.name,drain.name,gate.name,
                              bulk,(double)width,(double)length);
    }

    /** 
     * Returns an equivalent Transistor type, with the gate node
     * set to its resistive subnet base name.
     **/
    public Transistor getTransistorWithGateLumped() {
        HierName bulk = (type == DeviceTypes.N_TYPE) ? 
                        HierName.makeHierName("GND!") : 
                        HierName.makeHierName("Vdd!");
        return new Transistor((int)type,source.name,drain.name,
                              gate.name.getResistiveSubnetName(),
                              bulk,(double)width,(double)length);
    }

    /** 
     * True if the transistor's W:L ratio is less than 
     * {@link #WEAK_DRIVE_RATIO}.
     **/
    public boolean isWeak() { return (width/length <= WEAK_DRIVE_RATIO); }
}
