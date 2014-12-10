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

import com.avlsi.file.aspice.Diode;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;

/**********************************************************************
 * Diode device class, for use as an edge in a circuit graph.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **********************************************************************/
public final class DiodeDevice {

    /** Node attached to the associated transistor's source/drain **/
    final AbstractNode node;

    /** Width of the associated transistor in meters. **/
    private final float width;

    /** Length of the associated transistor in meters. **/
    private final float length;

    /** Area of the diode in square meters. **/
    private final float area;

    /** Perimeter of the diode in square meters. **/
    private final float perim;

    /** Type of diode, N_TYPE or P_TYPE. **/
    private final int type;

    /** 
     * Constructs a DiodeDevice from two AbstractNode terminals and
     * device parameters.
     * <pre>
     * "source"               "source"
     *   node __|\|__ Vdd       node __|/|__ GND
     *          |/|  (P_TYPE)          |\|  (N_TYPE)
     * </pre>
     **/
    DiodeDevice(final AbstractNode n, int t, float w, float l, float a, float p)
    {
        if (t != DeviceTypes.N_TYPE && t != DeviceTypes.P_TYPE)
            throw new IllegalArgumentException("Bad diode type: "+t);

        this.node = n;
        this.width = w;
        this.length = l;
        this.area = a;
        this.perim = p;
        this.type = t;
    }

    public Diode getDiode() {
        HierName drain = (type == DeviceTypes.N_TYPE) ?
                         HierName.makeHierName("GND!") :
                         HierName.makeHierName("Vdd!");
        return new Diode(type,node.name,drain,width,length,area,perim);
    }
}
