/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.ext;

import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;

/**
 * 
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class FET {

    private final int type;
    private final double area;
    private final double perimeter;
    private final HierName substrateNode;
    private final Terminal gate;
    private final Terminal source;
    private final Terminal drain;

    /**
     * Class constructor.  Canonicalizes the source and drain node
     * names by ensuring that source is lexicographically less than
     * drain.  This will ensure we have a canonical form for 
     * FETs.
     **/
    public FET(final int type,
               final double area,
               final double perimeter,
               final HierName substrateNode,
               final Terminal gate,
               final Terminal source,
               final Terminal drain)
    {
        if (type != DeviceTypes.N_TYPE && type != DeviceTypes.P_TYPE)
            throw new IllegalArgumentException("Bad fet type: " + type);

        this.type = type;
        this.area = area;
        this.perimeter = perimeter;
        this.substrateNode = substrateNode;
        this.gate = gate;

        // ensure that source <= drain, lexicographically
        if (source.getConnectingNode().compareTo(
                    drain.getConnectingNode()) <= 0) {
            this.source = source;
            this.drain = drain;
        } else {
            this.source = drain;
            this.drain = source;
        }
    }

    public int getType() {
        return type;
    }

    // area of the gate region in square centimicrons
    public double getArea() {
        return area;
    }

    // perimeter of the gate region in centimicrons
    public double getPerimeter() {
        return perimeter;
    }

    // the node to which the substrate of the transistor is connected
    public HierName getSubstrateNode() {
        return substrateNode;
    }

    public Terminal getGate() {
        return gate;
    }

    public Terminal getSource() {
        return source;
    }

    public Terminal getDrain() {
        return drain;
    }

}
