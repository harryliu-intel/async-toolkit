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

import com.avlsi.file.common.HierName;

/**
 * 
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Terminal {
    private final HierName connectingNode;
    private final double length;

    public Terminal(final HierName connectingNode,
                    final double length)
    {
        this.connectingNode = connectingNode;
        this.length = length;
    }

    public HierName getConnectingNode() {
        return connectingNode;
    }

    // the terminal length in centimicrons; the length of that segment of
    // the channel perimeter connecting to adjacent material, such as
    // polysilicon for the gate or diffusion for a source or drain
    public double getLength() {
        return length;
    }
}
