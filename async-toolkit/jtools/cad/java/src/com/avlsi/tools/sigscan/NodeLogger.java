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

package com.avlsi.tools.sigscan;

import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.tools.dsim.Node;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class NodeLogger implements NodeWatcher {

    protected final Sigscan sigscan;
    protected final long[] handles;

    public NodeLogger(Sigscan sigscan, String scopename,
                                       String name) {
        this(sigscan, new String[] {scopename},
                      new String[] {name},
                      1);
    }

    /**
     * Constructor.
     **/
    public NodeLogger(Sigscan sigscan, String[] scopenames,
                                       String[] names) {
        this(sigscan, scopenames, names, scopenames.length);
    }
    
    public NodeLogger(Sigscan sigscan, String[] scopenames,
                                       String[] names,
                                       int numAliases) {
        this.sigscan = sigscan;
        handles = new long[numAliases];
        for (int loop=0;loop<numAliases;loop++) {
            handles[loop] = sigscan.newAsyncVariable(scopenames[loop],
                                              names[loop]);
        }
    }

    public void nodeChanged(Node node, long time) {
        try {
            sigscan.convertAndSetTime(time);
        } catch (SigscanException e) {
            System.err.println(time+": "+getClass()+": "+e.getMessage());
            sigscan.incErrorCount();
        }
        for (int loop=0;loop<handles.length;loop++) {
            if (handles[loop] != 0)
                sigscan.asyncChange(handles[loop], node.getValue());
        }
    }


}

