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
package com.avlsi.tools.dsim;

import java.util.EventListener;

/**
 * An interface to programatically listens for changes to DSim Nodes.
 * 
 * Used by e.g. the cli part of {@link com.avlsi.util.cmdline.DSimModule} rather than
 * embedding "spewing to stdout" in the guts of the Scheduler.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/

public interface NodeWatcher extends EventListener {
     /** Called when an associated node changes. **/
     void nodeChanged(Node node, long time);
}

