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

package com.avlsi.circuit;

import com.avlsi.file.common.HierName;
/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface SourceInterface {
    /** Type of the source, pulse, sin, etc **/
    String getType();
    /** (optional) Name of the Source, returns null if unassigned **/
    HierName getName();
    /** Positive Terminal node name **/
    HierName getPositiveTerminal();
    /** Negative Terminal node name **/
    HierName getNegativeTerminal();
    /** Type specific arguments to the source  **/
    String[] getArguments();
}

