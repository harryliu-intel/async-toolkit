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

/**
 * Class for ...
 *
 * @author Aaron Denney
 * @version $Date$
 **/

interface Resetable {
    void resetStart();
    void resetStop();
}

