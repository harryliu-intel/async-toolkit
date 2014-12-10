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

package com.avlsi.tools.aspice;

/**
 * Interface for receiving a callback after each step in Jaspice
 *
 * @author Dan Daly
 * @version $Date$
 **/

interface JaspiceCallbackInterface {
    /** init is called during the first run() call to Jaspice **/
    void init()      throws JaspiceException;
    /** Update is called after every step in Jaspice **/
    void update()    throws JaspiceException;
    /** finish is called when Jaspice is shutting down **/
    void finish()    throws JaspiceException;
}   
  
