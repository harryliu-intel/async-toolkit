/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2java;

/**
 * Exception thrown by <code>SymbolTable.insert</code> if the symbol
 * is not defined.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class SymbolRedefinedException extends Exception {
    /**
     * Class constructor.
     **/
    public SymbolRedefinedException(final String message) {
        super(message);
    }
}
