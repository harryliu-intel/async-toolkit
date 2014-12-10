/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout;

public class CellProcessorException extends Exception {
    public CellProcessorException() {
	super();
    }
    public CellProcessorException( final String str ) {
	super(str);
    }
    public CellProcessorException( final Exception e ) {
	super(e);
    }
}
