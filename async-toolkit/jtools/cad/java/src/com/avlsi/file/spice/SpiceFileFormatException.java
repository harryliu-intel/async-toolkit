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

package com.avlsi.file.spice;

/**
 * Class to represent exceptions in parsing of .cdl files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class SpiceFileFormatException extends Exception {
    public SpiceFileFormatException(final String message) {
        super(message);
    }
    public SpiceFileFormatException(final String message,
                                    final Throwable cause) {
        super(message, cause);
    }
    public SpiceFileFormatException(final Throwable cause) {
        super(cause);
    }
}
