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

package com.avlsi.util.exception;

import java.io.CharArrayWriter;
import java.io.PrintWriter;

/**
 * Utilities operating on exceptions
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ExceptionUtils {
    private ExceptionUtils() {
        throw new AssertionFailure("ExceptionUtils self-instantiation");
    }

    public static String getStackTraceString(final Throwable t) {
        final CharArrayWriter caw = new CharArrayWriter();
        t.printStackTrace(new PrintWriter(caw));
        return caw.toString();
    }
}
