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

package com.avlsi.test;

/**
 * Throws by {@link AbstractTestCase#assertTrue} when an assertion fails.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TestFailedError extends Error {
    public TestFailedError() {
        super();
    }

    public TestFailedError(final String message) {
        super(message);
    }
}
