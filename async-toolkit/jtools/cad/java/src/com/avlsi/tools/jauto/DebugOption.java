/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.jauto;

public class DebugOption{

    /**
     * This class cannot be instantiated.
     **/
    private DebugOption() {
        throw new AssertionError();
    }

    public static final boolean            printAll = false;

    public static int                printLevel = 3;

    // print information in CastDesign::generateSourceSinkList()
    public static final boolean             printSSG = false; 
}
