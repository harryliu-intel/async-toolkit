/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;


/**
   Interface to a parsed command line argument.
 */
public interface CommandLineArg {

    /**
       @return The name of the command line argument.
     */
    String getName();

    /**
       @return The value of the command line argument or null if the
       argument had no value.
     */
    String getValue();

}
