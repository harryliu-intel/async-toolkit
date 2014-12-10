/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;


import java.util.NoSuchElementException;


import com.avlsi.util.cmdlineargs.CommandLineArg;

/**
   Iterator used to iterate over parsed command line arguments.
 */
public interface CommandLineArgsIterator {

    /**
       @return true if there is another command line argument in the iteration, false otherwise.
     */
    boolean hasNext();

    /**
       Gets the next command line argument in the iteration. Throws if there is not
       another command line argument in the iteration.
       @return The next command line argument.

       @throws NoSuchElementException
               If there is not another command line argument in the
               iterator.
     */
    CommandLineArg next();

}
