/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/* $Id$ 
   $DateTime$
   $Autor$
*/

package com.avlsi.util.cmdlineargs;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;


/**
 * Interface to a set of command line arguments to a program.  You just give an
 * implementation of this class the array strings passed to main and then you
 * can query for arguments and their values.  You can also iterate over all the
 * arguments that were not parsed.
 **/
public interface CommandLineArgs {

    /**
     * Returns an iterator that iterates over all arguments that were
     * understood to be arguments by the implementation of this interface.  An
     * implementation of this iterface may define the behaviour of the returned
     * iterator when an argument is specified twice or more.
     *
     * @return An iterator that iterates over all parsed arguments. 
     **/
    CommandLineArgsIterator iterator();

    /**
     * Queries the set of command line arguments for an argument with a
     * particular name.  If the requested argument has been specified more than
     * once only the first specification will be used to calculate the return
     * value of this function.
     *
     * @param argName The name of the argument to look for.
     * @return The first occurance of an argument with the specified name in
     * the list of arguments.
     **/
    CommandLineArg getArg( String argName );

    /**
     * Returns the value of the argument with the specified name if one was in
     * the list of command line arguments or the default value if the argument
     * was not specified.
     *
     * @param argName The name of the argument to look for.
     * @param defValue The return value of this method if no argument with the
     * specified name was in the list command line arguments.
     * @return The value of the argument if the argument was in the list, or
     * the specified default value if it was not.
     **/
    String getArgValue( String argName, String defValue );

    /**
     * Queries the set of command line arguments for an argument with a
     * particular name and returns whether or not that argument was specified.
     *
     * @param argName The name of the argument to look for.
     * @return true if the argument was specified, false otherwise.
     **/
    boolean argExists( String argName );


    /**
     * Returns an iterator that iterates over all the arguments that the
     * implementation of this interface did not consider valid arguments.
     *
     * @return An iterator that iterates all arguments that could not be parsed
     * by the implementation of this interface.
     **/
    StringContainerIterator nonParsedArgumentsIterator();

}
