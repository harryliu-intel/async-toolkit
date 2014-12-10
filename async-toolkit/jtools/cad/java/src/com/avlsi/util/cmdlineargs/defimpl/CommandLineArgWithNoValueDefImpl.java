/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;



import com.avlsi.util.cmdlineargs.CommandLineArg;

/**
   Implementation of com.avlsi.util.cmdlineargs.CommandLineArg where
   the argument has not value.
 */
public class CommandLineArgWithNoValueDefImpl implements CommandLineArg {

    String m_Name;

    /**
       @param argName The name of the argument.
     */
    public CommandLineArgWithNoValueDefImpl( String argName ) {
	m_Name = argName;
    }

    public String getName() {
	return m_Name;
    }

    public String getValue() {
	return null;
    }

}
