/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;

import com.avlsi.util.cmdlineargs.CommandLineArg;


/**
   Default implementation of com.avlsi.util.cmdlineargs.CommandLineArg.
   Just stores a pair of strings.
 */
public class CommandLineArgDefImpl implements CommandLineArg {

    private String m_Name;
    private String m_Value;

    /**
       @param name The name of the argument.
       @param value The value of the argument.
     */
    public CommandLineArgDefImpl( String name, String value ) {
	m_Name = name;
        m_Value = value;
    }

    public String getName() {
	return m_Name;
    }

    public String getValue() {
	return m_Value;
    }
}
