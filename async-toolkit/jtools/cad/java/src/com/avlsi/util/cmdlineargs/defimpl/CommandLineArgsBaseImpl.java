/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;



import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;




public abstract class CommandLineArgsBaseImpl implements CommandLineArgs {

    

    public CommandLineArg getArg( String argName ) {
	CommandLineArgsIterator i = iterator();
	CommandLineArg ret = null;
	while ( ( i.hasNext() ) ){
	    CommandLineArg currArg = i.next();
	    if ( currArg.getName().compareTo( argName ) == 0 ) {
		ret = currArg;
	    }
	}
	return ret;
    }
    public String getArgValue( String argName, String defValue ) {
	CommandLineArg arg = getArg( argName );
	if ( arg == null ) {
	    return defValue;
	}
	else {
	    return arg.getValue();
	}
    }
    
    public boolean argExists( String argName ) {
	return getArg( argName ) != null;
    }
}
