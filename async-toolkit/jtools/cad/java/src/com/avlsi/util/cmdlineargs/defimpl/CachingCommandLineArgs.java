/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Iterator;

import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Iterator2StringContainerIteratorAdapter;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;

import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgWithNoValueDefImpl;

public class CachingCommandLineArgs implements CommandLineArgs {

    Map m_Cache;

    /**
     * A list of parsed args.  Store this in addition to the map to iterate
     * over arguments that are repeated.
     **/
    List m_ParsedArgs;

    List m_NonParsedArgs;

    private class MapEntryToCommandLineArgWrapper implements CommandLineArg{
	private Map.Entry m_MapEntry;

	public MapEntryToCommandLineArgWrapper( Map.Entry mEntry ) {
	    m_MapEntry = mEntry;
	}
			    
	public String getName() {
	    return ( String ) m_MapEntry.getKey();
	}
	
	public String getValue() {
	    return ( String ) m_MapEntry.getValue();
	}
    }

    public CachingCommandLineArgs( CommandLineArgs args ){
	m_Cache = new HashMap();
        m_ParsedArgs = new ArrayList();
	
	CommandLineArgsIterator i = args.iterator();

	while ( i.hasNext() ) {
	    CommandLineArg curr = i.next();
            m_ParsedArgs.add(curr);
	    m_Cache.put( curr.getName(), curr.getValue() );
	}

	m_NonParsedArgs = new LinkedList();

	StringContainerIterator j = args.nonParsedArgumentsIterator();

	while ( j.hasNext() ) {
	    String curr = j.next();
	    m_NonParsedArgs.add( curr );
	}
	
    }

    public CommandLineArgsIterator iterator() {
	return new CommandLineArgsIterator() {
		private Iterator m_InnerIterator = m_ParsedArgs.iterator();
		
		public boolean hasNext() {
		    return m_InnerIterator.hasNext();
		}

		public CommandLineArg next() {
		    return ( CommandLineArg ) m_InnerIterator.next();
		}
	    };
    }

    public CommandLineArg getArg( String argName ) {
	if ( m_Cache.containsKey( argName ) ) {
	    String argValue = ( String ) m_Cache.get( argName );
	    if ( argValue != null ) {
		return new CommandLineArgDefImpl( argName, argValue );
	    }
	    else {
		return new CommandLineArgWithNoValueDefImpl( argName );
	    }
	}
	else {
	    return null;
	}
    }

    public String getArgValue( String argName, String defValue ) {
	if ( m_Cache.containsKey( argName ) && m_Cache.get(argName) != null ) {
	    return ( String ) m_Cache.get( argName );
	}
	else {
	    return defValue;
	}
    }

    public boolean argExists( String argName ) {
	return m_Cache.containsKey( argName );
    }

    public StringContainerIterator nonParsedArgumentsIterator() {
	return new Iterator2StringContainerIteratorAdapter( m_NonParsedArgs.iterator() );
    }

    public String toString() {
	return m_Cache.toString();
    }


    
    public static void main( String args[] ) {
	CommandLineArgs parsedArgs = 
	    new com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl( args );

	CommandLineArgs cachedArgs = new CachingCommandLineArgs( parsedArgs );

	CommandLineArgs theArgs = cachedArgs;

	CommandLineArgsIterator i = theArgs.iterator();
	
	while ( i.hasNext() ) {
	    CommandLineArg curr = i.next();
	    System.out.println("Name:");
	    System.out.println(curr.getName() );
	    System.out.println("Value:");
	    System.out.println(curr.getValue() );
	    System.out.println();
	}

	CommandLineArg testArg = theArgs.getArg( "foo" );
	if ( testArg != null ) {
	    System.out.println("--foo was an argument!");
	    if ( testArg.getValue() != null ) {
		System.out.println("--foo's value is: \"" + testArg.getValue() + "\"");
	    }
	}
	StringContainerIterator j = theArgs.nonParsedArgumentsIterator();
	System.out.println( "Nonparsed arguments");
	System.out.println( "-------------------" );
	while ( j.hasNext() ) {
	    String curr = j.next();
	    System.out.println( curr );
	}

	System.out.println( theArgs.toString() );
	

    }

}
