/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;


import java.lang.String;
import java.util.NoSuchElementException;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;

import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgWithNoValueDefImpl;


import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsBaseImpl;

/**
   Default implementation of com.avlsi.util.cmdlineargs.CommandLineArg.
   Arguments of the form --name=val and --name are parsed into CommandLineArg objects.
   All parsing is done on the fly so this implemention will be slow if
   there are alot of arguments or if the list of arguments is queried alot.
   This implemention doesn't really allocate any memory.
 */
public class CommandLineArgsDefImpl extends CommandLineArgsBaseImpl {

    private String[] m_Args;

    /**
       @param args The list of strings that are the command line arguments.
     */
    public CommandLineArgsDefImpl( String[] args ) {
	m_Args = args;
    }

    
    public CommandLineArgsIterator iterator() {
	return new CommandLineArgsIterator() {
		private String[] args = m_Args;

		private int m_CurrArg = 0;

		private CommandLineArg m_nextArg = null;

		private CommandLineArg parseArg( String arg ) {
		    CommandLineArg ret = null;
		    if ( arg.length() > 2 ) {
			if ( ( arg.charAt(0) == '-' ) &&
			     ( arg.charAt(1) == '-' ) ) {
			    StringBuffer argName = new StringBuffer();
			    int currCharIndex = 2;
			    while ( (  currCharIndex < arg.length() ) && 
                                    ( arg.charAt( currCharIndex) != '=' ) ){
				argName.append( arg.charAt( currCharIndex ) );
				++currCharIndex;
			    }
			    if ( currCharIndex < arg.length() ) {
				++currCharIndex;
				String argValue = arg.substring(currCharIndex);
				ret = new CommandLineArgDefImpl( argName.toString(), argValue );
			    }
			    else {
				ret = new CommandLineArgWithNoValueDefImpl( argName.toString() );
			    }
			}
		    }
		    return ret;
		}

		public boolean hasNext() {
		    boolean ret;
		    if ( m_nextArg == null ) {
			if ( m_CurrArg == args.length ) {
			    ret = false;
			}
			else {
			    while ( ( m_CurrArg < args.length ) && 
                                    ( m_nextArg == null  ) ) {
				m_nextArg = parseArg( args[ m_CurrArg ] );
				++m_CurrArg;
			    }
			    ret = m_nextArg != null;
			}
		    }
		    else {
			ret = true;
		    }
		    return ret;
		}

		public CommandLineArg next() {
		    CommandLineArg ret = m_nextArg;
		    if ( ret == null ) {
			if ( hasNext() ) {
			    ret = m_nextArg;
			    m_nextArg = null;
			}
			else {
			    throw new NoSuchElementException();
			}
		    }
		    else {
			m_nextArg = null;
		    }
		    return ret;
		}
	    };
    }

    public StringContainerIterator nonParsedArgumentsIterator() {
	return new StringContainerIterator() {
		private String[] args = m_Args;
		private int m_CurrArg;
		private String m_nextArg;
		
		private String parseArg( String arg ) {
		    String ret;
		    if ( arg.length() <= 2 ) {
			ret = arg;
		    }
		    else {
			if ( ( arg.charAt( 0 ) != '-' ) || ( arg.charAt( 1 ) != '-' ) ) {
			    ret = arg;
			}
			else {
			    ret = null;
			}
		    }
		    return ret;
		}
		public boolean hasNext() {
		    boolean ret;
		    if ( m_nextArg == null ) {
			if ( m_CurrArg == args.length ) {
			    ret = false;
			}
			else {
			    while ( ( m_CurrArg < args.length ) && 
                                    ( m_nextArg == null  ) ) {
				m_nextArg = parseArg( args[ m_CurrArg ] );
				++m_CurrArg;
			    }
			    ret = m_nextArg != null;
			}
		    }
		    else {
			ret = true;
		    }
		    return ret;
		}

		public String next() {
		    String ret = m_nextArg;
		    if ( ret == null ) {
			if ( hasNext() ) {
			    ret = m_nextArg;
			    m_nextArg = null;
			}
			else {
			    throw new NoSuchElementException();
			}
		    }
		    else {
			m_nextArg = null;
		    }
		    return ret;
		}
	    };
    }
	    

    /*
    public static void main( String args[] ) {
	CommandLineArgsDefImpl thisProgram = new CommandLineArgsDefImpl( args );

	CommandLineArgsIterator i = thisProgram.iterator();
	
	while ( i.hasNext() ) {
	    CommandLineArg curr = i.next();
	    System.out.println("Name:");
	    System.out.println(curr.getName() );
	    System.out.println("Value:");
	    System.out.println(curr.getValue() );
	    System.out.println();
	}

	CommandLineArg testArg = thisProgram.getArg( "foo" );
	if ( testArg != null ) {
	    System.out.println("--foo was an argument!");
	    if ( testArg.getValue() != null ) {
		System.out.println("--foo's value is: \"" + testArg.getValue() + "\"");
	    }
	}
	StringContainerIterator j = thisProgram.nonParsedArgumentsIterator();
	System.out.println( "Nonparsed arguments");
	System.out.println( "-------------------" );
	while ( j.hasNext() ) {
	    String curr = j.next();
	    System.out.println( curr );
	}
	

    }
    */       


}
