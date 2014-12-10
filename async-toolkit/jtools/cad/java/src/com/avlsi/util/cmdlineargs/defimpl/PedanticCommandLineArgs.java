/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;

import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Iterator2StringContainerIteratorAdapter;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;

import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgWithNoValueDefImpl;

public class PedanticCommandLineArgs implements CommandLineArgs {

    HashSet ok;
    HashSet used;
    CommandLineArgs inner;

    public PedanticCommandLineArgs( CommandLineArgs args ){
        ok = new HashSet();
        used = new HashSet();
        inner = args;
	
	CommandLineArgsIterator i = args.iterator();

	while ( i.hasNext() ) {
	    CommandLineArg curr = i.next();
            used.add( curr.getName() );
	}

        // make these args always legal
        ok.add("fulcrum-pdk-root");
        ok.add("package-root");
        ok.add("pedantic");

    }

    public CommandLineArgsIterator iterator() {
        return inner.iterator();
    }

    public CommandLineArg getArg( String argName ) {
        ok.add( argName );
        return inner.getArg( argName );
    }

    public String getArgValue( String argName, String defValue ) {
        ok.add( argName );
        return inner.getArgValue( argName, defValue );
    }

    public void argTag( String argName ) {
        ok.add( argName );
    }

    public boolean argExists( String argName ) {
        ok.add( argName );
        return inner.argExists( argName );
    }

    public StringContainerIterator nonParsedArgumentsIterator() {
	return inner.nonParsedArgumentsIterator();
    }

    public String toString() {
	return inner.toString();
    }

    StringBuffer pedanticList = new StringBuffer( "" );
    boolean checked = false;

    public boolean pedanticOK( boolean dothrow, boolean quiet ) {
        boolean badarg = false;
        boolean pedantic1 = false;
        boolean pedantic2 = false;
        checked = true;
        String pv;
        if (inner.argExists( "pedantic" ) )
            pv = inner.getArgValue( "pedantic", "1" );
        else {
            pv = System.getenv("FULCRUM_PEDANTIC");
            if (pv == null)
                pv = "0";
        }
	if ( pv.equals("1")) {
            pedantic1 = true;
        }
        else if ( pv.equals("2")) {
            pedantic1 = pedantic2 = true;
        }
        else if ( ! pv.equals("0")) {
            System.err.println("Warning: Unknown pedantic value '"+pv+"', should be 0, 1 or 2");
        }
        Iterator i = used.iterator();
        while (i.hasNext()) {
            String curr = (String) i.next();
            if ( ! ok.contains( curr ) ) {
                if (pedantic1) {
                    if (! quiet )
                        System.err.println( "Unknown argument --"+curr );
                    pedanticList.append( "Unknown argument --"+curr+"\n" );
                    badarg = true;
                }
            }
        }
        if (badarg && pedantic2 && dothrow)
            throw new RuntimeException("One or more illegal command line arguments");
        return ! badarg;
    }

    public boolean pedanticOK() {
        return pedanticOK( true , false );
    }
    
    public boolean pedanticOK( boolean dothrow ) {
        return pedanticOK( dothrow, false );
    }

    public String pedanticString() {
        if (! checked )
            pedanticOK( false, true );
        return new String(pedanticList);
    }

}
