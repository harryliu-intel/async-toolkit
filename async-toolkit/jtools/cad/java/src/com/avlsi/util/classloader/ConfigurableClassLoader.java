/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.classloader;




import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;

import java.io.File;
import java.io.FileNotFoundException;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.DataInputStream;

import java.io.IOException;
import java.io.FileNotFoundException;

import java.util.Iterator;

import com.avlsi.util.container.StringContainerIterator;

public class ConfigurableClassLoader extends ClassLoader {


    private SearchPath m_SearchPath;

    public ConfigurableClassLoader( SearchPath searchPath ) {
	this( ConfigurableClassLoader.class.getClassLoader(),  searchPath );
    }

    public ConfigurableClassLoader( ClassLoader parent, SearchPath searchPath ) {
	super( parent );
	m_SearchPath = searchPath;
    }

    private static String convertNameToPath( String name ) {
	return name.replace( '.', File.separatorChar ) + ".class";
	
    }

    private static byte[] getFileBytes( SearchPathFile theFile ) throws IOException {

	final long theFileLen = theFile.length();

        if ( theFileLen > 0 ) {
	
            final int intFileLen = ( int ) theFileLen;

            final byte[] ret = new byte[intFileLen];

            InputStream theInputStream = theFile.getStream();

            DataInputStream theData = new DataInputStream( theInputStream );

            theData.readFully( ret );
            return ret;
        }
        else {
            throw new IOException( "Could not get length of file." );
        }

	
    } 

    protected Class findClass( String name ) throws ClassNotFoundException {
    
	String classFileName = convertNameToPath( name );

	try {
	    SearchPathFile theClassFile = m_SearchPath.findFile( classFileName );

	    try {
		
		byte[] classBytes = getFileBytes( theClassFile );

		return defineClass( name, classBytes, 0, classBytes.length );

	    }
	    catch (IOException e) {
		String errorMessage = "Unable to load data for class " + 
		    name + " from file \"" + theClassFile.getName() + "\".";
		
		throw new ClassNotFoundException( errorMessage);
		
	    }

	}
	catch ( FileNotFoundException e ) {

	    String errorMessage = "Unable to find file \"" + classFileName + "\" for class \"" + name + "\" in:\n";

	    StringContainerIterator iter=m_SearchPath.getSearchPath();

	    while ( iter.hasNext() ) {
		String curr = iter.next() ;
		errorMessage = errorMessage + "    " + curr + "\n";
	    }

	    throw new ClassNotFoundException( errorMessage );
	    
	}
    }

}
