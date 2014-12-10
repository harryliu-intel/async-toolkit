/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.floorplanning;


import java.util.Set;
import java.util.HashSet;


import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.io.LineNumberReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;

import java.io.FileNotFoundException;
import java.io.IOException;


import com.avlsi.util.debug.Debug;

final class CellListFile {

    private final Set m_CellSet;

    private void readFromReader( final Reader r ) 
        throws IOException
    {
        final LineNumberReader lR = new LineNumberReader( r );
        
        String currLine;

        do {
            currLine = lR.readLine();
            if ( currLine != null ) {
                m_CellSet.add( currLine );
            }
        } while ( currLine != null );
    }

    private void readFromStream( final InputStream i ) 
        throws IOException 
    {
        
        readFromReader( new InputStreamReader( i, "UTF-8" ) );
    }
    
    private void readFromFile( final File f ) 
        throws IOException, FileNotFoundException
    {
        if ( ( f.isFile() ) && ( f.canRead() ) ) {
            try {
                readFromStream( new FileInputStream( f ) );
            }
            catch( FileNotFoundException e ) {
                Debug.assertTrue( false );
            }
        }
        else {
            throw new IllegalArgumentException();
        }
        
    }

    private void readFromFile( final String fn ) 
        throws IOException
    {
        final File f = new File( fn );
        readFromFile( f );
    }


    private CellListFile( ) {
        m_CellSet = new HashSet();
    }

    public CellListFile( final File f ) throws IOException {
        this();
        readFromFile( f );
    }

    public CellListFile( final String fn ) throws IOException {
        this();
        readFromFile( fn );
    }

    public Set getCellList() {
        return m_CellSet;
    }


} 
