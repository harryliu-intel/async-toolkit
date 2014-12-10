/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.util.rename;


import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.io.Reader;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;


public class BindRulNameInterfaceFactory implements CDLNameInterfaceFactory {

    private class CellMappings {
        private final String mOldCellName;
        private final String mNewCellName; 
        private final Map mNodeMap;
        private final Map mInstanceMap;


        public CellMappings( final String oldCellName,
                             final String newCellName ) {
            mOldCellName = oldCellName;
            mNewCellName = newCellName;
            
            mNodeMap = new HashMap();
            mInstanceMap = new HashMap();
        }

        public final String getOldCellName() {
            return mOldCellName;
        }

        public final String getNewCellName() {
            return mNewCellName;
        }

        public final String getNewNodeNameForOldNodeName( final String oldNodeName ) {
            return ( String ) mNodeMap.get( oldNodeName );
        }

        public final String getNewInstanceNameForOldInstanceName( final String oldInstanceName ) {
            return ( String ) mInstanceMap.get( oldInstanceName );
        }

        public final void addNodeMapping( final String oldNodeName,
                                          final String newNodeName ) {
            mNodeMap.put( oldNodeName, newNodeName );
        }

        public final void addInstanceMapping( final String oldInstanceName,
                                              final String newInstanceName ) {
            mInstanceMap.put( oldInstanceName,
                              newInstanceName );
        }

        public final Set getNewNodeNames( ) {
            final Iterator entryIter = mNodeMap.entrySet().iterator();
            
            final Set ret = new HashSet();

            while ( entryIter.hasNext() ) {
                final Map.Entry currEntry = ( Map.Entry ) entryIter.next();
                final String currNewName = ( String ) currEntry.getValue();
                ret.add( currNewName );
            }
            return ret;
        }

        public final Set getNewInstanceNames( ) {
            final Iterator entryIter = mInstanceMap.entrySet().iterator();

            final Set ret = new HashSet();

            while ( entryIter.hasNext() ) {
                final Map.Entry currEntry = ( Map.Entry ) entryIter.next();
                final String currNewName = ( String ) currEntry.getValue();
                ret.add( currNewName );
            }
            return ret;
        }
    }


    public class BindRulParser {
        private Map mTargetCellMaps;
        private CellMappings mCurrMap;
        
        private boolean mEOF;

        private Reader mSrcReader ;

        private final String readLine( ) 
            throws IOException
        {
            if ( ! mEOF ) {
                final StringBuffer ret = new StringBuffer();
                int readRet = 0;
                while ( ( readRet >= 0 ) && ( readRet != '\n' ) ) {
                    readRet = mSrcReader.read();
                    if ( ( readRet >= 0 ) && ( readRet != '\n' ) ) {
                        ret.append( ( char ) readRet );
                    }
                }
                if ( readRet < 0 ) {
                    mEOF = true;
                }
                return ret.toString();
            }
            else {
                return "";
            }
        }

        private void addCellNameMapping( final String originalCellName,
                                         final String newCellName ) {
            mCurrMap = ( CellMappings ) mTargetCellMaps.get( originalCellName );
            if ( mCurrMap == null ) {
                mCurrMap = new CellMappings( originalCellName, newCellName );
                mTargetCellMaps.put( originalCellName, mCurrMap );
            }
        }

        private void addNodeNameMapping( final String originalNodeName,
                                         final String newNodeName ) {
            mCurrMap.addNodeMapping( originalNodeName,
                                     newNodeName );
        }

        private void addInstanceNameMapping( final String originalInstanceName,
                                             final String newInstanceName ) {
            mCurrMap.addInstanceMapping( originalInstanceName,
                                         newInstanceName );
        }

        private List splitLine( final String currLine ) {
            int i;
            final int length = currLine.length();

            boolean inQuotes=false;

            final List ret = new ArrayList();

            final StringBuffer accum = new StringBuffer();

            for ( i = 0 ; i < length ; ++i ) {
                final char c = currLine.charAt( i );
                if ( Character.isWhitespace( c ) ) {
                    if ( inQuotes ) {
                        accum.append( c );
                    }
                    else {
                        ret.add( accum.toString() );
                        accum.delete( 0, accum.length() );
                    }
                        
                }
                else if ( c == '"' ) {
                    inQuotes = inQuotes ^ true;
                }
                else {
                    accum.append( c );
                }
            }
            if ( accum.length() > 0 ) {
                ret.add( accum.toString() );
            }
            return ret;

        }

        private String getFirstWord( final String str ) {
            return str.split( "\\p{Space}+" )[0];
        }

        private void parse( ) 
            throws IOException
        {
            while ( ! mEOF ) {
                final String currLine = readLine();
                
                if ( currLine.length() > 0 ) {
                
                    final String[] tokens = ( String[] ) splitLine( currLine ).toArray( new String[0] );
                    
                    if ( tokens.length >= 2 ) {
                        char statementType = tokens[0].charAt( 0 );
                        switch ( statementType ) {
                        case 'C':
                        case 'c':
                            addCellNameMapping( getFirstWord( tokens[1] ) , 
                                                getFirstWord( tokens[2] ) );
                            break;
                        case 'N':
                        case 'n':
                            addNodeNameMapping( tokens[1], tokens[2] );
                            break;
                        case 'I':
                        case 'i':
                            addInstanceNameMapping( tokens[1], tokens[2] );
                            break;
                        }
                    }
                }
            }
        }
        public BindRulParser( final Reader srcReader,
                              final Map targetCellMaps ) 
            throws IOException 
        {
            mTargetCellMaps = targetCellMaps;
            mCurrMap = null;
            mEOF = false;
            mSrcReader = srcReader;
            parse();
        }
    }
    
    private final class NameInterface implements CDLNameInterface {

        private final CDLNameInterface mChained;

        private final Map mCellsMappings;

        private final CellMappings mCellMappings;

        public NameInterface( final CDLNameInterface chained,
                              final String cellName,
                              final Map cellsMappings ) {
            mChained = chained;
            mCellsMappings = cellsMappings;
            mCellMappings = ( CellMappings ) cellsMappings.get( cellName );
            
        }

        public String renameCell( final String oldCellName ) throws CDLRenameException {
            final CellMappings cellMappings = ( CellMappings ) mCellsMappings.get( oldCellName );
            if ( cellMappings != null ) {
                return cellMappings.getNewCellName();
            }
            else {
                return mChained.renameCell( oldCellName );
            }
        }
        
        public String renameNode( final String oldNodeName ) 
            throws CDLRenameException 
        {
            if ( mCellMappings != null ) {
                final String newNodeName = 
                    mCellMappings.getNewNodeNameForOldNodeName( oldNodeName );
                if ( newNodeName != null ) {
                    return newNodeName;
                }
                else {
                    return mChained.renameNode( oldNodeName );
                }
            }
            else {
                return mChained.renameNode( oldNodeName );
            }
        }

        public String renameDevice( final String oldDeviceName ) 
            throws CDLRenameException 
        {
            if ( mCellMappings != null ) {
                final String newDeviceName = 
                    mCellMappings.getNewInstanceNameForOldInstanceName( oldDeviceName );
                if ( newDeviceName != null ) {
                    return newDeviceName;
                }
                else {
                    return mChained.renameDevice( oldDeviceName );
                }
            }
            else {
                return mChained.renameDevice( oldDeviceName );
            }
        }

        public String renameSubCellInstance( final String oldInstanceName ) 
            throws CDLRenameException 
        {
            if ( mCellMappings != null ) {
                final String newInstanceName = 
                    mCellMappings.getNewInstanceNameForOldInstanceName( oldInstanceName );
                if ( newInstanceName != null ) {
                    return newInstanceName;
                }
                else {
                    return mChained.renameDevice( oldInstanceName );
                }
            }
            else {
                return mChained.renameDevice( oldInstanceName );
            } 
        }

        public String renameTransistorModel( final String oldTransistorModel ) 
            throws CDLRenameException 
        {
            
            final CellMappings cellMappings = ( CellMappings ) mCellsMappings.get( oldTransistorModel );
            if ( cellMappings != null ) {
                return cellMappings.getNewCellName();
            }
            else {
                return mChained.renameTransistorModel( oldTransistorModel );
            }
        }
    }

    final Map mCellsMaps;
    final CDLNameInterfaceFactory mNameInterfaceFactory;
    
    public BindRulNameInterfaceFactory( final Reader srcReader,
                                        final CDLNameInterfaceFactory nameInterfaceFactory ) 
        throws IOException
    {
        mCellsMaps = new HashMap();
        new BindRulParser( srcReader, mCellsMaps );
        mNameInterfaceFactory = nameInterfaceFactory;
    }


    public CDLNameInterface getNameInterface( final String cellName ) 
        throws CDLRenameException
    {
        final CDLNameInterface chained = mNameInterfaceFactory.getNameInterface( cellName );

        return new NameInterface( chained, cellName, mCellsMaps );
    }

    public Set getNewCellNames( ) {
        final Iterator entryIter = mCellsMaps.entrySet().iterator();

        final Set ret = new HashSet();

        while ( entryIter.hasNext() ) {
            final Map.Entry currEntry = ( Map.Entry ) entryIter.next();
            final CellMappings currCellMappings = ( CellMappings ) currEntry.getValue();
            ret.add( currCellMappings.getNewCellName() );
        }
        return ret;        
    }

    public Set getNewNodeNamesForOldCellName( final String oldCellName ) {
        final CellMappings cellMappings = ( CellMappings ) mCellsMaps.get( oldCellName );
        if ( cellMappings != null ) {
            return cellMappings.getNewNodeNames();
        }
        else {
            return Collections.EMPTY_SET;
        }
    }

    public Set getNewInstanceNamesForOldCellName( final String oldCellName ) {
        final CellMappings cellMappings = ( CellMappings ) mCellsMaps.get( oldCellName );
        if ( cellMappings != null ) {
            return cellMappings.getNewInstanceNames();
        }
        else {
            return Collections.EMPTY_SET;
        }
    }
    


}
