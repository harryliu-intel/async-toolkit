/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.floorplanning;

import java.util.Map;
import java.util.HashMap;
import java.util.Properties;
import java.util.Iterator;
import java.util.NoSuchElementException;

import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Iterator2StringContainerIteratorAdapter;

import com.avlsi.floorplanning.PCellTypesInfo;
import com.avlsi.floorplanning.PCellTypeInfo;
import com.avlsi.floorplanning.TransistorTypeInfo;
import com.avlsi.floorplanning.PropertyFilePCellTypeInfo;
import com.avlsi.floorplanning.PropertyFileTransistorTypeInfo;

public class PCellTypesInfoImpl implements PCellTypesInfo {

    private final Map m_PCellTypesInfo;
    private final Map m_TransistorTypesInfo;

    public PCellTypesInfoImpl( final StringContainerIterator dirNames ) {
        m_PCellTypesInfo = new HashMap();
        m_TransistorTypesInfo = new HashMap();

        while ( dirNames.hasNext() ) {
            final String currDirName = dirNames.next();

            final File currDir = new File( currDirName );
            
            if ( currDir.isDirectory() && currDir.canRead() ) {
                final File[] infoFiles = currDir.listFiles();

                int i;
                for ( i = 0 ; i < infoFiles.length ; ++i ) {
                    if ( infoFiles[i].isFile() && infoFiles[i].canRead() ) {
                        try {
                            final InputStream infoFileStream = 
                                new FileInputStream( infoFiles[i] );
                            final Properties info =
                                new Properties();
                            info.load( infoFileStream );
                            
                            final boolean isTransistorInfo =
                                PropertyFileTransistorTypeInfo.isTransistorInfo( info );
                            if ( isTransistorInfo ) {
                                final PropertyFileTransistorTypeInfo tInfo =
                                    new PropertyFileTransistorTypeInfo( info ) ;
                                m_TransistorTypesInfo.put( tInfo.getModelName(),
                                                           tInfo );
                            }
                            else {
                                final PropertyFilePCellTypeInfo pInfo =
                                    new PropertyFilePCellTypeInfo( info ) ;
                                m_PCellTypesInfo.put( pInfo.getCellName(),
                                                      pInfo );
                            }
                        }
                        catch( FileNotFoundException e ) {
                        }
                        catch( IOException e ) {
                        }
                        
                    }
                }
                
            }
        }

    }

    public PCellTypeInfo getPCellTypeInfo( final String cellName ) {
        return ( PCellTypeInfo ) m_PCellTypesInfo.get( cellName );
    }

    public TransistorTypeInfo getTransistorInfo( final String modelName ) {
        return ( TransistorTypeInfo ) m_TransistorTypesInfo.get( modelName );
    }

    public StringContainerIterator getTransistorModels() {

        final Iterator keyIter = 
            m_TransistorTypesInfo.keySet().iterator();

        final StringContainerIterator ret =
            new Iterator2StringContainerIteratorAdapter( keyIter );

        return ret;
    }

    public Iterator getAllPCellTypeInfos() {
        return new Iterator() {
                final private Iterator firstIter =  m_TransistorTypesInfo.entrySet().iterator();
                final private Iterator secondIter = m_PCellTypesInfo.entrySet().iterator();

                public boolean hasNext() {
                    return ( firstIter.hasNext() || secondIter.hasNext() );
                }

                public Object next() {
                    if ( firstIter.hasNext() ) {
                        return ( ( Map.Entry ) firstIter.next()).getValue();
                    }
                    else {
                        return ( ( Map.Entry ) secondIter.next()).getValue();
                    }
                }
                public void remove() {
                    
                }
            };
    }

    public Iterator getNonTransistorPCellTypeInfos() {
        return new Iterator() {
                final private Iterator entryIter = m_PCellTypesInfo.entrySet().iterator();
                public boolean hasNext() {
                    return entryIter.hasNext();
                }

                public Object next() {
                    final Map.Entry currEntry = ( Map.Entry ) entryIter.next();
                    return currEntry.getValue();
                }
                
                public void remove() {
                    entryIter.remove();
                }
            };
    }
}
