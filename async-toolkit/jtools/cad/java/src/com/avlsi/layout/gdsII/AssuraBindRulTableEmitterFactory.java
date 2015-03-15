/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;


import java.text.MessageFormat;

import java.io.Reader;
import java.io.Writer;
import java.io.OutputStreamWriter;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.FileInputStream;

import java.util.Set;
import java.util.HashSet;

import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;
import com.avlsi.layout.gdsII.CachingTableEmitter;

public class AssuraBindRulTableEmitterFactory implements TableEmitterFactoryInterface {

   
   

    private static class Emitter implements TableEmitterInterface {

        private static final String cellMappingChar = "C";
        private static final String nodeMappingChar = "N";
        private static final String instanceMappingChar = "I";

        private static final String mappingOutputFormatStr =
            "{0} {1} {2}\n";
    
        private final Writer mWriter;
        private final StringBuffer mAccum;
        private final MessageFormat mMappingOutputFormatter;

        private final Set mTranslatedNodeNames;
        private final boolean mBindAllNodes;

        public Emitter( final Writer writer, final boolean bindAllNodes ) {
            mWriter = writer;
            mAccum = new StringBuffer();
            mMappingOutputFormatter = new MessageFormat( mappingOutputFormatStr );
            
            mTranslatedNodeNames = new HashSet();
            mBindAllNodes = bindAllNodes ;
        }

        private void emitName( final String mappingType,
                               final String cadenceName,
                               final String gdsIIName ) throws TableEmitterException {
            emitName( mappingType, cadenceName, gdsIIName, false );
        }

        private void emitName( final String mappingType,
                               final String cadenceName,
                               final String gdsIIName,
                               final boolean bindAll )
        throws TableEmitterException {
            if ( bindAll || ! cadenceName.equals( gdsIIName ) ) {
                final Object[] mappingOutputParams = {
                    mappingType,
                    cadenceName,
                    gdsIIName };
                
                mMappingOutputFormatter.format( mappingOutputParams,
                                                 mAccum,
                                                 null );
                try {
                    mWriter.write( mAccum.toString() );
                }
                catch ( IOException e ) {
                    throw new TableEmitterException( "Unable to emit mapping: " +
                                                     cadenceName + "=" + gdsIIName + ".",
                                                     e );
                }
                mAccum.delete( 0, mAccum.length() );
            }
        }

        public boolean haveCellName( final String castName ) {
            return false;
        }
        
        public void realEmitCellName( final String castName, 
                                      final String cadenceName, 
                                      final String gdsIIName ) throws TableEmitterException {
            emitName( cellMappingChar, cadenceName, gdsIIName );
        }

        public void emitCellName( final String castName, 
                                  final String cadenceName, 
                                  final String gdsIIName )  {
        }
    
        public boolean haveNodeName( final String castName ) {
            return mTranslatedNodeNames.contains( castName );
        }
    
        public void emitNodeName( final String castName, 
                                  final String cadenceName,
                                  final String gdsIIName ) throws TableEmitterException {
            if ( ! ( haveNodeName( castName ) ) ) {
                emitName( nodeMappingChar, cadenceName, gdsIIName,
                          mBindAllNodes );
                mTranslatedNodeNames.add( castName );
            }
        }
    
        public boolean haveInstanceName( final String castName ) {
            return false;
        }
        
        public void emitInstanceName( final String castName,
                                      final String cadenceName,
                                      final String gdsIIName ) throws TableEmitterException {
            emitName( instanceMappingChar, cadenceName, gdsIIName );
        }
        
        public void close() throws TableEmitterException {
            try {
                mWriter.write( '\n' );
                mWriter.flush();
            }
            catch ( IOException e ) {
                throw new TableEmitterException( "Unable to flush the file.", e );
            }
        }
     
    }

    
    private final boolean mBindAllNodes;
    private final Writer mWriter;

    public AssuraBindRulTableEmitterFactory( final File outputFile,
                                             final File header )
    throws TableEmitterException {
        this( outputFile, header, false );
    }

    public AssuraBindRulTableEmitterFactory( final File outputFile,
                                             final File header,
                                             final boolean bindAllNodes )
    throws TableEmitterException {
        mBindAllNodes = bindAllNodes;
        try {
            final FileInputStream inputStream = new FileInputStream( header );
            final Reader reader = new InputStreamReader ( inputStream, "UTF-8" );
            
            try {
                final OutputStream outputStream = new FileOutputStream( outputFile );
                final Writer writer = new OutputStreamWriter( outputStream );
                
                try {
                    int currCharInt ;
                    do {
                        currCharInt = reader.read();
                        if ( currCharInt > 0 ) {
                            writer.write( ( char ) currCharInt );
                        }
                    } while ( currCharInt >= 0 ) ;
                    writer.write( '\n' );
                    writer.write( '\n' );
                    
                }
                catch ( IOException e ) {
                    throw new TableEmitterException( "Unable to copy header.", e );
                }
                mWriter = writer;
                
            }
            catch ( FileNotFoundException e ) {
                throw new TableEmitterException( "Unable to open " + 
                                                 outputFile.toString() +
                                                 ".",
                                                 e );
            }
        }
        catch ( FileNotFoundException e ) {
            throw new TableEmitterException( "Unable to open " + 
                                             header.toString() +
                                             ".",
                                             e );
        }
        catch ( IOException e ) {
            throw new TableEmitterException( "Unable to open " + 
                                             header.toString() +
                                             ".",
                                             e );
        }
    }


    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName ) 
    throws TableEmitterException {
        final Emitter emitter = new Emitter( mWriter, mBindAllNodes );

        emitter.realEmitCellName( castCellName, cadenceCellName, gdsIICellName );
        return emitter;
    }


}
