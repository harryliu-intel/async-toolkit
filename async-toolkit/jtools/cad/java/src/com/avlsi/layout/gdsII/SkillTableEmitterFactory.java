/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;


import java.io.Writer;
import java.io.OutputStreamWriter;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;

import java.util.Set;
import java.util.HashSet;

import java.text.MessageFormat;

import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

public final class SkillTableEmitterFactory implements TableEmitterFactoryInterface {


    private static final String mappingOutputFormatStr =
        "( setarray {0} \"{1}\" \"{2}\" )\n";
    private final MessageFormat mMappingOutputFormatter;
    
    private static final String mappingHeaderFormatStr =
        "( defvar {0} ( makeTable \"foo\" nil ) )\n" +
        "( defvar {1} ( makeTable \"bar\" nil ) )\n" +
        "( defvar {2} ( makeTable \"baz\" nil ) )\n";
    private final MessageFormat mMappingHeaderFormatter;

    private static final class Emitter implements TableEmitterInterface {
        private final Writer mWriter;
        private final String mNodeNameMappingTableVarName;
        private final String mCellNameMappingTableVarName;
        private final String mInstanceNameMappingTableVarName;
        private final MessageFormat mMappingOutputFormatter;
        private final StringBuffer mAccum;
        private final Set mTranslatedCellNames;
        private final Set mTranslatedNodeNames;
        
        public Emitter( final Writer writer,
                        final String nodeNameMappingTableVarName,
                        final String cellNameMappingTableVarName,
                        final String instanceNameMappingTableVarName,
                        final MessageFormat mappingOutputFormatter ) {
            mWriter = writer;
            mNodeNameMappingTableVarName = nodeNameMappingTableVarName;
            mCellNameMappingTableVarName = cellNameMappingTableVarName;
            mInstanceNameMappingTableVarName = instanceNameMappingTableVarName;
            mMappingOutputFormatter = mappingOutputFormatter;
            mAccum = new StringBuffer();
            mTranslatedCellNames = new HashSet();
            mTranslatedNodeNames = new HashSet();
        }

        private void emitName( final String tableName,
                               final String cadenceName,
                               final String gdsIIName ) throws TableEmitterException {
            final Object[] mappingOutputParams = {
                tableName,
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

        public boolean haveCellName( final String castName ) {
            return mTranslatedCellNames.contains( castName );
        }
    
        public void emitCellName( final String castName, 
                                  final String cadenceName, 
                                  final String gdsIIName ) throws TableEmitterException {
            if ( ! ( haveCellName( castName ) ) ) {
                emitName( mCellNameMappingTableVarName, cadenceName, gdsIIName );
                mTranslatedCellNames.add( castName );
            }
        }
    
        public boolean haveNodeName( final String castName ) {
            return mTranslatedNodeNames.contains( castName );
        }
    
        public void emitNodeName( final String castName, 
                                  final String cadenceName,
                                  final String gdsIIName ) throws TableEmitterException {
            if ( ( ! ( haveNodeName( castName ) ) ) ) {   
                emitName( mNodeNameMappingTableVarName, cadenceName, gdsIIName );
                mTranslatedNodeNames.add( castName );
            }
            
        }
    
        public boolean haveInstanceName( final String castName ) {
            return false;
        }
        
        public void emitInstanceName( final String castName,
                                      final String cadenceName,
                                      final String gdsIIName ) throws TableEmitterException {
            emitName( mInstanceNameMappingTableVarName, cadenceName, gdsIIName );
        }
        
        public void close() throws TableEmitterException {
            try {
                mWriter.close();
            }
            catch ( IOException e ) {
                throw new TableEmitterException( "Unable to close the file.", e );
            }

        }
        
    }

    private final File mOutputDir;
    private final String mNodeNameMappingTableVarName;
    private final String mCellNameMappingTableVarName;
    private final String mInstanceNameMappingTableVarName;

    public SkillTableEmitterFactory( final File outputDir,
                                     final String nodeNameMappingTableVarName,
                                     final String cellNameMappingTableVarName,
                                     final String instanceNameMappingTableVarName ) {
        mOutputDir = outputDir;
        mNodeNameMappingTableVarName = nodeNameMappingTableVarName;
        mCellNameMappingTableVarName = cellNameMappingTableVarName;
        mInstanceNameMappingTableVarName = instanceNameMappingTableVarName;
        
        mMappingOutputFormatter = new MessageFormat( mappingOutputFormatStr );
           
        mMappingHeaderFormatter = new MessageFormat( mappingHeaderFormatStr );
        
    }

    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName )
        throws TableEmitterException {
        
        final File outputFile = new File( mOutputDir, cadenceCellName + ".names.il" );
        try {
            final OutputStream outputStream = new FileOutputStream( outputFile );
            final Writer writer = new OutputStreamWriter( outputStream, "UTF-8" );
            
            final StringBuffer accum = new StringBuffer();
            
            final Object[] headerParams = { mNodeNameMappingTableVarName,
                                            mCellNameMappingTableVarName,
                                            mInstanceNameMappingTableVarName };
            
            mMappingHeaderFormatter.format( headerParams, accum, null );
            try {
                writer.write( accum.toString() );
                
                final TableEmitterInterface skillEmitter =
                    new Emitter( writer,
                                 mNodeNameMappingTableVarName,
                                 mCellNameMappingTableVarName,
                                 mInstanceNameMappingTableVarName,
                                 mMappingOutputFormatter );

                return skillEmitter;
            }
            catch( IOException e ) {
                throw new TableEmitterException( "Unable to write header.",
                                                 e );
            }
        }
        catch ( FileNotFoundException e ) {
            throw new TableEmitterException( "Unable to open " + 
                                             outputFile.toString() +
                                             ".",
                                             e );
        }
        catch ( IOException e ) {
            throw new TableEmitterException( "Unable to open " + 
                                             outputFile.toString() +
                                             ".",
                                             e );
        }
                            
    }

}
