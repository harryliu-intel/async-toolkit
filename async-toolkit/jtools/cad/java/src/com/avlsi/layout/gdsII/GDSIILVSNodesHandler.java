/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;

import java.lang.String;
import java.lang.StringBuffer;

import java.util.Set;
import java.util.Iterator;
import java.util.List;

import java.text.MessageFormat;

import java.io.IOException;
import java.io.Writer;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.io.File;

import com.avlsi.util.container.Pair;

import com.avlsi.file.common.HierName;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;

import com.avlsi.file.cdl.parser.LVSNodesHandler;

import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;

/**
 * Writes SKILL table files for LVS nodes
 * open's a file for each cell and writes 
 * SKILL API calls for cell LVS nodes, and instance LVS nodes
 * see SKILL:
 * sw/cad/external-tools-integration/cadence/virtuoso/skill/schematic/lvsnodes.il
 **/
public class GDSIILVSNodesHandler implements LVSNodesHandler {

    //0=TableVarName
    //1=CellName
    private static final String lvsNodesHeaderFormatStr =
        "( setq {0} ( LVSNodesMakeEmptyLVSNodesTable \"{1}\" ) )\n"; 

    private final MessageFormat mLVSNodesHeaderFormatter;

    //0=TableVarName
    //1=lvsNodeGDSIIName
    private static final String cellLVSNodeOutputFormatStr =
        "( LVSNodesAddLVSNode {0} \"{1}\" )\n";
    private final MessageFormat mCellLVSNodeOutputFormatter;

    //0=TableVarName
    //1=InstanceVarName
    //2=instance name
    private static final String lvsNodesInstanceOutputFormatStr =
        "( setq \n" +
        "  {1}\n" +
        "  ( LVSNodesMakeLVSNodesInstance {0} \"{2}\" ) )\n";
    private final MessageFormat mLVSNodesInstanceOutputFormatter;
    
    //0=InstanceVarName
    //1=Node
    //2=NodeInInstance
    private static final String lvsNodesInstanceConnectionPairOutputFormatStr =
        "( LVSNodesMakeLVSNodesInstanceConnection\n" +
        "  {0}\n" +
        "  \"{1}\"\n" +
        "  \"{2}\" )\n";
    private final MessageFormat mLVSNodesInstanceConnnectionPairOutputFormatter;

    static final private class Emitter {

        private static final String currLVSNodesInstanceVarName = "GDSIILVSNodesCurrInstance";
        private final TableEmitterInterface mTableEmitter;
        private final CDLNameInterface mCadenceNamesNI;
        private final CDLNameInterface mGDSIINamesNI;
        private final Writer mWriter;
        private final MessageFormat mCellLVSNodeOutputFormatter;
        private final MessageFormat mLVSNodesInstanceOutputFormatter;
        private final MessageFormat mLVSNodesInstanceConnnectionPairOutputFormatter;

        private final String mTableVarName;

        private final StringBuffer mAccum;

        private final File mOutputFile;

        private void writeAccum() {
            try {
                mWriter.write( mAccum.toString() ); 
                mAccum.delete( 0, mAccum.length() ); 
            }
            catch ( IOException e ) {
                throw new RuntimeException( e );
            }
        }

        public Emitter( final File outputFile,
                        final TableEmitterInterface tableEmitter,
                        final CDLNameInterface cadenceNamesNI,
                        final CDLNameInterface gdsIINamesNI,
                        final MessageFormat lvsNodesHeaderFormatter,
                        final MessageFormat cellLVSNodeOutputFormatter,
                        final MessageFormat lvsNodesInstanceOutputFormatter,
                        final MessageFormat lvsNodesInstanceConnnectionPairOutputFormatter,
                        final String tableVarName,
                        final String cellName ) {
            try {
                mOutputFile = outputFile;
                mTableEmitter = tableEmitter;
                final OutputStream outputStream = new FileOutputStream( mOutputFile );
                mWriter = new OutputStreamWriter( outputStream, "UTF-8" );
                mCadenceNamesNI = cadenceNamesNI;
                mGDSIINamesNI = gdsIINamesNI;
                mCellLVSNodeOutputFormatter = cellLVSNodeOutputFormatter;
                mLVSNodesInstanceOutputFormatter = lvsNodesInstanceOutputFormatter;
                mLVSNodesInstanceConnnectionPairOutputFormatter = lvsNodesInstanceConnnectionPairOutputFormatter;
                mTableVarName = tableVarName;
                
                mAccum = new StringBuffer();
                
                final Object[] headerArgs = { tableVarName, gdsIINamesNI.renameCell( cellName ) };
                
                lvsNodesHeaderFormatter.format( headerArgs, mAccum, null );
                writeAccum();
                
            }
            catch ( CDLRenameException e ) {
                throw new RuntimeException( e );
            }
            catch ( IOException e ) {
                throw new RuntimeException( e );
            }
            
        }

        public void lvsNodesForInstance( final HierName instanceName,
                                         final List lvsNodeConnectionPairs ) {
            try {
                final Object[] instanceArgs = { mTableVarName, 
                                                currLVSNodesInstanceVarName, 
                                                mGDSIINamesNI.renameSubCellInstance( instanceName.getCadenceString() ) };

                mLVSNodesInstanceOutputFormatter.format( instanceArgs, mAccum, null );
                writeAccum();

                final Iterator connectionIter = lvsNodeConnectionPairs.iterator();
                
                Object[] connectionArgs = new Object[3];

                while ( connectionIter.hasNext() ) {
                    final Pair connection = ( Pair ) connectionIter.next();
                    final HierName node = ( HierName ) connection.getFirst();
                    final HierName nodeInInstance = ( HierName ) connection.getSecond();

                    connectionArgs[0] = currLVSNodesInstanceVarName;
                    connectionArgs[1] = mGDSIINamesNI.renameNode( node.getCadenceString() );
                    connectionArgs[2] = mGDSIINamesNI.renameNode( nodeInInstance.getCadenceString() );
                    
                    mLVSNodesInstanceConnnectionPairOutputFormatter.format( connectionArgs, mAccum, null );
                    writeAccum();
                }
                

            }
            catch ( CDLRenameException e ) {
                throw new RuntimeException( e );
            }
        }

        public void end( final Set cellLVSNodes ) {
            try {
                final Iterator nodeIter = cellLVSNodes.iterator();
                
                Object[] nodeArgs = new Object[2];

                while ( nodeIter.hasNext() ) {
                    final HierName node = ( HierName ) nodeIter.next();

                    final String castNodeName = node.getCadenceString();
                    final String cadenceNodeName = mCadenceNamesNI.renameNode( castNodeName );
                    final String gdsIINodeName = mGDSIINamesNI.renameNode( castNodeName );
                    
                    nodeArgs[0] = mTableVarName;
                    nodeArgs[1] = gdsIINodeName;

                    mCellLVSNodeOutputFormatter.format( nodeArgs, mAccum, null );
                    writeAccum();
                    
                    mTableEmitter.emitNodeName( castNodeName,
                                                cadenceNodeName,
                                                gdsIINodeName
                                                );

                }
                mWriter.close();
                mTableEmitter.close();
            }
            catch ( CDLRenameException e ) {
                throw new RuntimeException( e );
            }
            catch ( IOException e ) {
                throw new RuntimeException( e );
            }
            catch ( TableEmitterException e ) {
                throw new RuntimeException( e );
            }
        }
        public void abort( ) {
            try {
                mWriter.close();
                mOutputFile.delete();
                mTableEmitter.close();
            }
            catch ( IOException e ) {
                throw new RuntimeException( e );
            }
            catch ( TableEmitterException e ) {
                throw new RuntimeException( e );
            }
        }

    }


    private static final String tableVarName = "GDSIILVSNodesTable";

    private final TableEmitterFactoryInterface mTableEmitterFactory;
    private final CDLNameInterfaceFactory mCadenceNames;
    private final CDLNameInterfaceFactory mGDSIINames;
    private final File mOutputDir;

    private Emitter mCurrEmitter;

    public GDSIILVSNodesHandler( final TableEmitterFactoryInterface tableEmitterFactory,
                                 final CDLNameInterfaceFactory cadenceNames,
                                 final CDLNameInterfaceFactory gdsIINames,
                                 final File outputDir ) {
        mTableEmitterFactory = tableEmitterFactory;
        mCadenceNames = cadenceNames;
        mGDSIINames = gdsIINames;
        mOutputDir = outputDir;
        
        mCurrEmitter = null;

        mLVSNodesHeaderFormatter =
            new MessageFormat( lvsNodesHeaderFormatStr );
        mCellLVSNodeOutputFormatter =
            new MessageFormat( cellLVSNodeOutputFormatStr );
        mLVSNodesInstanceOutputFormatter =
            new MessageFormat( lvsNodesInstanceOutputFormatStr );
        mLVSNodesInstanceConnnectionPairOutputFormatter =
            new MessageFormat( lvsNodesInstanceConnectionPairOutputFormatStr );

    }
    
    public void startCell( final String cellName ) {
        assert mCurrEmitter == null;

        try {
            final CDLNameInterface gdsIINI = mGDSIINames.getNameInterface( cellName );
            final CDLNameInterface cadenceNI = mCadenceNames.getNameInterface( cellName );

            final String cadenceCellName = cadenceNI.renameCell( cellName );
            final String gdsIICellName = gdsIINI.renameCell( cellName );

            final TableEmitterInterface tableEmitter = mTableEmitterFactory.getTableEmitter( cellName,
                                                                                             cadenceCellName,
                                                                                             gdsIICellName );


            final File outputFile = new File( mOutputDir,
                                              cadenceNI.renameCell( cellName ) + 
                                              ".lvsNodes.il" );

            mCurrEmitter = new Emitter( outputFile,
                                        tableEmitter,
                                        cadenceNI,
                                        gdsIINI,
                                        mLVSNodesHeaderFormatter,
                                        mCellLVSNodeOutputFormatter,
                                        mLVSNodesInstanceOutputFormatter,
                                        mLVSNodesInstanceConnnectionPairOutputFormatter,
                                        tableVarName,
                                        cellName );
            
            
                                              
        }
        catch ( CDLRenameException e ) {
            throw new RuntimeException( e );
        }
        catch ( TableEmitterException e ) {
            throw new RuntimeException( e );
        }
        
    }

    public void lvsNodesForInstance( final HierName instanceName, 
                                     final List lvsNodeConnectionPairs ) {
        assert mCurrEmitter != null;
        mCurrEmitter.lvsNodesForInstance( instanceName, lvsNodeConnectionPairs );
    }

    public void endCell( final String cellName, final Set cellLVSNodes ) {
        mCurrEmitter.end( cellLVSNodes );
        mCurrEmitter = null;
    }

    public void abortCell( final String cellName ) {
        mCurrEmitter.abort();
        mCurrEmitter = null;
    }

}
