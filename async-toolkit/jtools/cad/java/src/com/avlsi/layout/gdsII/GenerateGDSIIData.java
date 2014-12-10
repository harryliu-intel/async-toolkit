/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout.gdsII;



import java.util.Map;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;

import java.io.File;
import java.io.OutputStream;
import java.io.Writer;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.IOException;

import java.text.MessageFormat;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;

import com.avlsi.io.FileSearchPath;

import com.avlsi.file.common.HierName;

import com.avlsi.cast.CastFileParser;

import com.avlsi.cast.impl.Environment;

import com.avlsi.cell.CellInterface;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.SplittingFactory;
import com.avlsi.file.cdl.parser.InternalNodeFilterFactory;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.LVSNodesCDLFactory;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;

import com.avlsi.tools.jauto.Cast2Cdl;
import com.avlsi.tools.jauto.PartialExtract;

import com.avlsi.layout.LVSNodes;
import com.avlsi.layout.LVSNodesForExtract;

import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.GDSIILVSNodesHandler;
import com.avlsi.layout.gdsII.FilterInternalNodesTableEmitterFactory;
import com.avlsi.layout.gdsII.SkillTableEmitterFactory;
import com.avlsi.layout.gdsII.AssuraBindRulTableEmitterFactory;
import com.avlsi.layout.gdsII.SharedTableEmitterFactory;

public class GenerateGDSIIData {
   
    private static void usage( String m ) {
        final String className = GenerateGDSIIData.class.getName();
        
        System.err.println( "Usage: generate_gdsII_data");
        System.err.println("  --cast-path=<cast-path> (defaults to .)");
        System.err.println("  --cast-version=<version> (defaults to 2)");
        System.err.println("  --cell=<cell> (name of cell to process)");
        System.err.println("  --bind-rul=<file> ( bind.rul file for LVS)");
        System.err.println("  --bind-all-nodes (include all nodes in output bind.rul)");
        System.err.println("  --output-root-cell-name=<cell>(defaults to fully-qualified cast name)");
        System.err.println("  --output-dir=<file>\n");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    private static void usage() {
        usage( null );
    }

    private static final class PartialExtractResultHandler implements PartialExtract.CellResultHandler {
        private static final String partialExtractTableVarName = "PartialExtractTable";

        private static final String partialExtractInstanceFormatStr =
            "( setarray ( arrayref {0} \"{1}\" ) \"{2}\" \"{3}\" )\n";
        
        private static final String partialExtractSubTypeFormatStr =
            "( setarray {0} \"{1}\" ( makeTable \"partialExtractInstanceTable\" nil ) )\n";
        
        private static final String partialExtractHeaderFormatStr =
            "( defvar {0} ( makeTable \"partialExtractTable\" nil ) )\n";
        private final MessageFormat mPartialExtractInstanceFormatter;
        private final MessageFormat mPartialExtractSubTypeFormatter;
        private final MessageFormat mPartialExtractHeaderFormatter;
        
        final StringBuffer accumulator;

        final File mOutputDir;

        Writer mCurrOutputWriter;

        private final Map mOldCellNameMap = new HashMap();
        private String mCurrCellName;

        Exception mError;

        final CDLNameInterface mGDSIINameInterface;
        final CDLNameInterface mCadenceNameInterface;

        public Exception getError() {
            return mError;
        }


        public void finish() 
            throws IOException{
            if ( mCurrOutputWriter != null ) {
                mCurrOutputWriter.close();
            }
        }

        public PartialExtractResultHandler( final File outputDir ) {

            mPartialExtractInstanceFormatter =
                new MessageFormat( partialExtractInstanceFormatStr );
            mPartialExtractSubTypeFormatter =
                new MessageFormat( partialExtractSubTypeFormatStr );
            mPartialExtractHeaderFormatter =
                new MessageFormat( partialExtractHeaderFormatStr );
            accumulator = new StringBuffer();

            mOutputDir = outputDir;
            mError = null;
            mCurrOutputWriter = null;
            mGDSIINameInterface = new GDS2NameInterface();
            mCadenceNameInterface = new CadenceNameInterface();
        }

        public void setCellName( final String cellName ) {
            if ( mError == null ) {
                try {
                    if ( mCurrOutputWriter != null ) {
                        mCurrOutputWriter.close();
                    }
                    
                    mCurrCellName = cellName;

                    final String cadenceCellName = mCadenceNameInterface.renameCell( cellName );

                    final File fileForCell = new File( mOutputDir, cadenceCellName + ".partial.il" );
                    final OutputStream streamForCell = new FileOutputStream( fileForCell );
                    final Writer writerForCell = new BufferedWriter( new OutputStreamWriter( streamForCell, "UTF-8" ) );
                    mCurrOutputWriter = writerForCell;
                    
                    final Object[] partialExtractHeaderParams = {
                        partialExtractTableVarName
                    };

                    mPartialExtractHeaderFormatter.format( partialExtractHeaderParams,
                                                           accumulator,
                                                           null );
                    mCurrOutputWriter.write( accumulator.toString() );
                    accumulator.delete( 0, accumulator.length() );
                        
                }
                catch ( CDLRenameException e ) {
                    mError = e;
                }
                catch( IOException e ) {
                    mError = e;
                }
            }
        }

        public void handleSubtype( final String subTypeName,
                                   final Map instances ) {
          if ( mError == null ) {
                try {
                    assert mCurrOutputWriter != null;
                    assert mCurrCellName != null;
                    
                    mOldCellNameMap.put(subTypeName,mCurrCellName);

                    final Object[] partialExtractSubTypeParams = {
                        partialExtractTableVarName,
                        mGDSIINameInterface.renameCell( subTypeName ) };
                    mPartialExtractSubTypeFormatter.format( partialExtractSubTypeParams, accumulator, null );
                    mCurrOutputWriter.write( accumulator.toString() );
                    accumulator.delete( 0, accumulator.length() );
                    
                    final Iterator instancesEntryIter = instances.entrySet().iterator();

                    while ( instancesEntryIter.hasNext() ) {
                        Map.Entry instanceEntry = ( Map.Entry ) instancesEntryIter.next();
                        final HierName instanceName = ( HierName ) instanceEntry.getKey();
                        final String instanceMasterName = ( String ) instanceEntry.getValue();
                        
                        final Object[] partialExtractInstanceParams = {
                            partialExtractTableVarName,
                            mGDSIINameInterface.renameCell( subTypeName ),
                            mGDSIINameInterface.renameSubCellInstance( instanceName.toString() ),
                            mGDSIINameInterface.renameCell( instanceMasterName ) };

                        mPartialExtractInstanceFormatter.format( partialExtractInstanceParams, accumulator, null );
                        mCurrOutputWriter.write( accumulator.toString() );
                        accumulator.delete( 0, accumulator.length() );
                    }

                }
                catch ( CDLRenameException e ) {
                    mError = e;
                }
                catch( IOException e ) {
                    mError = e;
                }
            }  
        }

        public Map getOriginalNames(final String subTypeName ) {
            return mOldCellNameMap;
        }
    }
    
    public static void writePartialExtractInfo( final PartialExtract peInfo,
                                                final File outputDir )
        throws Exception {
        
        final PartialExtractResultHandler handler = 
            new PartialExtractResultHandler( outputDir );
        peInfo.traverseResult( handler );
        handler.finish();
        Exception e = handler.getError();
        if(e != null) {
            throw e;
        }
    }

    public static void main( String args[] ) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CachingCommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String givenCellName = theArgs.getArgValue("cell", null);
        final String outputRootCellName = theArgs.getArgValue("output-root-cell-name", null);
        final String cellName;        
        cellName = givenCellName;

        final File bindRulFile = new File( theArgs.getArgValue( "bind-rul", "/dev/null" ));

        final String outputDirName = theArgs.getArgValue( "output-dir", null );

        pedanticArgs.argTag( "bind-all-nodes" );

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }

        final File outputDir;

        if ( outputDirName != null ) {
            outputDir = new File ( outputDirName );
        }
        else {
            outputDir = null ;
        }
        
        if ( ( outputDir != null ) && 
             ( ! ( outputDir.exists() ) ) ) {
            outputDir.mkdirs();
        }
        
        if ( ( cellName != null ) &&
             ( bindRulFile != null ) &&
             ( bindRulFile.canRead() ) &&
             ( outputDir != null ) &&
             ( outputDir.isDirectory() ) ) {

            final CastFileParser castParser =
                new CastFileParser( new FileSearchPath(castRoot), castVersion );

            final Cadencize cadencizer = new Cadencize(false);

            final PartialExtract.CellPlusMinus cellNameSpec =
                new PartialExtract.CellPlusMinusKeyword( cellName,
                                                  PartialExtract.Info.INCLUDE,
                                                  castParser, cadencizer);
            
            final CellInterface ci = castParser.getFullyQualifiedCell( cellNameSpec.getTop() );
            
            cadencizer.convert( ci );
            
            final CadenceNameInterface cadenceNameInterface = 
                new CadenceNameInterface();
            final CDLNameInterface renameTopCellGDSIINameInterface = 
                new GDS2NameInterface() {
                    public String renameCell(String oldCellName)
                        throws CDLRenameException {
                        if(outputRootCellName != null && 
                           !outputRootCellName.equals("") && 
                           oldCellName.equals(cellName)) {
                            return outputRootCellName;
                        }
                        else {
                            return super.renameCell(oldCellName);
                        }
                    }
                };

            final TrivialCDLNameInterfaceFactory cadenceNameInterfaceFactory =
                new TrivialCDLNameInterfaceFactory( cadenceNameInterface );

            final TrivialCDLNameInterfaceFactory gdsIINameInterfaceFactory =
                new TrivialCDLNameInterfaceFactory( renameTopCellGDSIINameInterface);
            

            // assura bind.rul table
            final File outputBindRul = new File( outputDir, "bind.rul" );
            final boolean bindAllNodes = theArgs.argExists("bind-all-nodes");
            final TableEmitterFactoryInterface bindRulEmitterFactory =
                new AssuraBindRulTableEmitterFactory( outputBindRul,
                                                      bindRulFile,
                                                      bindAllNodes );
            
            // names.il table
            final TableEmitterFactoryInterface skillEmitterFactory =
                new SkillTableEmitterFactory( outputDir,
                                              "CadenceNodeToGDSIINodeTable",
                                              "CadenceCellToGDSIICellTable",
                                              "CadenceInstanceToGDSIIInstanceTable" );

            final TableEmitterFactoryInterface emitterFactory;

            /**
             * We will emit both names.il and bind.rul now only if 
             * this is not a partial extraction.  The reason is that the 
             * names.il tables should have original names, while the bind.rul
             * should have the partial extract names
             **/

            if ( ! cellNameSpec.isEmpty() ) {
                emitterFactory = skillEmitterFactory;
            }
            else {
                emitterFactory =
                    ( new SplittingTableEmitterFactory
                      (bindRulEmitterFactory,
                       skillEmitterFactory) );
            }

            final TableEmitterFactoryInterface sharedEmitterFactory = 
                new SharedTableEmitterFactory( emitterFactory );

            final TableEmitterFactoryInterface filteredEmitterFactory =
                new FilterInternalNodesTableEmitterFactory
                ( cadencizer,
                  sharedEmitterFactory );
            
            final GenerateGDSIIDataFactory gdsIIDataFactory = 
                new GenerateGDSIIDataFactory
                ( filteredEmitterFactory,
                  cadenceNameInterfaceFactory,
                  gdsIINameInterfaceFactory );

            final CDLFactoryInterface originalCDLFactory;
            final CDLFactoryInterface partialExtractCDLFactory;
            final Map circuitTemplatesMap;

            /** If it's a partial extract, then want to execute the partial
             * extract netlist on the bind.rul emitter, and of course, create 
             * the templates for partial extract.  Otherwise, we just
             * write the names.il and bind.rul emitters
             **/
            if ( ! cellNameSpec.isEmpty() ) {
                circuitTemplatesMap = new HashMap();
                final Template templates =
                    new Template( circuitTemplatesMap );
                originalCDLFactory = 
                    new SplittingFactory( templates,
                                          gdsIIDataFactory);
                partialExtractCDLFactory = 
                    new InternalNodeFilterFactory
                    ( new GenerateGDSIIDataFactory
                      ( bindRulEmitterFactory,
                        cadenceNameInterfaceFactory,
                        gdsIINameInterfaceFactory ) );
            }
            else {
                circuitTemplatesMap = null;
                originalCDLFactory = gdsIIDataFactory;
                partialExtractCDLFactory = null;
            }

            final GDSIILVSNodesHandler lvsNodesHandler =
                new GDSIILVSNodesHandler( sharedEmitterFactory,
                                          cadenceNameInterfaceFactory,
                                          gdsIINameInterfaceFactory,
                                          outputDir ) ;

            final LVSNodes lvsNodesInfo = new LVSNodesForExtract( castParser,
                                                                  cadencizer );
            final CDLFactoryInterface lvsNodesEmitter = 
                new LVSNodesCDLFactory( lvsNodesInfo,
                                        originalCDLFactory,
                                        lvsNodesHandler );
            
            Cast2Cdl.outputCDL( ci,
                                castParser,
                                lvsNodesEmitter,
                                cadencizer,
                                false,
                                false );

            if ( ! cellNameSpec.isEmpty() ) {
                final PartialExtract pe =
                    new PartialExtract( circuitTemplatesMap,
                                        ci.getFullyQualifiedType(),
                                        cellNameSpec );
                pe.execute( partialExtractCDLFactory );
                writePartialExtractInfo( pe, outputDir );
            }

            final File fileForCell = 
                new File( outputDir, "name.txt" );
            final OutputStream streamForCell =
                new FileOutputStream( fileForCell );
            final Writer writerForCell =
                new BufferedWriter( new OutputStreamWriter( streamForCell, "UTF-8" ) );
            writerForCell.write( cadenceNameInterface.renameCell( ci.getFullyQualifiedType() ) );
            writerForCell.write( "\n" );
            writerForCell.close();
            
        }
        else {
            if ( cellName == null ) {
                System.out.println( "You must specify a cast cell name." );
            }
            if ( bindRulFile == null ) {
                System.out.println( "You must specify the location of the bind.rul file." );
            }
            else if ( ! ( ( bindRulFile.isFile() ) &&
                          ( bindRulFile.canRead() ) ) ) {
                System.out.println( "\"" + bindRulFile + "\" is not a readable file." );
            }
            if ( outputDir == null ) {
                System.out.println( "You must specify an output directory." );
            }
            else if ( ! ( outputDir.isDirectory() ) ) {
                System.out.println( "\"" + outputDir + "\" is not a directory." );
            }
            usage();
        }
    }
}
