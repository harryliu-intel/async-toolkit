/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.ipgen;


import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;
import java.util.LinkedHashSet;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStream;
import java.io.Writer;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;

import com.avlsi.cell.CellInterface;

import com.avlsi.cast.CastFileParser;

import com.avlsi.tools.cadencize.Cadencize;

import com.avlsi.tools.cosim.spec.CoSim;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.SplittingFactory;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.BindRulNameInterfaceFactory;

import com.avlsi.io.SearchPath;
import com.avlsi.io.FileSearchPath;

import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.AssuraBindRulTableEmitterFactory;
import com.avlsi.layout.gdsII.SkillTableEmitterFactory;
import com.avlsi.layout.gdsII.SplittingTableEmitterFactory;
import com.avlsi.layout.gdsII.FilterInternalNodesTableEmitterFactory;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.impl.simple.SimpleNetlistFactory;
import com.avlsi.netlist.util.FlatteningIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.tools.jauto.Cast2Cdl;
import com.avlsi.tools.prs2verilog.Prs2Verilog;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;

import com.avlsi.tools.ipgen.RenamingVerilogFactory;
import com.avlsi.tools.ipgen.PMCNameInterfaceFactory;


/**
   Calls cast2cdl and prs2verilog to generate
   skill name table files, cdl, flattened strength report, and verilog models
   for a specified cell.
 */
public class GenerateIPData {


    private static final class PRS2VerilogRun {

        private final String mOption;

        private final File mOutputDir;

        private final CastFileParser cfp;

        public PRS2VerilogRun( final String option,
                               final File outputDir,
                               final CastFileParser cfp ) {
            mOption = option ;
            mOutputDir = outputDir;
            this.cfp = cfp;
            
        }

        public boolean isValid() {
            return ( ! mOutputDir.isDirectory() ) &&
                mOutputDir.getParentFile().canWrite();
        }

        public void run( final CellInterface ci,
                         final VerilogFactoryInterface verilogFactory,
                         final CDLNameInterfaceFactory namerFactory) 
            throws IOException, CDLRenameException {

            final String[] prs2VerilogArgs =
                mOption != null && mOption.length() > 0 ?
                    new String[] { mOption } : new String[0];

            final CommandLineArgs parsedPRS2VerilogArgs =
                new CommandLineArgsDefImpl( prs2VerilogArgs );
            final CommandLineArgs argsWithConfigs =
                new CommandLineArgsWithConfigFiles( parsedPRS2VerilogArgs ); 
            final CommandLineArgs cachedPRS2VerilogArgs = 
                new CachingCommandLineArgs( argsWithConfigs );
            
            
            final Collection filelist = new ArrayList();
            final CDLNameInterface ni =
                namerFactory.getNameInterface(ci.getFullyQualifiedType());
            final Prs2Verilog.VisitorFactory verilogVisitorFactory = 
                new Prs2Verilog.MultipleFileVisitor(mOutputDir, null, filelist,
                                                    true, ni, false);

            final String spec = cachedPRS2VerilogArgs.getArgValue("spec", null);
            final CoSim cosim;
            if (spec == null) {
                cosim = null;
            } else {
                cosim = CoSim.getCoSim(ci.getFullyQualifiedType() + "{" +
                                       spec + "}", true);
            }
            
            final Map dependencies =
                Prs2Verilog.writeVerilog( ci, 
                                          verilogVisitorFactory,
                                          cfp,
                                          cachedPRS2VerilogArgs,
                                          verilogFactory,
                                          "netgraph",
                                          false,
                                          cosim);

            final Set goodfiles = new LinkedHashSet();
            Prs2Verilog.verilogFiles(dependencies, goodfiles);

            final FileWriter list =
                new FileWriter(new File(mOutputDir, "autogen-list"));
            for (Iterator i = filelist.iterator(); i.hasNext(); ) {
                list.write(i.next() + "\n");
            }
            list.close();

            final FileWriter verilog =
                new FileWriter(new File(mOutputDir, "verilog-list"));
            for (Iterator i = goodfiles.iterator(); i.hasNext(); ) {
                verilog.write(i.next() + "\n");
            }
            verilog.close();
        }

    }

    /*
      This class is used to prevent the PMCNameInterface from generating names
      that would collide with names generated from the mappings in the user specified bind.rul file.
     */
    private static final class ExistingNames implements PMCNameInterfaceFactory.ExistingNamesInterface {
    
        private BindRulNameInterfaceFactory mBindRulFactory;

        private Set mExistingCellNames;

        public ExistingNames() {
            mBindRulFactory = null;
            mExistingCellNames = null;
        }

        public void setBindRulFactory( final BindRulNameInterfaceFactory factory ){
            mBindRulFactory = factory;
            
            mExistingCellNames = new HashSet();

            final Set cellNamesFromFactory = factory.getNewCellNames();

            processNames( cellNamesFromFactory, mExistingCellNames );
        }

        
        private static void processNames( final Set srcSet, final Set targetSet ) {
            final Iterator nameIter = srcSet.iterator();
            
            while ( nameIter.hasNext() ) {
                final String currName = ( String ) nameIter.next();
                /* If name begins with an 'f' and ends with an 'm', strip the 'f' and 'm' off
                   because the PMC name interface will put them on names it creates.
                   It puts in the f and the m after it checks for collisions.  */
                if ( ( currName.charAt( 0 ) == 'f' ) && 
                     ( currName.charAt( currName.length() - 1 ) == 'm' ) ) {
                    targetSet.add( currName.substring( 1, currName.length() - 1 ) );
                }
                targetSet.add( currName );
            }
        }

        public Set getExistingTranslatedNamesForCell( final String cellName ) {
            final Set nodeNamesFromFactory = 
                mBindRulFactory.getNewNodeNamesForOldCellName( cellName );
            final Set instanceNamesFromFactory = 
                mBindRulFactory.getNewInstanceNamesForOldCellName( cellName );
            final Set ret = new HashSet( mExistingCellNames );
            processNames( nodeNamesFromFactory, ret );
            processNames( instanceNamesFromFactory, ret );
            return ret;
        }

    }

    private static final class FlatInstances {
    
        private final Map mCellsMap;

        public FlatInstances( ) {
            mCellsMap = new HashMap();
        }

        public final void addInstanceOfCell( final FlatteningIterator.FlatInstance instance  ) {
            final String masterNetlistName =
                instance.getInstanceMasterNetlist().getName().toString();
            final List existingInstances = getInstances( masterNetlistName );
            final List instances;
            if ( existingInstances == null ) {
                instances = new LinkedList();
                mCellsMap.put( masterNetlistName, instances );
            }
            else {
                instances = existingInstances;
            }
            instances.add( instance );
        }

        public final List getInstances( final String cellName ) {
            return ( List ) mCellsMap.get( cellName );
        }
        
    }

    private static FlatInstances getFlatInstancesOfCells( final AbstractNetlist top ) {
        final FlatInstances ret = new FlatInstances();
        final FlatteningIterator iter = new FlatteningIterator( top );

        while ( iter.hasNext() ) {
            final FlatteningIterator.FlatInstance currInstance =
                iter.next();
            ret.addInstanceOfCell( currInstance );
        }
        return ret;
    }


    private static void flattenStrengthReport( final HashMapTableEmitterFactory hashTableEmitterFactory,
                                               final AbstractNetlist top,
                                               final Reader reader,
                                               final Writer output,
                                               final String hierarchySeperator ) 
        throws IOException 
    {    
        final FlatInstances flatInstances = getFlatInstancesOfCells( top );
        final BufferedReader bReader = new BufferedReader( reader );

        
        List instancesOfCurrCell = null;
        HashMapTableEmitter currTableEmitter = null;

        String currLine = bReader.readLine();
        
        while ( currLine != null ) {
            final String[] tokens = currLine.split( "\\p{Space}+" );
            if ( tokens.length > 0 ) {
                int startIndex = 0;
                while ( ( startIndex < tokens.length ) &&
                        ( tokens[ startIndex ].length() == 0 ) ) {
                    ++startIndex;
                }
                if ( startIndex < tokens.length ) {
                    if ( ( tokens[startIndex].equals( "CELL" ) ) && 
                         ( tokens.length == ( startIndex + 2 ) ) ) {
                        final String cellCastName = tokens[startIndex + 1];
                        
                        final String currCellGDSIIName = 
                            hashTableEmitterFactory.getGDSIICellNameForCastCellName( cellCastName );
                        
                        instancesOfCurrCell = flatInstances.getInstances( currCellGDSIIName );
                        currTableEmitter = hashTableEmitterFactory.getEmitterForCell( cellCastName );
                    }
                    else if ( ( tokens[startIndex].equals( "STRENGTH" ) ) && 
                              ( tokens.length == ( startIndex + 4 ) ) ) {
                        final String castNodeName = tokens[startIndex + 1];
                        final String direction = tokens[startIndex + 2];
                        final String strength = tokens [startIndex + 3];
                        
                        final String gdsIINodeName = currTableEmitter.getGDSIINodeNameForCastNodeName( castNodeName );
                        
                        final Iterator instIter = instancesOfCurrCell.iterator();
                        while ( instIter.hasNext() ) {
                            final FlatteningIterator.FlatInstance currInst = 
                                ( FlatteningIterator.FlatInstance ) instIter.next();
                            
                            final String flatNodeName = currInst.getCanonicalNodeName( gdsIINodeName,
                                                                                       hierarchySeperator );
                            
                            output.write( flatNodeName + " " + direction + " " + strength + "\n" );
                        }
                    }
                    else {
                        System.out.println( "Ignoring: \"" + currLine + "\" \"" + tokens[0] + "\"");
                    }
                }
            }
            currLine = bReader.readLine();
        }
    }

    private static void usage() {
        final String className = GenerateIPData.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className );
        System.out.println("\t--cast-path=<cast-path> (defaults to .)");
        System.out.println("\t--cast-version=<version> (defaults to 2)");
        System.out.println("\t--cell=<cell> (name of cell to process)");
        System.out.println("\t--gdsII-name-interface=fulcrum | --gdsII-name-interface=pmc");
        System.out.println("\t--bind-rul=file" );
        System.out.println("\t--prs2verilog=file" );
        System.out.println("\t--strength-report=file" );
        System.out.println("\t--output-dir=<file>");
    }

    public static List parsePRS2VerilogOption( final String prs2VerilogArg, final File outputDir, final CastFileParser cfp ) {
        final String[] parts = prs2VerilogArg.split( ":" );
        final List result  = new LinkedList();
        if ( ( parts.length % 2 ) == 0 ) {
            int index;
            for ( index = 0 ; index < ( parts.length / 2 ) ; ++index ) {
                final int base = index * 2 ;
                final String testBenchDir = parts[ base ];
                final String prs2VerilogOption = parts[ base + 1 ];
                
                final File voutDir = new File( outputDir, testBenchDir );
                
                final PRS2VerilogRun currRun = new PRS2VerilogRun( prs2VerilogOption,
                                                                   voutDir,
                                                                   cfp );
                if ( currRun.isValid() ) {
                    result.add( currRun );
                }
            }
        }
        return result;
    }

    public static void main( String args[] ) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);

        
        final String bindRulFileName = theArgs.getArgValue( "bind-rul", null );
        final File bindRulFile;
        if ( bindRulFileName != null ) {
            bindRulFile = new File( bindRulFileName );
        }
        else {
            bindRulFile = null ;
        }
        
        final String outputDirName = theArgs.getArgValue( "output-dir", null );
        final File outputDir;
        if ( outputDirName != null ) {
            outputDir = new File( outputDirName );
        }
        else {
            outputDir = null ;
        }
        
        if ( ( outputDir != null ) && 
             ( ! ( outputDir.exists() ) ) ) {
            outputDir.mkdirs();
        }

        final String gdsNameInterfaceName = theArgs.getArgValue( "gdsII-name-interface", "fulcrum" );

        final String strengthReportFileName = theArgs.getArgValue( "strength-report", null );
        final File strengthReportFile;

        if ( strengthReportFileName != null ) {
            strengthReportFile = new File( strengthReportFileName );
        }
        else {
            strengthReportFile = null;
        }
        
        final CastFileParser castParser =
            new CastFileParser( new FileSearchPath(castRoot), castVersion );

        final List prs2VerilogRuns = parsePRS2VerilogOption( theArgs.getArgValue( "prs2verilog", "" ), 
                                                             outputDir,
                                                             castParser );
        
        if ( ( cellName != null ) &&
             ( bindRulFile != null ) &&
             ( bindRulFile.isFile() ) &&
             ( bindRulFile.canRead() ) &&
             ( outputDir != null ) &&
             ( outputDir.isDirectory() ) &&
             ( gdsNameInterfaceName != null ) &&
             ( strengthReportFile != null ) &&
             ( strengthReportFile.isFile() ) &&
             ( strengthReportFile.canRead() ) ) {

            final Cadencize cadencizer = new Cadencize( true );


            final CDLNameInterfaceFactory gdsIINameInterfaceFactory;

            ExistingNames existingNames = null;
            
            if ( gdsNameInterfaceName.equals( "fulcrum" ) ) {
                final CDLNameInterface fulcrumGDSIINameInterface = new GDS2NameInterface();
                gdsIINameInterfaceFactory = 
                    new TrivialCDLNameInterfaceFactory( fulcrumGDSIINameInterface );
            }
            else if ( gdsNameInterfaceName.equals( "pmc" ) ) {
                existingNames = new ExistingNames();
                gdsIINameInterfaceFactory = new PMCNameInterfaceFactory( castParser,
                                                                         cadencizer,
                                                                         existingNames );
                
            }
            else {
                gdsIINameInterfaceFactory = null;
            }

            if ( gdsIINameInterfaceFactory != null ) {

                final CellInterface ci = castParser.getFullyQualifiedCell( cellName );
            
                final File skillOutputDir = new File( outputDir, "ilnames" );
                if ( ! ( skillOutputDir.exists() ) ) {
                    skillOutputDir.mkdirs();
                }

                final TableEmitterFactoryInterface skillEmitterFactory =
                    new SkillTableEmitterFactory( skillOutputDir,
                                                  "CadenceNodeToGDSIINodeTable",
                                                  "CadenceCellToGDSIICellTable",
                                                  "CadenceInstanceToGDSIIInstanceTable" );

                cadencizer.convert( ci );

                final File outputBindRul = new File( outputDir, "bind.rul" );
                final TableEmitterFactoryInterface assuraEmitterFactory =
                    new AssuraBindRulTableEmitterFactory( outputBindRul,
                                                          bindRulFile );

                final TableEmitterFactoryInterface filteredAssuraEmitterFactory =
                    new FilterInternalNodesTableEmitterFactory( cadencizer,
                                                                assuraEmitterFactory );

                final TableEmitterFactoryInterface splitterFactory1 =
                    new SplittingTableEmitterFactory( skillEmitterFactory, 
                                                      filteredAssuraEmitterFactory );
                
                //Gets used to rename nodes in the strength report.
                final HashMapTableEmitterFactory hashTableEmitterFactory =
                    new HashMapTableEmitterFactory();
                
                final TableEmitterFactoryInterface splitterFactory2 =
                    new SplittingTableEmitterFactory( splitterFactory1, 
                                                      hashTableEmitterFactory );
            
                final InputStream inputBindRulStream = new FileInputStream( bindRulFile );
                final Reader inputBindRulReader =
                    new BufferedReader( new InputStreamReader( inputBindRulStream, "UTF-8" ) );
                
                final BindRulNameInterfaceFactory bindRulNameInterfaceFactory =
                    new BindRulNameInterfaceFactory( inputBindRulReader,
                                                     gdsIINameInterfaceFactory );
                
                if ( existingNames != null ) {
                    existingNames.setBindRulFactory( bindRulNameInterfaceFactory );
                }

                final CDLNameInterface cadenceNameInterface = new CadenceNameInterface();
                final CDLNameInterfaceFactory cadenceNameInterfaceFactory =
                    new TrivialCDLNameInterfaceFactory( cadenceNameInterface );

                final TablifyingNameInterfaceFactory nameInterfaceFactory =
                    new TablifyingNameInterfaceFactory( splitterFactory2,
                                                        cadenceNameInterfaceFactory,
                                                        bindRulNameInterfaceFactory );

                final File outputCDLFile = new File( outputDir, "gdsII.cdl" );
                final OutputStream outputCDLStream = new FileOutputStream( outputCDLFile );
                final Writer outputCDLWriter = new BufferedWriter( new OutputStreamWriter( outputCDLStream,
                                                                                           "UTF-8" ) );

                final CDLFactoryInterface cdlEmitter =
                    new CDLFactoryEmitter( outputCDLWriter, true, 76, true,
                                           false, "" );

                final SimpleNetlistFactory netlistFactory = new SimpleNetlistFactory();

                final CDLFactoryInterface splittingFactory = new SplittingFactory( cdlEmitter,  
                                                                                   netlistFactory );

                final CDLFactoryInterface cdlRenamer = new CDLRenameFactory( splittingFactory,
                                                                             nameInterfaceFactory );
                                                                                   
                final CDLFactoryInterface trueNames =
                    new Cast2Cdl.RealTransistorNames(cdlRenamer, castParser);

                Cast2Cdl.outputCDL(ci, castParser, trueNames, cadencizer, false, false);
                outputCDLWriter.close();
                nameInterfaceFactory.close();
                
                final CDLNameInterfaceFactory leftOverFactory;

                if ( gdsNameInterfaceName.equals( "fulcrum" ) ) {
                    final CDLNameInterface fulcrumGDSIINameInterface = new GDS2NameInterface();
                    leftOverFactory = 
                        new TrivialCDLNameInterfaceFactory( fulcrumGDSIINameInterface );
                }
                else if ( gdsNameInterfaceName.equals( "pmc" ) ) {
                    final PMCNameInterfaceFactory.ExistingNamesInterface exist =
                        new PMCNameInterfaceFactory.ExistingNamesInterface() {
                            public Set getExistingTranslatedNamesForCell(
                                    final String cellName ) {
                                final HashMapTableEmitter emitter =
                                    hashTableEmitterFactory.getEmitterForCell( cellName );
                                if (emitter == null)
                                    return Collections.EMPTY_SET;
                                final HashSet result = new HashSet();
                                result.addAll(emitter.getGDSCellNames());
                                result.addAll(emitter.getGDSNodeNames());
                                result.addAll(emitter.getGDSInstanceNames());
                                return result;
                            }
                        };
                    existingNames = new ExistingNames();
                    leftOverFactory = new PMCNameInterfaceFactory( castParser,
                                                                   cadencizer,
                                                                   exist );
                    
                }
                else {
                    leftOverFactory = null;
                }
                

                final VerilogFactoryInterface verilogFactory = 
                    new RenamingVerilogFactory( hashTableEmitterFactory,
                                                leftOverFactory );

                final Iterator prs2VerilogIter = prs2VerilogRuns.iterator();

                while ( prs2VerilogIter.hasNext() ) {
                    final PRS2VerilogRun curr = ( PRS2VerilogRun ) prs2VerilogIter.next();
                    curr.run( ci, verilogFactory, bindRulNameInterfaceFactory );
                }


                final String gdsIINameOfTopCell = 
                    hashTableEmitterFactory.getGDSIICellNameForCastCellName( cellName );

                final AbstractNetlist top = netlistFactory.getNetlist( gdsIINameOfTopCell );
                
                if ( top != null ) {

                    final InputStream strengthReportStream = new FileInputStream( strengthReportFile );
                    final Reader strengthReportReader = new InputStreamReader( strengthReportStream, "UTF-8" );

                    final File outputStrengthReportFile = new File( outputDir, cellName + ".strength.report.txt" );
                    final OutputStream outputStrengthReportStream = new FileOutputStream( outputStrengthReportFile );
                    final Writer outputStrengthReportWriter =
                        new BufferedWriter( new OutputStreamWriter( outputStrengthReportStream ) );

                    flattenStrengthReport( hashTableEmitterFactory,
                                           top,
                                           strengthReportReader,
                                           outputStrengthReportWriter,
                                           "." );

                    outputStrengthReportWriter.close();
                    strengthReportReader.close();

                }
            }
        }
        else {
            if ( cellName == null ) {
                System.out.println( "You must specify a cast cell name." );
            }
            if ( outputDir == null ) {
                System.out.println( "You must specify an output directory." );
            }
            else if ( ! ( outputDir.isDirectory() ) ) {
                System.out.println( "\"" + outputDir + "\" is not a directory." );
            }
            if ( bindRulFile == null ) {
                System.out.println( "You must specify an initial bind.rul file." );
            }
            else if ( ! ( ( bindRulFile.isFile() ) && ( bindRulFile.canRead() ) ) ) {
                System.out.println( "\"" + bindRulFile + "\" is not a readable file." );
            }
            if ( strengthReportFile == null ) {
                System.out.println( "You must specify a strength report file." );
            }
            else if ( ! ( ( strengthReportFile.isFile() ) && ( strengthReportFile.canRead() ) ) ) {
                System.out.println( "\"" + strengthReportFile + "\" is not a readable file." );
            }
            usage();
        }
    }

}
