/*
 * Copyright 2002-2009 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cast2skill;




import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.HashSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Collection;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import java.io.File;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.Writer;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.FileNotFoundException;

import java.text.MessageFormat;

import com.avlsi.io.FileSearchPath;
import com.avlsi.io.FileUtil;

import com.avlsi.util.container.SortingIterator;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;

import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.ports.PortDefinition;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;

import com.avlsi.cast.impl.Environment;

import com.avlsi.cast2.directive.DirectiveConstants;

import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;

import com.avlsi.layout.CellProcessor;
import com.avlsi.layout.CellProcessorInterface;
import com.avlsi.layout.CellProcessorException;
import com.avlsi.layout.CellDirs2Skill;
import com.avlsi.layout.AutoPins;
import com.avlsi.layout.PinPlaceException;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLstat;
import com.avlsi.file.cdl.parser.SplittingFactory;
import com.avlsi.file.cdl.parser.CellSetFactory;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.CDLLexer;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;

import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;

import com.avlsi.tools.cdl2skill.CDL2SkillFactory;

import com.avlsi.tools.dsim.ExceptionPrettyPrinter;

import com.avlsi.tools.ipgen.HashMapTableEmitterFactory;
import com.avlsi.tools.ipgen.TablifyingNameInterfaceFactory;

import com.avlsi.tools.jauto.Cast2Cdl;
import com.avlsi.tools.jauto.Floorplan;
import com.avlsi.tools.jauto.TechnologyData;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.tools.prs2verilog.GeneratePortMapping;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;

public final class Cast2Skill {

    private interface CellProcessorFactory {       
        CellProcessorInterface getCellProcessor( final CellInterface ci );
    }


    private static class RoutedCastParser {
        private final CastFileParser castParser;
        private final boolean routed;
        private final boolean rootOnly;
        private final Map<String,CellInterface> cache;
        public RoutedCastParser(final CastFileParser castParser,
                                final boolean routed,
                                final boolean rootOnly) {
            this.castParser = castParser;
            this.routed = routed;
            this.rootOnly = rootOnly;
            this.cache = new HashMap<String,CellInterface>();
        }
        public CellInterface getFullyQualifiedCell(final String name)
            throws CastSemanticException {
            final CellInterface candidate =
                castParser.getFullyQualifiedCell(name);
            if (routed) {
                final String type = candidate.getFullyQualifiedType();
                CellInterface cached = cache.get(type);
                if (cached == null) {
                    cached = candidate.routedSubcells(!rootOnly);
                    cache.put(type, cached);
                }
                return cached;
            } else {
                return candidate;
            }
        }
    }

    private static void processCells( final CellProcessorFactory cellProcessorFactory,
                                      final RoutedCastParser castParser,
                                      final Collection castCellNameCollection,
                                      final HashMapTableEmitterFactory castNameToCadenceNameMap,
                                      final File outputDir,
                                      final String fileNameSuffix ) throws Exception {

        final Iterator cellNameIter = castCellNameCollection.iterator();

        while ( cellNameIter.hasNext() ) {
            
            final String currCellName = ( String ) cellNameIter.next();
            
            final String cadenceCellName = castNameToCadenceNameMap.getCadenceCellNameForCastCellName( currCellName );
            assert cadenceCellName != null ;

            final File currOutputFile = new File( outputDir, cadenceCellName + fileNameSuffix );

            final OutputStream currOutputStream = new FileOutputStream( currOutputFile );

            final Writer currOutputWriter = 
                new BufferedWriter( new OutputStreamWriter( currOutputStream, "UTF-8" ) );

            final CellInterface currCell = castParser.getFullyQualifiedCell( currCellName );
 
            final CellProcessorInterface currProcessor = cellProcessorFactory.getCellProcessor( currCell );

            System.out.println( "Generating \"" + currOutputFile.getAbsolutePath() + "\"." );
            
            try {
                currProcessor.process( currOutputWriter );
            } 
            catch( CellProcessorException e ) {
                System.out.println( e ); 
            }

            currOutputWriter.close();
            
        }

    }

    private static void usage( ) {
        final String className = Cast2Skill.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "    --cast-path=<cast-path> (defaults to .)\n" +
                            "    --cast-version=<version> (defaults to 2)\n" +
                            "    --cell=<cell> (name of cell to process)\n" +
                            "    | --cells=<file> (containing list of cells to process)\n" +
                            "    --output-dir=<file>\n" +
                            "    [ --cadence-name ]\n" +
                            "    [ --cell-height=num ]\n" +
                            "    [ --do-file-template=file --uu-per-meter=num ]\n" +
                            "    [ --ignore-netlist ]\n" +
                            "    [ --instances-dir=dir ]\n" +
                            "    [ --library-list=file ]\n" +
                            "    [ --pin-type=pitched|inplace ]\n" +
                            "    [ --root-only ]\n" +
                            "    [ --suppress-directives ]\n" +
                            "    [ --suppress-pins ]");
    }

    public static Set filterGatesAndStacks( final Set srcCellSet,
                                            final RoutedCastParser castParser ) 
    throws CastSemanticException {

        final Set ret = new HashSet();

        final Iterator cellIter = srcCellSet.iterator();

        while ( cellIter.hasNext() ) {
            final String cellName = ( String ) cellIter.next();

            final CellInterface cell = castParser.getFullyQualifiedCell( cellName );

            final Boolean isNetlistPrimitive = ( Boolean )
                DirectiveUtils.getTopLevelDirective( cell, "netlist_primitive" );

            if ( ( isNetlistPrimitive == null ) ||
                 ( ! ( isNetlistPrimitive.booleanValue() ) ) ) {
                ret.add( cellName );
            }
        }
        return ret;
    }
        

    public static void writeStringCollectionToFile( final File targetFile,
                                                    final Collection stringCollection ) 
    throws IOException 
    {
        final OutputStream targetStream = new FileOutputStream( targetFile );
        final Writer targetWriter =
            new BufferedWriter( new OutputStreamWriter( targetStream, "UTF-8" ) );

        final Iterator strIter = stringCollection.iterator();

        while( strIter.hasNext() ) {
            final String str = ( String ) strIter.next();
            targetWriter.write( str + "\n" );
        }
        targetWriter.close();
    }

    private static HierName toHier( final String s ) {
        final HierName hs;
        try {
            hs = HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot create HierName: " + s, e);
        }
        return hs;
    }

    public static void main( final String args[] ) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final String castPath = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final List cellsToProcess = new ArrayList();
        if(theArgs.argExists("cell") ) {
            final String givenCellName = theArgs.getArgValue("cell", null);
            cellsToProcess.add(givenCellName);
        } else {
            if(!theArgs.argExists("cells") ) {
                System.err.println("Must specify --cell or --cells");
                usage();
                System.exit(1);
            }
            final String cellsFileName = theArgs.getArgValue("cells", null);
            final File cellsFile = new File(cellsFileName);
            cellsToProcess.addAll(FileUtil.getLines(cellsFile));
        }

        final TechnologyData techData;
        try {
            techData = TechnologyData.getTechnologyData(theArgs);
        } catch (Exception e) {
            System.err.println("Can't load technology data: " +
                               e.getMessage());
            System.exit(1);
            return;
        }

        final CadenceReverseNameInterface crni =
            new CadenceReverseNameInterface();
        if( theArgs.argExists( "cadence-name" ) ) {
            for(int i=0;i<cellsToProcess.size();i++) {
                final String c = (String)cellsToProcess.get(i);
                cellsToProcess.set(i,crni.renameCell(c));
            }
        }
        
        final boolean rootOnly = theArgs.argExists( "root-only" );
        final boolean ignoreNetlist = theArgs.argExists( "ignore-netlist" );
        final boolean routed = theArgs.argExists("routed");
        
        final String outputDirName = theArgs.getArgValue( "output-dir", null );
        final String skillFunctionPrefix = theArgs.getArgValue( "function-prefix", "NetlistTable" );
        final String skillTableName = theArgs.getArgValue( "table-name", "NetListTable" );
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
 
        if ( ( outputDir != null ) && ( outputDir.isDirectory() ) ) try {
            final Cadencize cadencizer =
                new Cadencize( false, Cadencize.VERILOG_PARTIAL );
            
            final CastFileParser realParser =
                new CastFileParser( new FileSearchPath(castPath), castVersion,
                                    new StandardParsingOption(theArgs));
            final RoutedCastParser castParser =
                new RoutedCastParser(realParser, routed, rootOnly);
            
            final File skillNetlistOutputDir = new File( outputDir, "ilnets" );
            if ( ! ( skillNetlistOutputDir.exists() ) ) {
                skillNetlistOutputDir.mkdirs();
            }
            
            final Map cellTemplateMap = new HashMap();
            final Template templateFactory = new Template( cellTemplateMap, "" );
            
            
            final Map cellStatMap = new HashMap();
            final Set libSet = new HashSet();
            final List cadenceCellNameList = new LinkedList();
            final String sizeEstimateFormatStr =
                "( " + skillFunctionPrefix + "SetCellWidth\n" +
                "  " + skillTableName + "\n" +
                "  {0,number,0.0000E0} )\n" +
                "( " + skillFunctionPrefix + "SetCellHeight\n" +
                "  " + skillTableName + "\n" +
                "  {1,number,0.0000E0} )\n";
            final MessageFormat sizeEstimateFormatter =
                new MessageFormat( sizeEstimateFormatStr );
            final Set<String> sizeError = new HashSet<String>();
            final Set<String> portDirError = new HashSet<>();
            final CDL2SkillFactory skillFactory =
                new CDL2SkillFactory( skillNetlistOutputDir,
                                      skillFunctionPrefix,
                                      skillTableName,
                                      libSet,
                                      cadenceCellNameList,
                                      cellTemplateMap,
                                      cellStatMap,
                                      techData ) {
                    final Map<String,Map<String,Integer>> directions =
                        new HashMap<>();
                    protected String makeStatisticsString(
                            final String subckt,
                            final CDLstat.CellStat stat ) {
                        final StringBuffer sb =
                            new StringBuffer(super.makeStatisticsString( subckt, stat ) );
                        final String castName;
                        try {
                            castName = crni.renameCell( subckt );
                        } catch (Exception e) {
                            throw new AssertionError(
                                    "Can't rename: " + subckt );
                        }

                        try {
                            final CDLstat.EstimatedSize estimate =
                                CDLstat.getEstimatedSize(
                                        castParser.getFullyQualifiedCell(
                                            castName ),
                                        stat );
                            sizeEstimateFormatter.format(
                                    new Object[] { estimate.width,
                                                   estimate.height },
                                    sb,
                                    null );
                        } catch (Exception e) {
                            sizeError.add(castName + ": " + e.getMessage());
                        }
                        
                        return sb.toString();
                    }
                    private void accumDir( final Map<String,Integer> map,
                                           final HierName canon,
                                           final Integer dir ) {
                        final String s = canon.getAsString( '.' );
                        Integer prevDir = map.get( s );
                        if ( prevDir == null ) {
                            map.put( s, dir );
                        } else if ( ! prevDir.equals( dir ) ) {
                            map.put( s, PortDefinition.INOUT );
                        }
                    }
                    private Map<String,Integer> getDirection( final String subckt )
                        throws Exception {
                        Map<String,Integer> result = directions.get(subckt);
                        if ( result == null ) {
                            result = new HashMap<>();
                            final CellInterface cell =
                                castParser.getFullyQualifiedCell(
                                    crni.renameCell( subckt ) );
                            final Map<String,Integer> mp = CellUtils.markPorts(cell);
                            final AliasedMap ports = cadencizer.convert(cell)
                                                               .getPortNodes();
                            for (String port : mp.keySet()) {
                                final HierName canon = (HierName)
                                    ports.getCanonicalKey( toHier( port ) );
                                accumDir( result, canon, mp.get(port) );
                            }
                            directions.put( subckt, result );
                        }
                        return result;
                    }
                    protected String getDirection( final String subckt,
                                                   final String terminalName ) {

                        String result = super.getDirection( subckt, terminalName );
                        try {
                            final Map<String,Integer> directions =
                                getDirection( subckt );
                            final int dir =
                                directions.get( crni.renameNode( terminalName ) );
                            switch ( dir ) {
                              case PortDefinition.IN:
                                result = "input";
                                break;
                              case PortDefinition.OUT:
                                result = "output";
                                break;
                              default:
                                result = "inputOutput";
                                break;
                            }
                        } catch (Exception e) {
                            portDirError.add(subckt + ": " + terminalName + ": " +
                                             e.getMessage());
                        }
                        return result;
                    }
                };
            
            final CDLNameInterface cadenceNameInterface = 
                new CadenceNameInterface( );

            final CDLNameInterfaceFactory cadenceNameInterfaceFactory =
                new TrivialCDLNameInterfaceFactory( cadenceNameInterface );

            final HashMapTableEmitterFactory hashTableEmitterFactory =
                new HashMapTableEmitterFactory();

            final TablifyingNameInterfaceFactory nameInterfaceFactory  =
                    new TablifyingNameInterfaceFactory( hashTableEmitterFactory,
                                                        cadenceNameInterfaceFactory,
                                                        cadenceNameInterfaceFactory );

            final CDLFactoryInterface factory1 = 
                new SplittingFactory( templateFactory,
                                      skillFactory );
            
            final Set allCellsSet = new HashSet();
            final CDLFactoryInterface cellSetFactory = 
                new CellSetFactory( allCellsSet );

            final CDLFactoryInterface renamingFactory =
                new CDLRenameFactory( factory1, nameInterfaceFactory );

            final CDLFactoryInterface factory2 = 
                new SplittingFactory( renamingFactory, cellSetFactory );

            final CDLFactoryInterface trueNamesEmitter =
                new Cast2Cdl.RealTransistorNames(factory2, realParser);

            //XXX - This will duplicate output of subcells
            for(Iterator i=cellsToProcess.iterator();i.hasNext();) {
                final String c = (String)i.next();
                final CellInterface ci = 
                    castParser.getFullyQualifiedCell( c );

            Cast2Cdl.outputCDL(ci, 
                               realParser,
                               trueNamesEmitter,
                               cadencizer,
                               false,
                               false,
                               true,
                               ! ignoreNetlist,
                               true);
            }

            if ( ! ignoreNetlist && skillFactory.getError() != null ) {
                throw new RuntimeException( skillFactory.getError() );
            }

            if ( ! ignoreNetlist && ( ! sizeError.isEmpty() ||
                                      ! portDirError.isEmpty() ) ) {
                if ( ! sizeError.isEmpty() ) {
                    System.err.println(
                            "Error estimating geometry of the following cell: " );
                    for ( String err : sizeError ) {
                        System.err.println( "\t" + err );
                    }
                }
                if ( ! portDirError.isEmpty() ) {
                    System.err.println(
                            "Error getting port directionality: " );
                    for ( String err : portDirError ) {
                        System.err.println( "\t" + err );
                    }
                }
                System.exit( 1 );
            }

            final File cadenceCellNameListFile = 
                new File( outputDir, "cellnames.txt" );

            writeStringCollectionToFile( cadenceCellNameListFile, 
                                         cadenceCellNameList );
            
            final File cadenceLibListFile = 
                new File( outputDir, "libnames.txt" );
            
            writeStringCollectionToFile( cadenceLibListFile, libSet );

            final Set cellSet;
            if ( rootOnly ) {
                cellSet = new HashSet();
                cellSet.addAll( cellsToProcess );
            }
            else {
                cellSet = filterGatesAndStacks( allCellsSet,
                                                castParser );   
            }
            
            if ( ! ( theArgs.argExists( "suppress-directives" ) ) ) {

                final File skillDirectivesOutputDir = new File( outputDir, "ildirectives" );
                if ( ! ( skillDirectivesOutputDir.exists() ) ) {
                    skillDirectivesOutputDir.mkdirs();
                }
                
                final boolean suppress =
                    theArgs.argExists( "suppress-wiring-directives" );

                final CellProcessorFactory directivesCellProcessorFactory =
                    new CellProcessorFactory () {
                        public CellProcessorInterface getCellProcessor( final CellInterface ci ) {
                            if (!suppress) {
                                //prepare cell by propagating wire directives
                                DirectiveUtils.propagateWireDirective(
                                        ci, cadencizer );
                            }
                            return new CellDirs2Skill( ci, suppress );
                        }
                    };
                
                processCells( directivesCellProcessorFactory,
                              castParser,
                              cellSet,
                              hashTableEmitterFactory,
                              skillDirectivesOutputDir,
                              ".directives.il" );
            }

            if ( ! ( theArgs.argExists( "suppress-pins" ) ) ) {
                
                final File autoPinsDir = new File( outputDir, "autopins" );
                if ( ! ( autoPinsDir.exists() ) ) {
                    autoPinsDir.mkdirs();
                }
                
                String heightString = theArgs.getArgValue( "cell-height", null );
                float height = 0;
                if ( heightString != null ) {
                    try {
                        height = Float.parseFloat( heightString );
                    }
                    catch ( NumberFormatException e ) {
                        System.out.println( "\"" + heightString + "\" is not a valid number." );
                        heightString = null;
                        height = 0;
                    }
                }
                final float cellHeight = height;
                final String pinType = theArgs.getArgValue( "pin-type", null );
                
                final CellProcessorFactory pinsCellProcessorFactory =
                    new CellProcessorFactory () {
                        public CellProcessorInterface getCellProcessor( final CellInterface ci ) {
                            try {
                                return new AutoPins( ci, cadencizer, pinType,
                                                     cellHeight );
                            } catch(PinPlaceException e) {
                                throw new RuntimeException(e);
                            }
                        }
                    };
                
                /* BUG: avoid AutoPins because it doesn't work in 14nm
                processCells( pinsCellProcessorFactory,
                              castParser,
                              cellSet,
                              hashTableEmitterFactory,
                              autoPinsDir,
                              ".pins.il" );
                */
            }

            final String verilogBlock = theArgs.getArgValue( "verilog-block", "rtl");
            final CDLNameInterface ni = new CadenceNameInterface();

            for(int c=0;c<cellsToProcess.size();c++) {
                final String cellName = (String)cellsToProcess.get(c);
                Map map = null;
                try {
                    map = com.avlsi.tools.prs2verilog.GeneratePortMapping.generateMapping( castParser.getFullyQualifiedCell(cellName), cellName, verilogBlock, null, cadencizer);
                } catch (Exception e) {}

                if (map != null) {
                    String cadenceCellName = hashTableEmitterFactory.getCadenceCellNameForCastCellName( cellName );
                    if (cadenceCellName == null && ni != null) {
                        cadenceCellName = ni.renameCell(cellName);
                    }
                    map.put("Vss", "GND");
                    map.put("Vdd", "Vdd");

                    final File currOutputFile = new File( outputDir, cadenceCellName + ".mapping.il" );
                    System.out.println( "Generating \"" + currOutputFile.getAbsolutePath() + "\"." );

                    final OutputStream currOutputStream = new FileOutputStream( currOutputFile );

                    final Writer currOutputWriter = 
                        new BufferedWriter( new OutputStreamWriter( currOutputStream, "UTF-8" ) );
                    currOutputWriter.write("(defun PinVerilogToCastTable ( )\n");
                    currOutputWriter.write("  (let (\n");
                    currOutputWriter.write("        ( Table ( makeTable `pvtc nil ) ) )\n");
                    for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) i.next();

                        currOutputWriter.write("              ( setarray Table \""+entry.getKey() + "\" \"" + ni.renameNode(entry.getValue().toString()) + "\" )\n");
                    }
                    currOutputWriter.write("    Table ) )\n\n");
                    currOutputWriter.write("(defun PinCastToVerilogTable ( )\n");
                    currOutputWriter.write("  (let (\n");
                    currOutputWriter.write("        ( Table ( makeTable `pctv nil ) ) )\n");
                    for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) i.next();

                        currOutputWriter.write("              ( setarray Table \""+ni.renameNode(entry.getValue().toString()) + "\" \"" + entry.getKey() + "\" )\n");
                    }
                    currOutputWriter.write("    Table ) )\n\n");
                    currOutputWriter.write("(defun PinVerilogToCast ( verilog )\n");
                    currOutputWriter.write("    ( arrayref ( PinVerilogToCastTable ) verilog ) )\n\n");
                    currOutputWriter.write("(defun PinCastToVerilog ( cast )\n");
                    currOutputWriter.write("    ( arrayref ( PinCastToVerilogTable ) cast ) )\n\n");

                    currOutputWriter.close();
                }
            }
            for(int c=0;c<cellsToProcess.size();c++) {
                final String cellName = (String)cellsToProcess.get(c);
                final CellInterface cell=castParser.getFullyQualifiedCell(cellName);
                final Map mp = CellUtils.markPorts(cell);
                final CadenceInfo cinfo = cadencizer.convert(cell);
                final AliasedMap ports = cinfo.getPortNodes();
                final AliasedSet nodes = cinfo.getLocalNodes();
                String cadenceCellName = hashTableEmitterFactory.getCadenceCellNameForCastCellName( cellName );
                if (cadenceCellName == null && ni != null) {
                    cadenceCellName = ni.renameCell(cellName);
                }

                // canonical lookup
                final File currOutputFile2 = new File( outputDir, cadenceCellName + ".canon.il" );
                System.out.println( "Generating \"" + currOutputFile2.getAbsolutePath() + "\"." );

                final OutputStream currOutputStream2 = new FileOutputStream( currOutputFile2 );

                final Writer currOutputWriter2 = 
                    new BufferedWriter( new OutputStreamWriter( currOutputStream2, "UTF-8" ) );

                int numpins = mp.size();
                int pinsPerSegment = 2000;
                int numsegments = 0;
                int pincnt=0;

                final Set<HierName> seen = new HashSet<HierName>();
                final Iterator i = new SortingIterator(mp.keySet().iterator());
                for (; i.hasNext(); ) {
                    final String s = (String) i.next();
                    final HierName hs;
                    try {
                        hs = HierName.makeHierName(s, '.');
                    } catch (InvalidHierNameException e) {
                        throw new RuntimeException("Cannot create HierName: " + s, e);
                    }
                    final HierName canon = (HierName) ports.getCanonicalKey(hs);
                    if (((Boolean) ports.getValue(hs)).booleanValue() &&
                        seen.add(canon)) {
                        String c1 = ni.renameNode(canon.getCadenceString());
                        for (Iterator j = nodes.getAliases(canon); j.hasNext(); ) {
                            final HierName aName= (HierName) j.next();
                            if (!aName.equals(canon) && !aName.isAnonymous()) {
                                if (pincnt % pinsPerSegment == 0) {
                                    if (pincnt != 0) {
                                        currOutputWriter2.write("    Table ) )\n\n");
                                    }
                                    numsegments++;
                                    currOutputWriter2.write("(defun PinCanonicalTable"+numsegments+" ( )\n");
                                    currOutputWriter2.write("  (let (\n");
                                    currOutputWriter2.write("        ( Table ( makeTable `pd nil ) ) )\n");
                                }
                                pincnt++;
                                currOutputWriter2.write("          ( setarray Table \""+ni.renameNode(aName.getCadenceString())+"\" ");
                                currOutputWriter2.write(" \""+c1+"\" ");
                                currOutputWriter2.write(" )\n");
                            }
                        }
                    }
                }
                if (pincnt > 0)
                    currOutputWriter2.write("    Table ) )\n\n");
                currOutputWriter2.write("(defun PinCanonicalTable ( )\n");
                currOutputWriter2.write("  (let ( Key map \n");
                currOutputWriter2.write("        ( Table ( makeTable `pd nil ) ) )\n");
                for (int segment = 1; segment <= numsegments; segment++) {
                    currOutputWriter2.write("        map = PinCanonicalTable"+segment+"( )\n");
                    currOutputWriter2.write("        foreach( Key map\n");
                    currOutputWriter2.write("            ( setarray Table Key arrayref( map Key ) ) )\n");
                }
                currOutputWriter2.write("    Table ) )\n\n");
                currOutputWriter2.close();
            }
        } catch (CastSemanticException e) {
            ExceptionPrettyPrinter.printException(e);
            System.exit(1);
        }
        else {
            if ( outputDirName == null ) {
                System.out.println( "You must specify an output directory." );
            }
            else if ( ! ( outputDir.isDirectory() ) ) {
                System.out.println( "\"" + outputDirName + "\" is not a directory and could not be created." );
            }
            usage();
            System.exit(1);
        }
    }
}
