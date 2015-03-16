/* Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.tools.cdl2cast;


import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Iterator;
import java.util.Collection;
import java.util.Collections;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.ListIterator;
import java.util.LinkedHashSet;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.io.Writer;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.InputStreamReader;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.util.text.StringUtil;

import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.BindRulNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CompositeCDLNameInterface;
import com.avlsi.file.cdl.util.rename.CompositeCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.layout.gdsII.GenerateGDSIIDataFactory;
import com.avlsi.layout.gdsII.SkillTableEmitterFactory;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;

import com.avlsi.file.cdl.parser.SplittingFactory;
import com.avlsi.file.cdl.parser.TemplateSplitterFactory;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLInlineFactory;
import com.avlsi.file.cdl.parser.CDLScalingFactory;
import com.avlsi.file.cdl.parser.Template;

import com.avlsi.util.container.Pair;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.io.IndentWriter;

import com.avlsi.util.exception.AssertionFailure;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.layout.gdsII.AssuraBindRulTableEmitterFactory;

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.cdl.parser.AspiceCellAdapter;

/**
 * This class writes out a hierarchical cdl file as a tree
 * of .cast files in a spec tree and digital tree, and a SKILL names.il table
 * directory of mappings from existing layout names -> desired layout names which
 * will be used in gds2hier.il dfII library munging.  It includes prs generated
 * from NetGraph.
 **/

public class CDL2Cast {

    /** The names for Vdd and GND **/
    static String Vdd;
    static String GND;
    private static Set IMPLIED_PORTS;

    /** renaming table for transistor types **/
    static Map transistorTypeMap;

    /** normalize width and length **/
    static double widthGrid;
    static double lengthGrid;

    /** should CAST include fized_size=false netlist? **/
    static boolean netlistInCast;
    
    private static void usage( String m ) {

        final String className = CDL2Cast.class.getName();
        
        System.out.println( "Usage: " +
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "    --cdl-file=file\n" +
                            "    --output-spec=dir\n" +
                            "    --output-cast=dir\n" +
                            "    [--pedantic]\n" +
                            "    [--vdd-node=name]\n" +
                            "    [--gnd-node=name]\n" +
                            "    [--node-prefix=prefix]\n" +
                            "    [--instance-prefix=prefix]\n" +
                            "    [--lib-name=name]\n" +
                            "    [--refinement-parent=type]\n" +
                            "    [--name-table-dir=.]\n" +
                            "    [--sub-type=num]\n" +
                            "    [--meters-per-input-unit=1]\n" +
                            "    [--layout-to-cdl-bind-rul=/dev/null]\n" +
                            "    [--output-cdl-to-layout-bind-rul=/dev/null]\n" +
                            "    [--bind-rul-in=/dev/null]\n" +
                            "    [--bind-rul-template=/dev/null]\n" +
                            "    [--skip-prs-generation]\n" +
                            "    [--all-cells]\n" +
                            "    [--netlist-in-cast]\n" +
                            "    [--rename-transistor-type=old1:new1,old2:new2,...]\n" +
                            "    [--width-grid=W]\n" +
                            "    [--length-grid=L]\n"
                            );
        if (m != null && m.length() > 0)
            System.err.println ( m );
        System.exit(1);
    }

    /**
     * returns a <code>Writer</code> to <code>fileName</code>
     **/
    public static Writer openOutputFile( final String fileName )
        throws IOException
    {
        final Writer ret;

        final OutputStream outputStream =
            new FileOutputStream( fileName );

        ret  = new BufferedWriter( new OutputStreamWriter( outputStream ) );

        return ret;
    }

    public static File makeDir( final String dirName ) {
        File dir;
        if ( dirName != null ) {
            dir = new File( dirName );
        }
        else {
            dir = null ;
        }

        if ( ( dir != null ) &&
             ( ! ( dir.exists() ) ) ) {
            dir.mkdirs();
        }
        return dir;
    }


    public static void main(String[] args)
        throws Exception
    {
        final CommandLineArgs parsedArgs =
            new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs );
        final CommandLineArgs cachedArgs =
            new CachingCommandLineArgs( argsWithConfigs );
        final PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs(cachedArgs);
        final CommandLineArgs theArgs = pedanticArgs;

        final Boolean allCells = theArgs.argExists( "all-cells" );

        final String sourceCDLFileName =
            theArgs.getArgValue( "cdl-file", null );

        final String inputBindRulFileName =
            theArgs.getArgValue( "bind-rul-in", "/dev/null" );

        final String bindRulHeaderFileName =
            theArgs.getArgValue( "bind-rul-header", "/dev/null" );

        /**
         * The location of the <cell>.names.il output.  These files contain
         * SKILL tables with naem mappings from
         **/
        final String outputSkillTableDirName =
            theArgs.getArgValue( "name-table-dir", "." );


        /**
         * The location of the digital cast output.  The digital cast
         * will contain port definitions and a subcells block for each cell
         * with a .SUBCKT in the cdl file
         **/
        final String outputSpecTreeDirName =
            theArgs.getArgValue( "output-spec", null );

        /**
         * The location of the digital cast output.  The digital cast
         * will contain port definitions and a subcells block for each cell
         * with a .SUBCKT in the cdl file
         **/
        final String outputCastTreeDirName =
            theArgs.getArgValue( "output-cast", null );

        /**
         * The names for Vdd and GND
         **/
        Vdd = theArgs.getArgValue("vdd-node", "vcc");
        GND = theArgs.getArgValue("gnd-node", "vss");
        IMPLIED_PORTS = new LinkedHashSet(Arrays.asList(new Object[] { Vdd, GND }));

        /**
         * Prefixes to apply to node and instance names to make sure
         * they aren't ambiguous.
         **/
        final String nodePrefix     = theArgs.getArgValue("node-prefix","");
        final String instancePrefix = theArgs.getArgValue("instance-prefix","");

        /**
         * The libname and subtype for backstop renamed cells
         **/
        final String subType = theArgs.getArgValue( "sub-type", "0" );
        final String libName = theArgs.getArgValue( "lib-name", "" );

        /**
         * Sometimes, generated CDL will have non meter units
         * e.g. M0 a b c d n w=1.2 l=0.13
         * will imlicitly have units of microns.
         * All length units in the output CAST will be multiplied
         * by this constant
         **/
        final double metersPerInputUnit =
            Double.parseDouble
            ( theArgs.getArgValue( "meters-per-input-unit", "1" ) );

        /** 
         * Emit width and length parameters as expressions of constants.
         */
        widthGrid  = Double.parseDouble(theArgs.getArgValue("width-grid","0"));
        lengthGrid = Double.parseDouble(theArgs.getArgValue("length-grid","0"));

        /**
         * Transistor type renaming table.
         **/
        final String renameTransistorTypeString =
            theArgs.getArgValue( "rename-transistor-type", "");
        transistorTypeMap = new TreeMap();
        for (String map : StringUtil.split(renameTransistorTypeString, ',')) {
            final String[] keyval = StringUtil.split(map, ':');
            transistorTypeMap.put(keyval[0],keyval[1]);
        }
 
        /**
         * The refinement parent for generated digital CAST cells
         **/
        final String refinementParent =
            theArgs.getArgValue( "refinement-parent", "NULL" );

        /**
         * bind.rul format mappings from cdl -> existing layout
         * Can be generated (.cxl) by running Assura LVS
         */
        final String cdlToLayoutInputBindRulFileName =
            theArgs.getArgValue( "layout-to-cdl-bind-rul", "/dev/null" );

        /**
         * bind.rul format mappings from cdl -> desired layout
         */
        final String cdlToDesiredLayoutOutputBindRulFileName =
            theArgs.getArgValue( "output-cdl-to-layout-bind-rul", "/dev/null" );

        final String outputCastCellsFileName =
            theArgs.getArgValue( "output-cast-cells", "/dev/null" );

        final Writer outputCastCellsWriter =
            new BufferedWriter(new FileWriter( new File(outputCastCellsFileName)));

        final boolean skipPrsGeneration =
            theArgs.argExists("skip-prs-generation");

        netlistInCast = theArgs.argExists("netlist-in-cast");

        if (! pedanticArgs.pedanticOK(false, true)) {
            usage(pedanticArgs.pedanticString());
        }

        if ( ( sourceCDLFileName != null ) &&
             ( inputBindRulFileName != null ) &&
             ( bindRulHeaderFileName != null ) &&
             ( outputSkillTableDirName != null ) &&
             ( outputSpecTreeDirName != null  ) &&
             ( outputCastTreeDirName != null  ) &&
             ( libName != null  ) &&
             ( subType != null  ) &&
             ( refinementParent != null ) &&
             ( cdlToLayoutInputBindRulFileName != null ) &&
             ( cdlToDesiredLayoutOutputBindRulFileName != null )
             ) {

            final File bindRulHeaderFile = new File(bindRulHeaderFileName);
            final File sourceCDLFile = new File( sourceCDLFileName );
            final File inputBindRulFile = new File( inputBindRulFileName );
            final File cdlToLayoutInputBindRulFile =
                new File( cdlToLayoutInputBindRulFileName );
            final File cdlToDesiredLayoutOutputBindRulFile =
                new File( cdlToDesiredLayoutOutputBindRulFileName );
            final File outputSkillTableDir;
            final File outputSpecTreeDir;
            final File outputCastTreeDir;

            if ( ( sourceCDLFile.isFile() ) &&
                 ( sourceCDLFile.canRead() ) ) {
                if ( ( inputBindRulFile.canRead() ) &&
                     ( cdlToLayoutInputBindRulFile.canRead() ) ) {
                    final InputStream sourceCDLInputStream =
                        new FileInputStream( sourceCDLFile );

                    final Reader sourceCDLReader =
                        new InputStreamReader( sourceCDLInputStream, "UTF-8" );

                    final InputStream inputBindRulInputStream =
                        new FileInputStream( inputBindRulFile );

                    final Reader inputBindRulReader =
                        new InputStreamReader( inputBindRulInputStream, "UTF-8" );

                    final InputStream cdlToLayoutInputBindRulStream =
                        new FileInputStream( cdlToLayoutInputBindRulFile );


                    final Reader cdlToLayoutInputBindRulReader =
                        new InputStreamReader( cdlToLayoutInputBindRulStream, "UTF-8" );

                    outputSpecTreeDir    = makeDir(outputSpecTreeDirName);
                    outputCastTreeDir = makeDir(outputCastTreeDirName);
                    outputSkillTableDir  = makeDir(outputSkillTableDirName);

                    /**
                     *  The cdl is split into 2 factories, one which turns
                     *  it into templates, one which creates a directory
                     *  of names.il tables.
                     *
                     *  The templates are renamed to CAST names, which are
                     *  the names mapped from CDL names by the bind-rul-in file
                     *  ( If no mapping is given, a backstop CAST-lagalizing
                     *    renaming is enforced )
                     *  They are then flattened if they have both components
                     *  and subcell instantiations, and templates with multiple
                     *  parameterizations are split into new templates
                     *
                     *  The names.il files map from the existing layout names
                     *  to the cadence translated CAST names.  The existing
                     *  layout names are just the CDL names mapped to layout
                     *  names with the layout-cdl-bind-rul
                     **/

                    /**
                     * The back-stop for convertng cdl names to CAST names
                     * Alter names so that they are CAST legal and have no
                     * special meaning, and so that node names don't conflict
                     * with instantiations
                     **/
                    final CDLNameInterface castLegalizingNI =
                        new CDLNameInterface() {
                            private String escape( final String name ) {
                                final String S0 = name.replaceAll( "\\." , "_D_" );
                                final String S1 = S0.replaceAll( "\\[", "_L_");
                                final String S2 = S1.replaceAll( "\\]", "_R_" );
                                final String S3 = S2.replaceAll( "-" , "_M_" );
                                return S3;
                            }

                            public String renameCell( final String oldCellName )
                                throws CDLRenameException {
                                final String S0;
                                if( !libName.equals("") )
                                    S0 = libName  + "." + oldCellName;
                                else
                                    S0 = oldCellName;
                                final String S1 = S0.replaceAll( "-" , "_M_" );
                                return S1 + "." + subType;
                            }

                            public String renameNode( final String oldNodeName )
                                throws CDLRenameException {
                                return escape(nodePrefix + oldNodeName);
                            }

                            public String renameDevice( final String oldDeviceName )
                                throws CDLRenameException {
                                return escape(instancePrefix + oldDeviceName);
                            }

                            public String renameSubCellInstance( final String oldInstanceName )
                                throws CDLRenameException {
                                return escape(instancePrefix + oldInstanceName);
                            }

                            public String renameTransistorModel( final String oldTransistorModel )
                                throws CDLRenameException {
                                return oldTransistorModel;
                            }

                        };

                    final CDLNameInterfaceFactory castLegalizingNIF =
                        new TrivialCDLNameInterfaceFactory(castLegalizingNI);

                    /**
                     * Maps from CDL names to desired CAST names w/ subtypes
                     * if not in bind.rul uses guaranteed CAST legal names
                     **/
                    final BindRulNameInterfaceFactory
                        cdlToCastNameInterfaceFactory =
                        new BindRulNameInterfaceFactory( inputBindRulReader,
                                                         castLegalizingNIF );

                    /**
                     * Maps from CDL names to desired layout names
                     * which are just cast names translated to cadence names
                     * This is what Cast2CDL will output, and thus
                     * is what we we'll LVS the layout against, so we want
                     * it to match
                     **/
                    final CDLNameInterfaceFactory
                        cdlToDesiredLayoutNameInterfaceFactory =
                        new CompositeCDLNameInterfaceFactory
                        ( cdlToCastNameInterfaceFactory,
                          new TrivialCDLNameInterfaceFactory
                          ( new CadenceNameInterface(Collections.EMPTY_SET)));


                    final CDLNameInterfaceFactory identityNIF =
                        new TrivialCDLNameInterfaceFactory
                        ( new IdentityNameInterface() );

                    /**
                     * Maps from CDL names to existing layout names, with
                     * identity backstop.  The cdlToLayoutInputBindRul will
                     * typically be from a .cxl file from running LVS
                     **/
                    final CDLNameInterfaceFactory
                        cdlToExistingLayoutNameInterfaceFactory =
                        new BindRulNameInterfaceFactory
                        ( cdlToLayoutInputBindRulReader,
                          identityNIF );

                    /**
                     * Writes bind.rul from cdl -> layout names
                     **/
                    final TableEmitterFactoryInterface
                        cdlToDesiredLayoutOutputBindRulEmitterFactory =
                        new AssuraBindRulTableEmitterFactory
                        ( cdlToDesiredLayoutOutputBindRulFile,
                          bindRulHeaderFile );

                    final CDLFactoryInterface bindRulFactory =
                        new GenerateGDSIIDataFactory
                        ( cdlToDesiredLayoutOutputBindRulEmitterFactory,
                          identityNIF,
                          cdlToDesiredLayoutNameInterfaceFactory );

                    /**
                     * Writes names.il table map from existing layout
                     * names to desired layout names
                     **/
                    final TableEmitterFactoryInterface skillEmitterFactory =
                        new SkillTableEmitterFactory
                        ( outputSkillTableDir,
                          "CadenceNodeToGDSIINodeTable",
                          "CadenceCellToGDSIICellTable",
                          "CadenceInstanceToGDSIIInstanceTable" );

                    final CDLFactoryInterface oldToNewLayoutNameTableFactory =
                        new GenerateGDSIIDataFactory
                        ( skillEmitterFactory,
                          cdlToExistingLayoutNameInterfaceFactory,
                          cdlToDesiredLayoutNameInterfaceFactory );

                    /**
                     * Output names.il and bind.rul
                     **/
                    final CDLFactoryInterface tableFactory =
                        new SplittingFactory( bindRulFactory,
                                              oldToNewLayoutNameTableFactory);
                    /**
                     * Create the templates
                     **/
                    final Map templates = new TreeMap();

                    final Template templateFactory =
                        new Template(templates);

                    /**
                     * split different parameterizations of templates
                     * into new templates
                     **/
                    final CDLFactoryInterface templateSplitterFactory =
                        new TemplateSplitterFactory(templateFactory) {
                            public void makeCall(HierName name,
                                                 String subName,
                                                 HierName[] args,
                                                 Map parameters,
                                                 Environment env) {
                                // don't specialize undefined subcircuits
                                if (templateFactory.containsTemplate(subName)) {
                                    super.makeCall(
                                        name, subName, args, parameters, env);
                                } else {
                                    templateFactory.makeCall(
                                        name, subName, args, parameters, env);
                                }
                            }
                        };

                    final AspiceCellAdapter aspicer;
                    final CDLFactoryInterface outputFactory;
                    if (skipPrsGeneration) {
                        aspicer = null;
                        outputFactory = templateSplitterFactory;
                    } else {
                        /**
                         * Also read CDL into AspiceCellAdapter -- AML
                         **/
                        aspicer = new AspiceCellAdapter() {
                            public void makeCall(HierName name,
                                                 String subName,
                                                 HierName[] args,
                                                 Map parameters,
                                                 Environment env) {
                                if (templateFactory.containsTemplate(subName)) {
                                    super.makeCall(name, subName, args,
                                                   parameters, env);
                                }
                            }
                        };
                        // this is where we branch between outputs :
                        // aspice and templates
                        outputFactory =
                            new SplittingFactory( templateSplitterFactory,
                                                  aspicer );
                    }

                    final CDLFactoryInterface renamedOutputFactory =
                        new CDLRenameFactory(outputFactory,
                                             cdlToCastNameInterfaceFactory);

                    /**
                     * Make templates and tables
                     **/
                    final CDLFactoryInterface splittingFactory =
                        new SplittingFactory( renamedOutputFactory,
                                              tableFactory);

                    final CDLFactoryInterface firstFactory =
                        new CDLScalingFactory( metersPerInputUnit,
                                               metersPerInputUnit,
                                               1,
                                               1,
                                               1,
                                               1,
                                               splittingFactory);

                    ReadCDLIntoFactory.readCDL( sourceCDLReader,
                                                firstFactory );

                    final CDL2Cast cdl2cast = new CDL2Cast();
                    try {
                        cdl2cast.writeTemplates(templates,
                                                aspicer,
                                                outputCastCellsWriter,
                                                outputSpecTreeDir,
                                                outputCastTreeDir,
                                                new String[] { refinementParent },
                                                allCells );
                        outputCastCellsWriter.close();

                    }
                    catch(Exception e ) {
                        e.printStackTrace();
                    }
                }
                else
                    System.out.println( "\"" +
                                        inputBindRulFileName +
                                        "\" is not a readable file." );
            }
            else {
                System.out.println( "\"" +
                                    sourceCDLFileName +
                                    "\" is not a readable file." );
            }
        }
        else {
            usage(null);
        }
    }

    /**
     * Get the subtype for a fqcn
     * e.g. lib.buffer.half.BUF_1of4.0 -> 0
     **/
    public String getSubType(HierName fqcn) {
        return fqcn.getSuffixString();
    }

    public String getCellModule(String fqcn) {
        try {
            final HierName hName =
                HierName.makeHierName(fqcn,'.');
            return getCellModule(hName).getCadenceString();
        } catch(InvalidHierNameException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Get the CAST name w/o subtype for a given fqcn
     * e.g. lib.buffer.half.BUF_1of4.0 -> lib.buffer.half.BUF_1of4
     **/
    public HierName getCellModule(HierName fqcn) {
        return fqcn.getParent();
    }

    /**
     * Get the CAST library name for a given fqcn
     * e.g. lib.buffer.half.BUF_1of4.0 -> lib.buffer.half
     **/
    public HierName getLibName(HierName fqcn) {
        return fqcn.getParent().getParent();
    }

    /**
     * Get the CAST library module for a given fqcn
     * e.g. lib.buffer.half.BUF_1of4 -> lib.buffer
     **/
    public HierName getLibraryModule(HierName fqcn) {
        return fqcn.getParent().getParent().getParent();
    }

    /**
     * Write templates in cast format to spec tree and digital tree
     **/
    public void writeTemplates( final Map templates,
                                final AspiceCellAdapter aspicer,
                                final Writer outputCastCellsWriter,
                                final File outputSpecTreeDir,
                                final File outputCastTreeDir,
                                final String[] refinementParents )
        throws Exception {
            writeTemplates(templates, aspicer, outputCastCellsWriter, outputSpecTreeDir,
                outputCastTreeDir, refinementParents, false);
    }

    public void writeTemplates( final Map templates,
                                final AspiceCellAdapter aspicer,
                                final Writer outputCastCellsWriter,
                                final File outputSpecTreeDir,
                                final File outputCastTreeDir,
                                final String[] refinementParents,
                                final Boolean allCells )
        throws Exception {

        //determine which templates should be inlined.
        //If a template contains both instantiation calls and
        //components(R,C,M, etc), then all the calls in that template
        //should be flattened to components so that the resulting
        //cast cell has either a subcells block or a netlist block, not both.

        /**
         * Set of leaf cells which have components.
         **/
        final Set leafSet = new HashSet();

        /**
         * Set of cells which are instantiated somewhere.
         * If a flattened cell is not instantiated, then it should not
         * be output.
         **/
        final Set instantiatedSet = new HashSet();

        /**
         * Set of cells that have components and subcells.
         * These will be flattened, and the flattened cells
         * automatically added to the templates
         **/
        final Set flattenedSet = new HashSet();

        /**
         * map from cast file to writer
         **/
        final Map digitalLibWriterMap = new HashMap();
        final Map emitterMap = new HashMap();
        final Set cellModuleDone = new HashSet();

        for(Iterator i=templates.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry)i.next();
            final String fqcn = (String)entry.getKey();
            final Template template = (Template)entry.getValue();
            final TemplateContentSniffer sniffer =
                 new TemplateContentSniffer(template);

            outputCastCellsWriter.write(fqcn+"\n");
            final HierName hName   = HierName.makeHierName(fqcn,'.');
            final HierName module  = getLibraryModule(hName);
            final HierName libName = getLibName(hName);

            /**
             * if this is the first time we've seen this library
             * open a writer to it and write the CAST Header
             **/
            if(!digitalLibWriterMap.containsKey(libName) ) {
                final Writer digitalLibWriter =
                    openFile( cellPath(outputCastTreeDir, module),
                              libName.getSuffixString() + ".cast" );
                digitalLibWriterMap.put(libName, digitalLibWriter);
                writeCastFileHeader(digitalLibWriter);
                digitalLibWriter.write("\n");
                digitalLibWriter.write("module " +
                                       libName.getCadenceString() + ";\n");
                digitalLibWriter.write("\n");
            }

            template.execute( sniffer );
            if(sniffer.hasComponents() ) {
                leafSet.add(template);
                for(Iterator j = sniffer.getSubCells(); j.hasNext(); ) {
                    String cellName = (String)j.next();
                    flattenedSet.add( templates.get( cellName ) );
                }
            }
            else {
                for(Iterator j = sniffer.getSubCells(); j.hasNext(); ) {
                    String cellName = (String)j.next();
                    instantiatedSet.add( templates.get( cellName ) );
                }
            }
        }

        CDLInlineFactory flatten =
            new CDLInlineFactory(false, null, false);
        flatten.addTargets(templates);

        for(Iterator i=templates.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry)i.next();
            String fqcn = (String)entry.getKey();
            Template template = (Template)entry.getValue();
            if ( ! allCells && flattenedSet.contains(template) &&
                !instantiatedSet.contains(template) )
                continue;

            final HierName hName      = HierName.makeHierName(fqcn,'.');
            final HierName libName    = getLibName(hName);
            final HierName cellModule = getCellModule(hName);
            final String subType      = getSubType(hName);

            // get NetGraph if one exists
            AspiceFile aspiceCell =
                aspicer == null ? null : aspicer.getCell(fqcn);
            NetGraph netgraph = null;
            if (aspiceCell!=null) {
                netgraph = new NetGraph(HierName.makeHierName(Vdd),
                                        HierName.makeHierName(GND));
                netgraph.addAspice(aspiceCell);
                netgraph.prepareForLvs();
            }

            final Writer specWriter =
                openFile( cellPath(outputSpecTreeDir, cellModule),
                          subType + ".cast" );
            final Writer digitalWriter =
                (Writer) digitalLibWriterMap.get(libName);

            writeTemplateToSpecTree(hName,
                                    template,
                                    netgraph,
                                    flatten,
                                    leafSet.contains(template),
                                    specWriter);

            specWriter.close();

            //only add a cellModule once to digital library cast file
            // and for gods sake, wait until we have prs for leaf cells
            if(cellModuleDone.contains(cellModule)||
               ( leafSet.contains(template) &&
                 aspicer != null &&
                 aspiceCell == null ) ) continue;
            cellModuleDone.add(cellModule);

            writeTemplateToCastTree(hName,
                                   refinementParents,
                                   template,
                                   templates,
                                   netgraph,
                                   flatten,
                                   leafSet.contains(template),
                                   digitalWriter);
        }

        /**
         * Close all digital CAST library files
         **/
        for(Iterator i=digitalLibWriterMap.entrySet().iterator();
            i.hasNext(); ) {
            Map.Entry entry = (Map.Entry)i.next();
            Writer writer = (Writer)entry.getValue();
            writer.close();
        }
    }

    /**
     * When executed by a template,
     * collects information about instantiations of subcells and components.
     * Used to determine what ever to do with this template.  e.g. if it has
     * both components and calls, it will be flattened
     **/
    private class TemplateContentSniffer extends CDLInterfaceSimplifier
    {
        private boolean bComponent = false;
        private boolean bCall = false;
        private Collection subCells = new HashSet();
        private final Template templ;

        TemplateContentSniffer(final Template templ) {
            this.templ = templ;
        }

        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters) {
            if (templ.containsTemplate(subName)) {
                bCall = true;
                subCells.add(subName);
            }
        }
        public void makeResistor(HierName name, HierName n1, HierName n2,
                                 double val) {
            bComponent = true;
        }

        public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                                  double val) {
            bComponent = true;
        }

        public void makeTransistor(HierName name, int type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   double w, double l) {
            bComponent = true;
        }

        public void makeDiode(HierName name, int type, HierName npos,
                              HierName nneg, double w, double l, double a,
                              double p) {
            bComponent = true;
        }
        public void makeInductor(HierName name, HierName npos, HierName nneg,
                                 double val) {
            bComponent = true;
        }
        public void makeBipolar(HierName name, int type, HierName nc,
                                HierName nb, HierName ne, double a) {
            bComponent = true;
        }
        public void beginSubcircuit(String subName, String[] in, String[] out) {
            throw new AssertionFailure("Should not happen!");
        }
        public void endSubcircuit(String subName) {
            throw new AssertionFailure("Should not happen!");
        }

        public boolean hasSubCells() { return bCall; }
        public Iterator getSubCells() { return subCells.iterator(); }
        public boolean hasComponents() { return bComponent; }
    }

    /**
     * Writes all non-port nodes references in all statements in a template
     * in CAST format.  This will presumably be called before writing
     * instantiations to the subcells block, as undefined nodes cannot be
     * referenced by instantiations
     **/
    private class CastLocalNodesDeclarationEmitter extends CDLInterfaceSimplifier
    {
        private final Collection nodes = new HashSet();
        private final Collection args = new HashSet();
        private final Writer w;

        public CastLocalNodesDeclarationEmitter(final Template template,
                                                final Writer w) {
            this.w = w;
            final Pair p = template.getArguments();
            final String[] in = (String []) p.getFirst();
            final String[] out = (String []) p.getSecond();
            for(int i=0; i<in.length; i++) {
                args.add(in[i]);
            }
            for(int i=0; i<out.length; i++) {
                args.add(out[i]);
            }
        }

        private void add(HierName node) {
            String val = node.getCadenceString();
            if(!args.contains(val))
                if(!nodes.contains(val)) {
                    nodes.add(val);
                    try {
                        w.write("node " + val + ";\n" );
                    }
                    catch(Exception e ) {
                        throw new RuntimeException(e);
                    }
                }
        }

        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters) {
            for(int i=0; i<args.length; i++) {
                add(args[i]);
            }
        }
        public void makeResistor(HierName name, HierName n1, HierName n2,
                                 double val) {
            add(n1);
            add(n2);
        }

        public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                                  double val) {
            add(npos);
            add(nneg);
        }

        public void makeTransistor(HierName name, int type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   double w, double l) {
            add(ns);
            add(nd);
            add(ng);
            add(nb);
        }

        public void makeDiode(HierName name, int type, HierName npos,
                              HierName nneg, double w, double l, double a,
                              double p) {
            add(npos);
            add(nneg);
        }

        public void makeInductor(HierName name, HierName npos, HierName nneg,
                                 double val) {
            add(npos);
            add(nneg);
        }

        public void makeBipolar(HierName name, int type, HierName nc,
                                HierName nb, HierName ne, double a) {
            add(nc);
            add(nb);
            add(ne);
        }

        public void beginSubcircuit(String subName, String[] in, String[] out) {
            throw new AssertionFailure("Should not happen!");
        }

        public void endSubcircuit(String subName) {
            throw new AssertionFailure("Should not happen!");
        }

        public Iterator getNodes() { return nodes.iterator(); }
    }

    private File cellPath(final File path, final HierName name) {
        return new File(path, name.getAsString(File.separatorChar));
    }

    /**
     * Opens a file given a directory and the file name, creating the directory
     * if necessary.
     **/
    private Writer openFile(final File dir, final String name)
        throws IOException {
        if (!dir.isDirectory() && !dir.mkdirs()) {
            throw new IOException("Unable to create directory: " + dir);
        }
        final File file = new File(dir, name);
        return new BufferedWriter(new FileWriter(file));
    }


    /**
     * Returns a <code>List</code> if <code>Integers</code> in
     * a bracket string
     * e.g.
     * [0][1] -> (0,1)
     * [0,1] -> (0,1)
     **/
    private final List<Integer> parseBracketStr( final String bracketStr ) {
        final int len = bracketStr.length();

        final StringBuffer accum = new StringBuffer();

        final List<Integer> ret = new ArrayList<>();

        boolean bracketOpen = false;

        for ( int i = 0 ; i < len ; ++i ) {
            final char c = bracketStr.charAt( i );

            switch ( c ) {
            case '[':
                if ( bracketOpen ) {
                    throw new RuntimeException( "\"" +
                                                bracketStr +
                                                "\" is not a valid array index string." );
                }
                bracketOpen = true;
                break;
            case ']':
                if ( ! bracketOpen ) {
                    throw new RuntimeException( "\"" +
                                                bracketStr +
                                                "\" is not a valid array index string." );
                }
                bracketOpen = false;

                ret.add( new Integer( accum.toString() ) );
                accum.delete( 0, accum.length() );
                break;
            case ',':
                if ( ! bracketOpen ) {
                    throw new RuntimeException( "\"" +
                                                bracketStr +
                                                "\" is not a valid array index string." );
                }
                ret.add( new Integer( accum.toString() ) );
                accum.delete( 0, accum.length() );
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                accum.append( c );
                break;
            default:
                throw new RuntimeException( "\"" +
                                            bracketStr +
                                            "\" is not a valid array index string." );
            }
        }
        return ret;
    }

    /**
     * Returns a <code>Pair</code> (stem<String>, indices<List<Integer>>) for a
     * <code>String</portNodeName>
     * e.g.
     * a[0][1] -> (a,(0,1))
     * d -> (d,())
     **/
    private final Pair<String,List<Integer>> parsePortNodeName( final String portNodeName ) {
        final int len = portNodeName.length();

        final int indexOfFirstOpenBracket = portNodeName.indexOf( '[' );

        final int indexOfLastCloseBracket = portNodeName.lastIndexOf( ']' );


        if ( ( ( indexOfFirstOpenBracket == -1 ) &&
               ( indexOfLastCloseBracket == -1 ) ) ||
             ( ( indexOfFirstOpenBracket > 0 ) &&
               ( indexOfLastCloseBracket > 0 ) &&
               ( indexOfLastCloseBracket == ( len - 1 ) ) &&
               ( ( indexOfFirstOpenBracket + 1 ) < indexOfLastCloseBracket ) ) ) {
            if ( indexOfFirstOpenBracket != -1 ) {
                final String stem = portNodeName.substring( 0, indexOfFirstOpenBracket );
                assert stem.length() > 0 ;
                final String bracketStr = portNodeName.substring( indexOfFirstOpenBracket, indexOfLastCloseBracket + 1 );
                return new Pair<>( stem, parseBracketStr( bracketStr ) );
            }
            else {
                return new Pair<>( portNodeName, Collections.emptyList() );
            }
        }
        else {
            throw new RuntimeException( "\"" +
                                        portNodeName +
                                        "\" is not a valid port name because a misplaced '[' or ']'." );
        }
    }

    private Pair<Map<String,List<Pair<Integer,Integer>>>,Set<String>>
        sniffArrays( Template template ) {
        final Pair<String[],String[]> p = template.getArguments();
        final String[] in = p.getFirst();
        final String[] out = p.getSecond();

        final List<String> ports =
            Stream.concat(Arrays.stream(in), Arrays.stream(out))
                  .filter(port -> !isImpliedPort(port))
                  .collect(Collectors.toList());

        return sniffArrays(ports);
    }

    /**
     * Given a <code>List</code> of <code>ports</code>, return
     * a <code>Pair<Map,Set></code> of
     * the Map is a <code>Map<String,List<Pair<Integer,Integer>>
     *   from stems to min/max ranges for each index of the array
     * the Set is a <code>Set<String></code>
     *   of stems in order of first appearance
     * So if ports = (b[0], a[0][0], a[0][1], a[1][0], a[1][1], d, DOG, b[1] )
     * returns
     * ({a->((0,1),(0,1)), b->((0,1)), d->(), DOG->()}, {b,a,d,DOG})
     **/
    private Pair<Map<String,List<Pair<Integer,Integer>>>,Set<String>>
        sniffArrays( final List<String> ports ) {
        final Map<String,List<Pair<Integer,Integer>>> portMap = new HashMap<>();
        final Set<String> portStems = new LinkedHashSet<>();

        for (String currPortNode : ports) {
            final Pair<String,List<Integer>> portInfo =
                parsePortNodeName( currPortNode );

            final String stem = portInfo.getFirst();
            final List<Integer> indices = portInfo.getSecond();

            final List<Pair<Integer,Integer>> existingPortDimensions =
                portMap.get( stem );

            if ( existingPortDimensions == null ) {
                final List<Pair<Integer,Integer>> dimensions;
                if ( indices.size() > 0 ) {
                    dimensions =
                        indices.stream()
                               .map(i -> new Pair<>( i, i ))
                               .collect(Collectors.toList());
                }
                else {
                    dimensions = Collections.emptyList();
                }
                portMap.put( stem, dimensions );
                portStems.add( stem );
            }
            else {
                if ( indices.size() == existingPortDimensions.size() ) {
                    final Iterator indexIter = indices.iterator();
                    final ListIterator dimensionIter =
                        existingPortDimensions.listIterator();
                    while ( indexIter.hasNext() ) {
                        final Integer index = ( Integer ) indexIter.next();
                        final Pair minMaxPair = ( Pair ) dimensionIter.next();

                        final Integer min = ( Integer ) minMaxPair.getFirst();
                        final Integer max = ( Integer ) minMaxPair.getSecond();

                        assert min.intValue() <= max.intValue();

                        if ( index.intValue() < min.intValue() ) {
                            dimensionIter.set( new Pair( index, max ) );
                        }
                        else if ( index.intValue() > max.intValue() ) {
                            dimensionIter.set( new Pair( min, index ) );
                        }
                    }

                }
                else {
                    throw new RuntimeException( "Can not change number of dimensions of \"" + stem + "\"." );
                }
            }
        }

        return new Pair<>( portMap, portStems );

    }

    /**
     * HACK to check for the nodes which are in the STD_CELL parent
     * should really check the refinement parent
    **/
    private boolean isImpliedPort(final String name) {
        return IMPLIED_PORTS.contains(name);
    }

    private void writeCellPorts(final Template template,
                                final NetGraph netgraph,
                                final Writer writer)
        throws IOException {
        writer.write("(");

        final Pair<Map<String,List<Pair<Integer,Integer>>>,Set<String>> portInfo =
            sniffArrays( template );

        final Map<String,List<Pair<Integer,Integer>>> portMap =
            portInfo.getFirst();
        final Set<String> portStems = portInfo.getSecond();

        final Iterator<String> stemIter = portStems.iterator();

        if ( stemIter.hasNext() ) writer.write("node ");

        while ( stemIter.hasNext() ) {
            final String stem = stemIter.next();
            final List<Pair<Integer,Integer>> dimensions = portMap.get( stem );
            String dir = "-+";
            if (netgraph!=null) { // infer directionality of port
                NetGraph.NetNode node =
                    netgraph.findNetNode(HierName.makeHierName(stem));
                if ((node!=null) && node.isGate() && !node.isOutput()) dir = "-";
                if ((node!=null) && !node.isGate() && node.isOutput()) dir = "+";
            }
            writer.write(dir + stem );
            if ( dimensions.size() > 0 ) {
                final Iterator<Pair<Integer,Integer>> dimenIter =
                    dimensions.iterator();
                writer.write( "[" );
                while ( dimenIter.hasNext() ) {
                    final Pair<Integer,Integer> minMaxPair = dimenIter.next();
                    final Integer min = minMaxPair.getFirst();
                    final Integer max = minMaxPair.getSecond();
                    assert min.intValue() <= max.intValue();

                    writer.write( min.toString() + ".." + max.toString() );
                    if ( dimenIter.hasNext() ) {
                        writer.write( ',' );
                    }
                }
                writer.write( ']' );
            }
            if ( stemIter.hasNext() ) {
                writer.write( ", " );
            }
        }

        writer.write(")");
    }

    /**
     * writes the CAST cell declaration
     * define "cell"(node b[0..1],
     *               node a[0..1,0..1],
     *               node d,
     *               node BAD)
     * from the examples above
     **/
    private void writeCellHeader(String cell,
                                 String[] refinementParents,
                                 Template template,
                                 NetGraph netgraph,
                                 Writer writer,
                                 boolean elidePorts)
        throws IOException {
        writer.write("define \"" + cell + "\"()" );

        if (!elidePorts) {
            writeCellPorts(template, netgraph, writer);
        }

        if (refinementParents.length > 0 ) {
            writer.write(" <:");
            for(int i=0; i<refinementParents.length; i++) {
                writer.write(" " + refinementParents[i]);
            }
        }
        writer.write(" ");
    }

    private void writeCastFileHeader(Writer w)
        throws IOException {
        w.write("/* Copyright " + (new GregorianCalendar()).get(Calendar.YEAR) +
                " Intel Corporation.  All rights reserved.\n");
        // Prevent Perforce from doing substitution on the string literal
        // (otherwise $ Author $ in the string literal will be expanded to be
        // the author of this Java file)
        w.write(" * $" + "Id$\n");
        w.write(" * $" + "DateTime$\n");
        w.write(" * $" + "Author$\n");
        w.write(" */\n");
        w.write("/* Automatically generated.  Modify at your own risk. */\n");
    }

    /**
     * a CDLFactory executed by a template for a mid-level cell
     * (with only instantiataions) to write a CAST spec definition.
     * just writes the subtypes all the instances
     **/

    private class CastSubTypeEmitter extends CDLFactoryAdaptor {
        private PrintWriter w;
        public CastSubTypeEmitter(Writer w ) {
            this.w = new PrintWriter(w);
        }

        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            String cellModule = getCellModule(subName);
            w.print(cellModule + " :> \n" );
            w.print("   " + subName + " " + name.getCadenceString() + ";\n" );
        }
    }

    /**
     * a CDLFactory executed by a template for a mid-level cell
     * (with only instantiataions) to write a CAST digital definition.
     * Just writes the instantiations
     **/
    private class CastSubCellEmitter extends CDLFactoryAdaptor {
        private PrintWriter w;
        private Map templates;
        private String subckt;
        public CastSubCellEmitter(Map templates, Writer w ) {
            this.templates = templates;
            this.w = new PrintWriter(w);
            this.subckt = "<unknown>";
        }
        public void beginSubcircuit(String subName, String[] in, String[] out,
                                    Map parameters, Environment env) {
            subckt = subName;
        }
        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            Map<String,HierName> implied = new HashMap<String,HierName>();
            final List realargs;

            Template template = (Template) templates.get(subName);
            if (template == null) {
                System.out.println("Warning: SUBCKT " + subckt +
                                   " instantiates unknown subcell " +
                                   name.getAsString('.') + "/" +
                                   subName);
                realargs = Arrays.asList(args);
            } else {
                realargs = new ArrayList();
                final Pair p = template.getArguments();
                final String[] in = (String []) p.getFirst();
                final String[] out = (String []) p.getSecond();
                if (in.length + out.length != args.length) {
                    System.out.println("Warning: parameter mismatch in " +
                                       "SUBCKT " + subckt + " instantiating " +
                                       name.getAsString('.') + "/" +
                                       subName);
                }

                int argidx = 0;
                for ( int i = 0 ; i < in.length && argidx < args.length;
                      ++i, ++argidx ) {
                    if (isImpliedPort (in[i])) {
                        implied.put( in[i], args[argidx] );
                    } else {
                        realargs.add( args[argidx] );
                    }
                }

                for ( int i = 0 ; i < out.length && argidx < args.length;
                      ++i, ++argidx ) {
                    if (isImpliedPort (out[i])) {
                        implied.put( out[i], args[argidx] );
                    } else {
                        realargs.add( args[argidx] );
                    }
                }
            }

            String cellModule = getCellModule(subName);
            w.print(cellModule + " " + name.getCadenceString() + "(" );
            for(Iterator realarg = realargs.iterator(); realarg.hasNext(); ) {
                w.print(" " + ((HierName) realarg.next()).getCadenceString());
                if (realarg.hasNext()) w.print("," );
            }
            w.print(")");

            StringBuffer impliedPorts = new StringBuffer();
            boolean sameImplied = true;
            for(Iterator i = IMPLIED_PORTS.iterator(); i.hasNext(); ) {
                final String imp = (String) i.next();
                final HierName actual = implied.get(imp);
                if (actual == null) {
                    impliedPorts.append(imp);
                } else {
                    sameImplied &= imp.equals(actual.getAsString('.'));
                    impliedPorts.append(actual);
                }
                if (i.hasNext()) impliedPorts.append(",");
            }
            if (!sameImplied) {
                w.print("(" + impliedPorts.toString() + ")");
            }
            w.print(";\n");
        }
    }


    private void writeTemplateToSpecTree(final HierName fqcn,
                                         final Template template,
                                         final NetGraph netgraph,
                                         final CDLInlineFactory flatten,
                                         final boolean isLeaf,
                                         final Writer writer)
        throws IOException {

        final String subType = getSubType(fqcn);
        final HierName cellModule = getCellModule(fqcn);

        final IndentWriter iw = new IndentWriter(writer);

        writeCastFileHeader(iw);
        iw.write("\n");
        iw.write("module " + cellModule.getCadenceString() + ";\n");
        iw.write("\n");
        writeCellHeader(subType,
                        new String[] {cellModule.getCadenceString()},
                        template,
                        netgraph,
                        iw,
                        true);
        iw.write("{\n");
        iw.nextLevel();

        if(isLeaf) {
            // write fixed size netlist to the SPEC
            iw.write("netlist {\n");
            iw.nextLevel();

            final CDLFactoryInterface emitter =
                new CDLFactoryEmitter(iw, true, 999, true, true, "" /*"/"*/,
                                      transistorTypeMap, widthGrid, lengthGrid) {
                    public void makeCall(HierName name,
                                         String subName,
                                         HierName[] args,
                                         Map parameters,
                                         Environment env) {
                        if (template.containsTemplate(subName)) {
                            // flatten through
                            flatten.makeCall(name, subName, args,
                                             parameters, env);
                        } else {
                            // emit reference to undefined subcircuit as is
                            super.makeCall(name, subName, args,
                                           parameters, env);
                        }
                    }
                };
            flatten.setProxy(emitter);
            template.execute(flatten);

            iw.prevLevel();
            iw.write("}\n");
            iw.write("directives { fixed_size = true; }\n");
        }
        else {
            iw.write("subtypes {\n");
            iw.nextLevel();

            final CDLFactoryInterface emitter = new CastSubTypeEmitter(iw);
            template.execute(emitter);

            iw.prevLevel();
            iw.write("}\n");
        }
        iw.prevLevel();
        iw.write("}\n");
    }

    private void writeTemplateToCastTree(final HierName fqcn,
                                         final String[] refinementParents,
                                         final Template template,
                                         final Map templates,
                                         final NetGraph netgraph,
                                         final CDLInlineFactory flatten,
                                         final boolean isLeaf,
                                         final Writer writer)
        throws IOException {

        final IndentWriter iw = new IndentWriter(writer);

        final HierName cellModule = getCellModule(fqcn);

        writeCellHeader(cellModule.getSuffixString(),
                        refinementParents,
                        template,
                        netgraph,
                        iw,
                        false);

        iw.write("{\n");
        iw.nextLevel();

        if (!isLeaf) {
            // write subcells body
            iw.write("subcells {\n");
            iw.nextLevel();

            final CDLFactoryInterface nodeEmitter =
                new CastLocalNodesDeclarationEmitter(template, iw);
            template.execute(nodeEmitter);

            final CDLFactoryInterface emitter =
                new CastSubCellEmitter(templates, iw);
            template.execute(emitter);

            iw.prevLevel();
            iw.write("}\n");
        }
        else if (netgraph!=null) {
            ProductionRuleSet prs = netgraph.getProductionRuleSet();
            if (prs.size()>0) {
                iw.write("prs {\n");
                iw.nextLevel();
                for (Iterator i = netgraph.getNodes().iterator(); i.hasNext(); ) {
                    NetGraph.NetNode node = (NetGraph.NetNode) i.next();
                    if (!node.isPort() && !node.isRail() && !node.isStaticizerInverter() &&
                        (node.isGate() || node.isOutput()))
                        iw.write("node \"" + node.name + "\";\n");
                }
                iw.write(prs.toString());
                iw.prevLevel();
                iw.write("}\n");
            }
            // write sizable netlist to the CAST
            if (netlistInCast) {
                iw.write("netlist {\n");
                iw.nextLevel();
                final CDLFactoryInterface emitter =
                    new CDLFactoryEmitter(iw, true, 999, true, true, "" /*"/"*/,
                                          transistorTypeMap, widthGrid, lengthGrid) {
                        public void makeCall(HierName name,
                                         String subName,
                                             HierName[] args,
                                             Map parameters,
                                             Environment env) {
                            if (template.containsTemplate(subName)) {
                                // flatten through
                                flatten.makeCall(name, subName, args,
                                                 parameters, env);
                            } else {
                                // emit reference to undefined subcircuit as is
                                super.makeCall(name, subName, args,
                                               parameters, env);
                            }
                        }
                    };
                flatten.setProxy(emitter);
                template.execute(flatten);
                iw.prevLevel();
                iw.write("}\n");
                iw.write("directives { fixed_size = false; }\n");
            }
        }
        iw.prevLevel();
        iw.write("}\n\n");
    }

}
