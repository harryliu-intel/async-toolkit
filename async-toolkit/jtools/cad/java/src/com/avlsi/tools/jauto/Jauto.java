/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Set;
import java.util.SortedSet;
import java.util.SortedMap;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.StringTokenizer;
import java.util.Date;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.fast.*;
import com.avlsi.fast.ports.*;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLFactoryFilter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.cast.*;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.debug.SimpleProfiler;
import com.avlsi.util.text.StringUtil;
import com.avlsi.netlist.*;
import com.avlsi.netlist.impl.*;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.FileUtil;
import com.avlsi.io.IndentWriter;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;

import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Triplet;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.InvalidCommandLineArgException;
import com.avlsi.util.cmdlineargs.MissingCommandLineArgException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.text.PrintfFormat;

import com.avlsi.tools.jauto.JautoMessageCenter;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;

import com.avlsi.util.ext.Exec;

/**
 * Main wrapper class
 *
 * @author Aaron Denney
 * @version $Date$
 **/

/**
 * Jauto is the main entrance class for transistor sizing tool "JAUTO", aka "P2N".
 * Major steps for the sizing flow are:
 * <p>1. Read in Cast design, sizing options, technology data
 * <p>2. Build design data structures, sizing data structures
 * <p>3. Call the main sizing routine
 * <p>4. Output sizing result in Cast and CDL format
 **/
public class Jauto {
    
    static final String versionNumber = "1.2";

    static final HierName Vdd = HierName.makeHierName("Vdd");
    static final HierName GND = HierName.makeHierName("GND");
    static final HierName _RESET = HierName.makeHierName("_RESET");

    static final JautoMessageCenter messageCenter = new JautoMessageCenter();

    private static final String SIZE_MODE = "size";
    private static final String SUBTYPE_MODE = "spec";
    private static final String UPDATE_MODE = "update";
    private static final String CHARGE_MODE = "resize";

    /**
     * Constructor.
     **/
    private Jauto() {
        throw new AssertionError();
    }

    private static String buildVersion() {
        return com.avlsi.util.debug.VersionInfo.getVersionString(Jauto.class);
    }

    private static void print(String name, double value) {
        System.out.println(name +  value);
    }

    private static void print(String name, int value) {
        System.out.println(name + value);
    }

    private static void printNS(String name, Object value) {
        System.out.println
            (name+(value==null?"Not specified":value));
    }

    private static void printNSD(String name, String value) {
        System.out.println
            (name+(value==null?"Not specified, set to default":value));
    }

    private static void printYN(String name, boolean value) {
        System.out.println
            (name+(value?"Yes":"No"));
    }

    private static int intArg(String arg) {
        return intArg(arg,-1);
    }

    private static int intArg(String arg, int def) {
        return arg != null ? Integer.parseInt(arg) : def;
    }

    private static double doubleArg(String arg) {
        return doubleArg(arg,-1);
    }

    private static double doubleArg(String arg, double def) {
        return arg != null ? Double.parseDouble(arg) : def;
    }

    private static float floatArg(String arg, float def) {
        return arg != null ? Float.parseFloat(arg) : def;
    }

    static long lastTime=System.currentTimeMillis();
    static long origTime=System.currentTimeMillis();
    static long lastMemory=0;
    private static String stats() {
        long newTime=System.currentTimeMillis();
        long deltaTime=newTime-lastTime;
        lastTime=newTime;
        long newMemory = Runtime.getRuntime().totalMemory()
            -Runtime.getRuntime().freeMemory();
        long deltaMemory=newMemory-lastMemory;
        lastMemory=newMemory;
        return (deltaTime/1000.0)+" sec, "+
            ((lastTime-origTime)/1000.0)+" sec total, "+
            (deltaMemory/1024.0)+"k, "+
            (lastMemory/1024.0)+"k total";
    }

    /**
     * Force a cell, and all recursively all its subcells to be fixed size, by
     * actually modifying the cell itself.  Use with extreme caution.
     **/
    private static void forceFixedSize(final CellInterface cell,
                                       final boolean val) {
        final DirectiveSource source = new DirectiveSource(BlockInterface.CELL);
        source.definition(DirectiveConstants.FIXED_SIZE, val);
        final BlockIterator bi =
            cell.getBlockInterface().iterator(BlockInterface.DIRECTIVE);
        bi.merge(new DirectiveBlock(source.getDirectiveInterface()));
    }

    private static void forceFixedSize(final CellInterface cell,
                                       final boolean val,
                                       final Set seen) {
        if (cell.isNode() || cell.isChannel() ||
            !seen.add(cell.getFullyQualifiedType())) return;

        forceFixedSize(cell, val);

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            Pair p = (Pair) i.next();
            CellInterface subcell = (CellInterface) p.getSecond();
            forceFixedSize(subcell, val, seen);
        }
    }

    private static String printArgValue(final String arg) {
        return arg == null ? "(null)"
                           : arg.equals("") ? "(empty)"
                                            : arg;
    }

    public static Map<AutoThreshold.Option,Boolean>
    parseAutoThresholdOptions(final String opt) {
        final EnumMap<AutoThreshold.Option,Boolean> result =
            new EnumMap<AutoThreshold.Option,Boolean>(
                    AutoThreshold.Option.class);
        for (String pipe : StringUtil.split(opt, '|')) {
            final String[] keyval = StringUtil.split(pipe, ':');
            final AutoThreshold.Option key =
                Enum.valueOf(AutoThreshold.Option.class,
                             keyval[0].toUpperCase());
            result.put(key, Boolean.valueOf(keyval[1]));
        }
        if (DebugOption.printLevel <= 1) {
            System.out.println("Parsed auto threshold options: " + result);
        }
        return result;
    }

    /**
     * Main program.  Loads up a cast file, parses it into the .fast
     * data structures, tries to also find CDL, etc.
     **/
    public static void main(String [] args) throws Exception {
        /**
         * Commandline arguments parsing
         **/

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final boolean quiet = theArgs.argExists("quiet");
        JautoUI.quiet = quiet;
        if (! quiet )
            System.err.println("\033[1;32m" + JautoUI.getCurrentTime() + " -- Starting Jauto main routine." + "\033[0m");
        
        final boolean showVersion = theArgs.argExists( "version" );
        if ( showVersion ) {
            System.out.println(buildVersion());
        }
        
        String cellName = theArgs.getArgValue("cellName", null );
        String layoutRoot = theArgs.getArgValue("layoutRoot", null );

        DebugOption.printLevel = intArg(theArgs.getArgValue("debug-level", null), 3);
        final String castRoot = theArgs.getArgValue("castInRoot", null );
        final String outRoot = theArgs.getArgValue("outRoot", null );
        final String cdlRoot = theArgs.getArgValue("cdlRoot", null );
        final String targetSpeed = theArgs.getArgValue("targetSpeed", null );
        final String tau = theArgs.getArgValue("tau", null );
        final String minTau = theArgs.getArgValue("minTau", null );
        final String dynamicEnergyPenalty = theArgs.getArgValue("dynamicEnergyPenalty",null);
        final String maxWidthN = theArgs.getArgValue("maxWidthN", null );
        final String maxWidthP = theArgs.getArgValue("maxWidthP", null );
        final String minWidthP = theArgs.getArgValue("minWidthP", null );
        final String minWidthN = theArgs.getArgValue("minWidthN", null );
        final String nmosWarnWidth = theArgs.getArgValue("nmosWarnWidth", null);
        final String pmosWarnWidth = theArgs.getArgValue("pmosWarnWidth", null);
        final String maxGroupSize = theArgs.getArgValue("maxGroupSize", null );
        final String convergeThres = theArgs.getArgValue("convergeThres", null );
        final String slackWarningThreshold = theArgs.getArgValue("slackWarningThreshold", null );
        final String strengthRatioWarningThreshold = theArgs.getArgValue("strengthRatioWarningThreshold", null);
        final String castVersion = theArgs.getArgValue("castversion", "2" );
        final String pathLimit = theArgs.getArgValue("pathLimit", null );
        String catPathReductionThreshold = theArgs.getArgValue("catPathReductionThreshold", null );
//        final String packageRoot = theArgs.getArgValue( "package-root", null );
        final String layoutAttribute = theArgs.getArgValue("defaultLayoutAttributes", null);

        /** 
         * Options for internal E*Tau^2 sweep.
         * Internal sweep will start from "tau"
         * increase "internalSweepStep" each time 
         * for "internalSweepNumber" times.
         **/
        final boolean internalSweep = theArgs.argExists("internalSweep");
        final String internalSweepNumber = theArgs.getArgValue("internalSweepNumber", null);
        final String internalSweepStep = theArgs.getArgValue("internalSweepStep", null);

        final boolean stepTau = theArgs.argExists("stepTau");

        double cutoff = Double.MAX_VALUE;
        double cutoffmin = 0;

        boolean nofloorplan = theArgs.argExists("noFloorplan");
        boolean sizeunit = theArgs.argExists("unitSize");

        final boolean noSizeStaticizers = theArgs.argExists("noSizeStaticizers");
        final boolean reSizeStaticizers = theArgs.argExists("reSizeStaticizers");
        final boolean skipSizing = theArgs.argExists("skipSizing") || reSizeStaticizers;

        final boolean dumpcell = theArgs.argExists("dumpcell");
        final boolean portDirectionOnly = theArgs.argExists("portDirectionOnly");
        final boolean checkOverStaticizingOnly = theArgs.argExists("checkOverStaticizingOnly");
        final boolean printSizingVersion = theArgs.argExists("printSizingVersion");
        final boolean pathBasedSizing = theArgs.argExists("pathBasedSizing");
        final String  minSolveHours = theArgs.getArgValue("min-solve-hours",null);
        final String  maxSolveHours = theArgs.getArgValue("max-solve-hours",null);
        long min_end_time = -1, max_end_time = -1;
        if (minSolveHours!=null) 
            min_end_time = (long) (3600000 * doubleArg(minSolveHours));
        if (maxSolveHours!=null)
            max_end_time = (long) (3600000 * doubleArg(maxSolveHours));
        final boolean outputRC = theArgs.argExists("outputRC");
        final boolean noDebug = theArgs.argExists("noDebug");
        final boolean reportFlatPaths = theArgs.argExists("reportFlatPaths");
        final boolean reportStrength = theArgs.argExists("reportStrength");
        final boolean debugGeometry = theArgs.argExists("debugGeometry");
        final boolean ignoreReset = theArgs.argExists("ignoreReset");
        final boolean completeCatPath = theArgs.argExists("completeCatPath");
        final boolean completeSizingPath = theArgs.argExists("completeSizingPath");
        boolean disableImmediateCatPathReduction =
            theArgs.argExists("disableImmediateCatPathReduction");
        final boolean noHaltOnWarn = theArgs.argExists("noHaltOnWarn");
        final boolean enableCatPath = 
            (theArgs.argExists("enableCatPath") && 
             !theArgs.getArgValue("enableCatPath", "1").equals("0"));
        final boolean enableAutoConstraint = theArgs.argExists("enableAutoConstraint");
        final boolean boundMinDelay = theArgs.argExists("boundMinDelay");
        final boolean earlyOut = theArgs.argExists("earlyOut");
        final String lengthUnit = theArgs.getArgValue("lengthUnit", "");
        final boolean useEqualityConstraints = theArgs.argExists("useEqualityConstraints");
        final String staticizer = theArgs.getArgValue("staticizer", null);
        final String weakInverter = theArgs.getArgValue("weak-inverter", null);
        final String smallInverter = theArgs.getArgValue("small-inverter", null);

        final boolean updateDirective = theArgs.argExists("updateDirective");
        final boolean proteusBudget = theArgs.argExists("proteusBudget");
        boolean disableSizingEnv =
            theArgs.argExists("disableAutomaticSizingEnvironment");
        if (proteusBudget) {
            disableImmediateCatPathReduction = true;
            catPathReductionThreshold = "-10";
            disableSizingEnv = true;
        }

        final String[] gateNames;
        {
            final String gates = theArgs.getArgValue("gates", null);
            if (gates == null) gateNames = new String[0];
            else gateNames = StringUtil.split(gates, ':');
        }

        final String cutoffstr = theArgs.getArgValue("cutoff",
                                 Double.toString( Double.MAX_VALUE ) );
        try {
            cutoff =
            Double.parseDouble( cutoffstr );
        }
        catch (NumberFormatException e) {
            System.err.println("Invalid cutoff size \"" + cutoffstr + "\"!");
        }

        final String cutoffminstr = theArgs.getArgValue("cutoffmin",
                                "0" );
        try {
            cutoffmin =
            Double.parseDouble( cutoffminstr );
        }
        catch (NumberFormatException e) {
            System.err.println("Invalid minimum cutoff size \"" + cutoffminstr + "\"!");
        }

        double scaleC=doubleArg(theArgs.getArgValue("scaleC",null),1);
        double scaleR=doubleArg(theArgs.getArgValue("scaleR",null),1);
        double scaleM=doubleArg(theArgs.getArgValue("scaleM",null),1);

        Double mfggrid = null;
        final String mfggridstr = theArgs.getArgValue("mfggrid", null);
        try {
            if (mfggridstr != null)
                mfggrid = Double.valueOf(mfggridstr);
        } catch (NumberFormatException e) {
            System.err.println("Invalid manufacturing grid \"" + mfggridstr + "\"!");
        }

        float minWidth = Float.NaN;
        final String minWidthStr = theArgs.getArgValue("minGateWidth", null);
        try {
            if (minWidthStr != null) minWidth = Float.parseFloat(minWidthStr);
        } catch (NumberFormatException e) {
            System.err.println("Invalid minimum stack/gate width \"" + minWidthStr + "\"!");
        }

        final String subtypePath = theArgs.getArgValue("subtypePath", null);
        final String mode = theArgs.getArgValue("mode", SUBTYPE_MODE);
        final String subtype = theArgs.getArgValue("subtype", null); 
        boolean sizeTrial = theArgs.argExists("trial");

        {
            StringContainerIterator argIter = theArgs.nonParsedArgumentsIterator();
            while ( argIter.hasNext() ) {
            final String arg = argIter.next();
            System.err.println("ignoring non-argument option '" + arg + "'.");
            }
            
        }
        final String pStacks = theArgs.getArgValue("pStacks", null);
        final String nStacks = theArgs.getArgValue("nStacks", null);
        final String hsizes = theArgs.getArgValue("hsizes", null);
        final String allName = theArgs.getArgValue("spice-output", "transistors.cdl");
        final boolean newSpice = theArgs.argExists("new-spice");
        final boolean generateDelaySignoff =
            theArgs.argExists("generateDelaySignoff");
        final String delaySignOff =
            theArgs.getArgValue("delaySignoffFile", null);
        final String netlistFromPrs =
            theArgs.getArgValue("netlist-from-prs", null);
        final String optimizeThreshold =
            theArgs.getArgValue("optimizeThreshold", null);
        final boolean generateSdcConstraint =
            theArgs.argExists("generateSdcConstraint");
        double sdcDelayScale = 1.0;
        Set<String> sdcLibs = Collections.emptySet();
        if (generateSdcConstraint) {
            try {
                sdcDelayScale =
                    CommandLineArgsUtil.getDoubleArgValue(theArgs,
                            "sdcDelayScale", 1.0);
            } catch (CommandLineArgFormatException e) {
                System.err.println(
                    "Argument to " + e.getArgName() + " is malformed: " +
                    printArgValue(e.getMessage()));
                usage();
            }

            final String sdclibs = theArgs.getArgValue("sdcLibs", null);
            if (sdclibs != null) {
                sdcLibs = new HashSet<String>();
                for (String lib : StringUtil.split(sdclibs, ':')) {
                    sdcLibs.add(lib);
                }
            }
        }
            
        if(printSizingVersion){
            System.out.println("Transistor sizing algorithm - Version: " + versionNumber);
            return;
        }
        
        if (mode.equals(SUBTYPE_MODE)) {
            sizeunit = true;
            nofloorplan = true;
            layoutRoot = null;
        } else if (mode.equals(SIZE_MODE)) {
            cellName = cellName + "." + subtype;
        } else if (mode.equals(CHARGE_MODE)) {
            sizeunit = true;
            nofloorplan = true;
            layoutRoot = null;
            sizeTrial = true;
            if (mode.equals(CHARGE_MODE)) cellName = cellName + "." + subtype;
        } else if (mode.equals(UPDATE_MODE)) {
            sizeunit = true;
            nofloorplan = true;
            layoutRoot = null;
        }

        final String partialExtract =
            theArgs.getArgValue("partialExtract", cellName);

        /**
         * Output commandline parsing results.
         **/
        
        if (cellName == null) {
            System.err.println("no main cell specified.");
            usage();
        }
        if (subtype == null && !mode.equals(SUBTYPE_MODE)) {
            System.err.println("You must specify a subtype to operate on.");
            usage();
        }
        if (castRoot == null) {
            System.err.println("no castroot / castpath specified.");
            usage();
        }
        if (outRoot == null) {
            System.err.println("no cast output directory specified.");
            usage();
        }
        if (cdlRoot == null) {
            System.err.println("no cdl output directory specified.");
            usage();
        }
        if (layoutRoot == null && !nofloorplan) {
            System.err.println("must set --layoutRoot unless --noFloorplan.");
            usage();
        }
        if (!mode.equals(SUBTYPE_MODE) &&
            !mode.equals(SIZE_MODE) && 
            !mode.equals(UPDATE_MODE) &&
            !mode.equals(CHARGE_MODE)) {
            System.err.println("Invalid mode specified: " + mode);
            usage();
        }

        final String violationReportLimitString =
            theArgs.getArgValue("violationReportLimit", "-1");
        final float violationReportLimit =
            Float.parseFloat(violationReportLimitString);

        // tdata should be final, but jikes doesn't know (and isn't
        // required to know) that usage() never returns.
        TechnologyData tdata = null;
        try {
            tdata = new TechnologyData(theArgs);
        } catch (CommandLineArgFormatException e) {
            System.err.println(
                "Argument to " + e.getArgName() + " is malformed: " +
                printArgValue(e.getMessage()));
            usage();
        } catch (InvalidCommandLineArgException e) {
            System.err.println(e.toString());
            usage();
        } catch (MissingCommandLineArgException e) {
            System.err.println(e.toString());
            usage();
        }

        final float fixedDelaybias =
            floatArg(theArgs.getArgValue("fixedSizeDelayBias",null),1);
        final float sizableDelaybias =
            floatArg(theArgs.getArgValue("sizableDelayBias",null),1);
        final boolean allFixedSize = theArgs.argExists("allFixedSize");
        final boolean routed = theArgs.argExists("routed");
        final boolean routedWireDebug = theArgs.argExists("routed-wire-debug");

        final FileSearchPath castPath = new FileSearchPath( castRoot ) ;
        
        if (! quiet ) {
            System.out.println("Jauto user name: " + System.getProperty("user.name", ""));
            System.out.println("\n\nJauto command line arguments summary:\n");

            System.out.println("\tJAUTO version number: " + versionNumber);

            printNS("\tCell name: ", cellName);
            printNS("\tSubtype: ", subtype);
            printNS("\tMode: ", mode);
            if (mode.equals(SIZE_MODE)) System.out.println("\tTrial sizing: " + sizeTrial);
            printNS("\tCast directory name: ", castRoot);
            printNS("\tOutput directory name: ", outRoot);
            printNS("\tSPICE file output: ", allName);
            printNS("\tCDL directory name: ", cdlRoot);
            printNS("\tLayout directory name: ", layoutRoot);
            printNS("\tTarget speed: ", targetSpeed);
            printNSD("\tCircuit unit delay : ", tau);
            printNSD("\tCircuit minimun unit delay : ", minTau);
            printNSD("\tMaximum transistor width for p-type gates: ", maxWidthP);
            printNSD("\tMaximum transistor width for n-type gates: ", maxWidthN);
            printNSD("\tMinimum transistor width for p-type gates: ", minWidthP);
            printNSD("\tMinimum transistor width for n-type gates: ", minWidthN);
            print("\tMinimum transistor width for staticizers : ",
                  tdata.getMinimumTransistorWidth());
            print("\tMinimum transistor length for staticizers : ",
                  tdata.getMinimumTransistorLength());
            print("\tDefault gate length : ", tdata.getDefaultGateLength());
            print("\tDefault diffusion length : ",
                  tdata.getDefaultDiffusionLength());
            printNSD("\tWarn for n-transistors larger than: ", nmosWarnWidth);
            printNSD("\tWarn for p-transistors larger than: ", pmosWarnWidth);
            printNSD("\tTechnology name : ", tdata.getTechnologyName());
            print("\tDelay model cut-off length for stack of N transistors: ", tdata.getStackLimitN());
            print("\tDelay model cut-off length for stack of P transistors: ", tdata.getStackLimitP());
            printNSD("\tMaximum sub-type group size : ", maxGroupSize);
            printNSD("\tUpper bound for number of paths in a leaf cell : ", pathLimit);
            printNSD("\tRelative difference threshold for concatenated path reduction : ", catPathReductionThreshold);
            print("\tDefault wire width : ", tdata.getDefaultWireWidth());
            print("\tDefault wire length : ", tdata.getDefaultWireLength());
            print("\tDefault wire spacing : ", tdata.getDefaultWireSpace());
            print("\tMinimum wire length : ", tdata.getMinimumWireLength());
            print("\tLayout scale factor X: ", tdata.getLayoutScaleX());
            print("\tLayout scale factor Y: ", tdata.getLayoutScaleY());
            print("\tUnit wire capacitance Ca : ",
                  tdata.getUnitWireCapacitanceCa());
            print("\tUnit wire capacitance Cfc_0 : ",
                  tdata.getUnitWireCapacitanceCfc_0());
            print("\tUnit wire capacitance Cfc_1 : ",
                  tdata.getUnitWireCapacitanceCfc_1());
            print("\tUnit wire capacitance Cfc_2 : ",
                  tdata.getUnitWireCapacitanceCfc_2());
            print("\tEffective unit wire resistance : ", tdata.getUnitWireResistance());
            print("\tElmore delay factor for gate delay : ", tdata.getElmoreDelayFactor());
            print("\tElmore delay factor for wire RC delay : ", tdata.getWireRCDelayFactor());
            print("\tWire resistance shielding factor : ", tdata.getResistanceShieldingFactor());
            print("\tWire resistance shielding threshold : ", tdata.getResistanceShieldingThreshold());
            print("\tScaling factor for wire capacitance : ", tdata.getCapacitanceScalingFactor());
            print("\tDefault load capacitance for spice file (not sizing) : ",
                  tdata.getDefaultLoadCapacitance());
            printYN("\tUse intrinsic cap delay model : ",
                    tdata.getUseIntrinsicCap());
            printNSD("\tDynamic energy penalty : ", dynamicEnergyPenalty);
            printNSD("\tSizing algorithm convergence threshold : ", convergeThres);
            printNSD("\tThreshold value for warning of small pre-sizing timing slack : ", slackWarningThreshold);
            printNSD("\tThreshold value for warning about large ratio between pull-up/pull-down strength : ", strengthRatioWarningThreshold);


            printYN("\tInternally sweep E*Tau^2 : ", internalSweep);
            if(internalSweep){
                printNSD("\tInternal sweep number : ", internalSweepNumber);
                printNSD("\tInternal sweep step size : ", internalSweepStep);
            }

            printYN("\tUse Tau stepping method for sizing : ", stepTau);

            printYN("\tSize staticizers : ", !noSizeStaticizers);
            printYN("\tReSize staticizers : ", reSizeStaticizers);
            printYN("\tSkip sizing stage : ", skipSizing);
            printNSD("\tSolver minimum time budget in hours: ", minSolveHours);
            printNSD("\tSolver maximum time budget in hours: ", maxSolveHours);
            printYN("\tEarly out on difficult sizing problems: ", earlyOut);
            printYN("\tRC output in CDL files : ", outputRC);
            printYN("\tReport timing analysis results after sizing : ", reportFlatPaths);
            printYN("\tReport flatterned strength after sizing : ", reportStrength);
            printYN("\tOutput Magic files for debuging geometry information : ", debugGeometry);
            printYN("\tIgnore _Reset! as port net during path generation : ", ignoreReset);
            printYN("\tEnable complete generation of concatenated paths : ", completeCatPath);
            printYN("\tEnable complete generation of sizing paths for fragment cells : ", completeSizingPath);
            printYN("\tAlways output CDL netlist, independent of sizing errors and/or warnings : ", noHaltOnWarn);
            printYN("\tEnable sizing using concatenated paths: ", enableCatPath);
            printYN("\tAutomatic re-set unsatisfiable delay constraint : ", enableAutoConstraint);
            printYN("\tPrint out port directions only : ", portDirectionOnly);
            printYN("\tCheck over-staticizing problem only : ", checkOverStaticizingOnly);
            printYN("\tBound delay constraints by minimum values : ", boundMinDelay);
            printYN("\tUse equality constraints : ", useEqualityConstraints);

            System.out.println("\tTransistor sizing method : "+
                               (pathBasedSizing?
                                "path-based sizing":"node-based sizing"));
            System.out.println("\tCapacitance scale : " + scaleC);
            System.out.println("\tResistance scale : "  + scaleR);
            System.out.println("\tTransistor scale : "  + scaleM);
            printNS("\tManufacturing grid : ", mfggrid);
            System.out.println("\tStaticizer gate name : " + staticizer);
            System.out.println("\tWeak inverter gate name : " + weakInverter);
            System.out.println("\tSmall inverter gate name : " + smallInverter);

            System.out.print("\tGates :");
            for (int i = 0; i < gateNames.length; i++)
                System.out.print(" " + gateNames[i]);
            System.out.println();

            printNS("\tSubtype path: ", subtypePath);

            System.out.print("\tGenerate stacks: ");
            if (pStacks != null && nStacks != null) {
                System.out.println("pStacks = " + pStacks + " nStacks = " + nStacks);
            } else {
                System.out.println("No.");
            }

            printNS("\tDefault layout attributes: ", layoutAttribute);
            printNS("\tPartial extract: ", partialExtract);
            printYN("\tForce fixed size for all cells: ", allFixedSize);
            printYN("\tApply routed directives: ", routed);

            System.out.println("\n\n");
        }


        /**
         * Load the cast design
         **/

        CastDesign d = null;

        SimpleProfiler.Probe parsingProbe = SimpleProfiler.getProbe("jauto.parsing");
        SimpleProfiler.Probe parsingDeallocProbe = SimpleProfiler.getProbe("jauto.parsing.dealloc");

        parsingProbe.enter();
        if(DebugOption.printLevel <= 3){
            System.out.println("Instantiating cast file parser");
        }
        final CastFileParser cfp = 
            (CastCacheManager.getDefault()).
            getCastFileParser(castPath, castVersion, false,
                              new StandardParsingOption(theArgs));
        
        if (! quiet)
            System.out.println("Reading cast files\n");
        if ( ! quiet )
            System.out.println("Loading fully qualified cell");
        final CspSizingEnv creator = disableSizingEnv ? null :
            new CspSizingEnv(
                cfp,
                new Cadencize(false),
                new HashSet<HierName>(Arrays.asList(GND, Vdd)));
        final boolean useSteinerTree =
            intArg(theArgs.getArgValue("useSteinerTree", null), 1) != 0;
        d = loadFullyQualifiedCell(cfp, cellName, Float.parseFloat(tau),
                                   fixedDelaybias, sizableDelaybias,
                                   tdata, routed,
                                   allFixedSize, Collections.EMPTY_SET,
                                   creator, useSteinerTree);

        final Collection<String> anonInsts = d.getAnonymousInstances();
        if (!anonInsts.isEmpty()) {
            System.err.println("Anonymous instances (possibly inlined or" +
                               " flattened) of non-wiring cells found" +
                               " in the following cells:");
            for (String type : anonInsts) {
                System.err.println("    " + type);
            }
            System.exit(1);
        }

        // refuse to create non-canonical subtypes
        if (mode.equals(SUBTYPE_MODE)) {
            final CellInterface realTop = d.getRealTopCell().cast_cell;
            final String map = (String) DirectiveUtils.getTopLevelDirective(
                                            realTop,
                                            DirectiveConstants.NAME_MAPPING);
            if (map != null) {
                final String full =
                    map.indexOf('.') == -1 ? realTop.getModuleName() + "." + map
                                           : map;
                if (!full.equals(realTop.getFullyQualifiedType())) {
                    System.err.println(
                            "ERROR: can't create non-canonical subtype (" +
                            realTop.getFullyQualifiedType() + " specifies " +
                            DirectiveConstants.NAME_MAPPING + " = " + full +
                            ")");
                    System.exit(1);
                }
            }
        }

        parsingProbe.exit();
        parsingDeallocProbe.enter();
        //CastCacheManager.getDefault().clearCache();
        Runtime.getRuntime().gc();
        parsingDeallocProbe.exit();


        /**
         * Set sizing parameters
         **/

        if ( ! quiet )
            System.out.println("Setting parameters");

        d.setMessageCenter(messageCenter);

        int pl = intArg(pathLimit, 500);
        double threshold = doubleArg(catPathReductionThreshold, 0.2);

        if ( ! quiet )
            System.out.println("Reading top level cell");
        
        SimpleProfiler.Probe sizingProbe = SimpleProfiler.getProbe("jauto.sizing");
        sizingProbe.enter();

        final CellType top = d.getTopLevelCell();
        /* Read in the floorplan information at this point.  Subtypes should
         * inheret this floorplan information. */
        if (layoutRoot!=null && !nofloorplan) {
            if(DebugOption.printLevel <= 3){
                System.out.println("Instantiating floorplan");
            }
            final Floorplan p = new Floorplan(layoutRoot);
            p.process(top);
        }
        sizingProbe.exit();

        if ( ! quiet )
            System.out.println("Getting minimal subtypes??");
        parsingProbe.enter();

        final UnaryPredicate<CellType> nfsPredicate;
        if (netlistFromPrs == null) {
            nfsPredicate = null;
        } else {
            final UnaryPredicate matcher =
                CellUtils.getTypeMatcher(
                        Arrays.asList(StringUtil.split(netlistFromPrs, ':')));
            nfsPredicate = new UnaryPredicate<CellType>() {
                public boolean evaluate(final CellType cell) {
                    return matcher.evaluate(cell.cast_cell);
                }
            };
        }

        final CellType newtop =
            top.instanceMinimalSubTypes
            (CastDesign.TRANSISTORS, staticizer, weakInverter, smallInverter, gateNames,
             cfp,
             nfsPredicate == null
             ? new UnaryPredicate.Constant<CellType>(false)
             : nfsPredicate,
             optimizeThreshold == null
                 ? null
                 : parseAutoThresholdOptions(optimizeThreshold));

        parsingProbe.exit();
        parsingDeallocProbe.enter();
        // XXX: test line, to get rid of cached AST
        //CastCacheManager.getDefault().clearCache();
        Runtime.getRuntime().gc();
        parsingDeallocProbe.exit();
        if (dumpcell) {
            System.out.println("Dumping cells");
            newtop.walkOnce(new CellTypeProcessor() { public void processCellType(CellType p) { p.print(); } });
            return;
        }

        // d.addFixed(CDL files);
        // d.addGlobalNets(d.getTopLevelCell());
        // FIXME: sizing, etc.

        /**
         * Just dump out the I/O directions for ports of all subtypes
         **/

        if(portDirectionOnly){
            System.out.println("\n\n------------------- Port direction information dump out -------------------\n");
            dumpPortDirections( d, new PrintWriter( System.out ) );
            System.out.println("\n\n-------------------- End of port direction dump -------------------------\n");
            
            return;
        }


        /**
         * Start of sizing process.
         **/
        TransistorSizingTool tool =
            new CGTransistorSizingTool(min_end_time,max_end_time,useEqualityConstraints);
        tool.setMessageCenter(messageCenter);

        List/*<GlobalNet>*/ listAllGlobalNets = new ArrayList/*<GlobalNet>*/();

        sizingProbe.enter();
        if ( ! quiet )
            System.out.println("Sizing");

        // set TransistorSizingTool options
        tool.setOptionUnitDelay(doubleArg(tau,-1));
        tool.setOptionSizeStaticizer(!noSizeStaticizers);
        tool.setOptionMinWidthP(doubleArg(minWidthP,-1));
        tool.setOptionMinWidthN(doubleArg(minWidthN,-1));
        tool.setOptionMaxWidthN(doubleArg(maxWidthN,-1));
        tool.setOptionMaxWidthP(doubleArg(maxWidthP,-1));
        tool.setOptionMinUnitDelay(doubleArg(minTau,-1));
        tool.setOptionNmosWarnWidth(doubleArg(nmosWarnWidth, -1));
        tool.setOptionPmosWarnWidth(doubleArg(pmosWarnWidth, -1));
        tool.setOptionSizeConvergeThreshold(doubleArg(convergeThres,-1));
        tool.setOptionSlackWarningThreshold(doubleArg(slackWarningThreshold,-1));
        tool.setOptionMaxSubtypesPerGroup(intArg(maxGroupSize,-1));
        tool.setOptionDynamicEnergyPenalty(doubleArg(dynamicEnergyPenalty,-1));
        tool.setOptionSizingMethod(pathBasedSizing ?
                                   TransistorSizingTool.PATH_BASED_SIZING_METHOD :
                                   TransistorSizingTool.NODE_BASED_SIZING_METHOD);
        tool.setOptionEnableAutoConstraint(enableAutoConstraint);
        tool.setOptionInternalSweep(internalSweep);
        tool.setOptionStepTau(stepTau);
        tool.setOptionEarlyOut(earlyOut);
        if (internalSweep) {
            tool.setOptionInternalSweepNumber(intArg(internalSweepNumber,-1));
            tool.setOptionInternalSweepStep(doubleArg(internalSweepStep,-1));
        }

        final Cadencize cad = new Cadencize(false);

        if (sizeunit){ // give unit sizes to all the transistors
            if(mode.equals(CHARGE_MODE)){
                GlobalNet.generateGlobalNets(newtop, listAllGlobalNets);
            }

            if (hsizes != null) {
                try {
                    readHalfOperatorSize(new FileReader(hsizes), d, scaleM);
                } catch (RuntimeException e) {
                    throw new RuntimeException("While parsing " + hsizes + ":" + e.getMessage());
                }
                setHalfOperatorSize(d);

                SizeStaticizers.sizeStaticizers(d);
            }
            else{
                unitSize(newtop,tool.getOptionMinWidthN(),tool.getOptionMinWidthP());
            }
        }
        else{ // Do some real sizing
            if ( ! quiet ) {
                System.err.println("\033[1;32m" + JautoUI.getCurrentTime() + " -- Starting Jauto sizing process." + "\033[0m");
                System.out.println(JautoUI.getCurrentTime() + " -- Starting Jauto sizing process.");
            }
            final MultiMap<GlobalNet,String> portNets =
                new MultiMap<GlobalNet,String>(
                    new IdentityHashMap<GlobalNet,Collection<String>>(),
                    MultiMap.<String> hashSetFactory());
            if (generateSdcConstraint) {
                GlobalNet.generateGlobalNets(newtop, listAllGlobalNets,
                    new GlobalNet.ConstructionListener() {
                        public void addCellNet(GlobalNet gn, CellNet cn) {
                            if (cn.isPortNet()) {
                                portNets.put(gn, cn.container.typeName);
                            }
                        }
                    });
            } else {
                GlobalNet.generateGlobalNets(newtop, listAllGlobalNets);
            }
            if (!noDebug) {
                JautoUI.dumpWireInformation(d, outRoot);
                if (routedWireDebug) {
                    JautoUI.dumpRoutedWireInformation(d, outRoot);
                }
            }
            if (!noDebug) JautoUI.dumpSinkSource(d, outRoot);

            // distribute instance delay bias to net sources; output possibly
            // very large debug file if requested
            final PrintWriter delayWriter =
                theArgs.argExists("debugDelayBias") ?
                    new PrintWriter(
                        new FileWriter(new File(outRoot, "delaybias.debug")))
                  : null;
            if (!theArgs.argExists("ignoreInstanceDelayBias")) {
                GlobalNet.updateDelayBias(newtop, cad, delayWriter);
            } else if (delayWriter != null) {
                delayWriter.println("Instance delaybias ignored due to " +
                                    "--ignoreInstanceDelayBias.");
            }
            if (delayWriter != null) delayWriter.close();

            if (theArgs.argExists("exitAfterSetup")) return;

            if(debugGeometry){
                GlobalNet.debugGeometry(d, outRoot);
            }

            if(checkOverStaticizingOnly){
                if ( ! quiet )
                    System.out.println("Checking design for over-staticizing problem (more than 1 staticizer on a global net)");
                checkStaticizerNumber(listAllGlobalNets);
                if ( ! quiet )
                    System.out.println("End of over-staticizing checking");
                return;
            }
            else{
                checkStaticizerNumber(listAllGlobalNets);
            }

            if (theArgs.argExists("solveForNegativeDelay")) {
                new NegativeDelayBudget(newtop, listAllGlobalNets,
                                        tool.getOptionUnitDelay());
            }

            if(!skipSizing){ // Do path generation only when necessary
                if(pathBasedSizing){
                    SizingPath.generatePaths(d.allCellTypes, ignoreReset,
                                             completeSizingPath, pl);

                    if(enableCatPath){
                        CatPath.generateCatPaths(
                                d.allCellTypes, ignoreReset, completeCatPath,
                                disableImmediateCatPathReduction);
                        CatPath.reduceCatPaths(d.allCellTypes, threshold);
                        if (delaySignOff != null) {
                            JautoUI.readDelaySignOff(d, delaySignOff);
                        }
                    }
                }
            }
            
            if(DebugOption.printLevel <= 1){
                recursivePrint(newtop);
            }
            
            tool.setOutputDirectoryName(outRoot);
            tool.setSynthesizedDesign(d);

            tool.getListAllSubtypes().addAll(d.allCellTypes);

            // Now really do some sizing
            if (!skipSizing) {
                JautoUI.dumpAllInformation(d, outRoot + File.separatorChar +
                                              "jauto.bs.info");
                String summary = tool.sizeTransistors();
                JautoUI.generateSizingSummary(summary, outRoot, -1);
            }
            else{
                tool.initializeSize(true);
            }
            
            // resize staticizers
            if (reSizeStaticizers)
                SizeStaticizers.sizeStaticizers(d);

            if(DebugOption.printLevel <= 3){
                System.out.println("End of transistor sizing process");
            }

            /**
             * Output results and debug information
             **/

            if(tool.hasUnsatisfiableConstraints){
                System.err.println("\n" + "\033[1;31m");
                System.err.println("***************************************************************");
                System.err.println("Problem contains unsatisfiable delay constraints.");
                System.err.println("Please refer to the output file for details");
                System.err.println("***************************************************************");
                System.err.println("\033[0m" + "\n");
            }

            final DelayCalculator delayCalc = new DelayCalculator(d, tool);
            delayCalc.calculateDelay();

            if (violationReportLimit > 0) {
                JautoUI.dumpViolations(d, outRoot, violationReportLimit);
            }

            if ( ! quiet )
                System.out.println("Start of debug information output process");

            if (!noDebug) {
                JautoUI.dumpSizeInformation(d, outRoot);
                JautoUI.dumpPathInformation(d, outRoot, tool.getOptionUnitDelay(), -1, proteusBudget);
                JautoUI.dumpDelayInformation(d, outRoot, tool.getOptionUnitDelay(), -1);
                JautoUI.dumpAllInformation(d, outRoot + File.separatorChar +
                                              "jauto.as.info");
                JautoUI.dumpElectromigrationInformation(d, outRoot);
                JautoUI.dumpStrengthRatioInformation
                    (d, outRoot, doubleArg(strengthRatioWarningThreshold, 4.0));
                JautoUI.dumpHierStrengthInformation(d, outRoot);
                JautoUI.checkSizes(d, tool);
                JautoUI.checkPaths(d, tool);
            }
            else if (proteusBudget)
                JautoUI.dumpPathInformation(d, outRoot, tool.getOptionUnitDelay(), -1, proteusBudget);
            if (generateDelaySignoff) JautoUI.dumpDelaySignOff(d, outRoot);

            if(reportStrength){
                JautoUI.dumpFlatStrengthInformation(d, outRoot);
            }
            
            if(reportFlatPaths){
                JautoUI.dumpFlatPathInformation(d, outRoot);
            }
            if (generateSdcConstraint) {
                JautoUI.dumpSdcConstraints(d, delayCalc, sdcDelayScale,
                                           portNets, sdcLibs, outRoot);
            }

            if ( ! quiet )
                System.out.println("End of debug information output process");
        }
        sizingProbe.exit();

        /**
         * End of Sizing process.
         **/
    
        // final CellType cell = findCell(newtop, cellName);
        // cell.print();
        if ( ! quiet ) {
            System.err.println("\033[1;32m" + JautoUI.getCurrentTime() + " -- Starting Jauto output process." + "\033[0m");
            System.out.println(JautoUI.getCurrentTime() + " -- Starting Jauto output process.");
        }

        final Double diffusionLength = new Double(d.getTechnologyData().defaultDiffusionLength);

        // if(noHaltOnWarn || !tool.hasUnsatisfiableConstraints) {
        if(true){
            final Map/*<Pair<Integer,Integer>,NetGraph>*/ stackMap;
            if (pStacks != null && nStacks != null) {
                // No need for LinkedHashMap as we never iterate over the map
                stackMap = new LinkedHashMap/*<Pair<Integer,Integer>,NetGraph>*/();
                makeStackMap(stackMap, new Integer(DeviceTypes.P_TYPE),
                             cfp, pStacks, Vdd, GND, cad);
                makeStackMap(stackMap, new Integer(DeviceTypes.N_TYPE),
                             cfp, nStacks, Vdd, GND, cad);
            } else {
                stackMap = null;
            }
            writeOut(newtop, outRoot, cellName, allName, cutoff, cutoffmin,
                     diffusionLength, outputRC, lengthUnit, scaleC, scaleR, mfggrid,
                     subtypePath, cfp, mode, subtype, sizeTrial, stackMap, cad,
                     tau, tool, layoutAttribute, d, listAllGlobalNets,
                     newSpice ? (float) tdata.getDefaultLoadCapacitance() :
                                Float.NaN,
                     minWidth, updateDirective,
                     new PartialExtract.CellPlusMinusKeyword(partialExtract, PartialExtract.Info.INCLUDE, cfp, cad),
                     tdata.getTechnologyName(), nfsPredicate);
        }

        if ( ! quiet )
            System.out.println("End of CDL output process");
    }

    static void makeStackMap(final Map/*<Pair<Integer,Integer>,NetGraph>*/ stackMap,
                             final Integer type,
                             final CastFileParser cfp, final String stacks,
                             final HierName Vdd, final HierName GND,
                             final Cadencize cad) {
        final String[] p = StringUtil.split(stacks, ':');
        for (int i = 1; i <= p.length; ++i) {
            try {
                final NetGraph stack =
                    CastDesign.getGateNetGraph(cfp, p[i - 1], Vdd, GND, cad);
                stackMap.put(new Pair(new Integer(i), type), stack);
            } catch (CastSemanticException e) {
                com.avlsi.tools.dsim.ExceptionPrettyPrinter.printException(
                        e, System.err);
                System.err.println("ERROR: Cannot parse stack/gate: " +
                                   p[i - 1]);
                System.exit(1);
            } catch (com.avlsi.prs.UnimplementableProductionRuleException e) {
                System.err.println("ERROR: Unimplementable production rule " +
                                   "in stack/gate: " + p[i - 1]);
                System.exit(2);
            }
        }
    }

    /**
     * Read half operator sizes from hsizes.debug generated by a
     * previous run of Jauto, and use those as the sizes for the half
     * operators.  This is used to speed up charge sharing (where the
     * transistor topology changes, but half operators stay the same)
     * by quickly outputting a updated netlist which can then be
     * simulated in SPICE.
     *
     * hsizes.out can contain more cells than what's being sized.
     * RuntimeException will also be thrown if a syntax error is
     * detected.
     *
     * @param r A Reader to read sizes from.
     * @param design The current design.
     **/
    static void readHalfOperatorSize(final Reader r, final CastDesign design, final double scaleM)
        throws IOException {
        final BufferedReader br = new BufferedReader(r);
        String line;
        CellType current = null;
        boolean inCell = false;
        int lineNum = 0;
        while ((line = br.readLine()) != null) {
            ++lineNum;
            if (line.startsWith("#")) {
                continue;
            } else if (line.startsWith("CELL") && !inCell) {
                final String[] tokens = StringUtil.tokenize(line);
                if (tokens.length != 3) {
                    throw new RuntimeException(lineNum + ":Syntax error: " + line);
                }
                inCell = true;
                current = (CellType) design.getCell(tokens[1]);
            } else if (line.equals("}") && inCell) {
                inCell = false;
            } else if (inCell && current != null) {
                final String[] tokens = StringUtil.tokenize(line);
                if (tokens.length != 3) {
                    throw new RuntimeException(lineNum + ":Invalid half operator size line: " + line);
                }
                final CellNet net = current.getNet(tokens[0]);
                if (net == null) continue;
                final double size;
                try {
                    size = Double.parseDouble(tokens[2]) * scaleM;
                } catch (NumberFormatException e) {
                    throw new RuntimeException(lineNum + ":Invalid size: " + line);
                }
                if (tokens[1].equals("+")) {
                    net.setUpSize(size);
                } else if (tokens[1].equals("-")) {
                    net.setDownSize(size);
                } else {
                    throw new RuntimeException(lineNum + ":Invalid direction: " + line);
                }
            }
        }
    }


    /**
     * This function is only used in the "charge sharing fix" mode.
     * For sizeable cells, it will read in effective sizes from input
     * file.  For fixed cells, it will call "initializeSize" to
     * calculate effective size for each half-operator from CDL
     * netlist.
     **/
    static void setHalfOperatorSize(final CastDesign design)
    {
        for (Iterator ita =  design.allCellTypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();
            List/*<HalfOperator>*/ lsta = cella.getListHalfOperators();

            if(!cella.isFixedSize()){
                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    hoa.resetWidth();
                }
                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    double size;
                    if (hoa.driveDirection ==
                            HalfOperator.DriveDirection.PULL_DOWN) {
                        size = hoa.outputNet.getDownSize();
                    }
                    else{ // PULL_UP
                        size = hoa.outputNet.getUpSize();
                    }
                    hoa.updateSize(size);
                }
            }
            else{
                if(DebugOption.printLevel <= 3){
                    System.out.println("Fixed-size cell found:");
                    System.out.println(cella.toString());
                }

                for (Iterator itb = lsta.iterator(); itb.hasNext(); ) {
                    HalfOperator hoa = (HalfOperator)itb.next();
                    hoa.initializeSize();
                }
            }
            cella.fixVariables();
        }
    }

    private static void dumpPortDirections( CastDesign theDesign, PrintWriter outputWriter ) {
        for (final Iterator ita = theDesign.allCellTypes.iterator(); ita.hasNext(); ) {
            final CellType sta = (CellType)ita.next();

            for (final Iterator itb = sta.getAllNets().iterator(); itb.hasNext(); ) {
                final CellNet cna = (CellNet)itb.next();

                if ( cna.isPortNet() ) {
                    outputWriter.print(sta.typeName + "\t\t" + 
                                       cna.canonicalName.getCadenceString() + "\t\t");
                    outputWriter.println
                        (CellNet.portDirectionString(cna.portDirection));
                }
            }
        }
    }
    
    private static class CspSizingEnv implements CellUtils.SizingEnvCreator {
        private class Driver {
            private final CellInterface cell;
            private final HierName outputPort;
            private final Collection<HierName> otherPorts;
            private final Map<HierName,HierName> impliedPorts;
            public Driver(final CellInterface cell,
                          final HierName outputPort,
                          final Collection<HierName> otherPorts,
                          final Map<HierName,HierName> impliedPorts) {
                this.cell = cell;
                this.outputPort = outputPort;
                this.otherPorts = otherPorts;
                this.impliedPorts = impliedPorts;
            }
        }

        private final CastFileParser cfp;
        private final Cadencize cad;
        private final Map<String,Driver> drivers;
        private final Set<String> synthCells;
        private final Set<HierName> avoidNodes;
        public CspSizingEnv(final CastFileParser cfp, final Cadencize cad,
                            final Set<HierName> avoidNodes) {
            this.cfp = cfp;
            this.cad = cad;
            this.synthCells = new HashSet<String>();
            this.avoidNodes = avoidNodes;
            this.drivers = new HashMap<String,Driver>();
        }

        private HierName toHier(final String s) {
            try {
                return HierName.makeHierName(s, '.');
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Can't create HierName: " + s, e);
            }
        }

        private Object getDirective(final CellInterface cell,
                                    final String dir,
                                    final String type,
                                    final HierName node,
                                    final AliasedSet aliases) {
            final Object defValue =
                DirectiveUtils.getTopLevelDirective(cell, dir);
            final Map values =
                DirectiveUtils.getTopLevelDirective(cell, dir, type);
            for (Iterator i = aliases.getAliases(node); i.hasNext(); ) {
                final Object val = values.get(i.next());
                if (val != null) return val;
            }
            return defValue;
        }

        private Driver getDriver(final String driverFqcn) {
            Driver result = null;
            if (drivers.containsKey(driverFqcn)) {
                result = drivers.get(driverFqcn);
            } else {
                final CellInterface driver =
                    cfp.getFullyQualifiedCellPretty(driverFqcn, System.err);
                if (driver == null) {
                    System.err.println("ERROR: Cannot parse sizing driver " +
                                       driverFqcn);
                } else {
                    final List<HierName> outPorts = new ArrayList<HierName>();
                    final Collection<HierName> otherPorts =
                        new ArrayList<HierName>();
                    final Map<HierName,HierName> impliedPorts =
                        new HashMap<HierName,HierName>();

                    (new CellUtils.MarkPort() {
                        protected void mark(final NodeType nodeType,
                                            final String name,
                                            final int direction) {
                            final HierName hname = toHier(name);
                            if (driver.isImpliedPort(name)) {
                                impliedPorts.put(
                                    hname,
                                    toHier(driver.getParentImpliedPort(name)));
                            } else if (direction > 0) {
                                outPorts.add(hname);
                            } else {
                                otherPorts.add(hname);
                            }
                        }
                    }).mark(driver);
                    final int size = outPorts.size();
                    if (size == 0) {
                        System.err.println("ERROR: Sizing driver " +
                                           driver.getFullyQualifiedType() +
                                           " has no output port");
                    } else if (size > 1) {
                        System.err.println("ERROR: Sizing driver " +
                                           driver.getFullyQualifiedType() +
                                           " has more than one output port");
                    } else {
                        result = new Driver(driver, outPorts.get(0),
                                            otherPorts, impliedPorts);
                    }
                }
                drivers.put(driverFqcn, result);
            }
            return result;
        }

        private void connectDriver(final CellInterface cell,
                                   final HierName prefix,
                                   final HierName node,
                                   final HierName instance,
                                   final String driverFqcn,
                                   final AliasedSet aliases,
                                   final Map<HierName,CellInterface> subcells) {
            final Driver driver = getDriver(driverFqcn);
            if (driver == null) {
                System.err.println("ERROR: Cannot add sizing driver on" +
                                   cell.getFullyQualifiedType() + "/" +
                                   node);
            } else {
                subcells.put(instance, driver.cell);
                aliases.makeEquivalent(
                        HierName.append(prefix, node),
                        HierName.append(instance, driver.outputPort));
                for (HierName hn : driver.otherPorts) {
                    final HierName full = HierName.append(instance, hn);
                    final NodeValue nv = new NodeValue(full);
                    aliases.add(full);
                    subcells.put(full, nv.getCell());
                }
                for (Map.Entry<HierName,HierName> entry :
                        driver.impliedPorts.entrySet()) {
                    aliases.makeEquivalent(entry.getValue(),
                            HierName.append(instance, entry.getKey()));
                }
            }
        }

        private void getSizingEnv(
                final CellInterface cell,
                final HierName prefix,
                final AliasedSet aliases,
                final Set<String> synthCells,
                final Map<HierName,CellInterface> subcells) {
            final AliasedSet locals = cad.convert(cell).getLocalNodes();
            final boolean internal = prefix == null;
            (new CellUtils.MarkPort() {
                protected void mark(final NodeType nodeType, final String name,
                                    final int direction) {
                    if (direction == 0 || ((direction > 0) == internal)) {
                        final HierName node = toHier(name);
                        if (!avoidNodes.contains(node)) {
                            final String dir =
                                internal ? DirectiveConstants.INTERNAL_DRIVER
                                         : DirectiveConstants.EXTERNAL_DRIVER;
                            final String driver = (String)
                                getDirective(cell, dir,
                                    DirectiveConstants.NODE_TYPE, node, locals);
                            if (driver != null) {
                                final HierName instance = toHier("$" + name);
                                connectDriver(cell, prefix, node, instance,
                                              driver, aliases, subcells);
                            }
                        }
                    }
                }
            }).mark(cell);
            if (synthCells != null && !subcells.isEmpty()) {
                synthCells.add(cell.getFullyQualifiedType());
            }
        }

        public void getInternalEnv(
                final CellInterface cell,
                final AliasedSet aliases,
                final Map<HierName,CellInterface> subcells) {
            getSizingEnv(cell, null, aliases, synthCells, subcells);
        }

        public void getExternalEnv(
                final CellInterface cell,
                final HierName prefix,
                final AliasedSet aliases,
                final Map<HierName,CellInterface> subcells) {
            getSizingEnv(cell, prefix, aliases, null, subcells);
        }

        public Set<String> getSynthesizedCells() {
            return synthCells;
        }
    }

    private static CellInterface createTopEnv(final CellInterface cell,
                                              final CspSizingEnv creator) {
        final AliasedSet/*<HierName>*/ aliases =
            new AliasedSet/*<HierName>*/(HierName.getComparator());
        final Map<HierName,CellInterface> subcells =
            new HashMap<HierName,CellInterface>();
        final HierName topInst = HierName.makeHierName("top");
        if (creator != null)
            creator.getExternalEnv(cell, topInst, aliases, subcells);

        final com.avlsi.cell.CellImpl result = new com.avlsi.cell.CellImpl("");
        result.setHasCompleteSubcellsBlock();
        result.addSubcellPair(topInst, cell, false);
        for (Map.Entry<HierName,CellInterface> entry : subcells.entrySet()) {
            result.addSubcellPair(entry.getKey(), entry.getValue(), false);
        }
        for (Iterator i = aliases.getCanonicalKeys(); i.hasNext(); ) {
            final HierName canon = (HierName) i.next();
            for (Iterator j = aliases.getAliases(canon); j.hasNext(); ) {
                final HierName alias = (HierName) j.next();
                result.addConnection(canon, alias);
            }
        }
        forceFixedSize(result, false);
        return result;
    }

    static CastDesign loadFullyQualifiedCell(final CastFileParser p,
                                             final String cellName,
                                             final float tau,
                                             final float fixedDelaybias,
                                             final float sizableDelaybias,
                                             final TechnologyData td,
                                             final boolean routed,
                                             final boolean allFixedSize,
                                             final Set skipCells,
                                             final CspSizingEnv creator,
                                             final boolean useSteinerTree)
            throws CastSyntaxException, CastSemanticException, IOException {

        //System.out.println("  calling CastFileParser.getFullyQualifiedCell");
        CellInterface theCell =
            p.getFullyQualifiedCellPretty(cellName, System.err);
        if (theCell == null) {
            System.err.println("ERROR: Cannot parse cell: " + cellName);
            System.exit(1);
        }
        if (routed) theCell = theCell.routedSubcells(true);
        if (creator != null)
            theCell =
                ((com.avlsi.cell.CellImpl) theCell).sizingEnvSubcells(creator);
        return loadFullyQualifiedCell(p,createTopEnv(theCell, creator),
                                      theCell, tau, fixedDelaybias,
                                      sizableDelaybias, td,
                                      allFixedSize,
                                      creator == null ?
                                          Collections.<String>emptySet()
                                        : creator.getSynthesizedCells(),
                                      skipCells, useSteinerTree);
    }

    static CastDesign loadFullyQualifiedCell(final CastFileParser p, 
                                             final CellInterface theEnv, 
                                             final CellInterface theCell, 
                                             final float tau,
                                             final float fixedDelaybias,
                                             final float sizableDelaybias,
                                             final TechnologyData td,
                                             final boolean allFixedSize,
                                             final Set<String> synthCells,
                                             final Set skipCells,
                                             final boolean useSteinerTree)
            throws CastSyntaxException, CastSemanticException, IOException {

        if (allFixedSize) forceFixedSize(theCell, true, new HashSet());
        if(DebugOption.printLevel <= 3){
            //System.out.println("PrintLevel " + DebugOption.printLevel);
            //System.out.println("  instantiating CastDesign");
        }
        final Cadencize cad = new Cadencize(true,
            new Cadencize.DefaultCallback(Cadencize.VERILOG_NONE) {
                public boolean mark(CellInterface cell, String block) {
                    return (block.equals(BlockInterface.CSP) &&
                            synthCells.contains(cell.getFullyQualifiedType()))
                        || Cadencize.NETLIST_PRIORITY.mark(cell, block);
                }
            });
        return new CastDesign(Vdd, GND, _RESET, tau, td, cad,
                              p, theEnv, theCell, fixedDelaybias,
                              sizableDelaybias, synthCells,
                              skipCells, false, false, useSteinerTree);
    }

    public static void usage() {
        System.err.println("java " + Jauto.class.getName() 
            + "\n <input .cast file> --cellName=<cellname>"
            + "\n --castInRoot=<castpath> --outRoot=<cast/report output directory>"
            + "\n --spice-output=<file> (name of SPICE output; defaults to transistors.cdl)"
            + "\n --cdlRoot=<raw cdl output directory>"
            + "\n --layoutRoot=<floorplanning directory>"
            + "\n --noFloorplan (ignores floorplanning information)"
            + "\n --staticizer=<cell> (fully qualified name of the staticizer)"
            + "\n --weak-inverter=<cell> (fully qualified name of the weak inverter)"
            + "\n --small-inverter=<cell> (fully qualified name of the small inverter)"
            + "\n --gates=<cell:...> (colon seperated list of fully qualified gate names)"
            + "\n [ --config=<configuration file name> ]"
            + "\n [ --targetSpeed=<ignored> ]"
            + "\n [ --unitSize ] (size to a constant value)"
            + "\n [ --castversion=<1 | 2> (defaults to 1) ]"
            + "\n [ --cutoff=<width> ] (cutoff width, defaults to Double.MAX_VALUE)"
            + "\n [ --cutoffmin=<width> ] (don't make width smaller than this (defaults to 0)"
            + "\n [ --defaultDiffusionLength=<length> ] (output AS, AD, PS, PD)"
            + "\n [ --outputRC ] (output wire R and C in CDL)"
            + "\n [ --noSizeStaticizers ] (don't size staticizers)"
            + "\n [ --reSizeStaticizers ] (resize staticizers only)"
            + "\n [ --lengthUnit={ mm | um | nm | pm | fm } ]"
            + "\n [ --scaleC=<scale> (scale parasitic capacitance estimate) ]"
            + "\n [ --scaleR=<scale> (scale parasitic resistance estimate) ]"
            + "\n [ --scaleM=<scale> (scale initial transistor width read from hsizes) ]"
            + "\n [ --mfggrid=<length in m> ] (round up to multiples of grid size) ]"
            + "\n [ --subtypePath=<path> ] (output sizing result as subtypes)"
            + "\n [ --mode={ spec | size | update | resize } ] (defaults to spec)"
            + "\n [ --subtype=<subtype> ] (meaning depend on mode)"
            + "\n [ --trial ] (trial sizing, only effective with --mode=size)"
            + "\n [ --pStacks=<cell:cell:...> ] (names of p-stack gates)"
            + "\n [ --nStacks=<cell:cell:...> ] (names of n-stack gates)"
            + "\n [ --hsizes=<file> ] (name of the file containing half operator sizes)"
            + "\n [ --noDebug ] (avoid writing any .debug files)"
            + "\n [ --updateDirective ] (update estimated_delay, cap even in fix sized cells)"
            + "\n [ --partialExtract=<fqcn+-> ] (output CDL for partial extraction)"
            + "\n [ --fixedSizeDelayBias=<bias> ] (apply delaybias to fixed size cells)"
            + "\n [ --sizableDelayBias=<bias> ] (apply delaybias to sizable cells)"
            + "\n [ --proteusBudget ] (creates only paths_top.debug for use by budget gen)"
            + "\n [ --debug-level=# ] (set debug level, higher is less debugging)"
            + "\n [ --quiet ] (eliminate a lot of screen dump)"
            + "\n [ --min-solve-hours=hrs] (minimum time to spend in CG solver)"
            + "\n [ --max-solve-hours=hrs] (maximum time to spend in CG solver)");
            
        System.exit(1);
    }

    static void unitSize(CellType newtop, double WN, double WP) {
        Debug.assertTrue(!newtop.manual);
        // Debug.assertTrue(!newtop.manual);
        newtop.walkOnce(new unitSizer(WN,WP));
    }

    static Format getSubtypeHeader(final String cellName,
                                   final String techName) {
        final DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM);
        final DateFormat timeFormat = DateFormat.getTimeInstance(DateFormat.LONG);
        final Date now = new Date();
        final StringBuffer header = new StringBuffer();
        header.append("'"); header.append(cellName); header.append("'");
        header.append(":");
        header.append(dateFormat.format(now));
        header.append(":");
        header.append(timeFormat.format(now));
        header.append(":");
        header.append(System.getProperty("user.name"));
        header.append("\n");
        header.append("Mode: {0}\n");
        header.append(buildVersion());
        header.append(" - ");
        header.append(versionNumber);
        header.append("\n");
        header.append("Technology: " + techName + "\n");
        return new MessageFormat(header.toString());
    }

    private static Iterator getLeafUpdate(final SortedMap typeMap, final String base) {
        final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
        Debug.assertTrue(subtypeMap != null);
        // Extract cells that are not fixed sized
        final UnaryPredicate unfixed = new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                final Pair p = (Pair) ((Map.Entry) o).getValue();
                return !CellUtils.isFixedSize((CellInterface) p.getFirst());
            }
        };
        final UnaryFunction val = new UnaryFunction() {
            public Object execute(final Object o) {
                return ((Map.Entry) o).getValue();
            }
        };
        return new MappingIterator(
                   new FilteringIterator(
                       subtypeMap.entrySet().iterator(), unfixed), val);
    }

    static void writeOut( final CellType top, 
                          final String outdir, 
                          final String cellName,
                          final String outFile, 
                          final double cutoff,
                          final double cutoffmin,
                          final Double diffusionLength,
                          final boolean outputRC,
                          final String lengthUnit,
                          final double scaleC,
                          final double scaleR,
                          final Double mfggrid,
                          final String subtypePath,
                          final CastFileParser cfp,
                          final String mode,
                          final String subtype,
                          final boolean sizeTrial,
                          final Map/*<Pair<Integer,Integer>,NetGraph>*/ stackMap,
                          final Cadencize cad,
                          final String tau,
                          final TransistorSizingTool tool,
                          final String layoutAttribute,
                          final CastDesign design,
                          final List/*<GlobalNet>*/ listAllGlobalNets,
                          final float spiceCap,
                          final float minWidth,
                          final boolean updateDirective,
                          final PartialExtract.CellPlusMinus fqcnSpec,
                          final String techName,
                          final UnaryPredicate<CellType> nfsPredicate) {
        /* Setup netlist processors according to commandline arguments */
        /* Print out AS/AD/PS/PD */
        DeviceProcessorInterface diffProc = null;
        if (diffusionLength != null) {
            diffProc = new DeviceProcessorInterface () {
                public DeviceProcessorInterface.Transistor
                processTransistor(DeviceProcessorInterface.Transistor d)
                {
                    d.parameters = new LinkedHashMap(d.parameters);
                    final double dlen = diffusionLength.doubleValue();
                    final double as = d.width * dlen;
                    final double ps = (d.width + dlen) * 2;
                    d.parameters.put("AS", new Double(as));
                    d.parameters.put("AD", new Double(as));
                    d.parameters.put("PS", new Double(ps));
                    d.parameters.put("PD", new Double(ps));
                    return d;
                }
            };
        }

        /* Scale capcitance */
        DeviceProcessorInterface scaleCProc = null;
        if (scaleC != 1.0) {
            scaleCProc = new DeviceProcessorInterface () {
                public DeviceProcessorInterface.Capacitor
                    processCapacitor(DeviceProcessorInterface.Capacitor d) {
                    d.capacitance *= scaleC;
                    return d;
                }
            };
        }

        /* Scale resistance */
        DeviceProcessorInterface scaleRProc = null;
        if (scaleR != 1.0) {
            scaleRProc = new DeviceProcessorInterface () {
                public DeviceProcessorInterface.Resistor
                    processResistor(DeviceProcessorInterface.Resistor d) {
                    d.conductance /= scaleR;
                    return d;
                }
            };
        }

        /* Round to manufacturing grid */
        final double RoundingTolerance = 1.02; // could be a PDK parameter
        final double [] RoundingMultiples = {1,2,4,6,8,12,16,24}; // for folding
        DeviceProcessorInterface mfggridProc = null;
        if (mfggrid != null) {
            final double gridsize = mfggrid.doubleValue();
            final double[] widths = design.getTechnologyData().getWidths();
            final long[] gridWidths = new long[widths.length];
            for (int i = 0; i < widths.length; ++i) {
                gridWidths[i] = Math.round(widths[i] / gridsize);
            }
            mfggridProc = new DeviceProcessorInterface() {
                HierName currentCell;
                String locationString;

                /* Funky rounding algorithm designed to choose the
                 * largest convenient multiple of gridsize without
                 * exceeding a rounding tolerance.  See BUG 11947. */
                private double round(double num) {
                    double x = 0.0;
                    for (int i = RoundingMultiples.length-1; i>=0; i--) {
                        double g = RoundingMultiples[i]*gridsize;
                        x = Math.round(num/g)*g;
                        if (x/num < RoundingTolerance && num/x < RoundingTolerance) break;
                    }
                    return x;
                }

                private double roundFixedWidths(final double num) {
                    final long grids = Math.round(num / gridsize);
                    int idx = Arrays.binarySearch(gridWidths, grids);
                    if (idx < 0) idx = -(idx + 1);
                    return idx < widths.length ? widths[idx] : Double.NaN;
                }

                private double roundLength(final double num) {
                    return round(num);
                }

                private double roundWidth(final double num) {
                    double result;
                    if (widths.length > 0) {
                        result = roundFixedWidths(num);
                        if (Double.isNaN(result)) {
                            System.err.printf(
                                "ERROR: Cannot round width %.3g (largest " +
                                "allowed is %.3g) for %s\n", num,
                                widths[widths.length - 1], locationString);
                            result = round(num);
                        }
                    } else {
                        result = round(num);
                    }
                    return result;
                }

                private double round(final double num, final boolean isWidth) {
                    return isWidth ? roundWidth(num) : roundLength(num);
                }
                    
                private void roundParam(Map m, String key, boolean isWidth) {
                    for (Iterator i = m.entrySet().iterator(); i.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) i.next();
                        final String realKey = (String) entry.getKey();
                        if (realKey.startsWith(key)) {
                            Double v = (Double) entry.getValue();
                            m.put(realKey, new Double(round(v.doubleValue(),
                                                            isWidth)));
                        }
                    }
                }

                public DeviceProcessorInterface.Transistor
                processTransistor(DeviceProcessorInterface.Transistor d) {
                    locationString = currentCell + " M" + d.name;
                    d.width = round(d.width, true);
                    d.length = round(d.length, false);
                    return d;
                }

                public DeviceProcessorInterface.Call
                processCall(DeviceProcessorInterface.Call d) {
                    locationString = currentCell + " X" + d.name;
                    roundParam(d.parameters, "NL", false);
                    roundParam(d.parameters, "NW", true);
                    roundParam(d.parameters, "PL", false);
                    roundParam(d.parameters, "PW", true);
                    return d;
                }

                public void processCell(HierName s) {
                    if (DebugOption.printLevel <= 1)
                        System.out.println("Round transistor widths in " + s);
                    currentCell = s;
                }
            };
        }

        final CellType[] trueNameCell = new CellType[] { null };
        DeviceProcessorInterface trueNameProc = new DeviceProcessorInterface() {
            public DeviceProcessorInterface.Transistor
            processTransistor(DeviceProcessorInterface.Transistor d)
            {
                if (trueNameCell[0] != null) {
                    d.type =
                        CellUtils.getTransistorModelName(
                                trueNameCell[0].cast_cell, d.type);
                }
                return d;
            }
        };

        // Setup two processors for creating SPICE netlist, one for fixed size
        // cells, and one for sizable cells
        final DeviceProcessor fixedCdlProc = new DeviceProcessor(null);
        final DeviceProcessor sizableCdlProc = new DeviceProcessor(null);
        fixedCdlProc.appendProcessor(trueNameProc);
        sizableCdlProc.appendProcessor(trueNameProc);
        if (scaleCProc != null) {
            fixedCdlProc.appendProcessor(scaleCProc);
            sizableCdlProc.appendProcessor(scaleCProc);
        }
        if (scaleRProc != null) {
            fixedCdlProc.appendProcessor(scaleRProc);
            sizableCdlProc.appendProcessor(scaleRProc);
        }
        if (mfggridProc != null) {
            // no rounding for fixed size netlists
            sizableCdlProc.appendProcessor(mfggridProc);
        }
        if (diffProc != null) {
            // should come after all processors that modify transistor widths
            fixedCdlProc.appendProcessor(diffProc);
            sizableCdlProc.appendProcessor(diffProc);
        }

        /* Setup processors to be used in output netlist block for subtypes.
         * Netlist block in a CAST should not contain AS/AD/PS/PD */
        final DeviceProcessor subtypeProc = new DeviceProcessor(null);
        if (scaleCProc != null) subtypeProc.appendProcessor(scaleCProc);
        if (scaleRProc != null) subtypeProc.appendProcessor(scaleRProc);
        if (mfggridProc != null) subtypeProc.appendProcessor(mfggridProc);

        final Format subtypeHeader = getSubtypeHeader(cellName, techName);

        final CellType cell = findCell(top, cellName);
        if (cell == null) {
            System.err.println("Cell " + cellName + " not found!");
            return;
        }

        // Store away all the arguments that are the same for convenience
        final BinaryFunction writeSubtype = new BinaryFunction() {
            public Object execute(final Object a, final Object b) {
                // XXX: Exception handling causes some annoyance.  Should think
                // of a better way to deal with this.
                try {
                    return SubtypeOutput.writeSubtype((SubtypeOutput.Policy) a,
                                                      (CellType) b,
                                                      subtypeProc,
                                                      lengthUnit,
                                                      cutoff,
                                                      cutoffmin,
                                                      stackMap,
                                                      GND,
                                                      Vdd,
                                                      minWidth);
                } catch (IOException e) {
                    return e;
                }
            }
        };

        SortedMap updateMap = null;
        final SubtypeOutput.Policy subtypePolicy;
        final Map<String,Exception> missingAlias =
            new TreeMap<String,Exception>();
        if (subtypePath == null) {
            subtypePolicy = null;
        } else if (mode.equals(SUBTYPE_MODE)) {
            subtypePolicy = null;
            final Object o = writeSubtype.execute(new SubtypeOutput.Subtype(Integer.parseInt(subtype), subtypePath, subtypeHeader, layoutAttribute, writeSubtype, design, missingAlias), cell);
            if (o instanceof IOException) {
                System.err.println("Cannot write subtype " + ((IOException) o).getMessage());
            }
        } else if (mode.equals(SIZE_MODE) || mode.equals(CHARGE_MODE)) {
            final Map globalNetMap = Collections.EMPTY_MAP;
            subtypePolicy = new SubtypeOutput.Size(subtypePath, sizeTrial, subtypeHeader, tau == null ? Float.NaN : (float) tool.getOptionUnitDelay(), globalNetMap, updateDirective);
        } else if (mode.equals(UPDATE_MODE)) {
            final String fullName = cellName + "." + subtype;
            final CellInterface updatee =
                cfp.getFullyQualifiedCellPretty(fullName, System.err);
            if (updatee == null) {
                System.err.println("ERROR: Cannot parse cell for update: " +
                                   fullName);
                System.exit(1);
            }
            updateMap = new TreeMap();
            SubtypeMerge.prepare(updatee, updateMap);
            subtypePolicy = null;
        } else {
            subtypePolicy = null;
        }

        File dir = new File(outdir);
        if (!dir.exists() && !dir.mkdir()) {
            System.err.println("Cannot write to directory " + outdir + "!");
            System.exit(1);
        }
        final File all = new File(outdir,outFile);
        Writer wall = null;
        try {
            wall = new BufferedWriter(new FileWriter(all));
        } catch (IOException e) {
            System.err.println("Cannot write to file " + all + ": " + e.getMessage());
            System.exit(2);
        }
        
        final Template templ = fqcnSpec.isEmpty() ?
            null : new Template(new LinkedHashMap());
        final CDLFactoryInterface cdlEmitter = templ == null ?
            (CDLFactoryInterface) new CDLFactoryEmitter(wall) : templ;
        final CDLFactoryInterface trueNamesEmitter =
            new Cast2Cdl.RealTransistorNames(cdlEmitter, cfp);

        final Stack dep = dependency(cell);
        final Set/*<String>*/ seen = new LinkedHashSet/*<String>*/();
        while (!dep.empty()) {
            final Object obj = dep.pop();
            if (obj instanceof CellType) {
                final CellType c = (CellType) obj;
                if (c.isWiringCell) continue;
                if (!seen.add(c.typeName)) continue;

                final CDLFactoryInterface cdlTarget;
                final Map stored = new HashMap();
                if (templ == null) {
                    /* Find all subcircuit instantiations, and emit their
                     * definitions first if they haven't been emitted already.
                     * Since the cells are processed from the bottom of the
                     * instantiation tree, the only calls that haven't already
                     * been processed must be those of gates and stacks.
                     */
                    cdlTarget = new CDLFactoryFilter(new Template(stored)) {
                        public void makeCall(HierName name, String subName,
                                             HierName[] args, Map parameters,
                                             Environment env) {
                            if (seen.add(subName)) {
                                final CellInterface ci =
                                    cfp.getFullyQualifiedCellPretty(subName, System.err);
                                if (ci == null) {
                                    System.err.println("ERROR: Cannot parse gate/stack library cell: " + subName);
                                } else {
                                    CDLOutput.writeCDL(ci, cad, trueNamesEmitter);
                                }
                            }
                            super.makeCall(name, subName, args, parameters, env);
                        }
                    };
                } else {
                    cdlTarget = templ;
                }
                final NetlistFormatter netlistEmitter =
                    new FactoryEmitter(cdlTarget, new PrintfFormat("%6.4e"));

                /* Output CDL */
                trueNameCell[0] = c;
                final DeviceProcessor cdlProc =
                    c.isFixedSize() ? fixedCdlProc : sizableCdlProc;
                CDLOutput.writeCDL(c, cdlProc, cutoff, cutoffmin, outputRC, stackMap,
                                   GND, Vdd,
                                   Float.isNaN(spiceCap) || (c == cell) ? spiceCap : -1,
                                   minWidth,
                                   netlistEmitter, (Visitor) netlistEmitter);

                if (templ == null) {
                    final Set subckts = stored.entrySet();
                    assert subckts.size() == 1;
                    for (Iterator i = subckts.iterator(); i.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) i.next();
                        final Template t = (Template) entry.getValue();
                        t.execute(trueNamesEmitter, Collections.EMPTY_MAP,
                                  NullEnvironment.getInstance(),
                                  (String) entry.getKey());
                    }
                }

                /* Output subtype */
                if (subtypePolicy != null && (nfsPredicate == null ||
                                              nfsPredicate.evaluate(c))) {
                    final Object o = writeSubtype.execute(subtypePolicy, c);
                    if (o instanceof IOException) {
                        System.err.println("Cannot write subtype " + ((IOException) o).getMessage());
                    }
                }

                /* Update mode.  Should be cleaned up. */
                if (subtypePath != null && com.avlsi.cell.CellUtils.isLeaf(c.cast_cell) && mode.equals(UPDATE_MODE)) {
                    for (Iterator i = getLeafUpdate(updateMap, c.typeName); i.hasNext(); ) {
                        final Object o = writeSubtype.execute(new SubtypeOutput.Update(subtypePath, subtypeHeader, (Pair) i.next()), c);
                        if (o instanceof IOException) {
                            System.err.println("Cannot write subtype " + ((IOException) o).getMessage());
                        }
                    }
                }
            } else {
                /* This is a gate.  We do not emit subtypes for gates. */
                final String s = (String) obj;
                if (!seen.add(s)) continue;
                final CellInterface ci =
                    cfp.getFullyQualifiedCellPretty(s, System.err);
                if (ci == null) {
                    System.err.println("ERROR: Cannot parse gate library cell: " + s);
                } else {
                    CDLOutput.writeCDL(ci, cad, trueNamesEmitter);
                }
            }
        }
        try {
            if (templ != null) {
                final PartialExtract pe =
                    new PartialExtract(templ.getTemplates(), cellName, fqcnSpec);
                pe.execute(new CDLFactoryEmitter(wall));
            }
            wall.flush();
        } catch (IOException e) {
            System.err.println("Cannot flush file " + all + ": " + e.getMessage());
        }
        if (!missingAlias.isEmpty()) {
            System.err.println(
                    "ERROR: the following " + DirectiveConstants.NAME_MAPPING +
                    " directives cannot be applied:\n");
            for (Map.Entry<String,Exception> miss : missingAlias.entrySet()) {
                final String alias = miss.getKey();
                final Exception reason = miss.getValue();
                if (reason == null) {
                    System.err.println(alias + " is not an defalias cell");
                } else {
                    System.err.print(alias + " cannot be parsed: ");
                    if (reason instanceof CastSemanticException) {
                        ExceptionPrettyPrinter.printException(
                                (CastSemanticException) reason,
                                System.err);
                    } else {
                        System.err.println(reason.getMessage());
                    }
                }
                System.err.println();
            }
            System.exit(1);
        }
    }

    static CellType findCell(final CellType top, final String name) {
        final CellType[] result = new CellType[1];
        top.walkOnce(new CellTypeProcessor() {
            public void processCellType(CellType c) {
                if (c.typeName.equals(name)) {
                    result[0] = c;
                }
            }
        });
        return result[0];
    }

    static Map getCapDelay(final CellType top) {
        final Map result = new HashMap();
        top.walkOnce(new CellTypeProcessor() {
            public void processCellType(CellType c) {
                for (Iterator i = c.getAllNets().iterator(); i.hasNext(); ) {
                    final CellNet net = (CellNet) i.next();
                    if (net.isInternalNet()) {
                        if (!result.containsKey(net.container.typeName)) {
                            result.put(net.container.typeName, new ArrayList());
                        }

                        assert net.getGlobalNets().size() == 1 :
                               "An internal CellNet has " +
                               net.getGlobalNets().size() + " GlobalNets!";

                        final HierName canon = net.getInternalCanonical();
                        assert canon != null :
                               "getInternalCanonical returns null for " + net;

                        final GlobalNet gnet =
                            (GlobalNet) net.getGlobalNets().get(0);

                        final Float up, dn;
                        if (gnet.getListSources().isEmpty()) {
                            up = dn = null;
                        } else {
                            up = new Float(net.getUpDelay());
                            dn = new Float(net.getDownDelay());
                        }

                        final Float cap = gnet.getListSinks().isEmpty() ?
                            null : new Float(gnet.getLoadCapacitance());

                        ((List) result.get(net.container.typeName)).add(new Pair(canon, new Triplet(cap, up, dn)));
                    }
                }
            }
        });
        return result;
    }

    static Stack dependency(CellType cell) {
        final Stack dep = new Stack();
        /* XXX: Assume walkMany will process the current cell, then recursively
         * traverse its children */
        cell.walkMany(new CellType.InstanceProcessor() {
            public void processInstance(CellType c, HierName instance,
                                        Collection<ConnectionInfo> path) {
                dep.push(c);
            }
        });
        return dep;
    }

    /** give everything reasonable initial sizes **/
    private static class unitSizer implements CellTypeProcessor {
        double WP,WN;
        public unitSizer(double WN, double WP) {
            this.WN = WN;
            this.WP = WP;
        }
        public void processCellType(CellType c) {
            if (c.listHalfOperators == null) return;
            for (Iterator i = c.listHalfOperators.iterator(); i.hasNext(); ) {
                HalfOperator ho = (HalfOperator) i.next();
                ho.resetWidth();
            }
            for (Iterator i = c.listHalfOperators.iterator(); i.hasNext(); ) {
                HalfOperator ho = (HalfOperator) i.next();
                ho.updateSize(ho.driveDirection==HalfOperator.DriveDirection.PULL_UP ? WP : WN);
            }
        }
    }

    public static void recursivePrint(CellType top)
    {
    
        System.out.println("*** Design summary before sizing: ");

        System.out.println("cell type name: " + top.typeName);
        System.out.println("subtype index: " + top.subtypenumber);

        System.out.println("number of subcells: " + top.getAllSubcells().size());
        System.out.println("number of cell nets: " + top.getAllNets().size());

//        System.out.println("number of half-operators: " + top.getListHalfOperators().size());


        System.out.println("\nsubcell listing:\n");
        for (Iterator ita = top.getAllSubcells().iterator(); ita.hasNext(); ) {
            CellType sta = (CellType)ita.next();
            System.out.println("cell type name: " + sta.typeName);
            System.out.println("subtype index: " + sta.subtypenumber);
        }


        System.out.println("\ncellnet listing:\n");
        for (Iterator itb = top.getAllNets().iterator(); itb.hasNext(); ) {
            CellNet cnta = (CellNet)itb.next();

            System.out.println(cnta.toString());
            System.out.println();
        }

/*
        System.out.println("\nhalf-operator listing:\n");
        for (Iterator itc = top.getListHalfOperators().iterator(); itc.hasNext(); ) {
            HalfOperator hoa = (HalfOperator)itc.next();
            System.out.println("\tvariable name: " + hoa.getVariableName());
        }
*/

        for (Iterator itd = top.getAllSubcells().iterator(); itd.hasNext(); ) {
            CellType sta = (CellType)itd.next();

            System.out.println("****************************************************");
            System.out.println("Going into: " + sta.typeName);
            System.out.println("****************************************************");
            recursivePrint(sta);
        }
    }


    /**
     * Check the number of the staticizers on all nets.  Every dynamic
     * node should have one and only one staticizer.  Due to our
     * design method, the tool could generate more than one
     * staticizers on one net, such as a shared bus.  So number
     * checking is very important procedure to make sure that no node
     * will be over-staticized, which leads to slow down or
     * mal-function.  Jlvs checks that the staticizers are correct
     * within one leaf cell.  This code doesn't!
     **/
    public static void checkStaticizerNumber(List lst1) {
        for (Iterator ita = lst1.iterator(); ita.hasNext(); ) {
            GlobalNet gna = (GlobalNet) ita.next();
            int nPullUp = 0, nPullDown = 0;
            HashSet visited = new HashSet();
            CellNet cna = gna.getTopCellNet();
            
            // find all driving half-operators of this global net
            for (Iterator itb = gna.getListSources().iterator(); itb.hasNext(); ) {
                NetSource nsra = (NetSource) itb.next();
                assert nsra.type == NetType.HALF_OPERATOR_TRANSISTOR
                    : "Globalnet should not have type-1 " +
                    "NetSource/NetSink at this stage.\n" +
                    "CellName: " + cna.container.typeName + "\n" +
                    "NetName: " + cna.canonicalName.getCadenceString();
                NetGraph.NetNode nna = nsra.source.outputNode;
                // avoid processing same node twice (TODO: a better way!)
                String name = nsra.getInstanceName() + "/" + nna.getName();
                if (visited.contains(name)) continue;
                visited.add(name);
                // look for up/down feedback in this leaf cell
                int hasPullUp = 0, hasPullDown = 0;
                for (Iterator itc = nna.getPaths().iterator(); itc.hasNext(); ) {
                    NetGraph.NetPath npa = (NetGraph.NetPath) itc.next();
                    if (npa.isFeedBack()){
                        int j = npa.getType();
                        if      (j == DeviceTypes.N_TYPE) { hasPullDown = 1; }
                        else if (j == DeviceTypes.P_TYPE) { hasPullUp   = 1; }
                    }
                }
                nPullDown += hasPullDown;
                nPullUp   += hasPullUp;
            }
            int nDrivers = visited.size();

            // check that node either has or does not have staticizer feedback
            if (!((nDrivers<=1 && nPullDown==0 && nPullUp==0) ||
                  (nPullDown==1 && nPullUp==1))) {
                String msa = "WARNING";
                String msb = "Bad staticizer\n";
                String msc = "CellName: " + cna.container.typeName + "\n"
                    + "NetName: " + cna.canonicalName.getCadenceString() + "\n"
                    + "Number of driving cells: " + nDrivers + "\n"
                    + "Number of staticizer pull-up cells:   " + nPullUp + "\n"
                    + "Number of staticizer pull-down cells: " + nPullDown + "\n";
                messageCenter.createMessage(1, 1, msa, msb, msc);
            }
        }
    }
}
