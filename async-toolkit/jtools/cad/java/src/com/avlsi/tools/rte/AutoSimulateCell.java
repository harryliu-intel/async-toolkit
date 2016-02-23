/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.rte;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import com.avlsi.util.htmlWriter.*;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.coverage.Monitor;

import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;

import com.avlsi.tools.tsim.*;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.spec.*;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.Rule;
import com.avlsi.tools.dsim.Node;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.ports.*;
import com.avlsi.tools.rte.htmlWriter.*;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.*;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.*;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import static com.avlsi.util.cmdlineargs.CommandLineArgsUtil.getFloatArgValue;
import static com.avlsi.util.cmdlineargs.CommandLineArgsUtil.getIntegerArgValue;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;

import com.avlsi.util.classloader.ConfigurableClassLoader;

/**
 * This class provides the facitliy to auto simulate the passed cell.
 * It understands directives as is defined in the castv2 spec. This
 * class is written to be used as part of Regression Test Engine, but 
 * can also be used to simulated any cell which has an `env` block in 
 * CAST. It will generate an output html file with the results of the 
 * simulation.
 * <p>
 * AutoSimulateCell understands the following directives:
 * <ul>
 * <li>Node cycle_node = {default:= null;}
 * <li>int cycle_count = {default:= 1000;}
 * <li>int cycle_time = {default:= 1000000;}
 * <li>float ntpc_spec = {default:= 18.0;}
 * <li>boolean unimpl = {default:=false;}
 * <li>boolean synchronous = {default:=false}
 * </ul>
 *
 * @author Abe Ankumah
 * @version $Revision$ $Date$
 */

public class AutoSimulateCell  {

    /** constants **/
    private static final String instanceName = "__rte_";
    private static final String envInstanceName = "_env";
    
    private static final int CYCLE_BY_NODE = 0x1;
    private static final int CYCLE_BY_TIME = 0x2;
    private static final int UNDEFINED = 0x0;
    private static final int cycleMargin = 500;
    /**
     * Time jitter used when doing performance simulations.  This is needed
     * only to get randomness from PRS rsources; should be a very small value.
     **/
    private static final float NTPC_JITTER = 0.01F;
    private static final int MIN_TCOUNT = 0x02;
    private static final float TIMED_JITTER = 0.20F;
    private String DEFAULT_CYCLE_TIME = "1000000";
    private String DEFAULT_CYCLE_COUNT = "50000";
    
    /** data members **/
    private DSim sim;
    private DSimUtil dsim_util;
    private CoSimParameters cosim;
    private String envname;
    private String cellEnvPrefix;
    private final File moduleDir;
    private String basedir;
    private String cellType;
    private String outputFileName;
    String moduleName;
    private String []path;
    private String []devicePath;
    private String cellFile;
    private Node cnode;
    private CSPProgram csp =null;
    private TreeMap tcountmap = null;
    private TreeMap<HierName,Float> ntpcslist = null;
    private TreeMap<HierName,Float> ntpcslist_signoff = null;
    private SimResults resclass = null;
    private boolean resetStressTest = false;
    
    private CellInterface cut = null;   //actual cell
    private CellInterface tenv = null;  //interface for env
    private WriteSimResults results;
    private Boolean fragment;
    private Boolean synchronous;
    private Boolean unimpl;
    private Float ntpc_spec;
    private Float timed_jitter;
    private Float cell_timed_jitter;
    private Float force_timed_jitter;
    private Boolean timed;
    private int cycle_mode = UNDEFINED;
    private int resetStresstests;
    private int resetStresscycles;
    
    private Integer default_cycle_count;
    private Integer default_cycle_time;
    private Integer force_cycle_count;
    private Integer cycle_count;
    private Integer cycle_time;
    private int historyPerNode;
    private boolean cosim_will_fail;
    private boolean rte_ignore;
    private CoSimSpec cell_rte_cosim_spec;
    private CoSimSpec rte_cosim_spec;
    private CoSimSpec cell_rte_env_cosim_spec;
    private CoSimSpec rte_env_cosim_spec;
    private HierName cycle_node;
    private boolean exception;
    
    private long dsim_rseed;
    private long totalMemory;
    private long freeMemory;
    private long usedMemory;
    
    //handling different delay models and support in dsim
    private boolean use_measured_delay = false;
    private boolean enable_digital_delay = false;
    private boolean enable_estimated_delay = false;
    private boolean enable_measured_delay = false;
    private float digitalTau;
    private float estimatedTau;
    private float measuredTau;
    private int dataSet;

    private final boolean ruleCoverage;
    
    /** Constructor **/
    public AutoSimulateCell(String modulename, String cellType, 
                            String outputFileName, String digital_delay,
                            String estimated_delay, String measured_delay,
                            String dataset, String use_measured_delay,
                            int historyPerNode, boolean ruleCoverage,
                            String []path, String []devicePath,
                            String bd, DSim dsim,
                            CastParsingOption opt){
        
        //Hack to get around directives bug. Default directives are no
        //exisitent if there's no directives block
        
        this.fragment = new Boolean("false");
        this.unimpl = new Boolean("false");
        this.synchronous = new Boolean("false");
        this.cell_timed_jitter = new Float(TIMED_JITTER);
        this.timed = new Boolean("false");
        this.default_cycle_time = new Integer(DEFAULT_CYCLE_TIME);
        this.default_cycle_count = new Integer(DEFAULT_CYCLE_COUNT);
        this.cycle_time = null;
        this.cycle_count = null;
        this.cycle_mode = CYCLE_BY_TIME;
        this.ntpcslist = new TreeMap<HierName,Float>();
        this.ntpcslist_signoff = new TreeMap<HierName,Float>();
        this.ruleCoverage = ruleCoverage;

        this.path = path;
        this.devicePath = devicePath;
        envname = null;
        basedir = bd+"/";;
        exception = false;
        this.outputFileName = outputFileName;
        
        this.cellType = modulename+"."+cellType;
        moduleName = modulename.replace('.', '/');
        this.moduleDir =
            new File(basedir+RegressionTestEngine.modules+moduleName);
        this.sim = dsim;
        this.cosim = new CoSimParameters();
        this.resclass = new SimResults();
        this.dsim_util = new DSimUtil();
        MergeDevice.clearConflicts();
        
        this.resetStresstests = 1000;
        this.resetStresscycles = 3;
        this.historyPerNode = historyPerNode;
        
        //extract and interpretate the different delay models
        this.use_measured_delay = use_measured_delay.equals("on");
        enable_digital_delay = !digital_delay.equals("off");
        enable_estimated_delay = !estimated_delay.equals("off");
        enable_measured_delay = !measured_delay.equals("off");

        if(enable_digital_delay){
            try {
                digitalTau = Float.parseFloat(digital_delay);
            } catch (NumberFormatException e) {
                enable_digital_delay = false;
                digitalTau = 1.0F;
            }
        }        
        if(enable_estimated_delay){
            try {
                estimatedTau = Float.parseFloat(estimated_delay);
            } catch (NumberFormatException e) {
                enable_estimated_delay = false;
            }
        }
        if(enable_measured_delay){
            try {
                dataSet = Integer.parseInt(dataset);
                measuredTau =  Float.parseFloat(measured_delay);
            } catch (NumberFormatException e) {
                enable_measured_delay = false;
            }
        }
         
        //get the memory useage here 
        this.totalMemory = Runtime.getRuntime().totalMemory() / 1024;
        //get the initial amount of memory used in case something goes wrong
        this.freeMemory = Runtime.getRuntime().freeMemory() / 1024;

        try {
            
            /** get ready to find and parse CAST **/
            FileSearchPath cFSP = new FileSearchPath(path);
            CastFileParser cfp = new CastFileParser(cFSP,"2", opt);

            DSim.get().enableFileCache(cfp);
            this.cut = cfp.getFullyQualifiedCell(modulename, cellType);

            /** set the devicePath params **/
            if(devicePath != null){
                FileSearchPath dPath = new FileSearchPath(devicePath);
                ClassLoader deviceLoader = new ConfigurableClassLoader(dPath);
                this.sim.setDeviceLoader( deviceLoader );
            }
            
            /** Garbage collect **/
            cfp = null;
            System.gc();
        
            cellFile =  moduleName + ".cast";
            sim.setCastPath(cFSP);
            
        }
        /** Something bad happend; set flag and print a short messgae **/
        catch(CastSemanticException e){
            exception = true;
            com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                .printException(e, System.out);
            com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                .printException(e, System.err);
        }
        catch(Exception e) {
            exception = true;
            System.out.println(e.toString());
            e.printStackTrace();
        }
        
        //Determine depth of results page
        int depth=2;
        for(int i=0; i < moduleName.length(); i++)
            if(moduleName.charAt(i) == '/')depth++;
        
        //Create the output director and create the file to write output to
        try {
            results = new WriteSimResults(moduleDir+"/"+outputFileName+".html");
        }
        catch(Exception e){
            exception = true;
            System.out.println(e.toString());
            e.printStackTrace();
        }
        
        
        if(!exception){
            /** Initialize page; and printCellInformation **/
            results.init(cut.getFullyQualifiedType(), this.basedir, depth);
            printCellInfo();
        }
        else {
            results.init(cellType, this.basedir, depth);
            results.println("AutoSimulateCell Exception");
            results.br();
        }        
        
        //if everything is succesful this is the amount of memory used
        this.totalMemory = Runtime.getRuntime().totalMemory() / 1024;
        this.freeMemory = Runtime.getRuntime().freeMemory() / 1024;
    }
    
    //
    // UTILITY ROUTINES; 
    //
    
    /** list all the nodes whose names begin with a given prefix, this allows
     * us to ignore nodes used in the environment, the set returned is used 
     * in coverage space merging.
     **/
    public void getAllNodes(String prefix){
        
        HashSet allnodes = new HashSet();
        Node node;
        Vector names = new Vector();
        sim.expandNodes("*", names, false, true);
        Enumeration en = names.elements();
        while (en.hasMoreElements()) {
            HierName name = (HierName)en.nextElement();
            if(name.toString().startsWith(prefix)){
                node = sim.findNode(name);
                if(node != null)
                    allnodes.add(node);
                else {
                    System.out.println("Node coverage information may "
                                       +" be inaccurate\n");
                }
            }
        }
        
        Iterator i = allnodes.iterator();
        while(i.hasNext()){
            node = (Node)i.next();
            String str = "#" +String.valueOf(node.getTCount()) 
                +" "+node.toString();
            System.out.println(str);
            results.println(str);
            results.br();
        }
        
    }

    /**
     * Print the passed <code>str</code> to stdout and the results page
     * <code>results</code>
     **/
    public void printHtmlStdout(String str, boolean br){
        if(str != null){
            System.out.println(str);
            results.println(str);
        }
        if(br)results.br();
    }
    
    /**
     * prints all unimplemented cells;
     */
    public String printUnImplCells(HashSet unimpl)
    {
        String ucells="";
        if(unimpl.isEmpty())return "None.";
        else {
            Iterator i = unimpl.iterator();
            while(i.hasNext()){
                CellInterface cell = (CellInterface)i.next();
                ucells += "<a href=\""+cell.getModuleName().replace('.','/')
                    +"\">"+cell.getType();
                if(i.hasNext())ucells += "</a>, ";
                else ucells += "</a>.";
            }
            return ucells;
        }
    }
    
    /**
     *  This method goes through the standard reset sequece for a cell. It 
     *  sets timed random and timed_jitter according what is specified in 
     *  the directives block.  
     **/
    
    /**
     * set the amount of jitter for a timed_random simulation; timed_delay
     * range; the default value requested in 0.33.
     **/
    public void setJitter(){
        final Float jitter = force_timed_jitter == null ? this.timed_jitter
                                                        : force_timed_jitter;
        sim.setTimedRandom(1F-jitter.floatValue(), 1F+jitter.floatValue());
        printHtmlStdout("[INFO] Random timing delay variation "
                        +sim.getTimedRandom(), true);
    }
    
    /**
     * This method goes through the `standard` <code>reset</code> sequece 
     * for a cell. 
     * 
     * @param randomType Value passed to <code>DSim.setRandom</code> can be
     *     either <code>NO_RANDOM, UNTIMED_RANDOM, TIMED_RANDOM </code>. The 
     *     value passed depends on whether the cell has a cycle_node or is 
     *     a synchronous cell.
     *
     * (See
     * {@link http://internal.avlsi.com/tree/hw/doc/...}
     * for how these heuristics are derived. )
     *
     **/
    public boolean reset(int randomType){
        
        /** Set random string for reporting **/
        String randstr="UNDEFINED";
        if(randomType == DSim.NO_RANDOM){ randstr= "NO_RANDOM"; }
        else if(randomType == DSim.UNTIMED_RANDOM){ randstr= "UNTIMED_RANDOM";}
        else if(randomType == DSim.TIMED_RANDOM ){ randstr= "TIMED_RANDOM"; }
        
        /** enable standard warnings **/
        sim.setWarn(true);
        sim.setError(true);
        sim.showTCounts(true);
        
        sim.setRandom(randomType);
        printHtmlStdout("[INFO] DSim Random on (" +randstr +").", true);
        
        DSimUtil dutil = new DSimUtil();
        dutil.resetDSim(DSimUtil.STANDARD_RESET);

        boolean unstab = !sim.getNodesWithValue(Node.VALUE_U).isEmpty();
        if(unstab)
            printHtmlStdout("[FAIL] Unstable nodes detected out of reset",
                            true);
        else
            printHtmlStdout("[INFO] Done with Reset",true);
        return unstab;
    }
    
    private Float useDef(final Float val, final Float def) {
        return val == null ? def : val;
    }

    private HierName getSimName(final HierName name) {
        return HierName.append(HierName.makeHierName(instanceName), name);
    }

    /** process and save top-level directives **/
    public void procDirectives(DirectiveBlock db){
        
        try {
            timed = (Boolean)db.lookup(DirectiveConstants.TIMED);
            cell_timed_jitter =
                useDef((Float)db.lookup(DirectiveConstants.TIMED_JITTER),
                       cell_timed_jitter);

            this.fragment = (Boolean)db.lookup(DirectiveConstants.FRAGMENT);
            this.synchronous = (Boolean)db.lookup(DirectiveConstants.SYNCHRONOUS);
            this.unimpl = (Boolean)db.lookup(DirectiveConstants.UNIMPL);
            this.rte_ignore = ((Boolean)db.lookup(DirectiveConstants.RTE_IGNORE)).
                booleanValue();
            this.cell_rte_cosim_spec =
                (CoSimSpec) db.lookup(DirectiveConstants.RTE_COSIM_SPEC);
            this.cell_rte_env_cosim_spec =
                (CoSimSpec) db.lookup(DirectiveConstants.RTE_ENV_COSIM_SPEC);
            
            //deal with directive implications synchronous implies timed
            if(this.synchronous.booleanValue())
                timed = this.timed = new Boolean("true");
        }
        catch(Exception e) {
            //e.printStackTrace();
        }
    }
    
    /** process and save prs directives **/
    public void procENVDirectives(DirectiveBlock db){
        
        //System.out.println("Processing ENV Directives");
        try {

            if((Integer)db.lookup(DirectiveConstants.CYCLE_TIME) != null){
                this.cycle_time = (Integer)
                    db.lookup(DirectiveConstants.CYCLE_TIME);
                this.cycle_mode = CYCLE_BY_TIME;
            }
            
            //new directives to control RTE modes
            if((Boolean)db.lookup(DirectiveConstants.RTE_IGNORE) != null){
                this.rte_ignore = ((Boolean)
                                   db.lookup(DirectiveConstants.RTE_IGNORE)).
                    booleanValue();
            }
            if (db.lookup(DirectiveConstants.RTE_COSIM_SPEC) != null) {
                this.rte_cosim_spec =
                    (CoSimSpec) db.lookup(DirectiveConstants.RTE_COSIM_SPEC);
            }
            if (db.lookup(DirectiveConstants.RTE_ENV_COSIM_SPEC) != null) {
                this.rte_env_cosim_spec =
                    (CoSimSpec) db.lookup(DirectiveConstants.RTE_ENV_COSIM_SPEC);
            }
            if((Boolean)db.lookup(DirectiveConstants.CSP_COSIM_WILL_FAIL) != null){
                this.cosim_will_fail = 
                    ((Boolean)db.lookup(DirectiveConstants.CSP_COSIM_WILL_FAIL)).
                    booleanValue();
            }

            //over-ride default cycle count
            if (force_cycle_count == null) {
                if((Integer)db.lookup(DirectiveConstants.CYCLE_COUNT) != null){
                    this.cycle_count = (Integer)
                        db.lookup(DirectiveConstants.CYCLE_COUNT);
                }
                else if(this.cut.containsCompletePrs()){
                    //if the cell is a leaf cell cycle by a factor of 10 more
                    this.cycle_count=new 
                        Integer(+(this.cycle_count.intValue()*10));
                }
            } else {
                this.cycle_count = force_cycle_count;
            }
            
            if((Integer)db.lookup(DirectiveConstants.RESET_CYCLE_COUNT) != null){
                //over-ride default value for reset stress test
                this.resetStresstests = 
                    ((Integer)db.lookup(DirectiveConstants.RESET_CYCLE_COUNT))
                    .intValue();
            }
            else if(cut.containsCompletePrs()){
                this.resetStresstests *= 10;
            }
            
            this.cycle_node=(HierName)db.lookup(DirectiveConstants.CYCLE_NODE);
            
            if(this.cycle_count != null && this.cycle_node != null)
                this.cycle_mode = CYCLE_BY_NODE;
            
            timed = (Boolean)db.lookup(DirectiveConstants.TIMED);
            timed_jitter =
                useDef((Float)db.lookup(DirectiveConstants.TIMED_JITTER),
                       timed_jitter);
            
            //deal with directive implications synchronous implies timed
            if(this.synchronous.booleanValue())
                timed = this.timed = new Boolean("true");
            
            Map<HierName,Float> ntpclist = (Map<HierName,Float>)
                DirectiveUtils.scaleNtpcSpec(cut,
                                db.getValues(DirectiveConstants.NTPC_SPEC, 
                                             DirectiveConstants.NODE_TYPE));
            Map<HierName,Float> ntpclist_signoff = (Map<HierName,Float>)
                DirectiveUtils.scaleNtpcSpecSignoff(cut,ntpclist);
                                                         
            if(ntpclist.size() > 0){
                resclass.setNTPCspec(true);
            }
            else { 
                results.println("No ntpc directive specified");
                results.br();
                resclass.setNTPCspec(false);
            }
            
            this.ntpcslist.putAll(ntpclist);
            this.ntpcslist_signoff.putAll(ntpclist_signoff);
            if (!this.ntpcslist.isEmpty()) {
                if(this.cycle_node == null) {
                    this.cycle_node = this.ntpcslist.firstKey();
                    if(this.cycle_count != null && this.cycle_node != null)
                        this.cycle_mode = CYCLE_BY_NODE;
                }
            }
        }
        catch(Exception e) {
            //REVIEW: why isn't this making its way into the stats page
            //e.printStackTrace();
            results.println("++++++++++ Error in Directives block ++++++++++");
            results.br();
            
        }
        return;
    }
    
    //this routine simply enables measured delay mesurements
    public void enableModifiedDelays(){
        if(enable_digital_delay)
            sim.enableDelayMode(DSim.DIGITAL_TAU, digitalTau);
        if(enable_estimated_delay)
            sim.enableDelayMode(DSim.ESTIMATED_TAU, estimatedTau);
        if(enable_measured_delay){
            sim.setMeasureDataSet(dataSet);
            sim.enableDelayMode(DSim.MEASURED_TAU, measuredTau);
        }
        if(enable_measured_delay)
            printHtmlStdout("<br>[INFO] Measuring NTPC on specified nodes "
                            +"(measured_delays)", true);
        else if(enable_estimated_delay)
            printHtmlStdout("<br>[INFO] Measuring NTPC on specified nodes "
                            +"(estimated_delays)", true);
        else if(enable_digital_delay)
            printHtmlStdout("<br>[INFO] Measuring NTPC on specified nodes "
                            +"(digital_delays)", true);
    }

    public void resetModifiedDelays(){
        sim.setRandom(DSim.NO_RANDOM);
        sim.disableDelayMode(DSim.ESTIMATED_TAU);
        sim.disableDelayMode(DSim.MEASURED_TAU);
        sim.disableDelayMode(DSim.DIGITAL_TAU);
    }
    
    private void rteCosimulate(
            final String cellType,
            final String instanceName,
            final String envName,
            final String envInstanceName,
            final boolean leafOnly,
            CoSimSpec rteCosimSpec,
            CoSimSpec rteEnvCosimSpec)
        throws Exception {
        rteCosimulate(cellType, instanceName, envName, envInstanceName,
                      leafOnly, rteCosimSpec, rteEnvCosimSpec,
                      new CoSimParameters());
    }

    private void rteCosimulate(
            final String cellType,
            final String instanceName,
            final String envName,
            final String envInstanceName,
            final boolean leafOnly,
            CoSimSpec rteCosimSpec,
            CoSimSpec rteEnvCosimSpec,
            CoSimParameters coSimParams)
        throws Exception {
        final CoSimSpec defaultSpec =
            new CoSimSpec(new ModeListLevelSpec(new ModeList(
                            new Mode[] {
                                Mode.PRS, Mode.SUBCELLS, Mode.CSP, Mode.JAVA
                            })),
                          new InstSpecList(new InstSpec[0]));
        if (rteCosimSpec == null) rteCosimSpec = defaultSpec;
        if (rteEnvCosimSpec == null) rteEnvCosimSpec = defaultSpec;

        final CoSimSpecList coSimSpecList;
        if (leafOnly) {
            // prs,subcells,csp,java
            coSimSpecList = new CoSimSpecList(new CoSimSpec[]{rteCosimSpec});
        } else {
            // java,csp,subcells,prs | prs,subcells,csp,java
            coSimSpecList = new CoSimSpecList(new CoSimSpec[]{
                    new CoSimSpec(new ModeListLevelSpec(new ModeList(
                                new Mode[] {
                                    Mode.JAVA, Mode.CSP,
                                    Mode.SUBCELLS, Mode.PRS
                                })),
                              new InstSpecList(new InstSpec[0])),
                    rteCosimSpec
            });
        }

        printHtmlStdout("[INFO] Cosim spec: " + cellType + coSimSpecList +
                        ":" + envName + "{" + rteEnvCosimSpec + "}", true);

        sim.cosimulate(cellType, instanceName, coSimSpecList, envName,
                       envInstanceName, rteEnvCosimSpec, coSimParams);
    }

    private void criticalAnalysis(final HierName node,
                                  final boolean addPrefix) throws IOException {
        final boolean quiet = addPrefix;
        final String prefix = addPrefix ? cellEnvPrefix + ":" + node
                                        : cellEnvPrefix;
        final String forNode = addPrefix ? " for " + node : "";
        final File criticalFile = new File(moduleDir, prefix + ".critical.gz");
        final PrintWriter criticalWriter =
            new PrintWriter(
                new GZIPOutputStream(
                    new FileOutputStream(criticalFile)));
        printHtmlStdout("[INFO] Critical path report" + forNode + ": " +
                        criticalFile, true);
        this.sim.listNodeEnablers(getSimName(node), true, criticalWriter);
        criticalWriter.close();

        final ArrayList<DSim.HistoryRecord> cycle =
            new ArrayList<DSim.HistoryRecord>();
        this.sim.findCriticalCycle(getSimName(node), cycle);
        if (cycle.isEmpty()) {
            printHtmlStdout("[WARN] No critical cycle identified" + forNode,
                            true);
        } else {
            final File cycleFile =
                new File(moduleDir, prefix + ".critical_cycle");
            final PrintWriter cycleWriter =
                new PrintWriter(new FileOutputStream(cycleFile));
            printHtmlStdout("[INFO] Critical cycle report" + forNode + ": " +
                            cycleFile, true);
            final boolean estimated =
                sim.useDelayMode(DSim.ESTIMATED_TAU);
            boolean allMeasured = true;
            boolean allBudget = true;
            DSim.HistoryRecord prev = null;
            float totalBudget = 0.0f;
            float totalDelay = 0.0f;
            for (final DSim.HistoryRecord h : cycle) {
                cycleWriter.println(h.toString(true, estimated, true));
                final int budget = h.getDelay();
                if (prev != null) {
                    final long delay = prev.time - h.time;
                    if (budget < 0) {
                        allBudget = false;
                        // no prs budget, because this is a CSP transition;
                        // assume budget is the same as the actual delay
                        totalBudget += delay;
                    } else {
                        totalBudget += budget;
                    }
                    totalDelay += delay;
                }
                if (h.delay_type != DSim.MEASURED_TAU)
                    allMeasured = false;
                prev = h;
            }
            printHtmlStdout("[" + (allMeasured ? "INFO" : "WARN") +
                            "] Critical cycle" + forNode + " contains " +
                            (allMeasured ? "only " : "non-") +
                            "measured transitions", true);
            final float tau = (totalDelay / totalBudget) * digitalTau;
            printHtmlStdout("[INFO] Effective tau of critical cycle" +
                            forNode + ": " + String.format("%.3f", tau),
                            true);
            cycleWriter.close();
        }
    }

    //this routine measures the ntpc spec on the specifed environment
    public boolean performanceSimulation(boolean modified_delays)
        throws Exception
    {
        boolean error = false;
        DSimUtil utils = new DSimUtil();
        if(modified_delays){
            
            printHtmlStdout("[INFO] DIGITAL_TAU: "+digitalTau 
                            +", ESTIMATED_TAU: "+estimatedTau 
                            +", MEASURED_TAU: "+measuredTau 
                            +", DATASET: "+dataSet, true);
            enableModifiedDelays();
        }
        else 
            printHtmlStdout("<br>[INFO] Measuring NTPC on specified nodes"
                            +"(prs delays)", true);
        
        sim.setTimedRandom(1 - NTPC_JITTER, 1 + NTPC_JITTER); 
        reseed();
        rteCosimulate(cellType, instanceName, this.envname, envInstanceName,
                      true, rte_cosim_spec, rte_env_cosim_spec);
        
        error |= reset(DSim.TIMED_RANDOM);
        this.cnode = this.sim.findNode(getSimName(cycle_node));
        for (HierName measurenode : ntpcslist.keySet()) {
            System.out.println("Measuring ntpc on " +measurenode);
            if (!utils.addMeasure(getSimName(measurenode).toString())) {
                printHtmlStdout("[WARN] Node " +measurenode 
                                +" does not exisit", true);
            }
        }
        utils.setMeasureVerbosity(true);
        
        if(this.cnode == null){                
            printHtmlStdout("<br>[FATAL] cycle_node:  "+cycle_node
                            +" doesn't exist; simulation will fail!", 
                            true);
        } else {
            printHtmlStdout("[INFO] cycling on " + cycle_node +" for "
                            +cycle_count.intValue() +" transitions", true);
        }
        //run the simulation for it to get to the steady state
        this.sim.cycle(this.cnode, (int)(cycle_count.intValue()*0.1));
        
        //clear the ntpc spec reading taken
        utils.restartStats();
        
        // record history for critical path determination
        if (enable_measured_delay) this.sim.setHistoryPerNode(historyPerNode);

        //things should have settled by now so now measure new ntpc
        int cv = cycle_count.intValue()- (int)(cycle_count.intValue()*0.1);
        this.sim.cycle(this.cnode, cv);

        error |= checkAnomalousState();
        
        printHtmlStdout("[INFO] " +(utils.printMeasureStats()).
                        replaceAll("\n","<br>"),true);
        getNTPCValues(ntpcslist,ntpcslist_signoff,utils, modified_delays);
                      
        if (enable_measured_delay) {
            try {
                criticalAnalysis(cycle_node, false);
                for (HierName node : ntpcslist.keySet()) {
                    // warning for missing node already given when measuring
                    // NTPC, so don't warn again
                    if (!node.equals(cycle_node) &&
                        this.sim.findNode(getSimName(node)) != null) {
                        criticalAnalysis(node, true);
                    }
                }
            } catch (IOException e) {
                printHtmlStdout(
                    "[WARN] Cannot get critical path/cycle information: " +
                    e.getMessage(), true);
            }
        }

        results.br();
                
        //remove all the associated stuff with this
        sim.rm_instantiation();
        return error;
    }

    /** check to see if any anamolous event occured **/
    public boolean checkAnomalousState() {

        //System.out.println("Checking Anomalous state");
        DSim.AnomalousEvent e1 = sim.lastInterferenceEvent();
        DSim.AnomalousEvent e2 = sim.lastGlitchEvent();
        DSim.AnomalousEvent e3 = sim.lastUnstabEvent();

        if (e1 != null) {
            results.print("[FAIL] Node "+e1.rule.target()+" went into "+
                          "interference at time "+e1.time);
            if (e1.node != null)
                results.println(" caused by "+e1.node);
            else results.println("");
        }
        if (e2 != null) {
            results.print("[FAIL] Node "+e2.rule.target()+" glitched "+
                          "at time "+e2.time);
            if (e2.node != null)
                results.println(" caused by "+e2.node);
            else results.println("");
            
        }
        if (e3 != null) {
            results.print("[FAIL] Node "+e3.rule.target()+" went unstable "+
                          "at time "+e3.time);
            if (e3.node != null)
                results.println(" caused by "+e3.node);
            else results.println("");
            
        }
        if (e1 == null && e2 == null && e3 == null) {
            results.println("[PASS] No interference, glitch, "
                            +"or instability detected.");
            results.br();
            return false;
        }
        //we had an error
        results.br();
        return true;
    }

    
    /** print all the merge device with conflicts **/
    public boolean checkMergeDevices(){
        
        Vector cm = MergeDevice.conflictingMerges();
        
        if(cm != null){
            if(cm.isEmpty()){
                System.out.println("There were no data conflicts");
                results.println("[PASS] No data conflicts detected");
                results.br();
                return false;
            }
            else {
                
                for(int i=0; i < cm.size(); i++){
                    MergeDevice device = (MergeDevice)cm.get(i);
                    printHtmlStdout("[FAIL] CoSim Failure on " 
                                    +device.getFullname() +" device (" 
                                    +device.firstConflict().toString() +")",
                                    true);
                }
                //clear the conflicts found associated witt this run
                MergeDevice.clearConflicts();

            }
            
            return true;
        }
        else { return false; }
    }
    
    /** check for potential deadlock **/
    public boolean checkDeadlock(long start_time, boolean status){
        
        /* if we haven't already detected an error condition, we check for 
         * deadlock. Just make sure that we got to where we wanted by cycling
         */
        if(!status){
            if(cycle_mode == CYCLE_BY_TIME){
                long desired = start_time + cycle_time;
                if(sim.getTime() < desired - cycleMargin || 
                   sim.getTime() > desired + cycleMargin ){
                    final String msg =
                        "FATAL: cycle by time terminated prematurly -- " +
                        "potential deadlock. Desired time: " + desired +
                        " actual time: " + sim.getTime();
                    results.println(msg);
                    results.br();
                    System.out.println(msg);
                    return true;
                }
            }
            if(cycle_mode == CYCLE_BY_NODE)
                if(cnode.getTCount() != cycle_count.intValue()){
                    final String msg =
                        "FATAL: cycle on " + cnode.getName() +
                        " terminated prematurly -- potential deadlock. " +
                        "Desired cycle count: " + cycle_count +
                        " actual cycle count: " + cnode.getTCount();
                    results.println(msg);
                    results.br();
                    System.out.println(msg);
                    return true;
                }
        } 
        
        return (false | status);
    }
    
    /**
     * removes nodes in the ignore coverage list from the tcountmap
     * Implementation should suffice now; explore better solution ...
     **/
    public void ignoreCoverage(){

        /** FIX ME, make this a class member at some point **/
        HashSet ignoreCoverageOnNodes = new HashSet();
        HashSet nodes2Delete = new HashSet();
        
        ignoreCoverageOnNodes.add("._RESET");
        ignoreCoverageOnNodes.add("._Reset");
        ignoreCoverageOnNodes.add(".Reset");
        ignoreCoverageOnNodes.add(".Vdd");
        ignoreCoverageOnNodes.add(".GND");
                
        
        Iterator ti = tcountmap.keySet().iterator();

        HierName nodeName;
        while(ti.hasNext()){
            nodeName = (HierName)ti.next();
            Iterator i = ignoreCoverageOnNodes.iterator();
            while(i.hasNext()){
                if(nodeName.toString().endsWith((String)i.next())){
                    nodes2Delete.add(nodeName);
                }
            }
        }
        
        /** need to do this to avoid concurrent modification of tcountmap **/
        Iterator i = nodes2Delete.iterator();
        while(i.hasNext()){
            tcountmap.remove((HierName)i.next());
        }
    }
    
    public void setDefaultCycleCount(int n){
            this.default_cycle_count = new Integer(n);
    }

    public void setForceCycleCount(Integer n) {
        this.force_cycle_count = n;
    }
    
    public void setDefaultCycleTime(int n){
            this.default_cycle_time = new Integer(n);
    }

    public void setForceTimedJitter(Float n) {
        this.force_timed_jitter = n;
    }

    private String ruleCoverage() {
        String covStr = "";

        final File coveredRules =
            new File(moduleDir, cellEnvPrefix + ".covered_rules.gz");
        covStr += "\n<BR>[INFO] Covered rules report file: " + coveredRules;

        final File uncoveredRules =
            new File(moduleDir, cellEnvPrefix + ".uncovered_rules.gz");
        covStr += "\n<BR>[INFO] Uncovered rules report file: " + uncoveredRules;

        try {
            final Writer coveredWriter =
                new BufferedWriter(
                    new OutputStreamWriter(
                        new GZIPOutputStream(
                            new FileOutputStream(coveredRules))));
            final Writer uncoveredWriter =
                new BufferedWriter(
                    new OutputStreamWriter(
                        new GZIPOutputStream(
                            new FileOutputStream(uncoveredRules))));
            final DSim.RuleCoverageStatistics stats =
                sim.reportRuleCoverage(
                    new DSim.RuleCoverageCallback() {
                        Writer w;
                        public boolean want(final Rule r,
                                            final boolean alwaysOn,
                                            final boolean alwaysOff) {
                            w = null;
                            if (r.coverageRequirement != Rule.IGNORE_COVERAGE &&
                                !alwaysOn && !alwaysOff) {
                                w = r.transitionCount == 0 ? uncoveredWriter
                                                           : coveredWriter;
                            }
                            return w != null;
                        }
                        public void rule(String r) throws IOException {
                            w.write(r);
                            w.write('\n');
                        }
                    }, false, false);
            final long coverable =
                stats.totalRules - stats.ignoredRules - stats.constantRules;
            final double ruleCoverage =
                100.0 * ((double) stats.coveredRules) / coverable;
            covStr += "\n<BR>[INFO] Total rules: " + stats.totalRules +
                      "\n<BR>[INFO] Ignored rules: " + stats.ignoredRules +
                      "\n<BR>[INFO] Constant rules: " + stats.constantRules +
                      "\n<BR>[INFO] Covered rules: " + stats.coveredRules +
                      "\n<BR>[INFO] Rule coverage: " +
                      (coverable == 0 ? "NA"
                                      : String.format("%.2f%%", ruleCoverage));
            coveredWriter.close();
            uncoveredWriter.close();
        } catch (IOException e) {
            covStr += "\n<BR>[WARN] Cannot get rule coverage information: " +
                e.getMessage();
        }

        return covStr;
    }

    /** perform coverage analysis **/
    public void coverageAnalysis(){
        int coverage = 0;
        boolean coverage_error = false;
        Node node;
        HierName hname;
        
        ignoreCoverage();
        Iterator i = tcountmap.keySet().iterator();
        while(i.hasNext()){
            hname = (HierName)i.next();
            node = sim.findNode(hname);
            if(node == null){
                coverage_error = true;
                continue;
            }
            if(node.getTCount() >= MIN_TCOUNT){coverage++;}
            
        }
        String covStr = "[INFO] Node coverage: " 
            +NumberFormatter.format(100.0*(double)coverage/tcountmap.size(),2) 
            +"%";
        if(coverage_error)
            covStr += "<BR>[WARN] coverage numbers might not be accurate"
                +"; nodes missing in DSim";

        if (ruleCoverage) {
            covStr += ruleCoverage();
        } else {
            covStr += "\n<BR>[INFO] Rule coverage disabled";
        }

        final File cspCov =
            new File(moduleDir, cellEnvPrefix + ".csp_coverage.zip");
        covStr += "\n<BR>[INFO] CSP coverage database: " + cspCov;

        final File cspMissed =
            new File(moduleDir, cellEnvPrefix + ".csp_misses.gz");
        covStr += "\n<BR>[INFO] CSP missed probes report: " + cspMissed;

        try {
            Monitor.global.setPath(cspCov.getPath());
            Monitor.global.save();

            final PrintStream missStream =
                new PrintStream(
                    new GZIPOutputStream(
                        new FileOutputStream(cspMissed)));
            final int misses = Monitor.global.listMisses(missStream);
            final int entries = Monitor.global.totalEntries();
            final double cspCoverage = 100.0 * (entries - misses) / entries;
            covStr += "\n<BR>[INFO] Total probes: " + entries +
                      "\n<BR>[INFO] Missed probes: " + misses +
                      "\n<BR>[INFO] CSP coverage: " +
                      (entries == 0 ? "NA"
                                    : String.format("%.2f%%", cspCoverage));
            missStream.close();
            if (missStream.checkError()) {
                throw new IOException("Error writing to " + cspMissed);
            }
        } catch (IOException e) {
            covStr += "\n<BR>[WARN] Cannot get CSP coverage information: " +
                e.getMessage();
        } finally {
            Monitor.global.clearAll();
        }


        System.out.println(covStr);
        results.println(covStr);
        results.br();
        return;
    }

    /** final coverage report --after coverage space merging **/
    public void totalCoverage(){
        int coverage = 0;
        Node node;
        HierName hname;
        Iterator i = tcountmap.keySet().iterator();
        
        while(i.hasNext()){
            hname = (HierName)i.next();
            if(((Integer)tcountmap.get(hname)).intValue() >= MIN_TCOUNT)
                {coverage++;}
            else {
                String str = "#" +((Integer)tcountmap.get(hname)).intValue() 
                    +" "+hname.toString();
                System.out.println(str);
            }
        }
        double c = 100.0*(double)coverage/tcountmap.size();
        printHtmlStdout("[INFO] TOTAL COVERAGE: " 
                        +NumberFormatter.format(c,2) +"%", true);
        resclass.setCoverage(c);
        
    }
    
    //measure the ntpc spec requested
    public void getNTPCValues(final TreeMap<HierName,Float> list,
                              final TreeMap<HierName,Float> list_signoff,
                              final DSimUtil utils, 
                              final boolean modified_delays){
        final Set<HierName> ntpcset = modified_delays ? list_signoff.keySet()
                                                      : list.keySet();
        // ntpc_spec may be specified for aliases of one node; getting the ntpc
        // on one alias clears the measurement for that node, so ntpc of NaN is
        // returned for subsequent aliases; avoid this by caching the value
        final Map<Node,Float> cache = new HashMap<Node,Float>();
        for (HierName hiername : ntpcset) {
            final Node node = sim.findNode(getSimName(hiername));

            final float ntpc_value;
            if (cache.containsKey(node)) {
                ntpc_value = cache.get(node);
            } else {
                ntpc_value = utils.getMeasure(getSimName(hiername).toString());
                // if node is not found, avoid adding an entry to the table
                if (node != null) cache.put(node, ntpc_value);
            }

            float ntpc_spec_value = list.get(hiername);
            float ntpcMargin = ntpc_spec_value * NTPC_JITTER;
            //sclae the value returned by dsim to match spec value
            if(modified_delays){
                ntpc_spec_value *= this.digitalTau;
                ntpcMargin = 0;
            }
            if(ntpc_value - ntpcMargin <= ntpc_spec_value){
                if(modified_delays)
                    resclass.setNTPCTargetModifiedDelay(true);
                else
                    resclass.setNTPCTarget(true);
                printHtmlStdout("[INFO] ntpc_spec info: " + hiername +
                                " (spec: " + ntpc_spec_value +
                                " measured: " + ntpc_value + ")",
                                true);
            }
            else {
                printHtmlStdout("[WARN] ntpc_spec violation: " + hiername +
                                " (spec: " + ntpc_spec_value +
                                " measured: " + ntpc_value + ")",
                                true);
                if(modified_delays)
                    resclass.setNTPCTargetModifiedDelay(false);
                else
                    resclass.setNTPCTarget(false);
            }
        }
        return;
    }

    /**
     * attach a node watcher at all cells for which we're computing the ntpc 
     **/
    public void reseed(){
        dsim_rseed = System.currentTimeMillis();
        sim.setRandomSeed(dsim_rseed);
        printHtmlStdout("[INFO] Setting DSim random seed to " +dsim_rseed, 
                        true);
    }
    
    private static class CantResetException extends RuntimeException {
        final String instance;
        final String beh;
        public CantResetException(final String instance, final String beh) {
            this.instance = instance;
            this.beh = beh;
        }
    }

    private static class CantResetParameters extends CoSimParameters {
        public void setBehavior(final String name, final int value) {
            if ((value & CSP) != 0) {
                throw new CantResetException(name, "csp");
            } else if ((value & JAVA) != 0) {
                throw new CantResetException(name, "java");
            } else {
                super.setBehavior(name, value);
            }
        }
    }

    private static boolean isImplemented(final CellInterface cell) {
        final HashSet unimpl = new HashSet();
        DSimHelper.cellStatistics(cell, unimpl);
        return unimpl.isEmpty();
    }

    /**
     * Simulate the passed cell in the context of the given enviroment in
     * DSim. 
     */
    public boolean simCurrentEnv(final boolean cosimulate,
                                 final boolean cutunimpl,
                                 final Collection<String> resetStressTested){
        
        boolean error = false;
        reseed();
        
        // if cycle_node is defined and the CUT has a PRS complete
        // implementation, then attempt to run reset stress test
        if (cutunimpl) {
            printHtmlStdout("[INFO] Reset stress test skipped because " +
                            "cell under test is not fully implemented", true);
        } else if (cycle_node == null) {
            printHtmlStdout("[INFO] Reset stress test skipped because " +
                            "no cycle_node specified", true);
        } else if (!isImplemented(tenv)) {
            printHtmlStdout("[INFO] Reset stress test skipped because " +
                            "environment is not fully implemented", true);
        } else {
            try {
                if(timed.booleanValue()){
                    sim.setRandom(DSim.TIMED_RANDOM);
                    setJitter();
                }
                else
                    sim.setRandom(DSim.UNTIMED_RANDOM);
                
                rteCosimulate(cellType, instanceName, this.envname,
                              envInstanceName, true, rte_cosim_spec,
                              rte_env_cosim_spec,
                              new CantResetParameters());
                DSimUtil dutil = new DSimUtil();
                printHtmlStdout("[INFO] Performing reset stress test...", 
                                true);
                printHtmlStdout("[INFO] Running " +this.resetStresstests
                                +" test(s) for " +this.resetStresscycles 
                                +" cycle(s) on " +cycle_node, true);
                
                boolean done;
                done = dutil.resetStressTest(this.resetStresstests,
                                             this.resetStresscycles,
                                             getSimName(cycle_node).toString(),
                                             DSimUtil.STANDARD_RESET); 
                error |= !done;
                if(!done){
                    printHtmlStdout("[FAIL] Reset Stress Test Appears to "
                                    +"have deadlocked", true);
                    
                }
                resetStressTested.add(tenv.getFullyQualifiedType());
                error |= checkAnomalousState();
                
                //remove instantiation we created for RST
                sim.rm_instantiation();
                sim.setRandom(DSim.NO_RANDOM);
            }
            catch (CantResetException e) {
                final String which =
                    e.instance.startsWith(instanceName) ? "rte_cosim_spec"
                                                        : "rte_env_cosim_spec";
                printHtmlStdout("[INFO] Reset stress test skipped because " +
                                which + " directive specified " + e.beh +
                                " behavior for " + e.instance, true);
            }
            catch(Exception e){
                error = true;
                printHtmlStdout("[FAIL] "
                                +"*** RST Exception: "+e.toString(), true);
            }
            catch(Throwable t){
                error = true;
                printHtmlStdout("[FAIL] "
                                +"*** RST Throwable: "+t.toString(), true);
            }
        }
        
        //measure the NTPC spec here 
        if (!this.ntpcslist.isEmpty()){
            try {
                //standard simulation with prs delays
                //if use_measured_delay=on then don't do this
                if(!use_measured_delay)
                    error |= performanceSimulation(false);
                if(use_measured_delay ||
                   enable_digital_delay   ||
                   enable_estimated_delay || 
                   enable_measured_delay)
                    error |= performanceSimulation(true);
                
            }
            catch(Throwable t){
                printHtmlStdout("[FATAL] Throwable: DEBUG"+t.toString(), true);
                t.printStackTrace();
            }
            finally {
                resetModifiedDelays();
                sim.setHistoryPerNode(0);
            }
        }

        //reseed for this simulation
        reseed();

        try {
            sim.emitCSPCoverageProbes = true;
            Monitor.global.clearAll();
            System.out.println("cellType: "+cellType);
            rteCosimulate(cellType, instanceName, this.envname, envInstanceName,
                          !cosimulate, rte_cosim_spec, rte_env_cosim_spec);
            System.gc();
            this.totalMemory = Runtime.getRuntime().totalMemory() / 1024;
            this.freeMemory = Runtime.getRuntime().freeMemory() / 1024;
        }
        catch(Exception e){
            printHtmlStdout("[FATAL] COSIM Exception: "+e.toString(), true);
        }
        catch(Throwable t){
            printHtmlStdout("[FATAL] Throwable: "+t.toString(), true);
        }
        finally {
            sim.emitCSPCoverageProbes = false;
        }

        /** set up coverage analysis **/
        if(tcountmap == null)tcountmap = getTCounts(instanceName);
        
        /** Reset circuit with appropirate mode and cycle as specified **/
        if(cycle_mode == CYCLE_BY_NODE){
            if(timed.booleanValue()||use_measured_delay){
                error |= reset(DSim.TIMED_RANDOM);
                setJitter();
            }
            else 
                error |= reset(DSim.UNTIMED_RANDOM);
            
            sim.clear_tcounts();
            
            /** should not happen strange **/
            if(cycle_node == null){
                printHtmlStdout("[WARN] Ambigious CycleMode; Using CycleTime", 
                                true);
                cycle_mode = CYCLE_BY_TIME;
            }
            else {
                this.cnode = this.sim.findNode(getSimName(cycle_node));
                printHtmlStdout("[INFO] cycling on " + cycle_node +" for "
                                +cycle_count.intValue() +" transitions", true);
                
                this.sim.cycle(this.cnode, cycle_count.intValue()); 
                resclass.setCycleNode(true);
            }
        }
        
        long start_time = 0;
        if(cycle_mode == CYCLE_BY_TIME){
            error |= reset(DSim.TIMED_RANDOM);
            setJitter();
            sim.clear_tcounts();
            printHtmlStdout("[INFO] cycling for " +cycle_time.intValue() 
                            +" DSim time units", true);
            System.out.println("Before cycling");
            start_time = this.sim.getTime();
            this.sim.cycle(cycle_time.intValue());
            System.out.println("After cycling");
            resclass.setCycleNode(false);
        }
        else if(cycle_mode != CYCLE_BY_NODE){
            printHtmlStdout("INTERNAL ERROR: cycle not determined" +cycle_mode,
                            true);
            error = true;
        }
        
        /** determine why simulation is over **/                
        error |= checkAnomalousState();
        
        /** check for conflicts on merge devices **/
        if(cosimulate)
            error |= checkMergeDevices();
        
        /** check for potential deadlock **/
        error |= checkDeadlock(start_time, error);
        
        /** now lets do converage analysis **/
        coverageAnalysis();
        addTCounts(tcountmap);

        results.p();
        if(error)
            results.println("&lt;&lt; FAILED &gt;&gt;"); 
        else 
            results.println("&lt;&lt; PASSED &gt;&gt;"); 
        
        /** simulation done, clear simulator **/        
        System.out.println("\n/***********done with " +envname 
                           +" environment ****************/\n");
        clearSimVars();
        System.out.println("Active threads: " +Thread.activeCount());
        return error;
    }

    //reset vars from run to run
    public void resetVars(){
        
        this.cycle_time = this.default_cycle_time;
        this.cycle_count = this.default_cycle_count;
        this.ntpcslist.clear();
        this.ntpcslist_signoff.clear();
        this.cycle_mode = CYCLE_BY_TIME;
        this.cycle_node = null;
        this.resetStresstests = 1000;
    }

    /** 
     * Runs the simulation for all the environments in the passed cell.
     * 
     **/
    public SimResults simulate(){
        return simulate(null);
    }

    public SimResults simulate(String[] envs){
        
        /** local variables for simulation **/
        String starttime ="";
        long starttime_ms =0;
        boolean testable = true;
        boolean error = false;
        EnvBlock envblock = null;
        Iterator envnames = null;
        BlockInterface directives = null;
        BlockIterator blocki = null;
        
        /** Begin tracking siumlation **/
        if(results != null){
            starttime = "Simulation started at: "+results.today();
            starttime_ms = System.currentTimeMillis();
        }
        
        /** Exception occured while constructing class instance **/
        // @REVIEW: set this in all the appropirate places
        if(exception){ 
            if(results != null){
                printHtmlStdout("[FATAL] AutoSimulateCell Exception", true);
                results.flush();
            }
            resclass.setRuntimeException(true);
            return resclass;
        }
        
        /** If we make it here then all is fine; sort of **/
        envblock = this.cut.getEnvironments();
        envnames = envs == null ? envblock.getNames()
                                : Arrays.asList(envs).iterator();
        
        /*** process CELL directives ***/
        directives = this.cut.getBlockInterface();
        blocki = directives.iterator(BlockInterface.DIRECTIVE);
        while(blocki.hasNext())
            procDirectives((DirectiveBlock)blocki.next());
        
        results.hr();
        
        /** Do not start something you can finish **/
        if(this.fragment.booleanValue()){
            printHtmlStdout("[INFO]Fragment directive detected", true);
            resclass.setFragment(true);
        }
        if(this.synchronous.booleanValue()){
            printHtmlStdout("[INFO] Synchronous directive sighting<p>", true);
            results.hr();
            resclass.setSynchronous(true);
        }
        if(this.unimpl.booleanValue()){
            printHtmlStdout("***** UNIMPLEMENTABLE *****<p>", true);
            resclass.setUnimplementable(true);
            testable = false;
        }
        if(this.rte_ignore) {
            printHtmlStdout("***** RTE_IGNORE *****<p>", true);
            resclass.setCycleNode(true);   //for report page
            resclass.setRTEIgnore(this.rte_ignore, "__CELL__");
            testable = false;
        }
        if(!envnames.hasNext() && testable){
            printHtmlStdout("***** MISSING TEST ENVIRONMENTS *****<p>", true);
            resclass.setMissingTest(true);
            //synchronus/fragments cells are okay without an environment
            if(!this.synchronous.booleanValue() && 
               !this.fragment.booleanValue()){
                error = true;
            }
            if(this.synchronous.booleanValue() ||
               this.fragment.booleanValue()){
                testable = false;
            }
        }
        if(!testable)results.hr();

        /** CellInterface appears testable at this point; **/
        if(testable) {
            //Iterate through the given tests
            int tried = 0;
            final boolean cutunimpl = !isImplemented(cut);
            final Collection<String> resetStressTested =
                new ArrayList<String>();

            while(envnames.hasNext()){
                
                resetVars();
                this.rte_ignore = false;
                this.cosim_will_fail = false;
                this.rte_cosim_spec = cell_rte_cosim_spec;
                this.rte_env_cosim_spec = cell_rte_env_cosim_spec;
                
                boolean cosimulate = false;
                /** Determine level we'll be simulating this cell at **/
                cosimulate = ((cut.containsJava() || cut.containsRunnableCsp())&&
                              (cut.containsCompletePrs() ||
                               cut.containsCompleteSubcells()));
                if(!cosimulate)
                    cosimulate = (cut.containsJava() || cut.containsRunnableCsp());
                
                this.envname = (String)envnames.next();
                this.cellEnvPrefix = outputFileName + ":" +
                                     CellUtils.hashMetaParameters(envname);
                
                /** print simulation information for this cell **/
                results.center();
                //print this banner for help debugging with logs
                System.out.println("\n/***********simulating " +this.envname 
                                   +" environment ****************/\n");
                results.println("simulating with <code>"+this.envname 
                                +"</code> environment"); 
                results.centerEnd();
                results.p();
                
                /** 
                 * get the actual directiveblock and extract info 
                 * and set class members 
                 **/
                try {
                    tenv = envblock.getNamedEnvironment(envname);
                } catch (CastSemanticException e) {
                    error = true;
                    printHtmlStdout("Simulation Exception: ", true);

                    com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                        .printException(e, System.out);

                    results.pre();
                    com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                        .printException(e, results);
                    results.preEnd();
                    results.hr();

                    com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                        .printException(e, System.err);
                    continue;
                }

                // if the user specified an invalid environment via --envs, for
                // example
                if (tenv == null) {
                    error = true;
                    printHtmlStdout("[WARN] ** Cannot find environment: " +
                                    envname, true);
                    continue;
                }

                if (!DSimHelper.isRealCell(tenv)) {
                    printHtmlStdout("[WARN] ** Simulation skipped: " +
                                    "no behavior to simulate", true);
                    results.p();
                    results.println("&lt;&lt; SKIPPED &gt;&gt;"); 
                    results.hr();
                    continue;
                }

                ++tried;

                BlockInterface btenv = tenv.getBlockInterface();
                BlockInterface env = btenv.iterator(BlockInterface.ENV).next();
                BlockIterator envbi = env.iterator(BlockInterface.DIRECTIVE);
                
                timed_jitter = cell_timed_jitter;
                if(envbi.hasNext()){
                    procENVDirectives((DirectiveBlock)envbi.next());
                }
                else 
                    printHtmlStdout("[WARN] ** NO DIRECTIVES SPECIFIED; "
                                    +"USING DEFAULTS. ******", true);
                
                /** 
                 * Now attempt to simulate the given cell with the context
                 * of the current environment and <code>cosimulate</code>
                 **/
                try {
                    resclass.setRTEIgnore(this.rte_ignore, this.envname);
                    resclass.setCosimWillFail(this.cosim_will_fail, this.envname);
                    if(!this.rte_ignore)
                        error |= simCurrentEnv(
                                cosimulate & !this.cosim_will_fail, cutunimpl,
                                resetStressTested);
                    else 
                        printHtmlStdout("***** RTE_IGNORE *****<p>", true);
                    
                    results.hr();
                }
                catch(Exception e){
                    /** Capture any potential exception **/
                    error = true;
                    printHtmlStdout("Simulation Exception: "+e.toString(), 
                                    true);
                    e.printStackTrace();
                    results.hr();
                } 
                
                try {
                    sim.rm_instantiation();
                }
                catch(Exception e){
                    error = true;
                    printHtmlStdout("Simulation Exception: "+e.toString(), 
                                    true);
                    break;
                }
            }
            if (tried == 0) {
                printHtmlStdout("***** MISSING TEST ENVIRONMENTS (NO ENVIRONMENT CAN BE SIMULATED) *****<p>", true);
                resclass.setMissingTest(true);
                results.hr();
            } else if (resetStressTested.isEmpty()) {
                printHtmlStdout(
                    "[WARN] ** No reset stress test was performed<p>", true);
            }
        }
        
        //compute final coverage numbers and simulation diagnostics
        results.println("<center>Diagnostics</center><br>");
        if(tcountmap != null)totalCoverage();
        results.p();
        
        long maxMemory = Runtime.getRuntime().totalMemory() / 1024;
        int numProc = Runtime.getRuntime().availableProcessors();
        
        this.usedMemory = this.totalMemory - this.freeMemory;
        printHtmlStdout("[INFO] Processors Available: "+numProc, true);
        printHtmlStdout("[INFO] Total Memory: "+this.totalMemory +"Kb", true);
        printHtmlStdout("[INFO] Free Memory: "+this.freeMemory+"Kb", true);
        printHtmlStdout("[INFO] Used Memory: "+this.usedMemory+"Kb", true);
        printHtmlStdout("[INFO] Max Available Memory: "+maxMemory+"Kb", true);
        printHtmlStdout("[INFO] "+
            com.avlsi.util.debug.VersionInfo.getVersionString(getClass()),
            true);

        results.p();
        printHtmlStdout(starttime, true);
        printHtmlStdout("Simulation ended at: "+results.today(), true);
        long duration = System.currentTimeMillis() - starttime_ms;
        duration /= 1000;  //convert to seconds
        printHtmlStdout("Simulation duration: "+duration +"s", true);
        resclass.setDuration(duration);
        results.end();
        
        if(testable){
            if(error) 
                resclass.setResult(SimResults.FAILED);
            else
                resclass.setResult(SimResults.PASSED);
        }
        return resclass;
    }

    public void linkCellDefinition(){
        
        int tab = 0;
        String cstr;
        Stack istack = new Stack();
        CellInterface curr;
        String cellDef = null;
        for(int i=0; i < this.path.length; i++){
            cellDef = this.path[i]+"/"+this.cellFile;
            if((new File(cellDef)).exists())break;
        }
        
        //get all the refinement information and stick it in a stack
        curr = this.cut;
        while(curr != null){
            istack.push(curr.getFullyQualifiedType());
            curr = curr.getDirectRefinementParent();
        }
        
        results.pre();
        while(!istack.empty()){
            for(int i=0; i < tab; i++)
                results.print("\t");
            if(tab != 0)results.print("+--");
            cstr = (String)istack.pop();
            if(istack.empty()){
                results.println("<A HREF=\""+"file:///"+cellDef
                                +"\" TARGET=\""
                                +RegressionTestEngine.mainFrame +"\">"
                                +cstr +"</A>");
            }
            else {
                results.println(cstr);
                //results.br();
                for(int i=0; i < tab+1; i++)
                    results.print("\t");
                results.println("|");
                //results.br();
            }
            tab++;
        }
        results.preEnd();
    }

    /** print pertinent information about this cell **/
    public void printCellInfo(){
        
        int nprs;
        HashSet unimpl = new HashSet();
        results.p();
        linkCellDefinition();
        
        nprs = DSimHelper.cellStatistics(cut, unimpl);
        results.p();
        Iterator i;
        
        results.table(1, "100%", 1, 1);
        results.println("<TR BGCOLOR=\"#CCCCFF\">");
        results.println("<TD COLSPAN=2><FONT SIZE=\"+2\">"
                        +"<B>Cell Summary</B></FONT></TD></TR>");
        i = cut.getPortDefinitions();
        while(i.hasNext()){
            PortDefinition port = (PortDefinition) i.next();
            results.println("<TR BGCOLOR=\"white\" CLASS=\"TableRowColor\">"
                            +"<TD ALIGN=\"right\" VALIGN=\"top\" WIDTH=\"1%\">"
                            +"<CODE>port</CODE></TD>");
            results.println("<TD><CODE><B>" +port.toString() +"</B></CODE>");
        }
        results.println("<TR BGCOLOR=\"white\" CLASS=\"TableRowColor\">"
                        +"<TD ALIGN=\"right\" VALIGN=\"top\" WIDTH=\"1%\">"
                        +"<CODE>#Rules</CODE></TD>");
        results.println("<TD><CODE><B>" +nprs +"</B></CODE>");
        results.println("<TR BGCOLOR=\"white\" CLASS=\"TableRowColor\">"
                        +"<TD ALIGN=\"right\" VALIGN=\"top\" WIDTH=\"1%\">"
                        +"<CODE>Dsim Seed</CODE></TD>");
        results.tableEnd();
        results.p();
        results.println("<dl><dt><b>Unimplemented Cells: </b><dd>");
        results.println(printUnImplCells(unimpl)+"</dl>");
    }

    /** clear all the data members which contorl the simulation **/
    public void clearSimVars(){
        envname = null;
    }

    /**
     **  MOVE THESE ROUTINES INTO DSIM !!! 
     **/
    public TreeMap getTCounts(String prefix){
        
        TreeMap tcount = new TreeMap();
        Node node;
        Vector names = new Vector();
        sim.expandNodes("*", names, false, true);
        Enumeration en = names.elements();
        while (en.hasMoreElements()) {
            HierName name = (HierName)en.nextElement();
            if(name.toString().startsWith(prefix)){
                node = sim.findNode(name);
                if(node != null){
                    tcount.put(name, new Integer(node.getTCount()));
                }
                else {
                    System.out.println("coverage numbers are inaccurate");
                }
            }
        }
        return tcount;
    }
    
    public void addTCounts(TreeMap tcounts){
        
        Node node;
        HierName hname;
        Iterator i = tcounts.keySet().iterator();
        
        while(i.hasNext()){
            hname = (HierName)i.next();
            node = sim.findNode(hname);
            if(node != null){
                Integer tcountval = 
                    new Integer(((Integer)tcounts.get(hname)).intValue()
                                +node.getTCount());
                tcounts.put(hname, tcountval);
            }
            else {
                System.out.println("coverage numbers are inaccurate");
            }
        }
        return;
    }
    

    /** get a string for the directory **/
    public String getResultPath(){ return this.moduleName; }

    /*** print usage for jdsim-auto utility ***/
    public static void usage(){
        System.out.println("\njdsim-auto: This utility invokes the "
                           +"AutoSimulateCell Module\n"
                           +" used by the Regression Test Engine. "
                           +"It provides\n"
                           +"a quick way to test cells before committing "
                           +"them\n"
                           +"\nOPTIONS\n"
                           +"\t\t --cell=<fully-qualified-cellName>\n"
                           +"\t\t --basedir=<results-dir>\n"
                           +"\t\t --digital-delay=digital_delay\n"
                           +"\t\t --estimated-delay=estimated_delay\n"
                           +"\t\t --measured-delay=measured_delay\n"
                           +"\t\t --dataset=dataset\n"
                           +"\t\t --use-measured-delay=[on|off]\n"
                           +"\t\t --cast-path=<cast-path>\n"
                           +"\t\t --device-path=<device-path>\n"
                           +"\t\t --history-per-node=<depth>\n"
                           +"\t\t --default-cycle-count=<count>\n"
                           +"\t\t --cycle-count=<count>\n"
                           +"\t\t --timed-jitter=<jitter>\n"
                           +"\t\t --rule-coverage=[on|off]\n"
                           +"\t\t [ --envs=env1:env2:... ]\n"
                           );
    }

    /**
     * This method allows us to run AutoSimulateCell as a stand-alone program 
     * instead as a supporting class to the RegressionTestEngine, simply specify 
     * the fully qualified name of the cell and the --resultpath is where the
     * output html file will be placed.
     **/
     public static void main(String []args)throws Exception{
         
         /*** parse command-line arguments here ***/
        String []path =null;
        String []devicePath=null;
        String basedir=null;
        String module = null, cellName = null;
        int profile = 1;
        String digital_delay = "off";      
        String dataset = "0";
        String measured_delay = "off";
        String estimated_delay = "off";
        String use_measured_delay = "off";
        String[] envs = null;
        int historyPerNode = 10;
        Integer defaultCycleCount = null;
        Integer cycleCount = null;
        Float timedJitter = null;
        String ruleCoverage = "on";
        
        /**** new command line processing stuff ****/
        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
        
        CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        
        CommandLineArgs theArgs = cachedArgs;
        
        try {
            basedir = theArgs.getArgValue("basedir", ".");
            digital_delay = theArgs.getArgValue("digital-delay", "off");
            estimated_delay = theArgs.getArgValue("estimated-delay", "off");
            measured_delay = theArgs.getArgValue("measured-delay", "off");
            use_measured_delay =
                theArgs.getArgValue("use-measured-delay", "off");
            dataset = theArgs.getArgValue("dataset", "0");
            historyPerNode =
                getIntegerArgValue(theArgs, "history-per-node", 10);
            defaultCycleCount =
                getIntegerArgValue(theArgs, "default-cycle-count", null);
            cycleCount = getIntegerArgValue(theArgs, "cycle-count", null);
            timedJitter = getFloatArgValue(theArgs, "timed-jitter", null);
            ruleCoverage = theArgs.getArgValue("rule-coverage", ruleCoverage);

            final String pathstr = theArgs.getArgValue("cast-path", null);
            if (pathstr != null) {
                path = RegressionTestEngine.getStringArray(pathstr);
            }

            final String dPathstr = theArgs.getArgValue("device-path", null);
            if (dPathstr != null) {
                devicePath = RegressionTestEngine.getStringArray(dPathstr);
            }

            if (theArgs.argExists("profile")) {
                profile = getIntegerArgValue(theArgs, "profile", 1);
                System.out.println("Profiling with " + profile + " iterations");
            }

            final String envStr = theArgs.getArgValue("envs", null);
            if (envStr != null) {
                envs = StringUtil.split(envStr, ':');
            }

            final String fcellName = theArgs.getArgValue("cell", null);
            if (fcellName != null) {
                int lastdot = fcellName.lastIndexOf('.');
                module = fcellName.substring(0, lastdot);
                cellName = fcellName.substring(lastdot+1, fcellName.length());
            }
        } catch (CommandLineArgFormatException e) {
            System.err.println("Invalid argument specified for " +
                               e.getArgName());
            AutoSimulateCell.usage();
            System.exit(-1);
        }

        if (cellName == null || module == null || basedir == null ||
            path == null) {
            AutoSimulateCell.usage();
            System.exit(-1);
        }

        if (timedJitter != null && (timedJitter < 0 || timedJitter > 1)) {
            System.err.println("ERROR: --timed-jitter must be in (0..1): " +
                               timedJitter);
            System.exit(-1);
        }
        
        DSim sim = DSim.get();
        sim.setCastVersion("2");
        
        /*** moduleName, cellType ****/
        basedir += "/";
        System.out.println(basedir+
                           RegressionTestEngine.modules+
                           module.replace('.', '/'));
        
        RegressionTestEngine.createDirectory(basedir
                                             +RegressionTestEngine.modules
                                             +module.replace('.','/'));
        String outputFileName;
        outputFileName = RegressionTestEngine.get().
            hashCellNameToFileName(cellName);
        
        for(int i=0; i < profile; i++){
            AutoSimulateCell cell =
                new AutoSimulateCell(module,cellName, 
                                     outputFileName, 
                                     digital_delay,
                                     estimated_delay,
                                     measured_delay,
                                     dataset,
                                     use_measured_delay,
                                     historyPerNode,
                                     ruleCoverage.equals("on"),
                                     path, devicePath,
                                     basedir, sim,
                                     new StandardParsingOption(theArgs));
            if (defaultCycleCount != null) {
                cell.setDefaultCycleCount(defaultCycleCount);
            }
            if (cycleCount != null) {
                cell.setForceCycleCount(cycleCount);
            }
            if (timedJitter != null) {
                cell.setForceTimedJitter(timedJitter);
            }
            System.out.println(((SimResults)cell.simulate(envs)).toString());
        }
        System.exit(0);
        return;
     }

    /************************************************************************/
    /* DEBUGGING and DEPRECATED ROUTINES FOR AUTOSIMULATE CELL;             */
    /************************************************************************/
    
    /** List all the Nodes in current dsim simulation **/
     public void listAllNodes(){
 
         Vector names = new Vector();
         sim.expandNodes("*", names, false, true);
         Enumeration en = names.elements();
         System.out.println("+++++++++++++ Listing all Nodes +++++++++++");
         while (en.hasMoreElements()) {
             HierName name = (HierName)en.nextElement();
             System.out.println("--+" +name.toString());
         }
     }
}

