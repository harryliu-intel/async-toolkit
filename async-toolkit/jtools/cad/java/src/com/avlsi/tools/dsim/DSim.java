/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.IOException;
import java.io.ByteArrayInputStream;
import java.io.DataInput;
import java.io.DataOutput;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Writer;

import java.math.BigInteger;

import java.text.NumberFormat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.regex.Pattern;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.impl.AmbiguousLookupException;
import com.avlsi.cast.impl.BoolValue;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;

import com.avlsi.cell.CellDelay;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.HierarchyInterface;
import com.avlsi.cell.NoSuchEnvironmentException;

import com.avlsi.csp.csp2java.runtime.CspRuntimeAbstractDevice;
import com.avlsi.csp.util.VisitorExceptionWithLocation;

import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.StructureType;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;

import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cell.CellConstants;
import com.avlsi.tools.cosim.ChannelDictionary;
import com.avlsi.tools.cosim.ChannelFactoryInterface;
import com.avlsi.tools.cosim.ChannelTimingInfo;
import com.avlsi.tools.cosim.CoSimHelper;
import com.avlsi.tools.cosim.CoSimInfo;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.DeviceConstructionException;
import com.avlsi.tools.cosim.NodeLinkageInterface;
import com.avlsi.tools.cosim.spec.*;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.Rule;
import com.avlsi.tools.prs2verilog.AbstractConverter;
import com.avlsi.tools.prs2verilog.Prs2Verilog;
import com.avlsi.tools.prs2verilog.verilog.VerilogEmitter;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryImpl;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.Arbiter;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.SharedBus;
import com.avlsi.tools.tsim.verilog.VerilogSharedBus;
import com.avlsi.tools.tsim.verilog.VpiInterface;
import com.avlsi.tools.tsim.verilog.SignalNotFoundException;
import com.avlsi.tools.tsim.Data;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.jflat.JFlat;
import com.avlsi.tools.sigscan.NodeLogger;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.SigscanException;

import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.AndBooleanExpression;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanUtils;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.Map2SetAdapter;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.MutableInt;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.SortingIterator;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.NullaryAction;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.NaturalStringComparator;
import com.avlsi.util.text.PrintfFormat;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.text.UnrepresentableCharException;

public class DSim implements NodeWatcher {

    private static DSim singleton = null;

    
    /** List of nodes in a searchable structure. **/
    //private final NodeStore nodes = new NodeStore();
    private final HashMap<HierName,Node> nodes = new HashMap<HierName,Node>();
    /** List of rules that act on the nodes. **/
    private final Vector rules = new Vector();
    /** Count of non-asserted production rules. **/
    private int nonAssertedRuleCount = 0;
    /** Scheduler responsible for Event queueing and execution. **/
    private DigitalScheduler sched = DigitalScheduler.get();
    /** Has a production rule set been loaded. **/
    public boolean loaded = false;
    /** Has an error occurred during instantiation? **/
    public boolean errors = false;
    /** Print out warnings? **/
    private boolean warn = true;
    /** Stop after any printed warnings? **/
    private boolean error = true;
    /** Stop on co-simulation conflict? **/
    public boolean haltOnConflict = true;
    /** Stop on CSP error statement? **/
    public boolean haltOnError = true;
    /** Print lots of gratuitous details? **/
    boolean verbose = false;
    /** Print gratuitous details related to STA? **/
    boolean staVerbose = false;
    /** Actually only controls printout of tcounts **/
    boolean countTransitions = false;
    private Random rand = new Random(1);
    /** Total number of nodes created **/
    private int nodeCount=0;
    /** Name of environment to use **/
    private String envName = null;
    /** Name of cell that gets the environment **/
    private HierName envCell = null;
    /** Name of the cell **/
    private String cellName = null;
    /** The cast file that's been loaded **/
    private CastFile loadedFile = null;
    /** Print exception stack traces and other such debugging crap? **/
    private boolean debug = false;
    /** Slack to put between CSP devices and split/merge devices **/
    private int cosimSlack = 1000;

    /** Name of the reset port in the cell **/
    private HierName resetPortName = null;

    /** Name of the reset node in top-level **/
    private HierName resetNodeName = null;

    private SearchPath m_CastFileSearchPath;

    private CommandLineArgs m_theArgs;

    /**
     * Cached <code>CastFileParser</code>.  Changes to the files
     * will not be detected!  
     *
     * <p>I tried using a {@link java.lang.ref.SoftReference}, but that was
     * dropped even though there appeared to be enough memory, and
     * <code>java -client</code> was run.
     **/
    private CastFileParser cachedCastFileParser;

    /**
     * True if the file cache is enabled.  Changes to the files will
     * not be detected!
     **/
    private boolean fileCacheEnabled;

    private ClassLoader m_DeviceLoader;

    /**
     * Have the ERROR node and the GND=&gt;ERROR+ rule been added yet?
     * Should only be set to back to false if the corresponding rules
     * and node are removed at the same time.
     **/
    private boolean ERRORHandlingDoneYet = false;

    /* controls whether we do coverage of generated CSP */
    public boolean emitCSPCoverageProbes=false;

    /**
     * Map from a node's canonical name to the name that used to request
     * its watch.
     **/
    private final Map/*<HierName,HierName>*/
    canonNodeNameToWatchedNodeNameMap =
        new HashMap/*<HierName,HierName>*/();
    
    /**
     * Processors of measured_delay directives.
     **/
    private Collection<ProcessMeasuredDelay.TableProcessor> measuredProcs =
        new ArrayList<ProcessMeasuredDelay.TableProcessor>();
    private Node measuredWarningContext = null;
    private final NullaryAction zeroDataNotifier = new NullaryAction() {
        public void execute() {
            final Pair warnPair = new Pair(measuredWarningContext, "zero");
            if (warnedCells.add(warnPair)) {
                System.err.println(
                    "\nWarning: Invalid data (bug 19653) removed for " +
                    measuredWarningContext.getName());
            }
        }
    };

    /** 
     * Anomalous event helper class.
     * Keeps track of the node that caused the anomaly, the rule 
     * which was affected, and the time the anomaly occurred.
     **/
    public static class AnomalousEvent {
        public Node node;
        public Rule rule;
	public long time;
	AnomalousEvent(Node n, Rule r, long t) {
            node = n;
            rule = r;
            time = t;
        }
    }
    /** AnomalousEvent for an interference event **/
    private AnomalousEvent interferenceEvent = null;
    /** AnomalousEvent for a glitch event **/
    private AnomalousEvent glitchEvent = null;
    /** AnomalousEvent for an instability event **/
    private AnomalousEvent unstabEvent = null;

    /**
     * The version of cast being used.
     **/
    private String castVersion;
    /**
     * Whether or not to create asserted production rules as well as
     * synthesizable ones when instantiating cells.
     **/
    private boolean handleAsserts = true;

    /**
     * Contains disjuncts of production rules that are specified in unsued_prs
     * directives, indexed by the targets of the production rules.
     **/
    private final Map/*<HierName,Set<BooleanExpressionInterface>>*/
        possiblyUnusedRules =
        new HashMap/*<HierName,Set<BooleanExpressionInterface>>*/();

    /**
     * Contains annotation from <code>signoff_constant</code> directives.
     **/
    private final Map/*<HierName,Integer>*/ constantNodes =
        new HashMap/*<HierName,Integer>*/();

    /**
     * Set of cells for which a warning about delay bias and after has
     * been issued.
     **/
    private final Set warnedCells = new HashSet();
    
    /**
     * possibilities for randomOrder.
     * <p>
     * NO_RANDOM, just use delay.
     * <p>
     * UNTIMED_RANDOM -- "timed" rules fire with variable delay. Untimed rules
     *     fire arbitrarily in between.
     * <p>
     * TIMED_RANDOM -- all rules have delay between delay*fastDelay and
     *     delay*slowDelay
     *
     * @see #randomOrder
     **/
    public static final int NO_RANDOM = 0, UNTIMED_RANDOM = 1, TIMED_RANDOM = 2;
    static final int VARIABLE_DELAY = TIMED_RANDOM;

    private static final String RESET_BODY_SUFFIX = ":body";
    private static final String RESET_ENV_SUFFIX = ":env";
    private static final float RESET_SLEW = 693; // see bug 18611

    class MeasuredDelay {
        class Data {
            private double extraDelay;
            private Pair[] data;
            public Data(final double extraDelay, final Pair[] data) {
                this.extraDelay = extraDelay;
                this.data = data;
            }
            public boolean equals(final Object o) {
                if (o instanceof Data) {
                    final Data d = (Data) o;
                    return extraDelay == d.extraDelay &&
                           Arrays.equals(data, d.data);
                }
                return false;
            }
            public Pair[] getData() {
                return data;
            }
            public double getExtraDelay() {
                return extraDelay;
            }
        }

        private final HierName prefix;
        private final HierName instance;
        private final CellInterface cell;
        private final Map<Pair<Node,Boolean>,Data> measured;
        private final Map<Node,HierName> portMap;
        private final double delayBias;
        public MeasuredDelay(final HierName prefix,
                            final HierName instance,
                            final CellInterface cell,
                            final double delayBias) {
            this.prefix = prefix;
            this.instance = instance;
            this.cell = cell;
            this.measured = new TreeMap<Pair<Node,Boolean>,Data>(
                    new Comparator<Pair<Node,Boolean>>() {
                        public int compare(Pair<Node,Boolean> p1,
                                           Pair<Node,Boolean> p2) {
                            int x = p1.getFirst().getName().compareTo(
                                p2.getFirst().getName());
                            if (x == 0) {
                                x = p1.getSecond().compareTo(p2.getSecond());
                            }
                            return x;
                        }
                    });
            this.portMap = new HashMap<Node,HierName>();
            this.delayBias = delayBias;
        }
        private String printDirective(final Triplet[] values) {
            if (values == null) {
                return "null";
            } else {
               final StringBuilder sb = new StringBuilder();
                for (Triplet value : values) {
                    sb.append(value.getFirst() + "/" + value.getSecond());
                    sb.append(" ");
                }
                return sb.toString();
            }
        }
        public double getDelayBias() {
            return delayBias;
        }
        public void addMeasured(
                final Node node,
                final HierName port,
                final boolean up,
                final Triplet[] values,
                final Double extraDelay,
                final Collection<ProcessMeasuredDelay.TableProcessor> procs) {
            final Pair<Node,Boolean> key = new Pair<Node,Boolean>(node, up);
            final Data old = measured.get(key);
            final Data curr =
                values == null ? null
                               : new Data(extraDelay,
                                          ProcessMeasuredDelay.update(
                                              prepareData(key, values),
                                              procs));
            measured.put(key, curr);
            assert old == null || old.equals(curr) : key + " different data";
            portMap.put(node, port);
        }
        public Map<Pair<Node,Boolean>,Data> getMeasured() {
            return measured;
        }
        private Pair[] prepareData(final Pair<Node,Boolean> key, final Triplet[] data) {
            final LinkedList result = new LinkedList();
            for (Triplet t : data) {
                final HierName prefix = (HierName) t.getFirst();
                final HierName trigger = (HierName) t.getSecond();
                if (trigger == null) {
                    result.addLast(new Pair(null, t.getThird()));
                } else {
                    final HierName fullName = HierName.append(prefix, trigger);
                    Node node = findNode(fullName);
                    if (node == null) {
                        node = createNode(fullName);
                    }
                    result.addFirst(new Pair(node, t.getThird()));
                }
            }
            return (Pair[]) result.toArray(new Pair[0]);
        }
        public HierName getPrefix() {
            return prefix;
        }
        public HierName getInstance() {
            return instance;
        }
        public CellInterface getCell() {
            return cell;
        }
        private HierName getSuffix(final HierName name,
                                   final HierName prefix) {
            if (name.isChildOf(prefix)) {
                return name.tail(prefix.getNumComponents());
            } else {
                return null;
            }
        }
        public HierName getPort(final Node node) {
            return portMap.get(node);
        }
        public void getContext(Map<Node,HierName> contexts) {
            if (instance == null) return;
            final Set<Node> checkAliases = new HashSet<Node>();
            final HierName fullPrefix = HierName.append(prefix, instance);
            for (Data data : measured.values()) {
                if (data != null) {
                    final Pair[] ps = data.getData();
                    for (Pair p : ps) {
                        final Node node = (Node) p.getFirst();
                        final HierName suffix = getSuffix(node.getName(),
                                fullPrefix);
                        if (suffix != null) {
                            contexts.put(node, suffix);
                        } else {
                            checkAliases.add(node);
                        }
                    }
                }
            }
            final MultiMap<Node,HierName> aliases =
                generateNodeToAliases(checkAliases);
            for (Node key : aliases.keySet()) {
                Collection<HierName> names = aliases.get(key);
                boolean found = false;
                for (HierName name : names) {
                    final HierName suffix = getSuffix(name, fullPrefix);
                    if (suffix != null) {
                        contexts.put(key, suffix);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    contexts.put(key, null);
                }
            }
        }
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("Prefix: " + prefix + "\n");
            sb.append("Instance: " + instance + "\n");
            sb.append("Cell: " + cell.getFullyQualifiedType() + "\n");
            sb.append("Measured: ");
            sb.append(measured.keySet().toString());
            return sb.toString();
        }
    }

    private Collection<MeasuredDelay> boundaryDelays =
        new ArrayList<MeasuredDelay>();
    private Collection<Node> interiorOutputPorts = Collections.emptyList();
    private MultiMap<String,HierName> routedInstancesByType = null;

    private DSim( SearchPath cFSP, ClassLoader deviceLoader  ) {
        this.m_CastFileSearchPath = cFSP;
        this.cachedCastFileParser = null;
        this.fileCacheEnabled = false;
        Debug.assertTrue( deviceLoader != null );
        this.m_DeviceLoader = deviceLoader;
        this.castVersion = null;
    } 

    /** Returns the singleton instance of the simulator. **/
    public static synchronized DSim get() {
        if (singleton == null) {
            singleton = new DSim(new FileSearchPath("."),
                                 DSim.class.getClassLoader());
        }
        return singleton;
    }

    /**
     * @throws IllegalStateException
     *         If the cast version has already been set.
     **/
    public void setCastVersion( String castVersion ) {
        if (this.castVersion == null) {
            Debug.assertTrue(castVersion != null);
            this.castVersion = castVersion;
            System.out.println("Setting cast version to " + castVersion);
        } else {
            throw new 
               IllegalStateException("You can only set the cast version once.");
        }
    }

    public void setDeviceLoader(ClassLoader deviceLoader) {
        m_DeviceLoader = deviceLoader;
    }

    public void setCastPath(SearchPath castPath) {
        m_CastFileSearchPath = castPath;
    }

    public void setArgs(CommandLineArgs theArgs) {
        m_theArgs = theArgs;
    }

    public void setCosimSlack(final int cosimSlack) {
        if (cosimSlack <= 0) {
            throw new IllegalArgumentException(
                "Cannot set cosim slack to non-positive number: " + cosimSlack);
        }
        this.cosimSlack = cosimSlack;
    }

    /**
     * Enables caching of parsed files, ASTs and <code>CellImpl</code>s.
     * Changes to the files will not be detected.  Pretty much useful
     * only for the <code>RTE</code>.
     **/
    public void enableFileCache() {
        fileCacheEnabled = true;
    }

    /**
     * Enables caching of parsed files, ASTs and <code>CellImpl</code>s using
     * the specified CastFileParser.  Changes to the files will not be
     * detected.  Pretty much useful only for the <code>RTE</code>.
     **/
    public void enableFileCache(final CastFileParser cfp) {
        enableFileCache();
        cachedCastFileParser = cfp;
    }

    /**
     * Disables caching of parsed files, ASTs and <code>CellImpl</code>s.
     **/
    public void disableFileCache() {
        fileCacheEnabled = false;
    }
    
    /**
     * If cache is enabled, flush it.
     **/
    public void flushFileCache() {
        cachedCastFileParser = null;
    }

    /**
     * Returns true if the file cache has been enabled with
     * {@link #enableFileCache}, and has not been subsequently
     * disabled with {@link #disableFileCache}.
     **/
    public boolean isFileCacheEnabled() {
        return fileCacheEnabled;
    }

    private CastFileParser newFileParser()
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        final CastParsingOption opt =
            m_theArgs == null ? CastParsingOption.DEFAULT
                              : new StandardParsingOption(m_theArgs);
        return new CastFileParser(m_CastFileSearchPath, castVersion, opt);
    }

    /**
     * Returns the cached cast file parser if there is one, otherwise
     * returns a new one.  This method will store the new parser if 
     * caching is enabled.
     **/
    private CastFileParser getCastFileParser()
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        if (fileCacheEnabled) {
            if (cachedCastFileParser== null) {
                cachedCastFileParser = newFileParser();
            }
            return cachedCastFileParser;
        } else {
            return newFileParser();
        }
    }

    /** Has DSim been activated? **/
    public static boolean activated() {
        return singleton != null;
    }

    /**
     * Can be called at any time and affects all instantiations until
     * the next time it's set.  If assert handling is on,
     * instantiating a cell digitally will create both its
     * synthesizable production rules and its asserted production
     * rules.  If it's off, only the synthesizable rules will be
     * created.
     **/
    public void setAssertHandling(boolean handleAsserts) {
        this.handleAsserts = handleAsserts;
    }

    /** Get current state of handleAsserts. **/
    public boolean getAssertHandling() {
        return handleAsserts;
    } 

    /** Set <code>cell</code> as the name of the cell under test and 
     **  <code>name</code> as the name of the environment to use.   **/
    private void setEnv(String cell, String name) {
        if (verbose) {
            System.out.println("Setting environment for "+cell+" to "+name);
        }
        envName = name;
        envCell = HierName.makeHierName(cell);
    }

    /** Loads file <code>name</code> and convert it into nodes and rules **/
    public void loadFile(String name) throws CastSyntaxException,
        CastSemanticException, IOException, NoSuchEnvironmentException {
        // Runtime r = Runtime.getRuntime();
        CastFileParser p = getCastFileParser();
        loadedFile = p.parse(name);
        CellInterface c = loadedFile.getEnvironmentCell();
        p = null;

        System.out.println("Parsed.");
        System.gc();

        instantiate(c, null);
    }

    public void cosimulate(
            final String cellType,
            final String instanceName,
            final CoSimSpecList coSimSpecList,
            final String envName,
            final String envInstanceName,
            final CoSimSpec envCoSimSpec)
        throws IOException,
               CastSemanticException,
               CastSyntaxException,
               DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchCellException,
               NoSuchEnvironmentException,
               NoSuchInstanceException {
        cosimulate(cellType, instanceName, coSimSpecList, envName,
                   envInstanceName, envCoSimSpec, new CoSimParameters());
   }

    /**
     * Cosimulates the cell <code>cellType</code> in the presence of the
     * environment <code>envName</code>.  
     * (See the
     * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
     * for the meaning of the rules.)
     *
     * @param cellType  The type of cell to be cosimulated.  
     *     May not be null.
     * @param instanceName  The name as which the cell should be
     *     instantiated.  May not be null.
     * @param coSimSpecList  Specifies how the cell is to be
     *     cosimulated.
     * @param envName  Environment in which to cosimulate the cell.
     *     May not be null.
     * @param envInstanceName  The name as which the cell's environment
     *     should be instantiated.  May not be null.
     * @param envCoSimSpec  Specifies how the environment is to be
     *     simulated.
     *
     * @throws IOException  If a needed cast file is not found or some
     *     other I/O error occurs.
     * @throws CastSemanticException  If there is a semantic error in a
     *     cast file.
     * @throws CastSyntaxException  If there is a syntax error in a cast
     *     file.
     * @throws NoSuchCellException  If a definition for <code>cellType</code>
     *     cannot be found.
     **/
    public void cosimulate(
            final String cellType,
            final String instanceName,
            final CoSimSpecList coSimSpecList,
            final String envName,
            final String envInstanceName,
            final CoSimSpec envCoSimSpec,
            final CoSimParameters coSimParams)
        throws IOException,
               CastSemanticException,
               CastSyntaxException,
               DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchCellException,
               NoSuchEnvironmentException,
               NoSuchInstanceException {
        CastFileParser cfp = null;
        // get the cell
        final CellInterface cell;
        if (cellType.indexOf('.') == -1) {
            if (loadedFile == null)
                throw new NoSuchCellException(cellType);
            cell = loadedFile.getCell(cellType);
        } else {
            cfp = getCastFileParser();
            cell = cfp.getFullyQualifiedCell(cellType);
        }
        cellName = cellType;

        if (cell == null)
            throw new NoSuchCellException(cellType);

        // XXX: need to handle fragments in some way!!!

        // TODO: flip all this around so we call
        // coSimSpecList.setCoSimParams(instanceName, cell, coSimParams)
        // Make cosim.spec.* implement some interface to enforce this

        // set up CoSimParameters as specified
        CoSimHelper.setCoSimParams(instanceName, cell, coSimSpecList,
                                   coSimParams, null, verbose);

        // Set the environment name
        setEnv(instanceName, envName);

        // set the behavior for envInstanceName as specified
        if (envName != null) {
            CoSimHelper.setCoSimParams(
                    envInstanceName, getEnvironmentBlock(cell), envCoSimSpec,
                    coSimParams, null, verbose);
        }

        // instantiate cell
        instantiateByName(cellType, instanceName, coSimParams, cfp);
    }

    public CellInterface getCell() throws CastSemanticException,
                                          CastSyntaxException,
                                          IOException {
        return getCastFileParser().getFullyQualifiedCell(cellName);
    }

    /** Make a synthetic environment cell containing 'inside' and having the
     **    same ports as 'parent'.                                        **/
    private CellInterface makeSyntheticEnv(final CellInterface inside, 
                                           final CellInterface parent,
                                           final HierName parentName, 
                                           AliasedSet nodeAliases) {
        final CellImpl outside =
            new CellImpl("$Synth", null, CellImpl.SYNTHETIC_CELL);
        final HierName env = HierName.makeHierName("_env");
        EnvironmentAliases envAliases = 
                                new EnvironmentAliases(parentName, nodeAliases);
        outside.addSubcellPair(env, inside, false);
        outside.setHasCompleteSubcellsBlock();

        /* For each of parent's ports, we add a port to inside. */
        final Iterator it = parent.getPortSubcellPairs();
        while (it.hasNext()) {
            /* Alias parentName.name to _env.name */
            final Pair p = (Pair)it.next();
            final CellInterface ci = (CellInterface)p.getSecond();
            final HierName name = (HierName)p.getFirst();
            if (name.equals(resetPortName)) {
                envAliases.aliasCell(ci, HierName.makeHierName(env, name),
                        resetNodeName.appendString(RESET_ENV_SUFFIX));
            } else {
                envAliases.aliasCell(ci, HierName.makeHierName(env, name),
                                     HierName.makeHierName(parentName, name));
            }
        }

        return outside;
    }

    class EnvironmentAliases {
        final HierName instanceName;
        AliasedSet nodeAliases;

        EnvironmentAliases(HierName name, AliasedSet nodeAliases) {
            instanceName = name;
            this.nodeAliases = nodeAliases;
        }

        void makeConnection(final HierName n1, final HierName n2) {
            // System.out.println("Aliasing nodes " + n1 + " and " + n2);
            nodeAliases.makeEquivalent(n1, n2);
        }

        void aliasCell(CellInterface c, HierName n1, HierName n2) {
            Iterator it = c.getSubcellPairs();
            if (!it.hasNext()) {
                if (c.isNode()) {
                    /* Node, add alias */

                    // Equivalent of getting the default values for the
                    // implied global ports, but a HACK.
                    if (n2.isGlobal()) {
                        n2 = HierName.trim(n2);
                    }

                    makeConnection(n1, n2);
                } else {
                    System.out.println("Non-node leaf cell in environment!");
                    System.out.println(c.getFullyQualifiedType() + ": " + n1);
                }
            } else {
                while (it.hasNext()) {
                    final Pair p = (Pair)it.next();
                    final CellInterface ci = (CellInterface)p.getSecond();
                    final HierName name = (HierName)p.getFirst();
                    aliasCell(ci, HierName.makeHierName(n1, name),
                                  HierName.makeHierName(n2, name));
                }
            }
        }
    }

    /** clear all the TCounts for all the nodes in the simulation **/
    public void clear_tcounts() {
        for (Node node : nodes.values()) {
            node.clearTCount();
        }
    }

    /** Return the total number of transition counts for all the nodes in the
     * simulation. **/
    public long tcounts_total() {
        final Set seen = new HashSet();
        long sum = 0;
        for (Node node : nodes.values()) {
            if (seen.add(node.getName())) sum += node.getTCount();
        }
        return sum;
    }


    /*
       @deprecated Need better reset scheme for devices.
     */
    public void clearEventQueues() {
        sched.clearEventQueues();
    }
    
    /** Get rid of an instantiation **/
    public void rm_instantiation(){
        /** reset class data members set during instantiation **/
        envName = null;
        envCell = null;
        resetPortName = null;
        resetNodeName = null;
        loaded = false;
        loadedFile = null;
        glitchEvent = null;
        interferenceEvent = null;
        unstabEvent = null;
        // clear history buffer, but still record same amount of history
        if (history != null) {
            history = new HistoryStore(history.capacity());
        }
        nodeCount = 0;
        
        /** remove all the watches associated with a given node **/
        
        for(Iterator i=nodes.values().iterator(); i.hasNext(); ) { 
            Node node = (Node)i.next();
            node.removeAllWatchers();
        }
        nodesToAliases = null;
        nodes.clear();
        rules.clear();
        nonAssertedRuleCount = 0;
        ERRORHandlingDoneYet = false;
        sched.clear();
        warnedCells.clear();
        constantNodes.clear();
        possiblyUnusedRules.clear();
        canonNodeNameToWatchedNodeNameMap.clear();
        measuredProcs.clear();
        measuredWarningContext = null;
        CspRuntimeAbstractDevice.clearAllCspDevices();
        boundaryDelays = new ArrayList<MeasuredDelay>();
        interiorOutputPorts = Collections.emptyList();
        routedInstancesByType = null;
        astaContext = null;
        savedCadenceInfo = null;
        savedOptCandidate = null;
        slintIgnores.clear();
        /** garbage collect here **/
        System.gc();
    }

    /**
     * Sets up node ERROR (if it doesn't already exist) and rules
     * driving it from GND and Vdd.  This ensures that GND and Vdd,
     * even if they aren't used for anything else, will be available
     * for reset.
     *
     * XXX: This might need to be split up so that the ERROR node is
     * created before the aliases are saved.
     **/
    private void createERRORHandling() {
        createNode(HierName.makeHierName("ERROR"));

        final HierName gndName = HierName.makeHierName("GND");
        final HierName vddName = HierName.makeHierName("Vdd");
        final HierName errorName = HierName.makeHierName("ERROR");
        final Node target = lookupNode(errorName);

        // GND => ERROR+
        createHierRule(new HierNameAtomicBooleanExpression(false, gndName),
                       target, ProductionRule.DOWN, 100, DIGITAL_TAU, false,
                       -1, -1,
                       false, false, true, null, null, Float.NaN, null, true,
                       null);
        createHierRule(new HierNameAtomicBooleanExpression(true, gndName),
                       target, ProductionRule.UP, 100, DIGITAL_TAU, false,
                       -1, -1,
                       false, false, true, null, null, Float.NaN, null, true,
                       null);

        // Vdd => ERROR-
        createHierRule(new HierNameAtomicBooleanExpression(true, vddName),
                       target, ProductionRule.DOWN, 100, DIGITAL_TAU, false,
                       -1, -1,
                       false, false, true, null, null, Float.NaN, null, true,
                       null);
        createHierRule(new HierNameAtomicBooleanExpression(false, vddName),
                       target, ProductionRule.UP, 100, DIGITAL_TAU, false,
                       -1, -1,
                       false, false, true, null, null, Float.NaN, null, true,
                       null);

        ERRORHandlingDoneYet = true;
    }

    private void createResetFork(boolean includeEnv) {
        createNode(resetNodeName);

        // _RESET => _RESET:body+
        final Node body =
            lookupNode(resetNodeName.appendString(RESET_BODY_SUFFIX));
        createHierRule(
                new HierNameAtomicBooleanExpression(false, resetNodeName),
                body, ProductionRule.DOWN, 100, DIGITAL_TAU, false, -1, -1,
                false, false, true, null, null, RESET_SLEW, null, true,
                null);
        createHierRule(
                new HierNameAtomicBooleanExpression(true, resetNodeName),
                body, ProductionRule.UP, 100, DIGITAL_TAU, false, -1, -1,
                false, false, true, null, null, RESET_SLEW, null, true,
                null);

        if (!includeEnv) return;

        // _RESET => _RESET:env+
        final Node env =
            lookupNode(resetNodeName.appendString(RESET_ENV_SUFFIX));
        createHierRule(
                new HierNameAtomicBooleanExpression(false, resetNodeName),
                env, ProductionRule.DOWN, 100, DIGITAL_TAU, false, -1, -1,
                false, false, true, null, null, RESET_SLEW, null, true,
                null);
        createHierRule(
                new HierNameAtomicBooleanExpression(true, resetNodeName),
                env, ProductionRule.UP, 100, DIGITAL_TAU, false, -1, -1,
                false, false, true, null, null, RESET_SLEW, null, true,
                null);
    }

    /**
     * Create dummy rules to simulate behavior of the environment to check for
     * short cutoff paths.  On input e1ofN channels, create rules from enable
     * to data; on output e1ofN channels, create rules from data to enable.
     **/
    private void createCutoffRules(final CellInterface cell,
                                   final int enableToData,
                                   final int dataToEnable,
                                   final HierName prefix) {
        (new CellUtils.MarkPort() {
            private void addRule(final Node trigger, final boolean negated,
                                 final Node target, final int count) {
                if (count > 0) {
                    final int dir =
                        (count % 2 == 0) ^ negated ? ProductionRule.DOWN
                                                   : ProductionRule.UP;
                    createHierRule(
                        new HierNameAtomicBooleanExpression(
                            negated, trigger.getName()),
                        target, dir, count * 100, DIGITAL_TAU, false, -1, -1,
                        false,
                        false, false, null, null, RESET_SLEW, null, true, null);
                }
            }
            protected void mark(final ChannelType channelType,
                                final String name,
                                final int direction) {
                final int rails = CellUtils.extractN(channelType.getTypeName());
                final Node e =
                    lookupNode(HierName.append(prefix, toHier(name + ".e")));
                for (int i = 0; i < rails; ++i) {
                    final Node d = lookupNode(
                        HierName.append(prefix, toHier(name + ".d[" + i + "]")));
                    if (direction < 0) {
                        if (enableToData > 0) {
                            addRule(e, false, d, enableToData);
                            addRule(e, true, d, enableToData);
                        }
                    } else {
                        if (dataToEnable > 0) {
                            addRule(d, false, e, dataToEnable);
                            addRule(d, true, e, dataToEnable);
                        }
                    }
                }
            }
        }).mark(cell);
    }

    private static HierName toHier(final String x) {
        try {
            return HierName.makeHierName(x, '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionError("Cannot create HierName from: " + x);
        }
    }

    private void aliasExtraPorts(final CellInterface env,
                                 final HierName envPrefix,
                                 final HierName cutPrefix,
                                 final AliasedSet nodeAliases) {
        for (Iterator i = env.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition port = (PortDefinition) i.next();
            final String mapped = env.getEnvExtraPortMapping(port.getName());
            if (mapped != null) {
                final HierName parent =
                    HierName.append(cutPrefix, toHier(mapped));
                (new CellUtils.MarkPort() {
                    protected void mark(final NodeType nodeType,
                                        final String name,
                                        final int direction) {
                        final HierName node = toHier(name);
                        final String part =
                            name.substring(port.getName().length());
                        final HierName cutNode = toHier(
                            StringUtil.replaceSubstring(parent + part, "][",
                                                        ","));
                        if (nodeAliases.contains(cutNode)) {
                            final HierName envNode =
                                HierName.append(envPrefix, node);
                            nodeAliases.makeEquivalent(cutNode, envNode);
                        } else {
                            System.err.println("warning: can't connect " + 
                                "extra port " + name + " to " + cutNode);
                        }
                    }
                }).mark(Collections.singletonList(port).iterator(), null,
                        PortDefinition.FORWARD);
            }
        }
    }

    static Triplet<HierName,CadenceInfo,HierName> localize(final HierName name)
    {
        return CellUtils.localize(name, get().savedCadenceInfo, true);
    }

    public static HierName canonize(final HierName name,
                                     final CadenceInfo ci) {
        final AliasedSet localNodes = ci.getLocalNodes();
        HierName canon = (HierName) localNodes.getCanonicalKey(name);
        if (canon == null) {
            HierName head = null;
            for (HierName h = name; h != null; h = h.tail()) {
                head = HierName.append(head, h.head());
                final CadenceInfo subci = ci.getSubcell(head);
                if (subci != null) {
                    final HierName subcanon = canonize(h.tail(), subci);
                    if (subcanon != null) {
                        final HierName full = HierName.append(head, subcanon);
                        canon = (HierName) localNodes.getCanonicalKey(full);
                        if (canon == null) canon = full;
                    }
                    break;
                }
            }
        }
        return canon;
    }


    /** Does the actual instantiation for loadFile and loadStream **/    
    public void instantiate(CellInterface c, CoSimParameters params)
        throws NoSuchEnvironmentException {
        AliasedSet nodeAliases = new AliasedSet(HierName.getComparator());
        CellInterface synth = null;
        CellInterface envblock = null;
        final CellInterface cut = c.getSubcell(envCell);
        /* One of the immediate subcells should be the CUT if an environment
         * name is specified.   */
        if (envName != null) {
            if (cut == null)
                throw new IllegalArgumentException(envCell + " not found");
            envblock = getEnvironmentBlock(cut);
            synth = makeSyntheticEnv(envblock, cut, envCell, nodeAliases);
            System.out.print("Collecting envblock aliases ");
            new DSimCellTraverser(new AliasCreator(nodeAliases), params).
                                                traverseCell(synth);
            new PortNodesTraverser(params, nodeAliases).traverse(synth);
            System.out.println("Done.");
        }
        System.out.print("Collecting aliases ");
        new DSimCellTraverser(new AliasCreator(nodeAliases), params).
                                        traverseCell(c);
        new PortNodesTraverser(params, nodeAliases).traverse(c);
        if (envblock != null) {
            aliasExtraPorts(envblock, toHier("_env"), envCell, nodeAliases);
        }
        System.out.println(" Done.");
        System.gc();
        System.out.print("Instantiating nodes...");
        saveAliases(nodeAliases);
        nodeAliases = null;
        System.out.println(" Done.");
        System.gc();
        if (! ERRORHandlingDoneYet) createERRORHandling();
        if (resetNodeName != null) createResetFork(envName != null);

        // adding dummy production rules to model environment for cutoff
        final int cutoffEnableToData =
            (int) getDouble("cutoff.enableToData", -1);
        final int cutoffDataToEnable =
            (int) getDouble("cutoff.dataToEnable", -1);
        if (cutoffEnableToData > 0 || cutoffDataToEnable > 0) {
            if (verbose) {
                System.out.println("Adding cutoff environment rules...");
            }
            createCutoffRules(cut, cutoffEnableToData, cutoffDataToEnable,
                              envCell);
        }

        if (useDelayMode(MEASURED_TAU)) prepareMeasured();
        System.out.print("Applying models ");

        final String candidateFqcn = getParameter("asta.candidate");
        final boolean astamizer = isAstaEnabled() && candidateFqcn != null;

        final Cadencize cadencizer = new Cadencize(false);
        errors = false;
        final ModelCreator creator = new ModelCreator(params, cadencizer,
                astamizer ? new HashSet<Node>() : null);
        errors |= creator.build(c, CoSimParameters.DIGITAL, null, false);
        if (astamizer) {
            interiorOutputPorts = creator.getInteriorPorts(c);
        }
        if (synth != null)
            errors |= new ModelCreator(params, cadencizer).build(synth,
                                                       CoSimParameters.DIGITAL, null,
                                                       true);
        if (astamizer) {
            try {
                final CellInterface candidate = getCastFileParser().getFullyQualifiedCell(candidateFqcn);
                errors |= new OptimizeThresholdCreator(params, cadencizer).build(candidate, CoSimParameters.DIGITAL, envCell, true);
                savedOptCandidate = SimpleCell.convert(candidate);
            } catch (Exception e) {
                System.err.print("Failed to parse cell specified by asta.candidate: ");
                if (e instanceof CastSemanticException) {
                    System.err.println();
                    ExceptionPrettyPrinter.printException(
                            (CastSemanticException) e, System.err);
                } else {
                    System.err.println(e.getMessage());
                }
            }
        }

        System.out.print(" Done");
        if (errors)
            System.out.print(", with errors");
        System.out.println('.');
        System.gc();
        // System.out.println("Mem free = " + r.freeMemory() + "/" + r.totalMemory());
        loaded = true;
    }

    private class DSimCellTraverser {
        final JFlat.CellProcessor proc;
        final CoSimParameters params;

        public DSimCellTraverser(final JFlat.CellProcessor p, 
                                 final CoSimParameters params) {
            proc = p;
            this.params = params;
        }

        public void traverseCell(final CellInterface cell) {
            traverseIter(null, cell, 0);
        }

        private void traverseIter(final HierName prefix, 
                                  final CellInterface cell, final int depth) {
            proc.process(prefix, cell, null, depth, null);
            
            for (Iterator i = cell.getAllSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                HierName fullName = HierName.append(prefix, subcellName);
                final int behavior = (params == null) ? CoSimParameters.DIGITAL :
                                      params.lookupBehavior(CoSimParameters.stripCoSimDigitalSuffix(fullName));

                if (subcell.isNode())
                    continue;
        
                if ((behavior != CoSimParameters.UNSPEC) &&
                        (behavior != CoSimParameters.DIGITAL))
                    traversePorts(fullName, subcell, depth+1);

                if (((behavior & CoSimParameters.DIGITAL) != 0) &&
                        (behavior != CoSimParameters.DIGITAL)) {
                    fullName = CoSimParameters.addCoSimDigitalSuffix(fullName);
                }

                if ((behavior == CoSimParameters.UNSPEC) ||
                        ((behavior & CoSimParameters.DIGITAL) != 0))
                    traverseIter(fullName, subcell, depth+1);
            }
        }

        private void traversePorts(final HierName prefix,
                                   final CellInterface cell, final int depth) {
            for (Iterator i = cell.getPortSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                final HierName fullName = HierName.append(prefix, subcellName);
        
                traverseIter(fullName, subcell, depth+1);
            }
        }
    }

    /** Traverse cells, setting up the correct aliases for the port nodes  
     **   which are co-simulated.                                         **/
    private class PortNodesTraverser {
        private final CoSimParameters params;
        AliasedSet aliasSet;

        PortNodesTraverser(final CoSimParameters params, AliasedSet aliasSet) {
            this.params = params;
            this.aliasSet = aliasSet;
        }

        public void traverse(CellInterface c) {
            traverseIter(null, c, 1);
        }

        private void traverseIter(final HierName prefix, 
                                  final CellInterface cell, 
                                  final int currentDepth) {
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                HierName fullName = HierName.append(prefix, subcellName);
                final int behavior = (params == null) ? CoSimParameters.DIGITAL :
                                       params.lookupBehavior(CoSimParameters.stripCoSimDigitalSuffix(fullName));

                if (subcell.isNode())
                    continue;

                if (((behavior & CoSimParameters.DIGITAL) != 0) &&
                        (behavior != CoSimParameters.DIGITAL)) {
                    HierName cosimName =
                        CoSimParameters.addCoSimDigitalSuffix(fullName);
                    process(fullName, cosimName, subcell);
                    fullName = cosimName;
                }

                if ((behavior == CoSimParameters.UNSPEC) ||
                                ((behavior & CoSimParameters.DIGITAL) != 0))
                    traverseIter(fullName, subcell, currentDepth+1);
            }
        }

        /** Alias all port nodes between the prefix and cosimName's  **/
        private void process(final HierName prefix, final HierName cosimName, 
                             final CellInterface cell) {
            (new CellUtils.MarkPort() {
                private boolean flatten = false;
                protected void mark(final StructureType structureType,
                                    final String name, final int direction) {
                    final boolean isDft =
                        !flatten &&
                        CellUtils.isDftChannel(cell, structureType.getTag());

                    if (isDft) flatten = true;
                    super.mark(structureType, name, direction);
                    if (isDft) flatten = false;
                }
                protected void mark(final ChannelType channelType,
                                    final String name, final int direction,
                                    final boolean inArray) {
                    if (flatten) {
                        super.mark(channelType, name, direction, inArray);
                    }
                }
                protected void mark(final NodeType nodeType, final String name,
                                    final int direction) {
                    HierName nodenm;
                    try {
                        nodenm = HierName.makeHierName(name, '.');
                    } catch (InvalidHierNameException e) {
                        throw new AssertionError("Cannot create HierName from" +
                                                 name);
                    }
                    if (nodenm.isGlobal())
                        nodenm = HierName.trim(nodenm);
                    aliasSet.makeEquivalent(
                                      HierName.append(prefix, nodenm), 
                                      HierName.append(cosimName, nodenm));
                }
            }).mark(cell);
        }
    }

    /** Given the CellInterface ci containing the subcell named envCell, return
     **   its environment block of the name envName.                    **/
    private CellInterface getEnvironmentBlock(final CellInterface ci)
        throws NoSuchEnvironmentException {
        return ci.getEnvironment(envName);
    }

    /** Instantiate a cell of the given name defined in the loaded cast file **/
    public void instantiateByName(String cellName,
                                  String instanceName, 
                                  CoSimParameters params,
                                  CastFileParser castParser)
        throws CastSemanticException,
               CastSyntaxException,
               IOException,
               NoSuchCellException,
               NoSuchEnvironmentException {
        final CellImpl fakeEnv =
            new CellImpl("fake", null, CellImpl.SYNTHETIC_CELL);
        CellInterface ci = null;

        final HierName instance;
        try {
            instance = HierName.makeHierName(instanceName, '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionError(e);
        }

        //If the cell name is fully qualified then
        //use a cast file parser to get a CellInterface.
        if ( cellName.lastIndexOf( '.' ) >= 0 ) {
            if (castParser == null)
                castParser = getCastFileParser();

            // do not pass in fakeEnv to avoid pollution caused by automatic
            // inlining
            ci = castParser.getFullyQualifiedCell(cellName, null, instance);

            EnvironmentAliases envAliases =
                new EnvironmentAliases(instance, null) {
                    void makeConnection(final HierName n1, final HierName n2) {
                        fakeEnv.addConnection(n1, n2);
                    }
                };

            // create implied port connections from ci to fakeEnv
            final Map<HierName,HierName> connections =
                new HashMap<HierName,HierName>();
            HierName upperReset = null, lowerReset = null;
            for (Iterator i = ci.getPortSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName hinst = (HierName) p.getFirst();
                final String sinst = hinst.getAsString('.');
                if (ci.isImpliedPort(sinst)) {
                    final HierName parent;
                    try {
                        parent =
                            HierName.makeHierName(
                                    ci.getParentImpliedPort(sinst), '.');
                    } catch (InvalidHierNameException e) {
                        throw new AssertionError(e);
                    }
                    if (parent.equals(DSimUtil._RESET_NAME)) upperReset = hinst;
                    if (parent.equals(DSimUtil._Reset_NAME)) lowerReset = hinst;
                    connections.put(hinst, parent);
                }
            }

            resetPortName =
                upperReset == null ? (lowerReset == null ? null : lowerReset)
                                   : upperReset;

            if (resetPortName != null) {
                resetNodeName = connections.get(resetPortName);
                connections.put(resetPortName,
                                resetNodeName.appendString(RESET_BODY_SUFFIX));
            }

            for (Map.Entry<HierName,HierName> entry : connections.entrySet()) {
                final HierName hinst = entry.getKey();
                final HierName parent = entry.getValue();
                final CellInterface subci = ci.getSubcell(hinst);
                fakeEnv.addSubcellPair(parent, subci, false);
                envAliases.aliasCell(subci, parent,
                        HierName.makeHierName(instance, hinst));
            }
        } else {
            //If the cell name was not a fully qualified cell name
            //we'll look in the loaded file for the cell.
            if (loadedFile != null)
                ci = loadedFile.getCell(cellName, fakeEnv, instance);
        }

        if (ci == null)
            throw new NoSuchCellException(cellName);

        fakeEnv.addSubcellPair( instance, ci, false );
        fakeEnv.setHasCompleteSubcellsBlock();

        instantiate( fakeEnv, params );

        if (verbose) {
           System.out.println("envCell is " + envCell +
                  ".  instanceName is " + instanceName + ".");
        }
    }

    private void createHierRule(AndBooleanExpressionInterface term, Node n,
                                int direction, int delay, byte delay_type,
                                boolean timed, float fastDelay,
                                float slowDelay, boolean isochronic,
                                boolean absoluteDelay, boolean assertedP,
                                Pair[] delays,
                                HierName prefix, float defaultSlew,
                                AliasedSet localNodes,
                                final boolean coverage_ignore,
                                final Map<Pair<List<Node>,BitSet>,
                                          Rule> astaGrayboxRules) {
        final LinkedList measuredDelay =
            delays == null ? null : new LinkedList();

        byte coverage_requirement = (coverage_ignore ? Rule.IGNORE_COVERAGE :
                                    Rule.MUST_BE_COVERED);

        int length = term.getConjuncts().size();
        Iterator i = term.getConjuncts().iterator();
        int j = 0;
        int sense [] = new int[length];
        Node guards [] = new Node[length];
        boolean defaultDelay = false;
        final Set possiblyUnused = (Set) possiblyUnusedRules.get(n.getName());
        final Collection prefixedGuards;
        if (possiblyUnused == null) {
            prefixedGuards = null;
        } else {
            prefixedGuards = new ArrayList(length);
        }
        while (i.hasNext()) {
            HierNameAtomicBooleanExpression a;
            a = (HierNameAtomicBooleanExpression) i.next();
            final HierName h = a.getName();
            
            final HierName fullName = prefixName(prefix, h);
            guards[j] = lookupNode(fullName);
            guards[j].setSlew(defaultSlew);
            sense[j] = a.getSense() ? 1 : 0;
            if (prefixedGuards != null)
                prefixedGuards.add(new HierNameAtomicBooleanExpression(
                        a.getSense(), guards[j].getName()));

            if (delays != null) {
                for (int k = 0; k < delays.length; ++k) {
                    final Node trigger = (Node) delays[k].getFirst();
                    if (trigger == null) {
                        if (!defaultDelay) {
                            measuredDelay.addLast(new Pair(null, delays[k].getSecond()));
                            defaultDelay = true;
                        }
                    } else if (guards[j].equals(trigger)) {
                        measuredDelay.addFirst(new Pair(guards[j], delays[k].getSecond()));
                    }
                }
            }
            j++;
        }

        // if this has been declared unused by an unused_prs directive, ignore
        // it for coverage purposes
        if (prefixedGuards != null) {
            final AndBooleanExpression and =
                new AndBooleanExpression(term.getSense(), prefixedGuards);
            if (possiblyUnused.remove(and))
                coverage_requirement = Rule.MUST_NOT_BE_COVERED;
        }

        final Rule newrule =
            new Rule(guards, sense, n, direction, delay, delay_type, timed,
                     fastDelay, slowDelay, isochronic,
                     absoluteDelay, assertedP,
                     measuredDelay == null ? null :
                        (Pair[]) measuredDelay.toArray(new Pair[0]),
                     coverage_requirement, prefix);
        if (isAstaEnabled() && !assertedP && astaGrayboxRules != null) {
            // collapse grayboxed rules
            newrule.falseConjuncts = 1;
            if (n.enabler != null) {
                // make sure all nodes are involved in grayboxing
                boolean grayboxed = true;
                for (Node guard : guards) {
                    if (guard.enabler == null) {
                        grayboxed = false;
                        break;
                    }
                }
                if (grayboxed) {
                    // guards + target
                    final List<Node> g = new ArrayList<Node>(guards.length + 1);
                    // senses of guards and target
                    final BitSet s = new BitSet(guards.length + 1);

                    for (int k = 0; k < guards.length; ++k) {
                        g.add((Node) guards[k].enabler);
                        s.set(k, sense[k] == 1);
                    }
                    g.add((Node) n.enabler);
                    s.set(guards.length, direction == 1);

                    // increment instance count (stored in the falseConjuncts
                    // field of Rule) of an existing rule, or store a new rule
                    final Pair<List<Node>,BitSet> key =
                        new Pair<List<Node>,BitSet>(g, s);
                    final Rule oldrule = astaGrayboxRules.get(key);
                    if (oldrule == null) {
                        astaGrayboxRules.put(key, newrule);
                    } else {
                        oldrule.falseConjuncts++;
                        newrule.falseConjuncts = 0;
                        newrule.setCanonRule(oldrule);
                    }
                }
            }
        }
        rules.add(newrule);
        if (!assertedP) nonAssertedRuleCount++;
        // System.out.println("Adding rule " + newrule.getGuardList());
    }

    private HierName prefixName(HierName prefix, HierName suff) {
        return (castVersion.equals("1")) ? HierName.prefixName(prefix, suff)
                                         : HierName.append(prefix, suff);
    }

    /** Remove terminal '!'s (global signals). **/
    public HierName stripGlobals(HierName h) {
        if (h.isGlobal())
            return HierName.trim(h);
        else
            return h;
    }

    /** Looks up a node in our node map. Due to a bug in the parser, creates unfound nodes. **/
    private Node lookupNode(HierName h) {
        //String name = sanitizeName(h);
        //Object lookup = nodes.getDigitalNode(name);
        h = stripGlobals(h);
        Object lookup = nodes.get(h);
        if (lookup==null) { 
            //throw new NoSuchElementException("No Node: "+name);  
            //System.err.println("Warning implicitly created node: "+name);
            return createNode(h);
        }
        return (Node)lookup;
    }
    /** Looks up a node in our node map.  If not present, use the passed in node. **/
    private Node addOrLookupNode(HierName h, Node n) {
        h = stripGlobals(h);
        Object lookup = nodes.get(h);
        if (lookup==null) {
            /* Add n to nodes */
            nodes.put(h, n);
            nodeCount++;
            return n;
        }
        return (Node)lookup;
    }
    /** Creates an alias for an already existant node in the node map. **/
    private Node aliasNode(HierName h, HierName alias) {
        //String name = sanitizeName(h);
        //Object lookup = nodes.getDigitalNode(name);
        h = stripGlobals(h);
        Object lookup = nodes.get(h);
        return aliasNode((Node)lookup, alias);
    }
    /** Creates an alias for an already existant node in the node map. **/
    private Node aliasNode(Node node, HierName alias) {
        // System.out.println("Aliasing "+node.getName()+" to "+alias);
        //nodes.alias(al, node);
        // System.err.println("Aliasing "+al+" to "+node.getName());
        alias = stripGlobals(alias);
        if (nodes.get(alias) != null && nodes.get(alias) != node) {
            System.err.println("Alias for " + alias + " to " + node + " created, but already listed as " + nodes.get(alias) + ".");
        }
        nodes.put(alias, node);
        return node;
    }
    /** Creates a new node and puts it in the node map. **/
    private Node createNode(HierName h) {
        //String name = sanitizeName(h);
        //Node node = new Node(name);
        //nodes.put(name, node);
        //node.setLabel(nodes.getMixNode(name));
        HierName n = stripGlobals(h);
        Node node = new Node(h);
        nodes.put(h, node);
        nodeCount++;
        return node;
    }

    /** 
     * Controls printing and recording of warnings. 
     * Whenever warning tracking is set, the anomalous event state
     * interferenceEvent, glitchEvent, and unstabEvent objects are
     * cleared
     **/
    public void setWarn(boolean w) { 
	//commented out as part of fixing bug#2716, (abe)
        //if (w) {
	//  interferenceEvent = null;
	//  glitchEvent = null;
	//  unstabEvent = null;
        //}
        warn=w; 
    }
    /** Returns the warning tracking state **/
    public boolean getWarn() { return warn; }

    /** controls stoping on warnings */
    public void setError(boolean e) { error=e; }
    public boolean getError() { return error; }

    /** controls printing of extra info */
    public void setVerbose(boolean v) { sched.setVerbose(verbose=v); }
    public boolean getVerbose() { return verbose; }
    
    /** Finds and returns a node in the map, or returns null. **/
    //public Node findNode(String nodeName) { return nodes.getDigitalNode(nodeName); }
    /** Prints out all of the aliases for a given node. **/
    //public void listAliases(String name) {
    //    String list[] = nodes.getAliases(name);
    //    if (list==null) { System.out.println("Aliases: Node not found."); }
    //    for (int i=0; i<list.length; i++) {
    //        System.out.println("  "+list[i]);
    //    }
    //}
    //public String[] getNodeCompletions(String s) {
    //   return nodes.getCompletions(s);
    //}

    /** Finds and returns a node in the map, or returns null. **/
    public Node findNode(String nodeName) {
        HierName h;
        try {
            h = HierName.makeHierName(nodeName, '.');
        } catch (InvalidHierNameException e) {
            return null;
        }
        return nodes.get(h);
    }
    /** Finds and return a node in the map, or creates it. **/
    public Node findOrAddNode(String nodeName) {
        HierName h;
        try {
            h = HierName.makeHierName(nodeName, '.');
        } catch (InvalidHierNameException e) {
            return null;
        }
        Node n = nodes.get(h);
        if (n == null) {
            n = createNode(h);
        }
        return n;
    }
            
    /** Finds and returns a node in the map, or returns null. **/
    public Node findNode(HierName nodeName) {
        return nodes.get(nodeName);
    }
    /** Prints out all of the aliases for a given node. **/
    public void listAliases(Node n) {
        Iterator i = nodes.keySet().iterator();
        while (i.hasNext()) {
            HierName h = (HierName) i.next();
            Node cur = (Node) nodes.get(h);
            if (n==cur) { System.out.println("  "+h.getAspiceString()); }
        }
    }
    /** Prints out all of the nodes that start with a given string. **/
    /*
    public void listNodes(String prefix) {
        Iterator i = nodes.getDigitalNodes(prefix);
        while (i.hasNext()) {
            Node n = (Node)i.next();
            String s = n.getName().getAspiceString();
            if (prefix==null || s.startsWith(prefix)) {
                System.out.println("  "+s);
            }
        }
    }
    */

    /** Prints out all of the nodes that have a given value, return true
	if we actually found a node like this; useful for STATUS U **/
    public boolean listNodes(int value) {
	
        Hashtable h = new Hashtable();
        boolean found = false;
        boolean header = true;
        for (Node n : nodes.values()) {
            if (n.getValue()==value) {
                found = true;   //we actually found a node this state
                if (header) {
                    System.out.println("Nodes with value: "
				       +Node.getNameForValue(value));
                    header = false;
                }
                // aliases currently map to same name, so
                // ensure just one output...
                if (h.get(n)==null) {
                    String s = n.getName().getAspiceString();
                    h.put(n,n);
                    System.out.println("  "+s);
                }
            }
        }
        System.out.println();
        return found;
    }
    
    /** Count number of nodes. */
    public int countNodes() {
        return nodes.size();
    }

    /** Count number of rules. */
    public int countRules() {
        return rules.size();
    }

    /** Count number of non-asserted rules. */
    public int countNonAssertedRules() {
        return nonAssertedRuleCount;
    }

    /** Returns a set of all nodes that currently have the given value. **/
    public Set getNodesWithValue(byte value) {
        HashSet set = new HashSet();
        for (Node n : nodes.values()) {
            if (n.getValue()==value) set.add(n);
        }
        return set;
    }

    /** Adds matching nodes or names to <code>v</code> based on <code>node</code>. **/
    public boolean expandNodes(String name, Collection v, boolean node, boolean nowarn) {
        //TriMapIterator i = (TriMapIterator)nodes.getDigitalNodes(name);
        //if (!i.hasNext()) {
        //    if (!nowarn)  { System.err.println("WARNING: node " + name + " undefined"); }
        //    return false;
        if (!isGlob(name)) {
            HierName h;
            try {
                h = HierName.makeHierName(name, '.');
            } catch (InvalidHierNameException e) {
                if (!nowarn)  { System.err.println("WARNING: node " + name + " Invalid"); }
                return false;
            }
            if (!nodes.containsKey(h)) {
                if (!nowarn)  { System.err.println("WARNING: node " + name + " undefined"); }
                return false;
            }
            if (node) {
                v.add(nodes.get(h));
            } else {
                v.add(h);
            }
            return true;
        }
        // Is a glob
        try {
            HierName.Glob1 g = new HierName.Glob1(name);
            HashSet s = new HashSet();
            boolean found = false;
            for (HierName h : nodes.keySet()) {
                if (!g.matches(h)) {
                    continue;
                }
                if (node)  
                    v.add(nodes.get(h));  
                else { 
                    if (!s.contains(nodes.get(h))) {
                        v.add(h); 
                        s.add(nodes.get(h));
                    }
                }
                found = true;
            }
            if (!found && !nowarn) {
                System.err.println("WARNING: node " + name + " undefined");
            }
            return found;
        } catch (UnrepresentableCharException e) {
            System.err.println("WARNING: unrepresentable string '" + name + "'");
            return false;
        }
    }
    /** Lists all of the nodes (with value) that match a single wildcard expression. **/
    /*
    public void listNodeValue(String nodeName) {
        Iterator i = nodes.iterator();
        if (!i.hasNext()) {
            System.err.println("Get Node " + nodeName + " not found");
        }
        while (i.hasNext()) {
            if (node) { v.add(i.next()); } 
            else { v.add(i.getLabel()); i.next(); }
        }
        return true;
    }
//    // Lists all of the nodes (with value) that match a single wildcard expression.
//    public void listNodeValue(String nodeName) {
//        TriMapIterator i = (TriMapIterator)nodes.getDigitalNodes(nodeName);
//        if (!i.hasNext()) {
//            System.err.println("Get Node " + nodeName + " not found");
//        }
//        while (i.hasNext()) {
//            System.out.println("  "+(Node)i.next()+" ("+i.getLabel()+")");
//        }
//    }
    */
    /** Gets the value for a single node. **/
    public byte getNodeValue(String nodeName) {
        Node n = findNode(nodeName);
        if (n == null) {
            System.err.println("Get Node " + nodeName + " not found");
            return -1;
        } else {
            return n.getValue();
        }
    }
    /** Sets the value for a single node. **/
    public int setNodeValue(String nodeName, byte val) {
        Node node = findNode(nodeName);
        if (node!=null) {
            //findNode(nodeName).setValueAndEnqueueDependents(val);
            node.setValueAndEnqueueDependents(val);
            return 1;
        }
        System.out.println("Set Node: "+nodeName+" not found");
        return -1;
    }
    /** Tells the simulation to halt (or not) when a given node changes **/
    public int breakpointNode(Node node, boolean on) {
            node.setBreakpoint(on);
            return 1;
    }
    /** Prints a list of nodes that will halt the simulation on change. **/
    public void listBreakpointNodes() {
        Vector uniqueNodes = new Vector();
        for (Node n : nodes.values()) {
            if (n.getBreakpoint() && !uniqueNodes.contains(n)) { 
                uniqueNodes.add(n);
                System.out.println("    "+n.getName());
            }
        }
    }
    /** Tells the simulation to print out when a given node changes **/
    public void watchNode(Node node, HierName name) {
       watchNode(node, name, this);
    }
    /** Tells the simulation to print out when a given node changes **/
    public void watchNode(Node node, HierName name, NodeWatcher w) {
        if (node!=null) {
            HierName oldWatchName = (HierName)
                canonNodeNameToWatchedNodeNameMap.put(node.getName(),
                                                      name);
            if (oldWatchName != null) {
                System.out.println("Node " + node.getName()
                                                 .getAsString('.') +
                                   " was already being watched as " +
                                   oldWatchName.getAsString('.') + '.');
                System.out.println("Now watched as " +
                                   name.getAsString('.') + " instead.");
            }
            node.addWatch(w);
        }
    }
    /** Tells the simulation not to print out when a given node changes **/
    public void unwatchNode(Node node) {
       unwatchNode(node, this);
    }
    /** Tells the simulation not to print out when a given node changes **/
    public void unwatchNode(Node node, NodeWatcher w) {
        if (node!=null) {
            canonNodeNameToWatchedNodeNameMap.remove(node.getName());
            node.removeWatch(w);
        }
    }
    /** Prints a list of nodes that will notify on change. 
        Does not account for watchAll. **/
    public void listWatchNodes() {
        Vector uniqueNodes = new Vector();
        for (Node n : nodes.values()) {
            if (n.isWatcher(this) && !uniqueNodes.contains(n)) { 
                uniqueNodes.add(n);
                System.out.println("    "+n.getName());
            }
        }
    }
    /** Turns on outputing when any node changes. **/
    public void watchAllNodes(boolean watch) { Node.watchAll = watch; }
    /** Are we outputting for all nodes? **/
    public boolean watchAllNodes() { return Node.watchAll; }
    /** Turns on printing of node transition counts. **/
    public void showTCounts(boolean ct) { countTransitions=ct; }
    /** Are we printing transition counts? **/
    public boolean showTCounts() { return countTransitions; }
    /** Returns a string of rules and debugging info for a given node. **/
    public String listNodeRules(Node node) {
        return node.listRules();
    }
    /** Returns a list of Rules that target a given node. **/
    public List<Rule> getTargetingRules(Node n) {
        List<Rule> ret = new ArrayList<Rule>();
        for (int i=0; i<rules.size(); i++) {
            Rule cur = (Rule)rules.elementAt(i);
            if (cur.target==n) { ret.add(cur); }
        }
        return ret;
    }
    /** Returns a set of Nodes that affect a given Rule. **/
    public Set<Node> getTargetingNodes(Rule r) {
        Set<Node> ret = new HashSet<Node>();
        for (Node n : nodes.values()) {
            // ensure no multiples from aliases
            if (n.targets(r)) ret.add(n);
        }
        return ret;
    }

    /** Prints out all nodes that are directly affected by the given node. **/
    public void listFanout(Node n, String indent, int level) {
        final Set<Node> uniqueNodes =
            new TreeSet<Node>(NaturalStringComparator.getInstance());
        for (Rule r : n.getTargets()) {
             Node n1 = r.target;
             uniqueNodes.add(n1);
        }
        for (Node n1 : uniqueNodes) {
            System.out.println(indent + n1);
            if (level > 0) {
                listFanout(n1, indent + "    ", level - 1);
            }
        }
    }

    /** Prints out all nodes that directly affect the given node. **/
    public void listFanin(Node n, String indent, int level) {
        final Set<Node> uniqueNodes =
            new TreeSet<Node>(NaturalStringComparator.getInstance());
        for (Rule r : getTargetingRules(n)) {
            for (Node n1 : getTargetingNodes(r)) {
                uniqueNodes.add(n1);
            }
        }
        for (Node n1 : uniqueNodes) {
             System.out.println(indent + n1);
             if (level > 0) {
                 listFanin(n1, indent + "    ", level - 1);
             }
        }
    }

    private static final class HistoryStore implements Iterable<HistoryRecord> {
        private final HistoryRecord[] data;
        private int start;
        private int size;

        public HistoryStore(final int capacity) {
            if (capacity < 1) {
                throw new IllegalArgumentException("Capacity must be >= 1");
            }

            this.data = new HistoryRecord[capacity];
            this.start = 0;
            this.size = 0;
        }

        public HistoryStore(final int capacity, final HistoryStore old) {
            this(capacity);
            if (old != null) {
                size = Math.min(capacity, old.size());
                int idx = Math.max(0, old.start - size);
                int len = old.start - idx;
                if (len > 0) {
                    System.arraycopy(old.data, idx, data, size - len, len);
                }

                int remain = size - len;
                if (remain > 0) {
                    System.arraycopy(old.data, old.capacity() - remain,
                                     data, 0, remain);
                }
                start = size == data.length ? 0 : size;
            }
        }

        public synchronized void add(final HistoryRecord record) {
            data[start++] = record;
            if (start == data.length) start = 0;
            if (size < data.length) size++;
        }

        public Iterator<HistoryRecord> iterator() {
            return new Iterator<HistoryRecord>() {
                private int remain = size;
                private int pos = start;
                public boolean hasNext() {
                    return remain > 0;
                }
                public HistoryRecord next() {
                    if (hasNext()) {
                        pos--;
                        if (pos < 0) pos += data.length;
                        remain--;
                        return data[pos];
                    } else {
                        throw new NoSuchElementException();
                    }
                }
                public void remove() {
                    throw new UnsupportedOperationException();
                }
            };
        }

        public int size() {
            return size;
        }

        public int capacity() {
            return data.length;
        }
    }

    /** Storage structure for node change recording. **/
    public static final class HistoryRecord {
        public final Event cause;
        public final Node target;
        public final long time;
        public final int tcount;
        public final byte newVal;
        public final byte delay_type;
        public final float slew;
        HistoryRecord(Node _target, Event _cause, long _time, int _tcount, byte _newVal, float _slew, byte _delay_type) {
            target=_target; cause=_cause; time=_time; tcount=_tcount; newVal=_newVal; slew=_slew; delay_type=_delay_type;
        }

        public String toString(final boolean measured,
                               final boolean estimated,
                               final boolean delay) {
            if (delay) {
                final int d = getDelay();
                final String fmt = measured ? " %4s" : " %6s";
                final String msg = d < 0 ? "---" : Integer.toString(d);
                return " " + getNodeString(target, time, tcount, slew,
                                           delay_type, newVal, measured,
                                           estimated, String.format(fmt, msg));
            } else {
                return toString(measured, estimated);
            }
        }

        public String toString(final boolean measured,
                               final boolean estimated) {
            return " " + getNodeString(target, time, tcount, slew, delay_type,
                                       newVal, measured, estimated);
        }

        /**
         * Return either the digital delay or estimated delay associated with
         * the rule that triggered this transition, or -1 if there is no rule
         * (for example, when transition is caused by CSP).
         **/
        public int getDelay() {
            final MutableInt delay = new MutableInt(-1);
            if (cause instanceof Node) {
                final Node enabler = (Node) cause;
                enabler.foreachRule(new Node.RuleFunc() {
                    public void accept(Rule r, int sense, Node n) {
                        final char dir = r.getDirection();
                        if (r.target == target &&
                            (dir == '+' && newVal == Node.VALUE_1 ||
                             dir == '-' && newVal == Node.VALUE_0)) {
                            delay.set(r.getDelay());
                        }
                    }
                });
            }
            return delay.get();
        }
    }
    /** Circular buffer of past node changes. **/
    HistoryStore history = null;
    boolean historyFilterCritical = false;
    /** Are we currently recording node changes? **/
    boolean recordHistory(final boolean critical) {
        return history != null && (!historyFilterCritical || critical);
    }
    /** Allocates average storage of <code>sz</code> events per node. **/
    public void setHistoryPerNode(int sz) {
        setHistoryPerNode(sz, false);
    }
    public void setHistoryPerNode(int sz, boolean filterCritical) {
        setHistoryCap(sz*nodeCount);
        historyFilterCritical = filterCritical;
    }
    /** Allocates storage for exactly cap change events. **/
    public void setHistoryCap(int cap) {
        // TODO truncate history as nec
        if (cap < 0)
            cap = 0;
        history = cap == 0 ? null : new HistoryStore(cap, history);
        //System.out.println("Set history cap to :"+cap);
    }
    /** Put a change event into the next slot in the circular buffer. 
        Allocates space as nec. **/

    public void recordEvent(Node n, Event cause, long time, int tcount,
                            byte newVal, float slew, byte delay_type) {
        history.add(new HistoryRecord(n, cause, time, tcount, newVal, slew,
                                      delay_type));
    }
    /** Execute an action for each transition in the history for a single node,
     *  or each prior enabling node based on <code>swithchNodes</code> **/
    public void trackNode(HierName name, boolean switchNodes,
                          UnaryAction<HistoryRecord> action) {
        if (history==null || history.size()==0) { return; }
        Node node = findNode(name);
        if (node==null) {
            System.err.println("History/Critical: Node not found: "+name); 
        } else {
            for (HistoryRecord h : history) {
                if (h.target==node) {
                    action.execute(h);
                    if (switchNodes && (h.cause instanceof Node))  
                        node = (Node)h.cause;
                }
            }
        }
    }
    /** Prints out history for a single node, or each prior enabling node 
        based on <code>swithchNodes</code> **/
    private void trackNode(final HierName name, final boolean switchNodes,
                           final boolean delay, final PrintWriter pw) {
        final boolean measured = useDelayMode(MEASURED_TAU);
        final boolean estimated = useDelayMode(ESTIMATED_TAU);
        trackNode(name, switchNodes,
                  new UnaryAction<HistoryRecord>() {
                      public void execute(final HistoryRecord h) {
                          pw.println(h.toString(measured, estimated, delay));
                      }
                  });
    }
    /** Prints out history for a single node. **/ 
    public void listNodeHistory(HierName name, boolean delay, PrintWriter pw) {
        trackNode(name, false, delay, pw);
    }
    /** Prints out history for each prior enabling node. **/ 
    public void listNodeEnablers(HierName name, boolean delay, PrintWriter pw) {
        trackNode(name, true, delay, pw);
    }

    public void findCriticalCycle(final HierName name,
                                  final Collection<HistoryRecord> cycle) {
        final MutableInt most = new MutableInt(Integer.MIN_VALUE);
        final LinkedHashMap<Pair<Node,Byte>,MutableInt> trans =
            new LinkedHashMap<Pair<Node,Byte>,MutableInt>();
        trackNode(name, true,
                  new UnaryAction<HistoryRecord>() {
                      public void execute(final HistoryRecord h) {
                          final Pair<Node,Byte> key =
                              new Pair<Node,Byte>(h.target, h.newVal);
                          MutableInt count = trans.get(key);
                          if (count == null) {
                             count = new MutableInt(0);
                             trans.put(key, count);
                          }
                          most.max(count.inc());
                      }
                  });

        // not enough data
        if (most.get() < 3) return;

        Pair<Node,Byte> key = null;
        for (Map.Entry<Pair<Node,Byte>,MutableInt> entry : trans.entrySet()) {
            if (most.equals(entry.getValue())) {
                key = entry.getKey();
                break;
            }
        }

        final Pair<Node,Byte> fkey = key;
        most.set(3);
        trackNode(key.getFirst().getName(), true,
                  new UnaryAction<HistoryRecord>() {
                      boolean started = false;
                      public void execute(final HistoryRecord h) {
                          final boolean matched =
                              h.target == fkey.getFirst() &&
                              h.newVal == fkey.getSecond().byteValue();
                          if (matched) started = true;
                          if (started) {
                              if (most.get() > 0) {
                                  cycle.add(h);
                                  if (matched) most.dec();
                              }
                          }
                      }
                  });
    }
    
    /** Returns the number of queued random events. **/
    public int randomCount() { return sched.randomCount(); }
    /** Returns the number of queued timed events. **/
    public int timedCount() { return sched.timedCount(); }
    /** Returns string of all pending events. Non-node events are ignored. **/
    public String pendingList() { return sched.pendingList(); }
    /** Asks the scheduler to cycle. <code>time</code> indicates: 0 - cycle once,
        positive - cycle until that time, negative - cycle until done or interrupted. **/
    public void cycle(long time) {
        // break out cycle into the 3 cases...
        if (sched!=null) { 
            if (time>0) { 
                //System.out.println("Cycling til: "+(time+getTime()));
                sched.cycleTo(time+getTime()); 
            } else if (time<0) { 
                //System.out.println("Cycling indefinitely");
                sched.cycle();  
            } else { 
                //System.out.println("Cycling Once");
                sched.cycleOnce();  
            }
        } else  
            System.err.println("Cycle error: no scheduler to cycle."); 
    }

    /** Cycles until the node "node" transitions tCount times. **/
    public void cycle(Node node, int tCount) {
        sched.cycle(node, tCount);
    }
    /** Cycles until the node named "name" transitions tCount times. **/
    public void cycle(String name, int tCount) throws InvalidHierNameException {
        Node n = findNode(name);
        if (n == null) {
            System.out.println("Node '"+name+"' does not exist.");
        } else {
            cycle(n, tCount);
        }
    }

    /** Stub.  Returns true iff there's been an instability in the simulation */
    public boolean hasBeenInstability() {
        return (_rand.nextInt() & (1 << 15)) == 0;
    }
    private static Random _rand = new Random();

    /** Stub.  Returns Iterator through map of all names. **/
    public Iterator getAllNames() {
        return null;
    }

    /** Asks the scheduler to stop after the next cycle. **/
    public void interrupt() {
        interrupt(DigitalScheduler.InterruptedBy.OTHER);
    }

    public void interrupt(DigitalScheduler.InterruptedBy what) {
        sched.interrupt(what);
    }

    /** Return whether the scheduler was interrupted. **/
    public DigitalScheduler.InterruptedBy isInterrupted() {
        return sched.isInterrupted();
    }

    private static String getSlewString(final int value, final float slew,
                                        final boolean measured) {
        if (measured) {
            return " " + (value == Node.VALUE_0 ? "-" : 
                   value == Node.VALUE_1 ? "+" : " ") +
                   String.format("%6.2f", slew);
        } else {
            return "";
        }
    }

    private static String getNodeString(final Node node,
                                        final long time,
                                        final int tcount,
                                        final float slew,
                                        final byte type,
                                        final byte val,
                                        final boolean measured,
                                        final boolean estimated,
                                        final String delay) {
        final StringBuilder sb = new StringBuilder();
        if (time != 0)
            sb.append(" @" + time);
        if (tcount >= 0)
            sb.append(" #" + tcount);
        sb.append(getSlewString(val, slew, measured));
        sb.append(getDelayTypeString(type, measured, estimated));
        if (delay != null) sb.append(" " + delay);
        sb.append(" " + node.getName() + ":" + Node.getNameForValue(val));
        return sb.toString();
    }

    private static String getNodeString(final Node node,
                                        final long time,
                                        final int tcount,
                                        final float slew,
                                        final byte type,
                                        final byte val,
                                        final boolean measured,
                                        final boolean estimated) {
        return getNodeString(node, time, tcount, slew, type, val, measured,
                             estimated, null);
    }

    /** Returns a long string representation for a node. **/
    public String getNodeString(Node node, long time, int transitions,
                                float slew, byte type) {
        return getNodeString(node, time, countTransitions ? transitions : -1,
                             slew, type, node.getValue(),
                             useDelayMode(MEASURED_TAU),
                             useDelayMode(ESTIMATED_TAU));
    }
    /** Returns a long string representation for a node. **/
    public String getNodeString(Node node) {
        return getNodeString(node, getTime(), node.getTCount(), node.getSlew(),
                             node.getDelayType());
    }
    private static String getDelayTypeString(final byte type,
                                             final boolean measured,
                                             final boolean estimated) {
        if (!estimated && !measured) {
            return "";
        } else {
            return type == DIGITAL_TAU ? " D" :
                       type == ESTIMATED_TAU ? " E" : " M";
        }
    }
    /** Notification by a watched node that it has changed. Prints details. **/
    public void nodeChanged(Node node, long time) {
        HierName name = (HierName)
            canonNodeNameToWatchedNodeNameMap.get(node.getName());
        // name may be null if this callback resulted from "watchall"
        // and not "watch"
        assert name != null || watchAllNodes();
        System.out.println(getNodeString(node, time, node.getTCount(),
                                         node.getSlew(), node.getDelayType()) +
                           (name != null ?
                                " (" + name.getAsString('.') + ')' :
                                ""));
    }

    /**
     * Something odd happened!  Unless we're ignoring errors, this'll
     * halt the program.
     **/
    public void eventWarning(Node lastEvent, Rule rule, String message) {
        if (!warn) { return; }
        //error=true;
        System.err.print("WARNING: node "+rule.target+" "+message+" at time "+getTime());
        if (lastEvent != null) { System.err.println(" caused by "+lastEvent); }
        if (error) { interrupt(); }
    }

    /** A node began interfering **/
    public void interferenceWarning(Node lastEvent, Rule rule) {
        if(warn)
	    interferenceEvent = new AnomalousEvent(lastEvent,rule,sched.getTime());
	eventWarning(lastEvent,rule,"interfering");
    }

    /** A node glitched **/
    public void glitchWarning(Node lastEvent, Rule rule) {
	if(warn)
	    glitchEvent = new AnomalousEvent(lastEvent,rule,sched.getTime());
        eventWarning(lastEvent,rule,"unstable (set to 0 or 1)");
    }

    /** A node went unstable **/
    public void unstabWarning(Node lastEvent, Rule rule) {
	if(warn)
	    unstabEvent = new AnomalousEvent(lastEvent,rule,sched.getTime());
        eventWarning(lastEvent,rule,"unstable (set to U)");
    }

    /** 
     * Returns the last interference AnomalousEvent, if
     * one occurred.  Returns null if no node when into
     * interference.
     **/
    public AnomalousEvent lastInterferenceEvent() { return interferenceEvent; }

    /** 
     * Returns the last glitch AnomalousEvent, if
     * one occurred.  Returns null if no node glitched.
     **/
    public AnomalousEvent lastGlitchEvent() { return glitchEvent; }

    /** 
     * Returns the last instability AnomalousEvent, if
     * one occurred.  Returns null if no node when unstable.
     **/
    public AnomalousEvent lastUnstabEvent() { return unstabEvent; }

    /**
     * Spits out the anomalous event state to System.out.
     **/
    public void printAnomalousState() {
        AnomalousEvent e1 = lastInterferenceEvent();
        AnomalousEvent e2 = lastGlitchEvent();
        AnomalousEvent e3 = lastUnstabEvent();
        if (e1 != null) {
            System.out.print("Node "+e1.rule.target+" went into "+
                               "interference at time "+e1.time);
            if (e1.node != null)
                System.out.println(" caused by "+e1.node);
            else System.out.println("");
        }
        if (e2 != null) {
            System.out.print("Node "+e2.rule.target+" glitched "+
                               "at time "+e2.time);
            if (e2.node != null)
                System.out.println(" caused by "+e2.node);
            else System.out.println("");
        }
        if (e3 != null) {
            System.out.print("Node "+e3.rule.target+" went unstable "+
                               "at time "+e3.time);
            if (e3.node != null)
                System.out.println(" caused by "+e3.node);
            else System.out.println("");
        }
        if (e1 == null && e2 == null && e3 == null) {
            System.out.println("No anomalous events.");
        }
    }

    /**
     * Something unusual happened.  If the user cares, print out, but
     * don't halt anything. 
     **/
    public void eventNotice(Node lastEvent, Rule rule, String message) {
        if (verbose && warn) {
            System.err.print("WARNING: node "+rule.target+" "+message+" at time "+getTime());
            if (lastEvent != null) { System.err.println(" caused by "+lastEvent); }
        }
    }
    /** Prints gratuitous info based on verbose flag. **/
    public void ui_out_verbose(String message) {
       if (verbose) { System.out.print(message); }
    }
    /** Prints a message. **/
    public void ui_out(String message) {
        System.out.print(message);
    }
    /** Returns the scheduler's current time. **/
    public long getTime() { return sched.getTime(); }
    /**
     * Sets the scheduler's current time.  Clears all events out to
     * avoid dealing with timing conflicts.  Mostly for debugging.
     **/
    public void setTime(long time) { sched.clearEventQueuesAtTime(time); }

    /** Compares a delay (offset from current time) to a given time. **/
    long compareDelayToTime(long delay, long time) { return (delay+getTime())-time; }

    /** Returns the simulator's interference state **/


    /**
     * How randomness is handled.
     * <table>
     * <tr>
     *      <th></th>
     *      <th>NO_RANDOM</th> <th>UNTIMED_RANDOM</th> <th>TIMED_RANDOM</th>
     *      <th>C characteristics</th> <th>java characteristics</th>
     * </tr>
     * <tr>
     * <th>event normal</th>
     *      <td>specified delay</td> <td>delay * range</td> <td>delay * range</td>
     *      <td>rule-&gt;timed</td> <td>rule.timed</td>
     * </tr>
     * <tr>
     * <th>event random</th>
     *      <td>specified delay</td> <td>random</td> <td>delay * range</td>
     *      <td>!rule-&gt;timed, event_time == MAX_INT</td>
     *      <td>!rule.timed; stuck on random queue</td>
     * </tr>
     * <tr>
     * <th>event immediate</th>
     *      <td>front of queue</td> <td>front of queue</td> <td>front of queue</td>
     *      <td>U (dis)appearing. or drule-&gt;delay=0 =&gt; event_time = -1 </td>
     *      <td>delay=0/-1 not handled correctly.</td>
     * </tr>
     * </table>
     * @see Rule#delay
     * @see #NO_RANDOM
     * @see #TIMED_RANDOM
     * @see #UNTIMED_RANDOM
     **/
    int randomOrder = NO_RANDOM;
    /** See randomOrder. **/
    public int getRandom() { return randomOrder; }
    /** See randomOrder. **/
    public void setRandom(int r) { randomOrder = r; }
    /** Sets the min and max range for random variations in timing. **/
    public void setTimedRandom(float fast, float slow) {
        slowDelay=slow; fastDelay=fast;
    }
    /** Returns a String for min and max range for random timing var. **/
    public String getTimedRandom() {
        return "("+fastDelay +"," +slowDelay +")";
    }
    
    /** Returns the min range for random variations in timing. **/
    public float getFastDelay() { return fastDelay; }
    /** Returns the max range for random variations in timing. **/
    public float getSlowDelay() { return slowDelay; }
    /** Set the random number generator seed. **/
    public void setRandomSeed(long r) {
        rand.setSeed(r);
        sched.setRandomSeed(rand.nextLong());
    }

    /** range delays are multiplied by fastDelay -- slowDelay. **/
    private float slowDelay=(float)(4.0/3.0), fastDelay=(float)(2.0/3.0);
        
    /** returns a float in [fastDelay, slowDelay). */
    protected float randomDelay(float fastDelay, float slowDelay) {
        return fastDelay + rand.nextFloat() * (slowDelay - fastDelay);
    }
        
    /** Schedule (or re-schedule) a node transition. */
    public void scheduleEvent(Node node, byte pending, long delay,
            boolean random, Node lastEvent) {
        boolean destRandom;
        long time;

        /* questionable... should perhaps be outside?
         * neither otherwise used in this method, but
         * convenient place. */
        node.pending=pending;
        node.enabler=lastEvent;

        time = delay + getTime();

        if (verbose)  
            ui_out("adding "+node+" -> "+Node.getNameForValue(node.pending)+
                   " to queue at time="+time+".\n"); 
        
        destRandom = randomOrder == UNTIMED_RANDOM && random;
        if (node.getIndex() >= 0 && destRandom != node.isRandom()) {
            // On a queue, and it isn't the right one.
            sched.removeEvent(node);
        }
        node.setRandom(destRandom); // force on correct queue
        if (node.getIndex() >= 0 && !destRandom &&
                DigitalScheduler.compareTime(time,node.eventTime) > 0) {
            if (warn) {
                ui_out("Queuing event for node later than outstanding"+
                       " event on EventQueue\n\tnode: " + node + " time= "+time+
                       " node.eventTime= "+node.eventTime+"\n");
            }
        }
        node.eventTime = time;
        sched.addEvent(node);
    }

    private boolean isGlob(String s) {
        int first = s.indexOf("*");
        if (first < 0) {
            return false;
        }
        int last = s.lastIndexOf("*");
        if (last != first) {
            return false;
        }
        return true;
    }

    /**
     * Used to traverse the cell hierarchy, instantiating all the nodes
     * and rules.  Should be much more memory efficient than flattening.
     *
     * Can also avoid keeping most names permanently, by using a local
     * name to node map.  Not yet implemented.
     *
     * IMPL note: keep all top-level names, no matter what.
     *
     * Strategy for following aliases properly:
     * We traverse the cell hierarchy, using DSimCellTraverser.
     * It passes in an aliased map from the node's portlists to
     * nodes in the parent cell (or even higher in the hierarchy).
     * Before checking in DSim's list of nodes we first check this
     * port list so everything uses the canonical name.
     **/

    public class AliasCreator implements JFlat.CellProcessor {
        private final AliasedSet collect;

        public AliasCreator(AliasedSet collect) {
            this.collect = collect;
        }

        private void process_dsim(HierName prefix, CellInterface cell, 
                          Cadencize cadencizer) {
            // System.err.println("portNodes are " + portNodes);
            // System.out.println("Locals... in "+prefix);
            addLocalAliases(prefix, cell);
        }

        public void process(HierName prefix, CellInterface cell, AliasedSet s,
                            int depth, Cadencize cadencizer) {
            if (depth < 2) System.out.print(".");
            // supercell's port list
            // addSuperPortAliases(s);
            // Our locals + port
            // System.err.println("Entering cell " + prefix);
            // System.err.println("localNodes are " + cadencizer.convert(cell).getLocalNodes());
            
            process_dsim(prefix, cell, cadencizer);
        }
        // Add the aliases in the map to DSim's.
        /*
        private void addSuperPortAliases(AliasedSet s) {
            final Iterator i = s.getCanonicalKeys();
            while (i.hasNext()) {
                final HierName canon = (HierName)i.next();
                // lookupNode(canon);
                final Iterator j = s.getAliases(canon);
                while (j.hasNext()) {
                    final HierName alias = (HierName)j.next();
                    if (alias.equals(canon)) { 
                        continue;
                    }
                    // System.out.println("Aliasing Port " + alias + " to " + canon);
                    // aliasNode(canon, alias);
                    collect.makeEquivalent(canon, alias);
                }
            }
        }
        */

        /**
         * Adds all the aliases available through the passed in Cadencize
         * AliasedSet.
         **/
        private void addLocalAliases(HierName prefix, CellInterface cell) {
            // System.out.println("Adding local aliases for " + prefix);
            final Iterator i = cell.getCanonicalNodes();
            while (i.hasNext()) {
                final HierName local = (HierName)i.next();
                
                final HierName canon = prefixName(prefix, local);
                // lookupNode(canon);
                final Iterator j = cell.getConnectedNodes(local);
                while (j.hasNext()) {
                    final HierName localAlias = (HierName)j.next();
                    if ( ! localAlias.equals(local) ) { 
                    
                    
                        HierName alias = prefixName(prefix, localAlias);
                        if ( alias.isGlobal() ) {
                            alias = HierName.trim(alias);
                        }
                        collect.makeEquivalent(canon, alias);
                    }
                }
            }
        }
    }
        
    private final float[] delayTau = new float[] { 1, Float.NaN, Float.NaN };
    public final static byte DIGITAL_TAU = 0;
    public final static byte ESTIMATED_TAU = 1;
    public final static byte MEASURED_TAU = 2;

    private float measuredCapScale = Float.NaN;

    /**
     * Enable using estimated_delay directives to calculate the delay.
     **/
    public void enableDelayMode(final int mode, final float tau) {
        assert tau > 0 && !Float.isNaN(tau);
        delayTau[mode] = tau;
    }

    public void disableDelayMode(final int mode) {
        delayTau[mode] = Float.NaN;
    }

    public boolean useDelayMode(final int mode) {
        return !Float.isNaN(delayTau[mode]);
    }

    public float getDelayTau(final int mode) {
        return useDelayMode(mode) ? delayTau[mode] : 1;
    }

    private int measureDataSet;
    public void setMeasureDataSet(int dataSet) {
        assert dataSet >= 0;
        this.measureDataSet = dataSet;
    }
    public int getMeasureDataSet() {
        return measureDataSet;
    }

    public void enableMeasuredCap(final float scale) {
        measuredCapScale = scale;
    }

    public boolean useMeasuredCap() {
        return !Float.isNaN(measuredCapScale);
    }

    static class SimpleCell implements HierarchyInterface {
        private final String fqcn;
        private final Map<HierName,SimpleCell> subcells;
        private final boolean isRouted;
        private final boolean isGraybox;
        private SimpleCell(final String fqcn, final boolean isRouted,
                           final boolean isGraybox) {
            this.fqcn = fqcn;
            this.isRouted = isRouted;
            this.isGraybox = isGraybox;
            subcells = new HashMap<HierName,SimpleCell>();
        }
        private void addSubcell(final HierName inst, final SimpleCell subcell) {
            subcells.put(inst, subcell);
        }
        public SimpleCell getSubcell(final HierName inst) {
            return subcells.get(inst);
        }
        public boolean isRouted() {
            return isRouted;
        }
        public boolean isGraybox() {
            return isGraybox;
        }
        public boolean isLeaf() {
            return subcells.isEmpty();
        }
        public String getType() {
            return fqcn;
        }
        private static SimpleCell convert(final CellInterface cell,
                                          final Map<String,SimpleCell> cache) {
            final String type = cell.getFullyQualifiedType();
            SimpleCell result = cache.get(type);
            if (result == null) {
                result = new SimpleCell(type, CellUtils.isRouted(cell),
                                        CellUtils.isAstaGraybox(cell));
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final HierName name = (HierName) p.getFirst();
                    final CellInterface ci = (CellInterface) p.getSecond();
                    result.addSubcell(name, convert(ci, cache));
                }
                cache.put(type, result);
            }
            return result;
        }
        public static SimpleCell convert(final CellInterface cell) {
            return convert(cell, new HashMap<String,SimpleCell>());
        }
        public String toString() {
            return "[SimpleCell " + fqcn + "]";
        }
    }

    private boolean astaEnabled = false;
    private boolean slintEnabled = false;
    private Map<HalfOp<Node>,
                Map<List<HalfOp<Node>>,Boolean>> slintIgnores =
        new HashMap<HalfOp<Node>,Map<List<HalfOp<Node>>,Boolean>>();
    private CadenceInfo savedCadenceInfo = null;
    private SimpleCell savedOptCandidate = null;

    public void enableAsta() {
        astaEnabled = true;
    }
    public void disableAsta() {
        astaEnabled = false;
        savedCadenceInfo = null;
        savedOptCandidate = null;
    }
    public boolean isAstaEnabled() {
        return astaEnabled;
    }

    public void enableSlint() { slintEnabled = true; }
    public void disableSlint() { slintEnabled = false; }
    public boolean isSlintEnabled() { return slintEnabled; }
    Collection<List<Pair<Node,Integer>>>
    getSlintIgnoreFrom(final Node n, final int dir) {
        Map<List<HalfOp<Node>>,Boolean> stored =
            slintIgnores.get(new HalfOp<Node>(n, dir > 0 ? true : false));
        if (stored == null) stored = Collections.emptyMap();

        final Collection<List<Pair<Node,Integer>>> result =
            new ArrayList<List<Pair<Node,Integer>>>();
        for (Map.Entry<List<HalfOp<Node>>,Boolean> entry :
                stored.entrySet()) {
            if (entry.getValue()) {
                final List<Pair<Node,Integer>> path =
                    new ArrayList<Pair<Node,Integer>>();
                for (HalfOp<Node> op : entry.getKey()) {
                    path.add(new Pair<Node,Integer>(op.node, op.getDir()));
                }
                result.add(path);
            }
        }
        return result;
    }

    /**
     * Returns whether the node is a dynamic node.
     **/
    static boolean isDynamic(Node n) {
        return n.getBreakpoint();
    }

    /**
     * Returns whether the node is a dynamic node staticized by a full
     * staticizer.
     **/
    static boolean hasFullStaticizer(Node n) {
        return isDynamic(n) && n.isRandom();
    }

    /**
     * Returns the inverse of the dynamic node used in the staticizer.  This
     * function can return null, if the node is staticized by a full
     * staticizer but the inverse node internal to the full staticizer isn't
     * represented in the simulation.
     **/
    static Node getStaticizerInverse(Node n) {
        return isDynamic(n) ? (Node) n.enabler : null;
    }

    /**
     * Special case handling for input DFT channels.  During reset, lower
     * enable; after reset, raise enable.
     **/
    private static class DftInputHandler implements NodeWatcher {
        private final Node enableNode;
        public DftInputHandler(final String name) {
            this(DSim.get().findNode(name + ".e"));
        }
        public DftInputHandler(final Node enableNode) {
            this.enableNode = enableNode;
        }
        public void nodeChanged(Node resetNode, long time) {
            final byte val = resetNode.getValue();
            if (val == Node.VALUE_0 || val == Node.VALUE_1) {
                setNode(enableNode, val);
            }
        }
        private void setNode(final Node node, final byte val) {
            node.scheduleImmediate(val);
        }
    }

    /**
     * Special case handling for output DFT channels.  During reset, lower data
     * rails; after reset, wait for the enable to raise, then raise the 2 rail.
     **/
    private static class DftOutputHandler implements NodeWatcher {
        private final Node[] dataNodes;
        private final Node enable;
        private boolean reset;
        public DftOutputHandler(final String name) {
            this(new Node[] { DSim.get().findNode(name + ".0"),
                              DSim.get().findNode(name + ".1"),
                              DSim.get().findNode(name + ".2") },
                 DSim.get().findNode(name + ".e"));
        }
        public DftOutputHandler(final Node[] dataNodes, final Node enable) {
            assert dataNodes.length == 3;
            this.dataNodes = dataNodes;
            this.enable = enable;
            this.reset = false;
            enable.addWatch(this);
        }
        public void nodeChanged(Node node, long time) {
            final byte val = node.getValue();
            if (node == enable) {
                if (!reset && val == Node.VALUE_1) {
                    setNode(dataNodes[dataNodes.length - 1], val);
                }
            } else {
                // node is _RESET
                if (val == Node.VALUE_0) {
                    for (int i = 0; i < dataNodes.length; ++i)
                        setNode(dataNodes[i], val);
                    reset = true;
                } else if (val == Node.VALUE_1) {
                    reset = false;
                }
            }
        }
        private void setNode(final Node node, final byte val) {
            node.scheduleImmediate(val);
        }
    }

    /**
     * Special case handling for output DFT channels.  During reset, lower data
     * rails; after reset, raise the 0 and 3 rails.
     **/
    private static class NewDftOutputHandler implements NodeWatcher {
        private final Node[] dataNodes;
        public NewDftOutputHandler(final String name) {
            this(new Node[] { DSim.get().findNode(name + "[0]"),
                              DSim.get().findNode(name + "[1]"),
                              DSim.get().findNode(name + "[2]"),
                              DSim.get().findNode(name + "[3]") });
        }
        public NewDftOutputHandler(final Node[] dataNodes) {
            assert dataNodes.length == 4;
            this.dataNodes = dataNodes;
        }
        public void nodeChanged(Node node, long time) {
            // node is _RESET
            final byte val = node.getValue();
            if (val == Node.VALUE_0) {
                for (int i = 0; i < dataNodes.length; ++i)
                    setNode(dataNodes[i], val);
            } else if (val == Node.VALUE_1) {
                setNode(dataNodes[0], val);
                setNode(dataNodes[3], val);
            }
        }
        private void setNode(final Node node, final byte val) {
            node.scheduleImmediate(val);
        }
    }

    private boolean existNodes(final String base, final String[] nodes) {
        for (String node : nodes) {
            if (findNode(base + node) == null) return false;
        }
        return true;
    }

    private static final ChannelTimingInfo DEFAULT_TIMING_INFO =
        new ChannelTimingInfo() {
            public int getSlack() { return 1; }
            public int getLatency() { return 200; }
            public int getCycleTime() { return 1800; }
            public int getDataNeutralEnableLatency() { return 725; }
            public int getDataValidEnableLatency() { return 675; }
            public int getEnableDataLatency() { return 200; }
            public int getCycleTimeIn() { return 1800; }
            public int getCycleTimeOut() { return 1800; }
        };

    private void handleDftChannel(final String base, final boolean in,
                                  final Node resetNode,
                                  final ChannelFactoryInterface factory) {
        // There are two scan protocols we must support.  We differentiate
        // between the two by looking at the existence of certain nodes.
        if (existNodes(base + ".", CellUtils.OLD_DFT_NODES)) {
            ui_out_verbose("Old ChanDft handler installed on " + base + "\n");
            resetNode.addWatch(in ? new DftInputHandler(base)
                                  : new DftOutputHandler(base));
        } else if (existNodes(base + ".", CellUtils.NEW_DFT_NODES)) {
            ui_out_verbose("New ChanDft handler installed on " + base + "\n");
            if (in) {
                factory.makeInputChannel(base + ".D", "standard.channel.e1of2",
                                         BigInteger.valueOf(2),
                                         1, DEFAULT_TIMING_INFO);
            } else {
                factory.makeOutputChannel(base + ".D", "standard.channel.e1of2",
                                          BigInteger.valueOf(2),
                                          1, DEFAULT_TIMING_INFO);
                resetNode.addWatch(new NewDftOutputHandler(base + ".C"));
            }
        } else {
            System.err.println("Warning: unsupported DFT channel (" +
                               base + ")");
        }
    }

    /**
     * Added special DFT handling for all DFT channels in the given CoSimInfo.
     **/
    private void handleDftChannels(final HierName prefix,
                                   final CoSimInfo info) {
        Iterator inDfts = info.getInputDftChannelNames();
        Iterator outDfts = info.getOutputDftChannelNames();
        if (inDfts.hasNext() || outDfts.hasNext()) {
            final Node resetNode = DSimUtil.getResetNode();
            if (resetNode == null) {
                System.err.println("Warning: No reset node to watch " +
                                   "(required to handle DFT channels in " +
                                   prefix + ")");
            } else {
                final ChannelFactoryInterface factory =
                    new CoSimInfo.NodeChannelFactory();
                final String root =
                    prefix == null ? "" : prefix.getAsString('.') + ".";
                while (inDfts.hasNext()) 
                    handleDftChannel(root + (String) inDfts.next(), true,
                                     resetNode, factory);
                while (outDfts.hasNext())
                    handleDftChannel(root + (String) outDfts.next(), false,
                                     resetNode, factory);
            }
        }
    }

    private class CanonizePrs implements UnaryFunction {
        final HierName prefix;
        public CanonizePrs(final HierName prefix) {
            this.prefix = prefix;
        }
        public Object execute(final Object o) {
            final HierName full = prefixName(prefix, (HierName) o);
            final Node node = findNode(full);
            // node maybe null if the instance containing the node is simulated
            // in csp, or if the node has no aliases
            if (node == null) return full;
            else return node.getName();
        }
    }

    private static final InstanceData DUMMY_INSTANCE = new InstanceData() {
        public InstanceData translate(final CellInterface cell,
                                      final CellInterface subcell,
                                      final HierName instance,
                                      final Cadencize cad) {
            return this;
        }

        public AttributeInterface put(final String name,
                                      final AttributeInterface attr) {
            throw new UnsupportedOperationException();
        }

        public AttributeInterface get(final String name) {
            return null;
        }

        public void updateExtraDelay(final CellInterface cell,
                                     final AliasedSet aliases) {
            throw new UnsupportedOperationException();
        }

        public void updateAstaExtraDelay(final CellInterface cell,
                                         final AliasedSet aliases) {
            throw new UnsupportedOperationException();
        }

        public void updateEstimatedDelay(final CellInterface cell,
                                         final AliasedSet aliases) {
            throw new UnsupportedOperationException();
        }

        public void updateEstimatedDelaySignoff(final CellInterface cell,
                                                final AliasedSet aliases) {
            throw new UnsupportedOperationException();
        }

        public void updateDelayBias(final CellInterface cell) {
            throw new UnsupportedOperationException();
        }

        public void updateMeasuredDelay(final HierName prefix,
                                        final CellInterface cell,
                                        final UnaryFunction canonizer,
                                        final float delayScale,
                                        final int dataSet) {
            throw new UnsupportedOperationException();
        }

        public void updateEstimatedCap(final HierName prefix,
                                       final CellInterface cell,
                                       final UnaryFunction canonizer) {
            throw new UnsupportedOperationException();
        }

        public void updateMeasuredCap(final HierName prefix,
                                      final CellInterface cell,
                                      final UnaryFunction canonizer) {
            throw new UnsupportedOperationException();
        }

        public float getExtraDelay(final boolean up, final HierName canon) {
            return 0;
        }

        public float getEstimatedDelay(final boolean up, final HierName canon) {
            return Float.NaN;
        }

        public float getEstimatedDelaySignoff(final boolean up,
                                              final HierName canon) {
            return Float.NaN;
        }

        public Triplet[] getMeasuredDelay(final boolean up,
                                          final HierName canon) {
            return null;
        }

        public float getEstimatedCap(final HierName canon) {
            return Float.NaN;
        }

        public float getMeasuredCap(final HierName canon) {
            return Float.NaN;
        }

        public float getDelayBias(final HierName instance) {
            return 1;
        }
    };

    Collection<MeasuredDelay> getBoundaryMeasuredDelays() {
        return Collections.unmodifiableCollection(boundaryDelays);
    }

    Collection<Node> getInteriorOutputNodes() {
        return Collections.unmodifiableCollection(interiorOutputPorts);
    }

    String getCandidateType(final HierName name) {
        final SimpleCell sc = (SimpleCell) getSubcell(savedOptCandidate,
                                                      name.tail());
        return sc.getType();
    }

    private RunStaticTiming.Context astaContext;
    public void setAstaContext(RunStaticTiming.Context astaContext) {
        this.astaContext = astaContext;
    }
    private double getAstaDelayBias() {
        return astaContext == null ? 1.0 : astaContext.getDelayBias();
    }

    /**
     * A half-operator.
     **/
    private static class HalfOp<T> {
        public final T node;
        public final boolean direction;
        public HalfOp(final T node, final boolean direction) {
            assert node != null;
            this.node = node;
            this.direction = direction;
        }
        public boolean equals(Object o) {
            if (o instanceof HalfOp) {
                final HalfOp op = (HalfOp) o;
                return node.equals(op.node) && direction == op.direction;
            } else {
                return false;
            }
        }
        public int hashCode() {
            return node.hashCode() + (direction ? 1 : 0);
        }
        public String toString() {
            return node.toString() + (direction ? "+" : "-");
        }
        public int getDir() {
            return direction ? 1 : 0;
        }
    }

    /**
     * Setup measured_delay processors.
     **/
    private void prepareMeasured() {
        final double postStep = getDouble("sta.postStep", Double.NaN);
        final int shape = (int) getDouble("sta.shape", 1);
        final double defOldTh1, defOldTh2, defNewTh1, defNewTh2;
        if (shape == 1) {
            defOldTh1 = 1.0 / 3.0;
            defOldTh2 = 2.0 / 3.0;
            defNewTh1 = 0.0;
            defNewTh2 = 1.0;
        } else {
            defOldTh1 = defOldTh2 = defNewTh1 = defNewTh2 = Double.NaN;
        }
        final double oldth1 = getDouble("sta.oldTh1", defOldTh1);
        final double oldth2 = getDouble("sta.oldTh2", defOldTh2);
        final double newth1 = getDouble("sta.newTh1", defNewTh1);
        final double newth2 = getDouble("sta.newTh2", defNewTh2);
        staVerbose = 1 == (int) getDouble("sta.verbose", 0);

        measuredProcs.add(
                new ProcessMeasuredDelay.RemoveZeroData(zeroDataNotifier));
        if (!Double.isNaN(postStep) && !Double.isNaN(oldth1) &&
            !Double.isNaN(oldth2)) {
            measuredProcs.add(
                new ProcessMeasuredDelay.PostStepCorrection(postStep,
                                                            oldth1,
                                                            oldth2));
            System.out.printf("Enabled postStep correction: " +
                              "postStep=%.3f oldTh1=%.3f oldTh2=%.3f\n",
                              postStep, oldth1, oldth2);
            if (staVerbose) {
                measuredProcs.add(
                    new ProcessMeasuredDelay.PrintTable("postStep"));
            }
        }

        if (!Double.isNaN(oldth1) && !Double.isNaN(oldth2) &&
            !Double.isNaN(newth1) && !Double.isNaN(newth2)) {
            measuredProcs.add(
                new ProcessMeasuredDelay.ShiftThreshold(oldth1, oldth2,
                                                        newth1, newth2,
                                                        shape));
            System.out.printf("Enabled extrapolated thresholds:" +
                " oldTh1=%.3f oldTh2=%.3f newTh1=%.3f newTh2=%.3f" +
                " shape=%d\n", oldth1, oldth2, newth1, newth2, shape);
            if (staVerbose) {
                measuredProcs.add(
                    new ProcessMeasuredDelay.PrintTable("extrapolated"));
            }
        }

        measuredProcs.add(
                new ProcessMeasuredDelay.ScaleDelay(
                    100.0 / getDelayTau(MEASURED_TAU)));
    }

    private interface HierarchyCallback {
        void execute(final HierName prefix, final HierName suffix,
                     final HierarchyInterface cell);
    }

    private static HierarchyInterface getSubcell(final HierarchyInterface cell,
                                                 final HierName instance,
                                                 final HierarchyCallback cb) {
        for (Pair<HierName,HierName> split :
                new IterableIterator<Pair<HierName,HierName>>(
                    HierName.getSplitsIterator(instance))) {
            final HierName prefix = split.getFirst();
            if (prefix == null) continue;

            final HierName suffix = split.getSecond();
            final HierarchyInterface subcell = cell.getSubcell(prefix);
            if (subcell != null) {
                cb.execute(prefix, suffix, subcell);
                if (suffix == null) {
                    return subcell;
                } else {
                    return getSubcell(subcell, suffix, cb);
                }
            }
        }
        return null;
    }

    private static HierarchyInterface getSubcell(final HierarchyInterface cell,
                                                 final HierName instance) {
        final HierarchyCallback empty =
            new HierarchyCallback() {
                public void execute(final HierName prefix,
                                    final HierName suffix,
                                    final HierarchyInterface cell) {
                }
            };
        return getSubcell(cell, instance, empty);
    }

    private static Pair<HierName,SimpleCell>
    getPredicateParent(final SimpleCell cell, final HierName instance,
                       final UnaryPredicate<SimpleCell> predicate) {
        final SimpleCell[] routed = new SimpleCell[] { null };
        final HierName[] path = new HierName[] { null };
        final HierarchyCallback cb =
            new HierarchyCallback() {
                public void execute(final HierName prefix,
                                    final HierName suffix,
                                    final HierarchyInterface cell) {
                    final SimpleCell curr = (SimpleCell) cell;
                    if (suffix != null && predicate.evaluate(curr)) {
                        routed[0] = curr;
                        path[0] = null;
                    } else {
                        path[0] = HierName.append(path[0], prefix);
                    }
                }
            };
        getSubcell(cell, instance, cb);
        return new Pair<HierName,SimpleCell>(path[0], routed[0]);
    }

    Iterator<HierName> getPredicateInstances(
        final HierName instance, final UnaryPredicate<SimpleCell> p) {
        final Pair<HierName,SimpleCell> routed =
            getPredicateParent(savedOptCandidate, instance, p);
        final HierName suffix = routed.getFirst();
        final SimpleCell sc = routed.getSecond();
        Collection<HierName> names =
            sc == null ? null : routedInstancesByType.get(sc.getType());
        if (names == null) names = Collections.emptyList();
        return new MappingIterator<HierName,HierName>(names.iterator(),
                new UnaryFunction<HierName,HierName>() {
                    public HierName execute(final HierName a) {
                        return suffix == null ? a : HierName.append(a, suffix);
                    }
                });
    }

    Iterator<HierName> getRoutedInstances(final HierName instance) {
        return getPredicateInstances(instance, 
            new UnaryPredicate<SimpleCell>() {
                public boolean evaluate(SimpleCell sc) {
                    return sc.isRouted();
                }
            });
    }

    Iterator<HierName> getGrayboxInstances(final HierName instance) {
        return getPredicateInstances(instance, 
            new UnaryPredicate<SimpleCell>() {
                public boolean evaluate(SimpleCell sc) {
                    return sc.isGraybox();
                }
            });
    }

    /**  Creates the models from the instantiation heirarchy  **/
    public class ModelCreator {
        private CoSimParameters params;
        private final Cadencize cadencizer;
        private boolean errorOccurred = false;
        protected CadenceInfo cinfo;

        /**
         * A map from FQCN to set of expanded timing arcs.
         **/
        private Map<String, Set<Pair<HalfOp<HierName>,
                                     HalfOp<HierName>>>> astaIgnoreCache =
            new HashMap<String, Set<Pair<HalfOp<HierName>,HalfOp<HierName>>>>();
        private Map<Pair<List<Node>,BitSet>,Rule> astaGrayboxRules =
            new HashMap<Pair<List<Node>,BitSet>,Rule>();
        private Map<String,
                    Map<HierName,Pair<HierName,Boolean>>> staticizerInfo =
            new HashMap<String,Map<HierName,Pair<HierName,Boolean>>>();
        private Map<String,ProductionRuleSet> staticizerPrs =
            new HashMap<String,ProductionRuleSet>();
        private final Collection<Node> boundaryNodes;

        private final UnaryFunction canonizer =
            new UnaryFunction() {
                public Object execute(final Object o) {
                    final Node node = findNode((HierName) o);
                    // at this point, saveAliases() has already finished,
                    // and all aliases created; some Nodes may not exist,
                    // but they have no aliases, so the canonical name is
                    // just itself
                    return node == null ? o : node.getName();
                }
            };

        public ModelCreator(CoSimParameters params, Cadencize cadencizer) {
            this(params, cadencizer, null);
        }

        public ModelCreator(CoSimParameters params, Cadencize cadencizer,
                            Collection<Node> boundaryNodes) {
            this.params = params;
            this.cadencizer = cadencizer;
            this.boundaryNodes = boundaryNodes;
        }

        public Collection<Node> getInteriorPorts(final CellInterface cell) {
            // find all leaf instances that drive boundary ports
            final Set<HierName> instances = new HashSet<HierName>();
            foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (r.prefix != null &&
                        boundaryNodes.contains(r.target())) {
                        instances.add(r.prefix);
                    }
                }
            });

            // accumulate all port nodes used as input in boundary leaf cells
            final Set<Node> inputPorts = new HashSet<Node>();
            for (final HierName instance : instances) {
                final CellInterface subcell =
                    (CellInterface) getSubcell(cell, instance);
                final AliasedMap ports =
                    cadencizer.convert(subcell).getPortNodes();;
                final HashSet<HierName> input = new HashSet<HierName>();
                final HashSet<HierName> output = new HashSet<HierName>();
                for (Iterator i = subcell.getProductionRuleSet()
                                         .getProductionRules();
                     i.hasNext(); ) {
                    final ProductionRule pr = (ProductionRule) i.next();
                    if (ports.contains(pr.getTarget())) {
                        output.add(pr.getTarget());
                    }
                    BooleanUtils.foreachHierName(pr.getGuard(), 
                        new UnaryAction() {
                            public void execute(final Object o) {
                                final HierName atom = (HierName) o;
                                if (ports.contains(atom)) {
                                    input.add(atom);
                                }
                            }
                        });
                }

                // only select ports that are input only
                for (HierName port : input) {
                    if (output.contains(port)) continue;
                    final HierName full = prefixName(instance, port);
                    final Node node = findNode(full);
                    inputPorts.add(node);
                }
            }

            // remove all input ports of boundary leaf cells actually on the
            // boundary to get only the interior ports
            inputPorts.removeAll(boundaryNodes);

            return inputPorts;
        }

        public boolean build(CellInterface cell, int defaultBehavior, 
                          HierName prefix, final boolean isEnv) {
            // build is the external entry point, and this is the only place it
            // is called pass -1 as arbitrationMode, as it won't be used
            // because there should be no arbiters in the first level
            cinfo = cadencizer.convert(cell);
            if (isAstaEnabled()) savedCadenceInfo = cinfo;
            final AliasedSet aliases = cinfo.getLocalNodes();
            final InstanceData instData = new InstanceData();
            instData.updateExtraDelay(cell, aliases);
            instData.updateAstaExtraDelay(cell, aliases);
            instData.updateEstimatedDelay(cell, aliases);
            instData.updateEstimatedDelaySignoff(cell, aliases);
            instData.updateEstimatedCap(prefix, cell, canonizer);
            instData.updateMeasuredCap(prefix, cell, canonizer);
            instData.updateMeasuredDelay(
                prefix, cell,
                canonizer,
                1, getMeasureDataSet());
            instData.updateDelayBias(cell, getAstaDelayBias());
            if (!isEnv) {
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface ci = (CellInterface) p.getSecond();
                    final HierName name =
                        HierName.append(prefix, (HierName) p.getFirst());
                    final Map markedPorts = CellUtils.markPorts(ci);
                    for (Iterator j = markedPorts.entrySet().iterator();
                         j.hasNext(); )
                    {
                        final Map.Entry entry = (Map.Entry) j.next();
                        final String port = (String) entry.getKey();
                        HierName hierPort;
                        try {
                            hierPort = HierName.makeHierName(port, '.');
                        } catch (InvalidHierNameException e) {
                            throw new RuntimeException("Cannot make HierName from " + port, e);
                        }
                        warnedCells.add(lookupNode(prefixName(name, hierPort)));
                    }
                }
            }

            final Map<HalfOp<Node>,Set<HalfOp<Node>>> emptyAstaIgnore =
                Collections.<HalfOp<Node>,Set<HalfOp<Node>>>emptyMap();

            createModels(prefix, cell, defaultBehavior, -1, null, instData,
                         emptyAstaIgnore, null, isEnv);
            process(prefix, prefix, cell, 1, defaultBehavior, false, instData,
                    emptyAstaIgnore, null, false, isEnv);
            return errorOccurred;
        }

        private HalfOp<Node> getHalfOp(final HierName prefix,
                                       final HalfOp<HierName> op) {
            final Node n = lookupNode(HierName.append(prefix, op.node));
            return n == null ? null : new HalfOp<Node>(n, op.direction);
        }

        private HalfOp<Node> findHalfOp(final HierName prefix,
                                        final HalfOp<HierName> op) {
            final Node n = findNode(HierName.append(prefix, op.node));
            return n == null ? null : new HalfOp<Node>(n, op.direction);
        }

        /**
         * Determine whether the specified guard and target is subject to
         * asta_ignore.
         **/
        private boolean matchAstaIgnore(
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> map,
                final HalfOp guard, final HalfOp target) {
            final Set<HalfOp<Node>> s = map.get(guard);
            return s != null && (s.contains(null) || s.contains(target));
        }

        /**
         * Update the data structure with a new ignored timing arc from guard
         * to target.  Try to avoid creating new objects if possible.
         **/
        private Map<HalfOp<Node>,Set<HalfOp<Node>>> updateAstaIgnore(
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> initial,
                Map<HalfOp<Node>,Set<HalfOp<Node>>> modified,
                final HalfOp<Node> guard, final HalfOp<Node> target) {
            if (modified == initial) {
                modified = new HashMap<HalfOp<Node>,Set<HalfOp<Node>>>(initial);
            }
            final Set<HalfOp<Node>> initialSet = initial.get(guard);
            Set<HalfOp<Node>> modifiedSet = modified.get(guard);
            if (modifiedSet == null || initialSet == modifiedSet) {
                if (initialSet == null) {
                    modifiedSet = new HashSet<HalfOp<Node>>();
                } else {
                    modifiedSet = new HashSet<HalfOp<Node>>(initialSet);
                }
                modified.put(guard, modifiedSet);
            }
            modifiedSet.add(target);
            return modified;
        }

        /**
         * Augment the data structure with asta_ignore directives from cell.
         * Return the same map if nothing needs to be added.
         **/
        private Map<HalfOp<Node>,Set<HalfOp<Node>>> updateAstaIgnore(
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> initial,
                final CellInterface cell,
                final HierName prefix) {
            final Set<Pair<HalfOp<HierName>,HalfOp<HierName>>> ignores =
                getAstaIgnore(cell);
            Map<HalfOp<Node>,Set<HalfOp<Node>>> result = initial;

            for (Pair<HalfOp<HierName>,HalfOp<HierName>> ignore : ignores) {
                final HalfOp<Node> guard = getHalfOp(prefix, ignore.getFirst());
                if (guard != null) {
                    if (ignore.getSecond() == null) {
                        if (!matchAstaIgnore(result, guard, null)) {
                            result = updateAstaIgnore(initial, result, guard,
                                                      null);
                        }
                    } else {
                        final HalfOp<Node> target =
                            getHalfOp(prefix, ignore.getSecond());
                        if (target != null) {
                            if (!matchAstaIgnore(result, guard, target)) {
                                result = updateAstaIgnore(initial, result,
                                                          guard, target);
                            }
                        }
                    }
                }
            }

            return result;
        }

        /**
         * Expand node to node+, node-.
         **/
        private HalfOp<HierName>[] getHalfOp(final Pair<HierName,Boolean> op) {
            final HierName h = op.getFirst();
            final Boolean b = op.getSecond();
            if (b == null) {
                return new HalfOp[] { new HalfOp<HierName>(h, false),
                                      new HalfOp<HierName>(h, true) };
            } else {
                return new HalfOp[] { new HalfOp<HierName>(h, b) };
            }
        }

        /**
         * Store expanding timing arcs to be ignored.
         **/
        private void getAstaIgnore(
                final String fqcn,
                final Set<Pair<HalfOp<HierName>,HalfOp<HierName>>> result,
                final String dirName,
                final Map directive) {
            for (Iterator i = directive.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final Collection<Pair<HierName,Boolean>> key =
                    (Collection<Pair<HierName,Boolean>>) entry.getKey();
                if (key.isEmpty() || key.size() > 2) {
                    System.err.println(
                        "WARNING: One or two elements expected in the " +
                        " parameter to " + dirName + " in " + fqcn);
                } else {
                    final Boolean val = (Boolean) entry.getValue();
                    if (!val.booleanValue()) {
                        System.err.println(
                            "WARNING: Cannot set " + dirName + " to false in " +
                            fqcn);
                    } else {
                        final Iterator<Pair<HierName,Boolean>> it =
                            key.iterator();
                        final HalfOp<HierName>[] guard = getHalfOp(it.next());
                        final HalfOp<HierName>[] target =
                            it.hasNext() ? getHalfOp(it.next())
                                         : new HalfOp[] { null };
                        for (HalfOp<HierName> g : guard) {
                            for (HalfOp<HierName> t : target) {
                                result.add(new Pair<HalfOp<HierName>,
                                                    HalfOp<HierName>>(g, t));
                            }
                        }
                    }
                }
            }
        }

        private void
        processIgnoreDirective(final CellInterface cell,
                               final String directive,
                               final UnaryAction<Map> func) {
            final String type =
                DirectiveTable.arrayify(DirectiveConstants.DEEP_HALFOP_TYPE);

            // cell directive
            final Map cellLevel =
                DirectiveUtils.getTopLevelDirective(cell, directive, type);
            func.execute(cellLevel);

            // prs or subcells directive
            final BlockInterface cb = cell.getBlockInterface();
            final List<BlockInterface> db = Arrays.asList(
                DirectiveUtils.getUniqueBlock(cb, BlockInterface.PRS),
                DirectiveUtils.getUniqueBlock(cb, BlockInterface.SUBCELL)
            );
            final Map prsSubcell =
                DirectiveUtils.getMultipleBlockDirective(db, directive, type);
            func.execute(prsSubcell);
        }

        /**
         * Merge cell level directives with prs/subcells level directives.
         **/
        private void
        getAstaIgnore(final CellInterface cell,
                      final Set<Pair<HalfOp<HierName>,HalfOp<HierName>>> result,
                      final String directive) {
            processIgnoreDirective(cell, directive,
                new UnaryAction<Map>() {
                    public void execute(Map m) {
                        getAstaIgnore(cell.getFullyQualifiedType(), result,
                                      directive, m);
                    }
                }
            );
        }

        /**
         * Merge cell level directives with prs/subcells level directives
         * (possibly including slint_ignore directives); store result in a
         * cache.
         **/
        private Set<Pair<HalfOp<HierName>,HalfOp<HierName>>>
        getAstaIgnore(final CellInterface cell) {
            final String fqcn = cell.getFullyQualifiedType();
            Set<Pair<HalfOp<HierName>,HalfOp<HierName>>> result =
                astaIgnoreCache.get(fqcn);
            if (result == null) {
                result = new HashSet<Pair<HalfOp<HierName>,
                                          HalfOp<HierName>>>();
                getAstaIgnore(cell, result, DirectiveConstants.ASTA_IGNORE);

                if (result.isEmpty()) {
                    result = Collections.<Pair<HalfOp<HierName>,
                                               HalfOp<HierName>>>emptySet();
                }
                astaIgnoreCache.put(fqcn, result);
            }
            return result;
        }

        private void recordMeasureDelay(final HierName prefix,
                                        final HierName instance,
                                        final CellInterface subcell,
                                        final InstanceData instData,
                                        final CadenceInfo cinfo) {
            final double delayBias =
                instance == null ? instData.getDelayBias(null)
                                 : instData.getDelayBias(instance);

            final MeasuredDelay delay =
                new MeasuredDelay(prefix, instance, subcell, delayBias);

            final AliasedMap ports = cinfo.getPortNodes();
            for (Iterator i = ports.getCanonicalKeys(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                if (Boolean.FALSE.equals(ports.getValue(port))) continue;

                final HierName full =
                    HierName.append(prefix, HierName.append(instance, port));
                final Node target = lookupNode(full);
                if (boundaryNodes != null) boundaryNodes.add(target);
                assert target != null : "Cannot get node for " + full;
                HierName fullCanon = canonize(target.getName(), this.cinfo);
                if (fullCanon == null) fullCanon = target.getName();
                for (boolean dir : new boolean[] { true, false }) {
                    final Triplet[] ts =
                        instData.getMeasuredDelay(dir, fullCanon);
                    final double extraDelay =
                        instData.getExtraDelay(dir, fullCanon);
                    delay.addMeasured(target, port, dir, ts, extraDelay,
                                      measuredProcs);
                }
            }

            boundaryDelays.add(delay);
        }

        private Map<String,HierName> grayboxes = new HashMap<String,HierName>();

        private void processSignoffConstant(final HierName prefix,
                                            final Map signoffConstant) {
            for (Iterator i = signoffConstant.entrySet().iterator();
                 i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName full =
                        prefixName(prefix, (HierName) entry.getKey());
                final Node node = findNode(full);
                // node might not exist, because the instance containing the
                // node is simulated in csp
                final HierName name = node == null ? full : node.getName();
                constantNodes.put(name, entry.getValue());
            }
        }

        private void processUnusedPrs(final HierName prefix,
                                      final Map unusedPrs) {
            final UnaryFunction canonizeFunc = new CanonizePrs(prefix);
            for (Iterator i = unusedPrs.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final ProductionRuleSet ruleSet =
                    (ProductionRuleSet) entry.getKey();
                final boolean unused =
                    ((Boolean) entry.getValue()).booleanValue();
                for (Iterator j = ruleSet.getProductionRules(); j.hasNext(); ) {
                    final ProductionRule rule = (ProductionRule) j.next();
                    final HierName target =
                        (HierName) canonizeFunc.execute(rule.getTarget());
                    final BooleanExpressionInterface prefixed =
                        BooleanUtils.mapBooleanExpressionHierNames(
                                rule.getGuard(), canonizeFunc);
                    Set s = (Set) possiblyUnusedRules.get(target);
                    if (s == null) {
                        s = new HashSet();
                        possiblyUnusedRules.put(target, s);
                    }
                    s.addAll(prefixed.DNFForm().getDisjuncts());
                }
            }
        }

        /**
         * Returns true if there exist a rule from guard -&gt; target.
         **/
        private boolean ruleExist(HalfOp<Node> guard, HalfOp<Node> target) {
            for (Rule r : guard.node.getTargets(guard.getDir())) {
                if (r.target == target.node && r.dir == target.getDir()) {
                    return true;
                }
            }
            return false;
        }

        private String
        getSlintIgnoreError(final String message,
                            final String fqcn,
                            final HierName prefix,
                            final Collection<Pair<HierName,Boolean>> halfops) {
            final StringBuilder sb = new StringBuilder();
            sb.append("Invalid slint_ignore directive (" + fqcn + "/" + prefix +
                      "): " );
            sb.append(message);
            sb.append(": slint_ignore({");
            boolean first = true;
            for (Pair<HierName,Boolean> halfop : halfops) {
                if (first) first = false;
                else sb.append(", ");
                sb.append(halfop.getFirst());
                final Boolean up = halfop.getSecond();
                if (up != null) sb.append(up ? "+" : "-");
            }
            sb.append("}");
            return sb.toString();
        }

        private void updateSlintIgnores(final String fqcn,
                                        final HierName prefix,
                                        final Map directives) {
            for (Iterator i = directives.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final Collection<Pair<HierName,Boolean>> halfops =
                    (Collection<Pair<HierName,Boolean>>) entry.getKey();
                if (halfops.size() < 2) {
                    System.err.println(getSlintIgnoreError(
                            "path too short",
                            fqcn, prefix, halfops));
                } else {
                    ArrayList<HalfOp<Node>> path =
                        new ArrayList<HalfOp<Node>>(halfops.size());
                    HalfOp<Node> lastOp = null;
                    for (Pair<HierName,Boolean> p : halfops) {
                        final HalfOp<HierName>[] expanded = getHalfOp(p);
                        if (expanded.length != 1) {
                            System.err.println(getSlintIgnoreError(
                                    "direction required " + expanded[0].node,
                                    fqcn, prefix, halfops));
                            break;
                        }

                        final HalfOp<Node> op = findHalfOp(prefix, expanded[0]);
                        if (op == null) {
                            System.err.println(getSlintIgnoreError(
                                    "can't find node " + expanded[0].node,
                                    fqcn, prefix, halfops));
                            break;
                        } else {
                            if (lastOp != null && !ruleExist(lastOp, op)) {
                                System.err.println(getSlintIgnoreError(
                                        "no rule " + lastOp + " -> " + op,
                                        fqcn, prefix, halfops));
                                break;
                            } else {
                                path.add(op);
                            }
                        }
                        lastOp = op;
                    }

                    // if no errors processing the directive
                    if (path.size() == halfops.size()) {
                        Map<List<HalfOp<Node>>,Boolean> ignores =
                            slintIgnores.get(path.get(0));
                        if (ignores == null) {
                            ignores =
                                new HashMap<List<HalfOp<Node>>,Boolean>();
                            slintIgnores.put(path.get(0), ignores);
                        }
                        ignores.put(path, (Boolean) entry.getValue());
                    }
                }
            }
        }

        /**
         * Look for instantiations of STATICIZER and WEAK_INV in the netlist
         * block to determine dynamic nodes; construct fake production rules
         * needed to model the small inverter in a full staticizer.
         **/
        private Pair<Map<HierName,Pair<HierName,Boolean>>, ProductionRuleSet>
        getStaticizerInfo(final CellInterface cell) {
            Map<HierName,Pair<HierName,Boolean>> result =
                staticizerInfo.get(cell.getFullyQualifiedType());
            ProductionRuleSet resultPrs =
                staticizerPrs.get(cell.getFullyQualifiedType());
            if (result == null) {
                final Map<HierName,Pair<HierName,Boolean>> info =
                    new HashMap<HierName,Pair<HierName,Boolean>>();
                final NetlistBlock nb =
                    (NetlistBlock) cell.getBlockInterface()
                                       .iterator(BlockInterface.NETLIST)
                                       .next();
                final ProductionRuleSet prs = new ProductionRuleSet();
                nb.getCDLTemplate().execute(
                    new com.avlsi.file.cdl.parser.CDLFactoryAdaptor() {
                        public void makeCall(HierName name, String subName,
                                             HierName[] args, Map params,
                                             Environment env) {
                            if (subName.startsWith("gate.STATICIZER.")) {
                                final HierName GND = args[0];
                                final HierName Vdd = args[1];
                                final HierName node = args[2];

                                final HierName invNode =
                                    CellUtils.getSmallInverterNode(name)
                                             .getFirst();
                                info.put(node,
                                    new Pair<HierName,Boolean>(invNode, true));

                                final HierNameAtomicBooleanExpression guard =
                                    new HierNameAtomicBooleanExpression(
                                            true, node);
                                prs.addProductionRule(new ProductionRule(
                                    guard,
                                    invNode, GND, ProductionRule.DOWN,
                                    false, false, false, false, 100, false));
                                prs.addProductionRule(new ProductionRule(
                                    guard.negated(),
                                    invNode, Vdd, ProductionRule.UP,
                                    false, false, false, false, 100, false));
                            } else if (subName.startsWith("gate.WEAK_INV.")) {
                                info.put(args[3],
                                    new Pair<HierName,Boolean>(args[2], false));
                            }
                        }
                    });

                if (info.isEmpty()) {
                    result = Collections.emptyMap();
                    resultPrs = null;
                } else {
                    result = info;
                    resultPrs = prs;
                }
                staticizerInfo.put(cell.getFullyQualifiedType(), result);
                staticizerPrs.put(cell.getFullyQualifiedType(), resultPrs);
            }

            return new Pair<Map<HierName,Pair<HierName,Boolean>>,
                            ProductionRuleSet>(result, resultPrs);
        }

        /**
         * Annotate dynamic nodes and their inverses.
         **/
        private void annotateDynamicNodes(
                final CellInterface cell,
                final HierName prefix,
                final Map<HierName,Pair<HierName,Boolean>> stats) {
            for (Map.Entry<HierName,Pair<HierName,Boolean>> entry :
                    stats.entrySet()) {
                final Node dynamicNode =
                    findNode(HierName.prefixName(prefix, entry.getKey()));
                if (dynamicNode == null) {
                    System.err.println("Dynamic node not found: " +
                            entry.getKey() + " in " + prefix + "/" +
                            cell.getFullyQualifiedType());
                } else {
                    if (dynamicNode.getBreakpoint()) {
                        System.err.println(
                                "Dynamic node multiply staticized: " +
                                entry.getKey() + " in " + prefix + "/" +
                                cell.getFullyQualifiedType());
                    }

                    // mark as dynamic node
                    dynamicNode.setBreakpoint(true);

                    // inverse of dynamic node
                    final Node invNode =
                        lookupNode(HierName.prefixName(
                                   prefix, entry.getValue().getFirst()));

                    // annotate whether it's a full staticizer
                    if (entry.getValue().getSecond()) {
                        dynamicNode.setRandom(true);
                    }

                    // set inverse node of dynamic node
                    dynamicNode.enabler = invNode;
                }
            }
        }

        private void process(
                final HierName prefix, 
                final HierName abstractPrefix, CellInterface cell,
                int depth, int defaultBehavior,
                boolean slaveAbove,
                final InstanceData instData,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final HierName grayboxPrefix,
                final boolean inGraybox,
                final boolean isEnv) {
            if (depth < 2)  System.out.print(".");
            // process subcells in lexicographic order, to ensure the random
            // seed associated with CSP processes can be reproduced
            Iterator it = new SortingIterator(
                    cell.getSubcellPairs(),
                    new Comparator() {
                        public int compare(Object o1, Object o2) {
                            final Pair p1 = (Pair) o1;
                            final Pair p2 = (Pair) o2;
                            return ((HierName) p1.getFirst()).compareTo(
                                    (HierName) p2.getFirst());
                        }
                    });
            // System.out.println("prefix is " + prefix);
            if (!it.hasNext() && !cell.isNode()) {
                System.out.println(prefix + " has no subcells --" +
                        cell.getFullyQualifiedType() );
            }

            // process signoff_constant directives
            processSignoffConstant(
                    prefix,
                    DirectiveUtils.getSubcellDirective(
                        cell,
                        DirectiveConstants.SIGNOFF_CONSTANT,
                        DirectiveConstants.DEEP_NODE_TYPE));

            // process unused_prs directives
            processUnusedPrs(
                    prefix,
                    DirectiveUtils.getSubcellDirective(
                        cell,
                        DirectiveConstants.UNUSED_PRS,
                        DirectiveConstants.DEEP_RULE_TYPE));

            if (isAstaEnabled() &&
                ObjectUtils.equals(abstractPrefix, envCell)) {
                if (astaContext == null) {
                    recordMeasureDelay(abstractPrefix, null, cell,
                                       instData, cadencizer.convert(cell));
                } else {
                    final Map<HierName,Node> nodes =
                        new HashMap<HierName,Node>();
                    for (HierName name : astaContext.getNames()) {
                        Node node = findNode(name);
                        if (node == null) {
                            node = lookupNode(canonize(name, this.cinfo));
                        }
                        nodes.put(name, node);
                    }
                    astaContext.setNodes(nodes);
                }
            }

            while (it.hasNext()) {
                final Pair p = (Pair) it.next();
                final CellInterface ci = (CellInterface)p.getSecond();
                if (ci.isNode()) continue;
                final HierName instName = (HierName) p.getFirst();
                final HierName name = HierName.append(prefix, instName);
                final HierName abstractName = HierName.append(abstractPrefix,
                                                              instName);
                // System.out.println("name is " + name);
                // System.out.println("CoSimInfo for " + name.getAsString('.') + " is");
                // System.out.println(ci.getCoSimInfo());
                final String abstractNameString =
                    abstractName.getAsString('.');
                assert params != null;
                int behavior =
                    (params == null) ?
                        CoSimParameters.UNSPEC :
                        params.lookupBehavior(abstractNameString);
                if (isAstaEnabled() && behavior == CoSimParameters.NULL) {
                    recordMeasureDelay(abstractPrefix, instName, ci,
                                       instData, cadencizer.convert(ci));
                    System.out.println("ASTA blackbox found: " +
                            abstractNameString + "/" +
                            ci.getFullyQualifiedType());
                }

                final HierName grayboxName;
                boolean subGraybox = inGraybox;
                if (isAstaEnabled() && CellUtils.isAstaGraybox(ci)) {
                    subGraybox = true;
                    System.out.println("ASTA graybox found: " +
                            abstractNameString + "/" +
                            ci.getFullyQualifiedType());
                    HierName inst = grayboxes.get(ci.getFullyQualifiedType());
                    if (inst == null) {
                        if (!CellUtils.isRouted(ci)) {
                            System.err.println(
                                "ERROR: ASTA graybox " +
                                ci.getFullyQualifiedType() + " is not routed");
                            errorOccurred = true;
                        }
                        grayboxes.put(ci.getFullyQualifiedType(), name);
                        grayboxName = null;
                    } else {
                        grayboxName = inst;
                    }
                } else {
                    if (grayboxPrefix == null) {
                        grayboxName = null;
                    } else {
                        grayboxName = HierName.append(grayboxPrefix, instName);
                    }
                }

                if (subGraybox) {
                    final CadenceInfo cinfo = cadencizer.convert(ci);
                    final AliasedSet nodes = cinfo.getLocalNodes();
                    final AliasedMap ports = cinfo.getPortNodes();
                    for (Iterator i = nodes.getCanonicalKeys(); i.hasNext(); ) {
                        final HierName canon = (HierName) i.next();
                        if (ports.getCanonicalKey(canon) != null) continue;
                        final Node curr =
                            lookupNode(HierName.append(name, canon));
                        // point the primary node to itself
                        final Node primary = grayboxName == null ? curr :
                            lookupNode(HierName.append(grayboxName, canon));
                        assert curr.enabler == null;
                        curr.enabler = primary;
                    }
                }

                behavior = behavior & ~CoSimParameters.NULL;

                // assert behavior != params.UNSPEC;
                if (behavior == CoSimParameters.UNSPEC) {
                  // if (name.getAsString('.').equals("_env")) 
                  //   behavior = params.BEH_ALL;
                  // else
                    behavior = defaultBehavior;
                }

                // use futureBehavior because we must be able to pass BEH_ALL
                // to createModels, but is_cosim needs to be computed
                // without the possibility of BEH_ALL
                final int futureBehavior =
                    behavior == CoSimParameters.BEH_ALL ?
                        defaultBehavior :
                        behavior;
                final boolean is_cosim =
                    ((futureBehavior & CoSimParameters.DIGITAL) != 0) &&
                    (futureBehavior != CoSimParameters.DIGITAL);

                final int arbitrationMode;
                // Note that we set the arbitration mode even if we
                // aren't doing a java or csp simulation.  This is
                // harmless because nothing will look at it.
                if (is_cosim)
                    arbitrationMode = Arbiter.LINKED_SLAVE;
                else if (slaveAbove)
                    arbitrationMode = Arbiter.LINKED_MASTER;
                else
                    arbitrationMode = Arbiter.NON_LINKED;

                final InstanceData newInstData = ci.isChannel() ?
                    DUMMY_INSTANCE
                  : instData.translate(cell, ci, (HierName) p.getFirst(),
                                       cadencizer);

                final Map<HalfOp<Node>,Set<HalfOp<Node>>> newAstaIgnore =
                    isAstaEnabled() ? updateAstaIgnore(astaIgnore, ci, name)
                                    : astaIgnore;

                // special slint handling for staticizers
                final Pair<Map<HierName,Pair<HierName,Boolean>>,
                           ProductionRuleSet> statsPrs;
                if (isSlintEnabled() && ci.containsNetlist()) {
                    statsPrs = getStaticizerInfo(ci);
                } else {
                    statsPrs = null;
                }

                createModels(name, ci, behavior, arbitrationMode,
                             params.lookupVerilogLevel(abstractNameString),
                             newInstData, newAstaIgnore,
                             statsPrs == null ? null : statsPrs.getSecond(),
                             isEnv);

                if (statsPrs != null) {
                    annotateDynamicNodes(ci, name, statsPrs.getFirst());
                }

                behavior = futureBehavior;

                if ((behavior & CoSimParameters.JAVA) != 0
                        && ! (ci.getJavaCoSimInfo().isEmpty()))
                    /* If a java cosimulation is desired, and we have found a
                     * java model for this cell, do not simulate java in
                     * subcells of this cell.  */
                    behavior = (behavior & ~CoSimParameters.JAVA);

                if ((behavior & CoSimParameters.CSP) != 0
                        && ci.containsRunnableCsp())
                    /* If a csp cosimulation is desired, and we have found a
                     * csp model for this cell, do not simulate csp in 
                     * subcells of this cell.  */
                    behavior = (behavior & ~CoSimParameters.CSP);

                if ((behavior & CoSimParameters.VERILOG) != 0)
                    /* If a verilog cosimulation is desired, and we have
                     * found a verilog model for this cell, do not simulate
                     * verilog in subcells of this cell.  */
                    behavior = (behavior & ~CoSimParameters.VERILOG);

                if (is_cosim)
                    process(CoSimParameters.addCoSimDigitalSuffix(name),
                            abstractName, ci, depth+1, CoSimParameters.DIGITAL,
                            true, newInstData, newAstaIgnore, grayboxName,
                            subGraybox, isEnv);
                else if (behavior == CoSimParameters.DIGITAL)
                    process(name, abstractName, ci, depth+1, behavior,
                            slaveAbove, newInstData, newAstaIgnore, grayboxName,
                            subGraybox, isEnv);
                else if (behavior != 0) 
                    throw new AssertionError("Cosim error:  Invalid behavior");

            }

            // process slint_ignore directives
            if (isSlintEnabled()) {
                final String fqcn = cell.getFullyQualifiedType();
                processIgnoreDirective(cell, DirectiveConstants.SLINT_IGNORE,
                    new UnaryAction<Map>() {
                        public void execute(Map m) {
                            updateSlintIgnores(fqcn, prefix, m);
                        }
                    }
                );
            }
        }

        protected void createModels(
                final HierName prefix,
                final CellInterface cell, 
                final int behavior,
                final int arbitrationMode,
                final String verilogLevel,
                final InstanceData instData,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final ProductionRuleSet extraPrs,
                final boolean isEnv) {
           
            // if (prefix == null) 
            //    System.out.println("prefix is null!");
            // else
            //    System.out.println("Behavior of cell " + prefix.getAsString('.') + 
            //                                        " is " + behavior);

            final long seed = rand.nextLong();
            ChannelDictionary dict = null;

            if (behavior == CoSimParameters.BEH_ALL) {  /* Do everything */
                if (cell.containsCompletePrs())
                    createRules(prefix, cell, instData, astaIgnore, extraPrs,
                                isEnv);
                dict = cell.getCoSimInfo(behavior).createNodeChannels(prefix, cell, getDelayTau(DIGITAL_TAU));
                try {
                    if (cell.getJavaCoSimInfo().getClassName() != null)
                        cell.buildJavaClass(prefix, dict, m_DeviceLoader,
                                arbitrationMode);
                    if (cell.containsRunnableCsp()) {
                        cell.buildCSPClass(prefix, dict, arbitrationMode,
                                           emitCSPCoverageProbes && !isEnv,
                                           seed, getDelayTau(DIGITAL_TAU));
                        // let prs handle DFT
                        if (!cell.containsCompletePrs())
                            handleDftChannels(prefix,
                                              cell.getCoSimInfo(behavior));
                    }
                } catch (ClassNotFoundException e1) {
                    System.out.println("Class of " + prefix.getAsString('.') +
                        " not found: " + e1.getMessage());
                    errorOccurred = true;
                } catch (InstantiationException e2) {
                    System.out.println(); // get off "applying models" line
                    if (!doSelfPrint(e2, System.out))
                        System.out.println("Error instantiating class of " +
                                           prefix.getAsString('.') +
                                           " of type " +
                                           cell.getFullyQualifiedType() +
                                           " --- " + e2.getMessage());
                    errorOccurred = true;
                } catch (DeviceConstructionException e) {
                    // TODO: something better here!
                    Debug.assertTrue(false, e.getMessage());
                    errorOccurred = true;
                }
                return;
            }
            assert behavior != CoSimParameters.UNSPEC;
            if (behavior == 0)  /* Do nothing */
                return;        
            /* Digital if not otherwise specified */
            if (behavior == CoSimParameters.UNSPEC ||   
                behavior == CoSimParameters.DIGITAL) {
                    /* Digital-only */
                    createRules(prefix, cell, instData, astaIgnore, extraPrs,
                                isEnv);
                    return;
            }
            if ((behavior & CoSimParameters.VERILOG) != 0) {
                // Do not support cosimulation with verilog.
                if (behavior != CoSimParameters.VERILOG)
                    throw new IllegalStateException
                        ("Cosimulation with verilog not supported.");

                final CoSimInfo coSimInfo =
                    cell.getCoSimInfo(CoSimParameters.CSP);

                linkVerilogPorts(prefix.getAsString('.'), cell,
                                 verilogLevel, cadencizer);

                // TODO: make sure that the verilog has been instantiated

                return;
            }
            if ((behavior & CoSimParameters.DIGITAL) != 0) {
                /* Cosimulation with digital.  At the moment, this is the only
                 * kind of cosimulation supported.  */
                createRules(CoSimParameters.addCoSimDigitalSuffix(prefix), cell,
                            instData, astaIgnore, extraPrs, isEnv);
                dict = CoSimInfo.createSplitMerges
                        (cell.getCoSimInfo(behavior), prefix, cell, cosimSlack,
                         verbose);
            } else {
                dict = cell.getCoSimInfo(behavior).createNodeChannels(prefix, cell, getDelayTau(DIGITAL_TAU));
            }
          try {
            // XXX: We currently do not support cosimulation of JAVA and CSP.
            // When support is added, we will have to tweak the
            // arbitrationMode as follows:  if the mode is master, both
            // cannot be master, so make one a master and the other a slave.
            // We probably want java to be the master by analogy with
            // prs being master.  Java is more detailed than csp, hence
            // closer to prs.
            if ((behavior & ~CoSimParameters.DIGITAL) == CoSimParameters.JAVA) {
                /* Create Java block Java class */
                cell.buildJavaClass(prefix, dict, m_DeviceLoader,
                        arbitrationMode);
            } else if ((behavior & ~CoSimParameters.DIGITAL) 
                    == CoSimParameters.CSP) {
                /* Create CSP Java class */
                cell.buildCSPClass(prefix, dict, arbitrationMode,
                                   emitCSPCoverageProbes && !isEnv,
                                   seed, getDelayTau(DIGITAL_TAU));
                // if cosimulating with prs, let prs handle DFT
                if (behavior == CoSimParameters.CSP)
                    handleDftChannels(prefix, cell.getCoSimInfo(behavior));
            } else
                throw new IllegalStateException(
                        "Cell has illegal cosim behavior parameter specified");
          } catch (ClassNotFoundException e1) {
            System.out.println("Class of " + prefix.getAsString('.') + 
                                " not found: " + e1.getMessage());
            errorOccurred = true;
          } catch (InstantiationException e2) {
              System.out.println(); // get off "applying models" line
              if (!doSelfPrint(e2, System.out))
                  System.out.println("Error instantiating class of " + 
                                     prefix.getAsString('.') + " of type " +
                                     cell.getFullyQualifiedType() +
                                     " --- " + e2.getMessage());
              errorOccurred = true;
          } catch (DeviceConstructionException e) {
              // TODO: something better here!
              e.printStackTrace();
              Debug.assertTrue(false, e.getMessage());
              errorOccurred = true;
          }
        }

        private void createRules(
                final HierName prefix,
                final CellInterface cell,
                final InstanceData instData,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final ProductionRuleSet extraPrs,
                final boolean isEnv) {
            // System.out.println("Creating rules for " + (prefix != null ? prefix.getAsString('.') : "(null)"));
            processSignoffConstant(
                    prefix,
                    DirectiveUtils.getPrsDirective(
                        cell,
                        DirectiveConstants.SIGNOFF_CONSTANT,
                        DirectiveConstants.NODE_TYPE));
            processUnusedPrs(
                    prefix,
                    DirectiveUtils.getPrsDirective(
                        cell,
                        DirectiveConstants.UNUSED_PRS,
                        DirectiveConstants.RULE_TYPE));

            createRulesFromSet(prefix, cell, cell.getProductionRuleSet(),
                               false, instData, astaIgnore, isEnv);
            if (extraPrs != null) {
                createRulesFromSet(prefix, cell, extraPrs, false, instData,
                                   astaIgnore, isEnv);
            }
            if (handleAsserts) {
                createRulesFromSet(prefix, cell,
                                   cell.getAssertedProductionRuleSet(), true,
                                   instData, astaIgnore, isEnv);
            }
        }

        private AndBooleanExpressionInterface filterAstaIgnore(
                final AndBooleanExpressionInterface and,
                final HierName prefix,
                final HalfOp<Node> target,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final Node localVdd,
                final Node localGND) {
            final Collection conjuncts = and.getConjuncts();
            final Collection result = new ArrayList();
            for (Iterator i = conjuncts.iterator(); i.hasNext(); ) {
                HierNameAtomicBooleanExpression a;
                a = (HierNameAtomicBooleanExpression) i.next();
                final HierName h = a.getName();
                final HierName fullName = prefixName(prefix, h);
                final Node guard = lookupNode(fullName);
                String reason = null;
                if (guard == localGND) {
                    reason = "GND";
                } else if (guard == localVdd) {
                    reason = "Vdd";
                } else if (matchAstaIgnore(astaIgnore,
                                           new HalfOp<Node>(guard,
                                                            a.getSense()),
                                           target)) {
                    reason = "directive";
                } else {
                    result.add(a);
                }
                if (reason != null) {
                    ui_out_verbose("ASTA ignored (" + reason + "): " +
                                   guard.getName() +
                                   (a.getSense() ? "+" : "-") + " -> " +
                                   target.node.getName() +
                                   (target.direction ? "+" : "-") + "\n");
                }
            }
            return new AndBooleanExpression(and.getSense(), result);
        }

        /**
         * helper function for createRules.  Targets of asserted rule
         * are assumed to be weird globals that don't need prefixing.
         * (This prevents the creation of x.L.ERROR, x.R.ERROR, etc.)
         **/
        private void createRulesFromSet(
                final HierName prefix,
                final CellInterface cell,
                final ProductionRuleSet prsSet,
                boolean assertedP,
                final InstanceData instData,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final boolean isEnv) {
            // unimplementable directives
            final boolean unimplementable = 
                ((Boolean) DirectiveUtils.getTopLevelDirective
                 (cell, DirectiveConstants.UNIMPL)).booleanValue();

            final boolean coverage_ignore = 
                ((Boolean) DirectiveUtils.getTopLevelDirective
                 (cell, DirectiveConstants.COVERAGE_IGNORE)).booleanValue();

            final float defaultSlew =
                ((Float) DirectiveUtils.getTopLevelDirective
                 (cell, DirectiveConstants.DEFAULT_SLEW)).floatValue();

            final AliasedSet localNodes =
                cadencizer.convert(cell).getLocalNodes();
            // REVIEW: perhaps there is a better way than using Cadencize
            final CellDelay cellDelay =
                new CellDelay(cell, localNodes, prsSet.getProductionRules(),
                              instData.getDelayBias(null), true);

            final Node localVdd, localGND;
            try {
                localVdd = findNode(HierName.makeHierName(prefix, "Vdd"));
                localGND = findNode(HierName.makeHierName(prefix, "GND"));
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Cannot create HierName: " + prefix);
            }

            // process rules in cell
            for (Iterator i = prsSet.getProductionRules(); i.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) i.next();
                final HierName h;
                if (assertedP) h = pr.getTarget();
                else h = prefixName( prefix, pr.getTarget() );
                final int d = pr.getDirection();
                final boolean updir = d == ProductionRule.UP;

                final HierName canonTarget = 
                    (HierName) localNodes.getCanonicalKey(pr.getTarget());

                final float extraDelay =
                    instData.getExtraDelay(updir, canonTarget);
                final float afterDelay =
                    cellDelay.getDelay(pr.getTarget(), updir, 100) + extraDelay;

                // an after 0 production rule overrides estimated or measured
                // delays associated the rule if it is also annotated as
                // "isochronic" and "unstab"
                final boolean isAfterZero =
                    pr.isIsochronic() && pr.isUnstable() && afterDelay == 0;

                final float estimated =
                    useDelayMode(ESTIMATED_TAU) ?
                        instData.getEstimatedDelay(updir, canonTarget) :
                        Float.NaN;

                float fdelay;
                if (isAfterZero || Float.isNaN(estimated)) {
                    fdelay = afterDelay;
                    if (useDelayMode(DIGITAL_TAU)) {
                        fdelay *= getDelayTau(DIGITAL_TAU);
                    }
                } else {
                    final float signoff =
                        instData.getEstimatedDelaySignoff(updir, canonTarget);
                    fdelay = 100 *
                             (Float.isNaN(signoff) ? estimated : signoff) /
                             getDelayTau(ESTIMATED_TAU);
                }

                final int delay = Math.round(fdelay);
                final boolean timed = pr.isTimed();
                final boolean isochronic = pr.isIsochronic();
                final Node target = lookupNode(h);

                target.setSlew(defaultSlew);

                // mark unstable nodes
                if (pr.isUnstable() || pr.isMetastable())
                    target.setUnstable(updir);

                final Triplet[] measuredRaw = useDelayMode(MEASURED_TAU) ?
                    instData.getMeasuredDelay(updir, target.getName()) : null;

                final float measuredCap =
                    instData.getMeasuredCap(target.getName());
                final float estimatedCap =
                    instData.getEstimatedCap(target.getName());

                // Warn about 0 delay rules, and missing measure_delay
                // directives unless cell is unimplementable or used in the
                // environment
                if (!unimplementable && !isEnv) {
                    if (delay == 0) {
                        final String type = cell.getFullyQualifiedType();
                        final String node = pr.getTarget().getAsString('.');
                        Pair warnPair = new Pair(type, node);
                        // don't warn redundantly
                        if (warnedCells.add(warnPair)) {
                            System.err.print("\nWarning: " + type + "/" +
                                               node + (updir ? "+" : "-") +
                                               " has zero delay");
                            if (isAfterZero) {
                                String ignored = "";
                                if (!Float.isNaN(estimated)) {
                                    ignored = " estimated_delay";
                                }
                                if (measuredRaw != null) {
                                    if (!ignored.equals("")) ignored += " and";
                                    ignored += " measured_delay";
                                }
                                if (!ignored.equals("")) {
                                    System.err.print(
                                            ";" + ignored +
                                            " directives are ignored because" +
                                            " of isochronic unstab rule");
                                }
                            }
                            System.err.println('.');
                        }
                    }
                    if (!assertedP && useDelayMode(MEASURED_TAU) &&
                        measuredRaw == null && !isAfterZero) {
                        Pair warnPair = new Pair(target, updir ? Boolean.TRUE
                                                               : Boolean.FALSE);
                        if (!warnedCells.contains(target) &&
                            warnedCells.add(warnPair)) {
                            final String type = cell.getFullyQualifiedType();
                            System.err.println("\nWarning: Using " +
                                (Float.isNaN(estimated) ? "digital" :
                                                          "estimated") +
                                " delay for (" + h + "/" + type + ") " +
                                target.getName() + (updir ? "+" : "-"));
                        }
                    }
                    if (!assertedP && Float.isNaN(measuredCap) &&
                        useMeasuredCap()) {
                        Pair warnPair = new Pair(target, "measured_cap");
                        if (!warnedCells.contains(target) &&
                            warnedCells.add(warnPair)) {
                            final String type = cell.getFullyQualifiedType();
                            System.err.println("\nWarning: Using " +
                                (Float.isNaN(estimatedCap) ? "zero" :
                                                             "estimated") +
                                " cap for (" + h + "/" + type + ") " +
                                target.getName());
                        }
                    }
                }

                target.setCapacitance(
                    (Float.isNaN(measuredCapScale) ? 1 : measuredCapScale) *
                    (Float.isNaN(measuredCap)
                        ? (Float.isNaN(estimatedCap) ? 0 : estimatedCap)
                        : measuredCap));

                final Pair[] measured;
                if (measuredRaw == null || isAfterZero) {
                    measured = null;
                } else {
                    final Pair[] raw = new Pair[measuredRaw.length];
                    for (int j = 0; j < measuredRaw.length; ++j) {
                        final HierName prfx =
                            (HierName) measuredRaw[j].getFirst();
                        final HierName trig =
                            (HierName) measuredRaw[j].getSecond();
                        raw[j] = new Pair(trig == null ? null : lookupNode(HierName.append(prfx, trig)), measuredRaw[j].getThird());
                    }
                    if (staVerbose) {
                        ProcessMeasuredDelay.printTable(raw, target, d,
                                                        "raw data");
                    }
                    measuredWarningContext = target;
                    measured =
                        ProcessMeasuredDelay.update(raw, measuredProcs);
                }
                // create the dsim Rules
                final BooleanExpressionInterface b = pr.getGuard();
                final OrBooleanExpressionInterface dnf = b.DNFForm();
                final HalfOp<Node> targetOp = new HalfOp<Node>(target, updir);
                for (Iterator j = dnf.getDisjuncts().iterator(); j.hasNext(); )
                {
                    final AndBooleanExpressionInterface and =
                        (AndBooleanExpressionInterface)j.next();
                    final AndBooleanExpressionInterface filtered;
                    if (isAstaEnabled()) {
                        filtered = filterAstaIgnore(and, prefix, targetOp,
                                                    astaIgnore, localVdd,
                                                    localGND);
                        if (filtered.getConjuncts().isEmpty()) continue;
                    } else {
                        filtered = and;
                    }

                    createHierRule(filtered, target, d, delay,
                                   Float.isNaN(estimated) ? DIGITAL_TAU
                                                          : ESTIMATED_TAU,
                                   timed, pr.getFastDelay(), pr.getSlowDelay(),
                                   isochronic,
                                   pr.isAbsolute(), assertedP, measured, prefix,
                                   defaultSlew, localNodes,
                                   coverage_ignore || assertedP || isEnv,
                                   astaGrayboxRules);
                }
            }
        }

        protected void createHierRule(
                AndBooleanExpressionInterface term, Node n,
                int direction, int delay, byte delay_type,
                boolean timed, float fastDelay, float slowDelay,
                boolean isochronic,
                boolean absoluteDelay, boolean assertedP,
                Pair[] delays,
                HierName prefix, float defaultSlew,
                AliasedSet localNodes,
                final boolean coverage_ignore,
                final Map<Pair<List<Node>,BitSet>,Rule> astaGrayboxRules) {
            DSim.this.createHierRule(term, n, direction, delay, delay_type,
                                     timed, fastDelay, slowDelay, isochronic,
                                     absoluteDelay,
                                     assertedP, delays, prefix, defaultSlew,
                                     localNodes, coverage_ignore,
                                     astaGrayboxRules);
        }

        protected boolean isAstaEnabled() {
            return DSim.this.isAstaEnabled();
        }
    }

    private class OptimizeThresholdCreator extends ModelCreator {
        public OptimizeThresholdCreator(CoSimParameters params,
                                        Cadencize cadencizer) {
            super(params, cadencizer);
        }

        protected void createModels(
                final HierName prefix,
                final CellInterface cell, 
                final int behavior,
                final int arbitrationMode,
                final String verilogLevel,
                final InstanceData instData,
                final Map<HalfOp<Node>,Set<HalfOp<Node>>> astaIgnore,
                final ProductionRuleSet extraPrs,
                final boolean isEnv) {
            super.createModels(prefix, cell, behavior, arbitrationMode,
                               verilogLevel, instData, astaIgnore, extraPrs,
                               isEnv);
            if (CellUtils.isRouted(cell) && !CellUtils.isLeaf(cell)) {
                if (routedInstancesByType == null) {
                    routedInstancesByType =
                        new MultiMap<String,HierName>(
                            new HashMap<String,Collection<HierName>>(),
                            MultiMap.<HierName> arrayListFactory());
                }
                routedInstancesByType.put(cell.getFullyQualifiedType(), prefix);
            }
        }

        protected void createHierRule(
                AndBooleanExpressionInterface term, final Node n,
                final int direction, int delay, byte delay_type,
                boolean timed, boolean isochronic,
                boolean absoluteDelay, boolean assertedP,
                Pair[] delays,
                final HierName prefix, float defaultSlew,
                AliasedSet localNodes,
                final boolean coverage_ignore,
                final Map<Pair<List<Node>,BitSet>,Rule> astaGrayboxRules) {
            if (delays == null) return;

            // identical types have the same data, so skip it
            final CadenceInfo curr =
                (CadenceInfo) getSubcell(cinfo, prefix.tail());
            final CadenceInfo prev =
                (CadenceInfo) getSubcell(savedCadenceInfo, prefix);
            if (curr == prev) return;

            boolean defaultDelay = false;
            final LinkedList measuredDelay = new LinkedList();
            final int length = term.getConjuncts().size();
            Set<Rule> common = null;
            for (Iterator i = term.getConjuncts().iterator(); i.hasNext(); ) {
                final HierNameAtomicBooleanExpression a
                    = (HierNameAtomicBooleanExpression) i.next();
                final HierName h = a.getName();
                
                final HierName fullName = prefixName(prefix, h);
                final Node guard = lookupNode(fullName);
                final Set<Rule> affected = new HashSet<Rule>();
                guard.foreachRule(
                    new Node.RuleFunc() {
                        public void accept(final Rule r, final int sense,
                                           final Node dummy) {
                            if (r.dir == direction && r.target() == n &&
                                r.prefix.equals(prefix))
                                affected.add(r);
                        }
                    }
                );
                if (common == null) {
                    common = affected;
                } else {
                    common.retainAll(affected);
                }

                for (int k = 0; k < delays.length; ++k) {
                    final Node trigger = (Node) delays[k].getFirst();
                    if (trigger == null) {
                        if (!defaultDelay) {
                            measuredDelay.addLast(new Pair(null, delays[k].getSecond()));
                            defaultDelay = true;
                        }
                    } else if (guard.equals(trigger)) {
                        measuredDelay.addFirst(new Pair(guard, delays[k].getSecond()));
                    }
                }
            }

            for (Rule r : common) {
                r.addMeasured((Pair[]) measuredDelay.toArray(new Pair[0]), 1);
            }
        }

        protected boolean isAstaEnabled() {
            return false;
        }
    }

    /*  Once an AliasedSet is saved, it is no longer safe to make new aliases
     *  to names referenced in the AliasedSet.                               */
    private void saveAliases(AliasedSet s) {
        final Iterator i = s.getCanonicalKeys();
        while (i.hasNext()) {
            final HierName canon = (HierName)i.next();
            lookupNode(canon);
            final Iterator j = s.getAliases(canon);
            while (j.hasNext()) {
                final HierName alias = (HierName)j.next();
                if (alias.equals(canon)) { 
                    continue;
                }
                aliasNode(canon, alias);
            }
        }
    }

    public Iterator<String> getDeviceNames() {
        return sched.getDeviceNames();
    }

    public AbstractDevice getDevice(final String deviceName) {
        return sched.getDevice(deviceName);
    }
    

    /** Turn debugging output on or off (i.e. exception stack trace dumping) **/
    public void setDebug(boolean state) {
        debug = state;
    }

    private void linkVerilogPorts
        (final /*@ non_null @*/ String instanceName,
         final /*@ non_null @*/ CellInterface cell,
         final /*@ non_null @*/ String verilogLevel,
         final /*@ non_null @*/ Cadencize cadencizer) {

        final Map markedPorts = CellUtils.markPorts(cell);
        final AliasedMap portNodes = cadencizer.convert(cell).getPortNodes();

        for (final Iterator iCanon = portNodes.getCanonicalKeys();
             iCanon.hasNext(); ) {
            final HierName canonPort = (HierName) iCanon.next();
            final String nodeName = canonPort.getAsString('.');

            for (final Iterator iConn = portNodes.getAliases(canonPort);
                 iConn.hasNext(); ) {
                final HierName connPort = (HierName) iConn.next();
                final String connNodeName = connPort.getAsString('.');
                final Integer dir = (Integer) markedPorts.get(connNodeName);
                if (dir == null) {
                    assert iConn.hasNext();
                    // keep looking for a node connected to the canonical
                    // port
                    continue;
                }
                final int direction = dir.intValue();
                final String fullNodeName = instanceName + '.' + nodeName;

                final Node node = findNode(fullNodeName);
                System.err.println("dsim node: " + node);
                final SharedBus sharedBus =
                    new VerilogSharedBus
                        ("top." + VerilogUtil.escapeIfNeeded(instanceName),
                         VerilogUtil.escapeIfNeeded(nodeName),
                         sigscan, opts, 1);
                System.err.println("Creating shared bus: " +
                                   sharedBus.getFullname());
                if (direction == PortDefinition.IN) {
                    System.err.println("Creating input node for verilog: " +
                            fullNodeName);
                    sharedBus.drivenByNode(node, 0);
                } else if (direction == PortDefinition.OUT) {
                    System.err.println("Creating output node for verilog:" +
                            fullNodeName);
                    sharedBus.driveNode(node, 0);
                } else {
                    System.err.println("Unknown direction for " + nodeName +
                                       " not added.");
                }
                // we found a node connected to the canonical port,
                // stop there.
                break;
            }
        }
    }

    //
    //Sigscan Support
    //
    
    //The Signalscan Database used by DSim to put any nodes or sharedbuses
    //it creates into
    private Sigscan sigscan=null;
    //The DebugOpts for any SharedBuses DSim creates
    private DebugOpts opts=new DebugOpts();

    private MultiMap<Node,HierName> nodesToAliases =null;
    
    MultiMap<Node,HierName> generateNodeToAliases(final Set<Node> filter) {
        final MultiMap<Node,HierName> nodesToAliases =
            new MultiMap<Node,HierName>(
                new HashMap<Node,Collection<HierName>>(),
                MultiMap.<HierName>arrayListFactory());
        for (Map.Entry<HierName,Node> entry : nodes.entrySet()) {
            final Node node = entry.getValue();
            if (filter == null || filter.contains(node)) {
                nodesToAliases.put(node, entry.getKey());
            }
        }
        return nodesToAliases;
    }
    
    /**
     * @throws IllegalStateException
     *         If a sigscan database has not been opened with
     *         {@link #openSigscan}.
     **/
    public void logSignals(String regex) {
        logSignals(regex, false);
    }

    public boolean logSignalsFixed(String sname) {
        final HierName name;
        try {
            name = HierName.makeHierName(sname, '.');
        } catch (InvalidHierNameException e) {
            System.err.println("Can't create HierName: " + sname);
            return false;
        }

        final Node node = findNode(name);
        if (node == null) {
            System.err.println("Can't find node: " + name);
            return false;
        } else {
            HierName parent = name.getParent();
            String sparent = "global";
            if (parent != null) {
                sparent = parent.toString();
                sname = name.getSuffixString();
            }  
            final NodeLogger logger = new NodeLogger(sigscan, sparent, sname);
            logger.nodeChanged(node, getTime());
            node.addWatch(logger);
            return true;
        }
    }

    public void logSignals(String regex, boolean useCanonialOnly) {
        if (sigscan == null)
            throw new IllegalStateException("No Sigscan database enabled");
        assert(nodes != null);
        
        String parsedRegex = StringUtil.getRegexForGlob(regex);
        if (debug)
            System.out.println(
                    "Searching node names with regex:\n\t"+parsedRegex);

        final Pattern pat;
        try {
            pat = Pattern.compile(parsedRegex);
        } catch (java.util.regex.PatternSyntaxException e) {
            throw new IllegalStateException("PatternSyntaxEror:\n\t"+
                                            e.getMessage());
        }

        int totalAliases = 0,totalNodes=0;
        
        if (useCanonialOnly) {
            final Set<HierName> seen = new HashSet();
            for (Node node : nodes.values()) {
                HierName name = node.getName();
                if (seen.add(name)) {
                    String sname = name.toString();
                    if (pat.matcher(sname).matches()) {
                        HierName parent = name.getParent();
                        String sparent = "global";
                        if (parent != null) {
                            sparent = parent.toString();
                            sname = name.getSuffixString();
                        }  
                        final NodeLogger logger =
                            new NodeLogger(sigscan, sparent, sname);
                        logger.nodeChanged(node, getTime());
                        node.addWatch(logger);
                        totalNodes++;
                        totalAliases++;
                    }
                }
            }
        } else {
            //First, build a mapping from nodes to aliases
            if (nodesToAliases == null) {
                nodesToAliases = generateNodeToAliases(null);
                System.out.println("Node-to-alias map created, "+
                                   nodesToAliases.keySet().size()+
                                   " nodes, "+
                                   nodes.keySet().size()+
                                   " aliases.");
            }

            for (Node node : nodesToAliases.keySet()) {
                int numAliases = 0;
                Collection<HierName> alist = nodesToAliases.get(node);
                //Here is where you would restrict the scope of the
                //logging if you wanted (use arraylists instead of
                //fixed arrays?)
                //It may be easier to augment the AliasedLogger
                //constructor to accomodate arraylists
                String[] names = new String[alist.size()];
                String[] scopenames = new String[alist.size()];
                for (HierName name : alist) {
                    final HierName parent = name.getParent();
                    final String matchString = name.toString();
                    if (parent == null || pat.matcher(matchString).matches()) {
                        final String sparent;
                        final String sname;
                        if (parent == null) {
                            sparent = "global";
                            sname = matchString;
                        } else {
                            sparent = parent.toString();
                            sname = name.getSuffixString();
                        }  

                        names[numAliases] = sname;
                        scopenames[numAliases] = sparent;
                        numAliases++;
                        totalAliases++;
                        //System.out.println("New alias = "+matchString);
                    }
                }
                if (numAliases > 0) {
                    final NodeLogger logger =
                        new NodeLogger(sigscan, scopenames, names, numAliases);
                    logger.nodeChanged(node, getTime());
                    node.addWatch(logger);
                    totalNodes++;
                }
            }
        
        }
        System.out.println(totalNodes+" nodes, "+totalAliases+
                           " aliases added to database with "+
                           "search string "+regex);
    }

    /**
     * Opens a sigscan database with the specified file name and
     * a dsim to real time conversion factor of <code>5e-12</code>
     * seconds per dsim time unit.
     **/
    public void openSigscan(final String filename) throws SigscanException {
        openSigscan(filename, 1e-9/1800d);
    }

    /** The smallest unit of time in signalscan = 10^ timeScale seconds **/
    private static final int timeScale = -15;
    
    /**
     * Opens a sigscan database with the specified file name and
     * a dsim to real time conversion factor of <code>secsPerDSim</code>
     * seconds per dsim time unit.
     **/
    public void openSigscan(
            final String filename,
            final double secsPerDSim)
        throws SigscanException {
        sigscan = new Sigscan(filename,false, timeScale);
        sigscan.setSigscanConversion(secsPerDSim/
                Math.pow(10, sigscan.getTimeScale()));
    }

    /**
     * Returns the sigscan database currently in use.
     **/
    public Sigscan getSigscan() { return sigscan; }

    public DebugOpts getDebugOpts() { return opts; }

    public void setSigscan(Sigscan s) { this.sigscan = s; }

    /**
     * get the value on a given channel; method will return an
     * integer for the value on a given channel. The method
     * can also be used to get the value on an array of channels
     *
     *  INVALID CHANNEL (UNDEFINED) : -2
     *  NEUTRAL: -1
     *  UNSTALENODE IN LIST: -3
     *  else {  we actually have a valid value  }
     *
     **/
    public int getChannel(String base){
        Node node = null;
        int j, valid, value;
        j = valid = value = 0;
        while(true){
            try {
                node = findNode(HierName.makeHierName(base+"."+j, '.'));
            } catch(InvalidHierNameException e){
                return -2;
            }
            if(node == null)break;
            if(node.getValue() == Node.VALUE_1) { value += j; valid++;}
            else if(node.getValue() == Node.VALUE_U){valid+=2;}
            j++;
        }
        if (valid == 1) return value;
        else if (j==0) return -2;
        else if (valid>0) return -3;
        else return -1;
    }

    
    public void cleanup() {
        if (sigscan != null) sigscan.close();
    }    

    /**
     * Returns the value of a given constant in a given cell.
     */
    public Value getCellConstant(String cellName, String constName)
        throws RecognitionException, TokenStreamException,
               AmbiguousLookupException, CastSemanticException,
               CastSyntaxException, IOException {
        Environment env = new CellConstants(getCastFileParser()).getTopLevelConstants(cellName);
        return env.lookup(Symbol.create(constName));
    }

    private static final Object INVERTED = Boolean.TRUE;
    private static final Object NONINVERTED = Boolean.FALSE;

    /**
     * Stores intermediate results from rule coverage analysis.
     **/
    private class RuleCoverageData {
        public final Map/*<Rule,TreeMapWithInteger<Node,Object>>*/ nodesByRule;
        /** Set of rules that is always on. **/
        public final Set/*<Rule>*/ alwaysOnRules;
        /** Set of rules that is always off. **/
        public final Set/*<Rule>*/ alwaysOffRules;
        /** Set of nodes that is always on. **/
        public final Set/*<Node>*/ alwaysOnNodes;
        /** Set of nodes that is always off. **/
        public final Set/*<Node>*/ alwaysOffNodes;
        public RuleCoverageData(final Map nodesByRule,
                                final Set alwaysOnRules,
                                final Set alwaysOffRules,
                                final Set alwaysOnNodes,
                                final Set alwaysOffNodes) {
            this.nodesByRule    = nodesByRule;
            this.alwaysOnRules  = alwaysOnRules;
            this.alwaysOffRules = alwaysOffRules;
            this.alwaysOnNodes  = alwaysOnNodes;
            this.alwaysOffNodes = alwaysOffNodes;
        }
    }

    /**
     * Stores statistics from rule coverage.
     **/
    public static class RuleCoverageStatistics {
        public final long totalRules;
        public final long ignoredRules;
        public final long constantRules;
        public final long coveredRules;
        public RuleCoverageStatistics(final long totalRules,
                                      final long ignoredRules,
                                      final long constantRules,
                                      final long coveredRules) {
            this.totalRules = totalRules;
            this.ignoredRules = ignoredRules;
            this.constantRules = constantRules;
            this.coveredRules = coveredRules;
        }
    }

    /**
     * Analyze production rules and propagate constants taking into account
     * relevant directives.
     *
     * @param progress true means print progress indicator on stdout
     * @return result of the analysis
     */
    private RuleCoverageData computeRuleCoverage(final boolean progress) {
        // this is where the real stuff begins
        final Map nodesByRule    = new IdentityHashMap(countNonAssertedRules());
        final Set alwaysOnRules  = new Map2SetAdapter(new IdentityHashMap());
        final Set alwaysOffRules = new Map2SetAdapter(new IdentityHashMap());
        final Set alwaysOnNodes  = new Map2SetAdapter(new IdentityHashMap());
        final Set alwaysOffNodes = new Map2SetAdapter(new IdentityHashMap());

        Node.RuleFunc rf = new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (r.asserted) return;

                    TreeMapWithInteger m =
                        (TreeMapWithInteger) nodesByRule.get(r);
                    if (m == null) {
                        m = new TreeMapWithInteger();
                        nodesByRule.put(r, m);
                    }
                    m.count++;
                    Object sens = (sense == 0 ? INVERTED : NONINVERTED);
                    Object old = m.put(n.getName(), sens);
                    if (old != null && old != sens) {
                        if (progress)
                            System.out.print('\r');
                        System.out.println("Badness: " + n.getName() + " has" +
                                           " both senses in the same rule");
                    }
                }
            };

        final HierName gndName = HierName.makeHierName("GND");
        final Node gndNode = lookupNode(gndName);
        final HierName vddName = HierName.makeHierName("Vdd");
        final Node vddNode = lookupNode(vddName);

        // Step 1: compute the Nodes that affect each Rule

        int i = 0, percent = -1;
        int size = nodes.size();

        Set doneThat =
            new Map2SetAdapter(new IdentityHashMap(nodeCount));

        for (Iterator<Node> it = nodes.values().iterator(); it.hasNext(); i++) {
            if (progress) {
                int p = (i + 1) * 100 / size;
                if (p != percent) {
                    percent = p;
                    System.out.print("\rStep 1 of 4: " + percent + "%");
                    System.out.flush();
                }
            }
            
            Node n = it.next();
            if (doneThat.add(n))
                n.foreachRule(rf);
        }

        // Step 2: compute the Rules that affect each Node

        final Set possiblyChangedNodes =
            new Map2SetAdapter(new IdentityHashMap());

        final Map rulesByNode = new IdentityHashMap(doneThat.size());
        doneThat = null;

        i = 0;
        percent = -1;
        size = rules.size();

        for (Iterator it = rules.iterator(); it.hasNext(); i++) {
            if (progress) {
                int p = (i + 1) * 100 / size;
                if (p != percent) {
                    percent = p;
                    System.out.print("\rStep 2 of 4: " + percent + "%  ");
                    System.out.flush();
                }
            }
            
            Rule r = (Rule) it.next();
            if (r.asserted) continue;

            Node n = r.target();
            Set s = (Set) rulesByNode.get(n);
            if (s == null) {
                s = new Map2SetAdapter(new IdentityHashMap());
                rulesByNode.put(n, s);
            }
            s.add(r);

            if (r.coverageRequirement == Rule.MUST_NOT_BE_COVERED) {
                alwaysOffRules.add(r);
                possiblyChangedNodes.add(r.target());
            }
        }

        // Step 3: compute Nodes and Rules which cannot transition

        Node.RuleFunc propagateOn = null;
        Node.RuleFunc propagateOff = null;

        for (int j = 0; j < 2; j++) {
            final int sense2 = j;
            propagateOn = propagateOff;
            propagateOff = new Node.RuleFunc() {
                    public void accept(Rule r, int sense1, Node n) {
                        if (r.asserted) return;

                        int sense = sense1 ^ sense2;
                        if (sense == 0) {
                            alwaysOffRules.add(r);
                            possiblyChangedNodes.add(r.target());
                        } else {
                            TreeMapWithInteger m =
                                (TreeMapWithInteger) nodesByRule.get(r);
                            assert m != null : "m is null";
                            m.count--;
                            if (m.count < 0)
                                throw new RuntimeException("bug 7014");
                            if (m.count == 0) {
                                alwaysOnRules.add(r);
                                possiblyChangedNodes.add(r.target());
                            }
                        }
                    }
                };
        }

        alwaysOnNodes.add(vddNode);
        alwaysOffNodes.add(gndNode);

        for (Iterator k = constantNodes.entrySet().iterator();
             k.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) k.next();
            final HierName name = (HierName) entry.getKey();
            final Node node = findNode(name);
            if (node == null) continue;

            final int val = ((Integer) entry.getValue()).intValue();
            if (val == 0) {
                alwaysOffNodes.add(node);
            } else if (val == 1) {
                alwaysOnNodes.add(node);
            }
        }

        i = 0;

        Set newAlwaysOnNodes  = new Map2SetAdapter(new IdentityHashMap());
        Set newAlwaysOffNodes = new Map2SetAdapter(new IdentityHashMap());

        newAlwaysOnNodes.addAll(alwaysOnNodes);
        newAlwaysOffNodes.addAll(alwaysOffNodes);

        while (newAlwaysOnNodes.size() > 0 ||
               newAlwaysOffNodes.size() > 0) {
            if (progress) {
                System.out.print("\rStep 3 of 4: pass " + ++i);
            }

            for (Iterator it = newAlwaysOnNodes.iterator();
                 it.hasNext(); ) {
                Node n = (Node) it.next();
                n.foreachRule(propagateOn);
            }
            for (Iterator it = newAlwaysOffNodes.iterator();
                 it.hasNext(); ) {
                Node n = (Node) it.next();
                n.foreachRule(propagateOff);
            }
            
            newAlwaysOnNodes = new Map2SetAdapter(new IdentityHashMap());
            newAlwaysOffNodes = new Map2SetAdapter(new IdentityHashMap());

            for (Iterator it = possiblyChangedNodes.iterator();
                 it.hasNext(); ) {
                Node n = (Node) it.next();
                Set s = (Set) rulesByNode.get(n);
                assert (s != null);
                boolean mustBeOn = false, mustBeOff = false;
                boolean couldBeOn = false, couldBeOff = false;
                for (Iterator it2 = s.iterator(); it2.hasNext(); ) {
                    Rule r = (Rule) it2.next();
                    if (alwaysOnRules.contains(r)) {
                        if (r.getDirection() == '+')
                            mustBeOn = true;
                        else
                            mustBeOff = true;
                    } else if (!alwaysOffRules.contains(r)) {
                        if (r.getDirection() == '+')
                            couldBeOn = true;
                        else
                            couldBeOff = true;
                    }
                }

                if (mustBeOn && mustBeOff) {
                    if (progress)
                        System.out.print('\r');
                    System.out.println("Extreme badness: " + n.getName() +
                                       " must be both on and off");
                }

                if (mustBeOn) {
                    if (!alwaysOnNodes.contains(n)) {
                        alwaysOnNodes.add(n);
                        newAlwaysOnNodes.add(n);
                    }
                } else if (mustBeOff) {
                    if (!alwaysOffNodes.contains(n)) {
                        alwaysOffNodes.add(n);
                        newAlwaysOffNodes.add(n);
                    }
                } else {
                    if (couldBeOn && !couldBeOff &&
                        !alwaysOnNodes.contains(n)) {
                        alwaysOnNodes.add(n);
                        newAlwaysOnNodes.add(n);
                    } else if (couldBeOff && !couldBeOn &&
                               !alwaysOffNodes.contains(n)) {
                        alwaysOffNodes.add(n);
                        newAlwaysOffNodes.add(n);
                    } else if (!couldBeOn && !couldBeOff) {
                        if (progress)
                            System.out.print('\r');
                        System.out.println("Warning: " + n.getName() +
                                           " is floating");
                    }
                }
            }

            possiblyChangedNodes.clear();
        }

        return new RuleCoverageData(nodesByRule, alwaysOnRules, alwaysOffRules,
                                    alwaysOnNodes, alwaysOffNodes);
    }

    public interface RuleCoverageCallback {
        boolean want(final Rule rule, boolean alwaysOn, boolean alwaysOff);
        void rule(String r) throws IOException;
    }

    public RuleCoverageStatistics reportRuleCoverage(final Writer dest,
                                                     final boolean progress,
                                                     final boolean full)
        throws IOException {
        final RuleCoverageCallback cb = new RuleCoverageCallback() {
            public boolean want(final Rule r, final boolean alwaysOn,
                                final boolean alwaysOff) {
                return full || (r.transitionCount == 0 &&
                                r.coverageRequirement != Rule.IGNORE_COVERAGE &&
                                !alwaysOn && !alwaysOff);
            }
            public void rule(String r) throws IOException {
                dest.write(r);
                dest.write('\n');
            }
        };

        return reportRuleCoverage(cb, progress, full);
    }

    /**
     * Creates a human-readable (or at least Perl-script-readable) textual
     * report of the PRS DNF rule coverage.
     * @param dest     Writer to write the textual output to
     * @param progress whether to print progress indicator on stdout
     * @param full     whether to output full report
     */
    public RuleCoverageStatistics reportRuleCoverage(
            final RuleCoverageCallback cb,
            final boolean progress,
            final boolean full)
        throws IOException {

        /* We probably don't need scan_coverage_model anymore
        // warn if scan_coverage_model isn't true

        boolean scan_coverage_model = false;

        try {
            scan_coverage_model = ((BoolValue)getCellConstant("standard.null.NULL", "scan_coverage_model")).getValue();
        } catch (Exception e) {
            // leave scan_coverage_model false
        }

        if (!scan_coverage_model) {
            System.out.println("Warning: Don't you want to set scan_coverage_model?");
        }
        */

        try {
            final RuleCoverageData coverageData = computeRuleCoverage(progress);

            // Step 4: Write the file by iterating over all Rules

            int i = 0, percent = -1;
            int size = rules.size();

            int ignoreRules = 0, constantRules = 0, nonConstantCovered = 0;

            for (Iterator it = rules.iterator(); it.hasNext(); i++) {
                if (progress) {
                    int p = (i + 1) * 100 / size;
                    if (p != percent) {
                        percent = p;
                        System.out.print("\rStep 4 of 4: " + percent +
                                         "%        ");
                        System.out.flush();
                    }
                }
                
                Rule r = (Rule) it.next();
                if (r.asserted) continue;

                boolean alwaysOn = coverageData.alwaysOnRules.contains(r);
                boolean alwaysOff = coverageData.alwaysOffRules.contains(r);

                boolean coverageIgnore =
                    (r.coverageRequirement == Rule.IGNORE_COVERAGE);

                if (coverageIgnore)
                    ignoreRules++;
                else if (alwaysOn || alwaysOff)
                    constantRules++;
                else if (r.transitionCount != 0)
                    nonConstantCovered++;

                if (cb.want(r, alwaysOn, alwaysOff)) {
                    Map m = (Map) coverageData.nodesByRule.get(r);
                    assert m != null;
                    StringBuffer sb = new StringBuffer();
                    for (Iterator it2 = m.entrySet().iterator();
                         it2.hasNext(); ) {
                        Map.Entry e = (Map.Entry) it2.next();
                        if (sb.length() > 0)
                            sb.append(" & ");
                        if (e.getValue() == INVERTED)
                            sb.append('~');
                        sb.append(e.getKey());
                    }
                    sb.append(" -> ");
                    sb.append(r.target().getName());
                    sb.append(r.getDirection());
                    if (full) {
                        sb.append(" (");
                        sb.append(r.transitionCount);
                        sb.append(')');
                        boolean space = false;
                        if (coverageData.alwaysOnRules.contains(r)) {
                            space = true;
                            sb.append(" *");
                        } else if (coverageData.alwaysOffRules.contains(r)) {
                            space = true;
                            sb.append(" X");
                        }
                        if (coverageIgnore) {
                            if (!space)
                                sb.append(' ');
                            sb.append('#');
                        }
                    }
                    cb.rule(sb.toString());
                }
            }
 
            final RuleCoverageStatistics stats =
                new RuleCoverageStatistics(countNonAssertedRules(),
                                           ignoreRules,
                                           constantRules,
                                           nonConstantCovered);

            // Step 5: print some interesting statistics
            if (progress) {
                NumberFormat fmt = NumberFormat.getPercentInstance();
                fmt.setMinimumFractionDigits(2);
                fmt.setMaximumFractionDigits(2);

                size = countNonAssertedRules();
                double rules = size;
                double constRules = constantRules;
                double covered = nonConstantCovered;
                double nonconst = size - constantRules - ignoreRules;
                double ign = ignoreRules;

                String[][] pad = new String[][]
                    {{ Integer.toString(size),
                       Integer.toString(ignoreRules),
                       Integer.toString(constantRules),
                       Integer.toString(nonConstantCovered) },
                     { fmt.format(ign / rules),
                       fmt.format(constRules / rules),
                       fmt.format(covered / nonconst) },
                     { Integer.toString(size),
                       Integer.toString(size - constantRules - ignoreRules) }};

                for (int k = 0; k < pad.length; k++) {
                    int max = 0;
                    for (int j = 0; j < pad[k].length; j++)
                        max = Math.max(max, pad[k][j].length());

                    for (int j = 0; j < pad[k].length; j++)
                        pad[k][j] = StringUtil.padLeft(pad[k][j], max, ' ');
                }

                System.out.println("\rTotal rules:    " + pad[0][0]);
                System.out.println("Ignored rules:  " + pad[0][1] + " (" +
                                   pad[1][0] + " of " + pad[2][0] + ')');
                System.out.println("Constant rules: " + pad[0][2] + " (" +
                                   pad[1][1] + " of " + pad[2][0] + ')');
                System.out.print("Covered rules:  " + pad[0][3] + " (" +
                                 pad[1][2] + " of " + pad[2][1] + ')');
            }

            return stats;
        } finally {
            if (progress)
                System.out.println();
        }
    }

    /**
     * Return a string that is a properly escaped Verilog hierarchical
     * reference for <code>tail</code> with respect to <code>top</code>.  The
     * main problem is to handle inlined instances correctly, because such
     * instance names contains a dot which in that case is not a hierarchy
     * delimiter.
     **/
    private String getVerilogString(HierName tail, final CellInterface top,
                                    final UnaryPredicate ignoreCheck) {
        HierName inst = null;
        if (ignoreCheck.evaluate(top)) return null;
        if (CellUtils.isLeaf(top) || CellUtils.isWiring(top)) {
            inst = tail;
            tail = null;
        }
        while (tail != null) {
            final HierName head = tail.head();
            tail = tail.tail();
            inst = prefixName(inst, head);
            final CellInterface subcell = top.getSubcell(inst);
            if (subcell != null) {
                if (CellUtils.isWiring(subcell) && tail != null) {
                    inst = prefixName(inst, tail);
                    tail = null;
                }
                final String curr =
                    VerilogUtil.escapeIfNeeded(inst.getAsString('.'));
                if (tail == null) {
                    return curr;
                } else {
                    final String next =
                        getVerilogString(tail, subcell, ignoreCheck);
                    if (next == null) return null;
                    else return curr + "." + next;
                }
            }
        }
        return VerilogUtil.escapeIfNeeded(inst.getAsString('.'));
    }

    public void reportCoverageConstantNodes(Writer dest, List ignoreList,
                                            final boolean progress)
        throws Exception {
        try {
            final CellInterface top =
                getCastFileParser().getFullyQualifiedCell(cellName);
            final RuleCoverageData coverageData = computeRuleCoverage(progress);
            final UnaryPredicate checkIgnore =
                CellUtils.getTypeMatcher(ignoreList);

            System.out.print("\rStep 4 of 4              ");
            System.out.flush();
            for (final Iterator it = coverageData.alwaysOnNodes.iterator();
                 it.hasNext();) {
                final Node node = (Node) it.next();
                if (!node.getName().head().getAsString('.').equals("x"))
                    continue;
                final String vnode =
                    getVerilogString(node.getName().tail(), top, checkIgnore);
                if (vnode != null) dest.write("force " + vnode + " = 1;\n");
            }
            for (final Iterator it = coverageData.alwaysOffNodes.iterator();
                 it.hasNext();) {
                final Node node = (Node) it.next();
                if (!node.getName().head().getAsString('.').equals("x"))
                    continue;
                final String vnode =
                    getVerilogString(node.getName().tail(), top, checkIgnore);
                if (vnode != null) dest.write("force " + vnode + " = 0;\n");
            }
        } finally {
            if (progress)
                System.out.println();
        }
    }

    /**
     * Yes, this is an odd bird, but it's convenient in implementing
     * reportRuleCoverage().
     */
    private static class TreeMapWithInteger extends TreeMap {
        public int count = 0;
    }

    /**
     * If any of the chained exceptions in t is a VisitorExceptionWithLocation,
     * make it print its own pretty message, and return true.
     * If not, return false.
     */
    public static boolean doSelfPrint(Throwable t, PrintStream p) {
        while (t != null) {
            if (t instanceof VisitorExceptionWithLocation) {
                ((VisitorExceptionWithLocation)t).printSelf(p);
                return true;
            }
            t = t.getCause();
        }
        return false;
    }

    public interface ProgressIndicator {
        void percentCompleted(int percent);
    }

    public void foreachRule(final Node.RuleFunc ruleFunc) {
        foreachRule(ruleFunc, null);
    }

    public void foreachRule(final Node.RuleFunc ruleFunc,
                            final ProgressIndicator indicator) {
        final int size = nodes.size();
        boolean gen = false;
        int i = 0;
        int percent = -1;
        for (Iterator<Node> it = nodes.values().iterator(); it.hasNext(); i++) {
            if (indicator != null) {
                final int p = (i + 1) * 100 / size;
                if (p != percent) {
                    percent = p;
                    indicator.percentCompleted(percent);
                }
            }
            
            final Node n = it.next();
            if (i == 0) gen = n.getGeneration();
            if (n.getGeneration() == gen) {
                n.foreachRule(ruleFunc);
                n.setGeneration(!gen);
            }
        }
    }

    void foreachNode(final UnaryPredicate<Node> nodeFunc) {
        boolean gen = false;
        boolean first = true;
        for (Iterator<Node> it = nodes.values().iterator(); it.hasNext(); ) {
            final Node n = it.next();
            if (first) {
                first = false;
                gen = n.getGeneration();
            }
            if (n.getGeneration() == gen) {
                if (nodeFunc.evaluate(n)) n.setGeneration(!gen);
            }
        }
    }

    // Parameters persist across cosimulations
    private final HashMap<String,String> parameters =
        new HashMap<String,String>();

    public void setParameter(final String key, final String val) {
        if (val == null) {
            parameters.remove(key);
        } else {
            parameters.put(key, val);
        }
    }
    public String getParameter(final String key) {
        return parameters.get(key);
    }
    public double getDouble(final String key, final double def) {
        final String val = getParameter(key);
        if (val == null) return def;
        else {
            try {
                return Double.parseDouble(val);
            } catch (NumberFormatException e) {
                System.err.println(
                    "Warning: expected floating number for parameter " +
                    key + ", found " + val + "; using default value " +
                    def);
                return def;
            }
        }
    }
}
