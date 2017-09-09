/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: $
 * $DateTime: $
 * $Author: $
 */

package com.avlsi.tools.dsim;

import com.avlsi.fast.ports.NodeType;
import com.avlsi.file.common.HierName;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.functions.BinaryPredicate;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.SortingIterator;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.util.TreeMap;
import java.util.function.IntSupplier;

/**
 * Static DSim utility functions.
 **/
public class DSimUtil {

    private DSim dsim = null;
    private MeasureNodeWatcher measure = null;
    private CombinedNodeWatcher<HaltOnCycleWatcher> haltWatcher = null;
    private Node GND;
    private Node Vdd;
    private Node _RESET;
    private Node START;
    private Node DLY;
    private Node ERROR;
    private Node ENV_RESET;

    public static final int STANDARD_RESET = 0;

    public static final HierName _RESET_NAME =
        HierName.makeHierName("_RESET");
    public static final HierName START_NAME =
        HierName.makeHierName("START");
    public static final HierName DLY_NAME =
        HierName.makeHierName("DLY");
    // FIXME: _env should not be hardcoded here, as it could be different
    public static final HierName ENV_RESET_NAME =
        HierName.append(HierName.makeHierName("_env"), _RESET_NAME);

    /**
     * DSimUtil constructor.  Looks up nodes in preparation for
     * doing stuff.  Shouldn't be called until a cell is instantiated.
     **/
    public DSimUtil() {
        dsim = DSim.get();
        GND = dsim.findNode("GND");
        Vdd = dsim.findNode("Vdd");
        _RESET = dsim.findNode(_RESET_NAME);
        START  = dsim.findNode(START_NAME);
        DLY    = dsim.findNode(DLY_NAME);
        ENV_RESET = dsim.findNode(ENV_RESET_NAME);
        ERROR = dsim.findNode("ERROR");
    }

    /** 
     * Resets DSim.  Reset is performed with full random (untimed)
     * transitions.  Following reset, the timing model is restored to
     * its prior value.  START+ follows _RESET+.
     * 
     * @throws IllegalArgumentException
     * @throws IllegalStateException
     **/
    public void resetDSim(int protocol, IntSupplier init) {
        // Store initial state
        int     random_state = dsim.getRandom();
        boolean warn_state   = dsim.getWarn();
        boolean error_state  = dsim.getError();

        // Disable error checking & reporting
        dsim.setWarn(false);
        dsim.setError(false);

        // Enable random timing
        dsim.setRandom(DSim.UNTIMED_RANDOM);

        // Initialize nodes
        //dsim.clearEventQueues();
        if (DLY!=null) DLY.setValueAndEnqueueDependents(Node.VALUE_0); // might be overridden by env
        if (GND!=null) GND.setValueAndEnqueueDependents(Node.VALUE_0);
        if (Vdd!=null) Vdd.setValueAndEnqueueDependents(Node.VALUE_1);

        if (protocol == STANDARD_RESET) {
            Node resetNode = _RESET;
            if (resetNode == null) resetNode = ENV_RESET;
               
            // Reset and cycle
            if (resetNode!=null) resetNode.scheduleImmediate(Node.VALUE_0);
            if (START!=null) START.scheduleImmediate(Node.VALUE_0);
            initResetNodes(init);
            dsim.cycle(-1);

            // Turn on error checking & reporting, then exit reset
            Set unstabSet = dsim.getNodesWithValue(Node.VALUE_U);
            if (!unstabSet.isEmpty()) {
                System.out.println(
                    "Nodes unstable during reset:");
                printNodeSet(unstabSet);
            }
            dsim.setWarn(true);
            dsim.setError(true);

            // de-assert _RESET
            if (resetNode!=null) {
                resetNode.scheduleImmediate(Node.VALUE_1);
            }

            // second phase doreset if START node exists
            if (START!=null) {
                if (resetNode!=null) dsim.cycle(-1);
                START.scheduleImmediate(Node.VALUE_1);
            }
        }
        else {
            throw new IllegalArgumentException(
                "Unknown reset protocol specified: "+protocol);
        }

        // Restore DSim state
        dsim.setWarn(warn_state);
        dsim.setError(error_state);
        dsim.setRandom(random_state);
    }

    /**
     * Returns the best of what DSim has to offer in the way of reset
     * nodes.  Preferentially picks _RESET, then _env._RESET, then
     * null.
     **/
    public static Node getResetNode() {
        Node n = DSim.get().findNode(_RESET_NAME);
        if (n == null) n = DSim.get().findNode(ENV_RESET_NAME);
        return n;
    }

    /**
     * Performs repeated random-timing reset tests.  This test is intended
     * to catch timing races involving reset (such as the one that killed
     * A2S_40 in F1).  
     *
     * @param numTests Number of reset cycles to run.
     * @param numCycles Number of cycles to run for between each reset.
     * @param cycleNode Node to use for post-reset cycling.
     * @param resetProtocol Reset protocol to use (STANDARD_RESET).
     * 
     * @throws IllegalArgumentException
     * @throws IllegalStateException
     *
     * Returns true if run successfully (no deadlock); false otherwise
     **/
    public boolean resetStressTest(int numTests, 
				   int numCycles, 
				   String cycleNode,
				   int resetProtocol,
                   IntSupplier init) {
	boolean done = true;
	int cnode_tcounts;
        Node cnode = dsim.findNode(cycleNode);
        if (cnode == null) {
            throw new IllegalStateException(
                "Node '"+cycleNode+"' does not exist.");
        }
        if (numTests < 0) {
            throw new IllegalArgumentException(
                "Negative numTests to resetStressTest().");
        }
	
        for (int i=0; i<numTests; i++) {
            resetDSim(resetProtocol, init);
	    cnode_tcounts = cnode.getTCount();
            dsim.cycle(cnode,2*numCycles);
            if (cnode.getTCount()-2*numCycles < cnode_tcounts) {
                System.out.println(
                    "Reset stress test deadlocked in reset cycle " + (i + 1));
                done = false;
            }
        }
	return done;
    }

    public void initResetNodes(final IntSupplier s) {
        for (Node n :
                new IterableIterator<Node>(
                    new SortingIterator<Node>(
                        new FilteringIterator<Node>(
                            dsim.getNodes(),
                            n -> n.getInit() != Node.Init.UNDEFINED),
                        new Comparator<Node>() {
                            public int compare(Node a, Node b) {
                                return a.getName().compareTo(b.getName());
                            }
                        }))) {
            final byte init;
            switch (n.getInit()) {
              case ZERO:
                init = 0; break;
              case ONE:
                init = 1; break;
              case RANDOM:
                init = (byte) s.getAsInt(); break;
              default:
                throw new AssertionError("init value = " + n.getInit());
            }
            n.setValueAndEnqueueDependents(init);
        }
    }

    public void resetInputNodes(final CellInterface cell,
                                final String instanceName) {
        final Node reset = DSimUtil.getResetNode();
        if (reset != null) {
            // identify all input ports
            final Collection<Node> inputPorts = new HashSet<Node>();
            (new CellUtils.MarkPort() {
                protected void mark(final NodeType nodeType,
                                    final String name,
                                    final int newDir) {
                    if (newDir < 0) {
                        final Node n = dsim.findNode(instanceName + "." + name);
                        // don't add special nodes
                        if (n != null && n != GND && n != Vdd && n != reset) {
                            inputPorts.add(n);
                        }
                    }
                }
            }).mark(cell);

            if (!inputPorts.isEmpty()) {
                reset.addWatch(new NodeWatcher() {
                    public final void nodeChanged(Node node, long time) {
                        if (node.getValue() == Node.VALUE_0) {
                            for (Node inputPort : inputPorts) {
                                inputPort.scheduleImmediate(Node.VALUE_0);
                            }
                        }
                    }
                });
            }
        }
    }

    /**
     * Class for measuring the times of transitions on a particular
     * node.  Will calculate average period and output a breakdown
     * of observed periods.
     **/
    static class MeasureNodeStats implements NodeWatcher {
        private byte phase = Node.VALUE_U;
        private int numIgnored = 0;
        private long startTime;
        private long totalTime = 0;
        private long totalPeriods = 0;
        private float[] slewStat = new float[] { Float.POSITIVE_INFINITY,
                                                 Float.NEGATIVE_INFINITY,
                                                 0 };
        private long totalCycles = 0;
        private float measuredNTPC = Float.NaN;
        private TreeMap cycles;
        private Node n;
        private boolean longOutput;
        private boolean continuousOutput;

        MeasureNodeStats(Node node, boolean verbose, boolean continuous) { 
            cycles = new TreeMap();
            n = node; 
            longOutput = verbose;
            continuousOutput = continuous;
            phase = n.getValue();
        }

        public final void nodeChanged(Node node, long time) {
            byte state = n.getValue();
            if (state == Node.VALUE_U) {
                numIgnored++;
                startTime = 0;
                phase = Node.VALUE_U;
            }
            else {
                if (phase == Node.VALUE_U) {
                    phase = state;
                    startTime = time;
                }
                else if (state == phase) {
                    if (startTime == 0) startTime = time;
                    else {
                        totalTime += time - startTime;
                        totalPeriods++;
                        Long period = new Long((time - startTime + 50)/100);
                        startTime = time;
                        newCycle(period);
                    }
                }
                final float slew = n.getSlew();
                slewStat[0] = Math.min(slew, slewStat[0]);
                slewStat[1] = Math.max(slew, slewStat[1]);
                slewStat[2] += slew;
                totalCycles++;
                if (continuousOutput) printStats();
            }
        }

        public void newCycle(Long period) {
            Long cnt = (Long)cycles.get(period);
            if (cnt == null)
                cnt = new Long(1);
            else
                cnt = new Long(cnt.longValue()+1);
            cycles.put(period,cnt);
        }

        String printStats() {
            boolean first = true;
            String s = "";
            String long_str = "(";
            Set periods = cycles.keySet();
            Iterator it = periods.iterator();
            while (it.hasNext()) {
                Long period = (Long)it.next();
                Long cnt = (Long)cycles.get(period);
                if (!first) long_str += " ";
                else first = false;
                long_str += period.toString() + "[";
                if (cnt.longValue() >= 10) {
                    float percent = (float)(cnt.longValue() * 100) 
                                    / totalPeriods;
                    long_str += percent + "%";
                }
                else 
                    long_str += cnt.longValue();
                long_str += "]";
            }
            if (numIgnored > 0) long_str += "U[" + numIgnored + "]";
            long_str += " [" + slewStat[0] + ":" + slewStat[1] + ":" +
                        slewStat[2] / totalCycles + "]";
            long_str += ")";
            float averagePeriod = (float)totalTime / totalPeriods / 100;
            measuredNTPC = averagePeriod;
            String str = n.getName().getAsString('.') + ": "+averagePeriod+"t";
            if (longOutput) str += " " + long_str;
            if (totalPeriods != 0 && averagePeriod != 0.0) {
                //
                // Only print out the measurement if it's meaningful.
                // (Error before cycling or full random timing can 
                // prevent this from being the case.)
                //
                System.out.println(str);
                s += str;
            }
            return s;
        }
        
        //return the measured NTPC 
        float getMeasuredNTPC(){return  measuredNTPC; }
        
        void clearMeasuredNTPC(){measuredNTPC = Float.NaN; }

        void restart() {
            phase = n.getValue();
            startTime = 0;
            totalTime = 0;
            totalPeriods = 0;
            numIgnored = 0;
            cycles = new TreeMap();
            totalCycles = 0;
            slewStat = new float[] { Float.POSITIVE_INFINITY,
                                     Float.NEGATIVE_INFINITY,
                                     0 };
        }

        void setVerbose(boolean verbose) {
            longOutput = verbose;
        }

        Node getNode() {
            return n;
        }
    }

    static class HaltOnCycleWatcher extends MeasureNodeStats {
        private final BinaryPredicate p;
        private final Integer tpc;
        HaltOnCycleWatcher(final Node node, final BinaryPredicate p,
                           final int tpc) {
            super(node, false, false);
            this.tpc = Integer.valueOf(tpc);
            this.p = p;
        }
        public void newCycle(Long period) {
            if (p.evaluate(period, tpc)) {
                System.out.println("Last cycle on " + getNode().getName() +
                                   " was " + period + " transitions.");
                DSim.get().interrupt();
            }
        }
    }

    static class CombinedNodeWatcher<T extends NodeWatcher>
    implements NodeWatcher, Iterable<Map.Entry<Node,T>> {
        private final Map<Node,T> map;
        public CombinedNodeWatcher() {
            this(new HashMap<Node,T>());
        }
        public CombinedNodeWatcher(final Map<Node,T> map) {
            this.map = map;
        }
        public T addNode(Node node, T watcher) {
            node.addWatch(this);
            return map.put(node, watcher);
        }
        public T deleteNode(Node node) {
            final T stored = map.remove(node);
            if (stored != null) node.removeWatch(this);
            return stored;
        }
        public T getNode(Node node) {
            return map.get(node);
        }
        public final void nodeChanged(Node node, long time) {
            final T stored = map.get(node);
            if (stored != null) {
                stored.nodeChanged(node, time);
            }
        }
        public void clear() {
            for (Node node : map.keySet()) node.removeWatch(this);
            map.clear();
        }
        public Iterator<Map.Entry<Node,T>> iterator() {
            return Collections.unmodifiableMap(map).entrySet().iterator();
        }
    }

    /**
     * Node Watcher class which watches a list of nodes and tallies 
     * their transitions statistics.
     **/
    static class MeasureNodeWatcher
    extends CombinedNodeWatcher<MeasureNodeStats> {
        private boolean verbose;

        MeasureNodeWatcher(boolean verbose) { 
            this.verbose = verbose;
        }

        void addNode(Node node, boolean continuous) {
            addNode(node, new MeasureNodeStats(node, verbose, continuous));
        }

        void addNode(Node node) { addNode(node,false); }
        
        float getMeasuredNTPC(Node node) {
            MeasureNodeStats mns = getNode(node);
            if(mns != null){
                float ntpc_value= mns.getMeasuredNTPC();
                mns.clearMeasuredNTPC();
                return ntpc_value;
            }
            else {
                System.out.println("Measure node "+node +" node found");
                return Float.NaN;
            }
        }
        
        public void clear() {
            verbose = false;
            super.clear();
        }

        void restart() {
            for (Map.Entry<Node,MeasureNodeStats> entry : this) {
                entry.getValue().restart();
            }
        }

        String printStats() {
            StringBuilder s = new StringBuilder();
            for (Map.Entry<Node,MeasureNodeStats> entry : this) {
                s.append(entry.getValue().printStats());
            }
            return s.toString();
        }

        void setVerbose(boolean v) {
            verbose = v;
            for (Map.Entry<Node,MeasureNodeStats> entry : this) {
                entry.getValue().setVerbose(verbose);
            }
        }
    }

    /** Adds a node to the measure list **/
    public boolean addMeasure(String node, boolean continuous) {
        if (measure == null) {
            measure = new MeasureNodeWatcher(false);
        }
        Node n = dsim.findNode(node);
        if (n == null) {
            System.out.println("Node '"+node+"' doesn't exist.");
            return false;
        }
        else {
            measure.addNode(n,continuous);
            return true;
        }
    }

    public boolean addMeasure(String node) {
        return addMeasure(node,false);
    }

    /** Retrive NTPC for node we measured ***/
    public float getMeasure(String node) {
        if (measure == null) { return Float.NaN; }
        Node n = dsim.findNode(node);
        if (n == null) {
            System.out.println("Node '"+node+"' doesn't exist.");
            return Float.NaN;
        }
        else {
            return measure.getMeasuredNTPC(n);
        }
    }

    /** To be called whenever the user ^C's (ideally) **/
    public String printMeasureStats() {
        return printMeasureStats(true);
    }

    public String printMeasureStats(boolean resetStats) {
        String s =null;
        if (measure == null) return s;
        s = measure.printStats();
        if (resetStats) measure.restart();
        return s;
    }

    /** Restart new ntpc calculations **/
    public void restartStats () {
        measure.restart();
    }

    /** Sets measure output to be verbose **/
    public void setMeasureVerbosity(boolean verbose) {
        if (measure != null) measure.setVerbose(verbose);
    }

    /** Clears all measures **/
    public void clearMeasures() {
        //avoid throwing a null pointer exception
        if(measure != null)
            measure.clear();
    }
    
    public void haltOn(Node node, BinaryPredicate p, int tpc) {
        if (haltWatcher == null) {
            haltWatcher = new CombinedNodeWatcher<HaltOnCycleWatcher>();
        }
        haltWatcher.addNode(node, new HaltOnCycleWatcher(node, p, tpc));
    }

    public void haltOff(final Node node) {
        if (haltWatcher != null) haltWatcher.deleteNode(node);
    }

    /** 
     * Prints the set of nodes to System.out in a compact manner, nicely 
     * formatting to an 80 column line width.
     **/
    void printNodeSet(Set s) {
        int col = 3;
        Iterator it = s.iterator();
        System.out.print("  ");
        while (it.hasNext()) {
            String name = ((Node)it.next()).getName().getAsString('.');
            col += name.length() + 1;
            if (col > 80) {
                System.out.print("\n  ");
                col = 4 + name.length();
            }
            System.out.print(name + " ");
        }
        System.out.println("");
    }
}
