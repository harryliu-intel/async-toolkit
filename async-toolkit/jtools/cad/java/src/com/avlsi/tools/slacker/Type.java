package com.avlsi.tools.slacker;

import java.io.*;
import java.util.*;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.ext.Exec;
import com.avlsi.util.functions.UnaryPredicate;
import java.util.List;
import java.util.ArrayList;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Type implements Comparable {
    /** Additional cost to add a buffer on a dont_touch channel */
    private static final double DONT_TOUCH_WEIGHT = 1e4;

    /** Type of cell **/
    final String name;

    /** Is this a slacker_leaf=true cell? **/
    final boolean isLeaf;

    /** Suppress recursively processing the same Type more than once **/
    boolean visited = false;

    /** Channels */
    final MultiSet channels = new MultiSet();

    /** Subcells */
    final MultiSet subcells = new MultiSet();

    /** Input ports */
    final List inPorts = new ArrayList(); // of Channel

    /** Output ports */
    final List outPorts = new ArrayList(); // of Channel

    /** Index of time offset for the last instance of this Type. */
    int timeIndex = -1;

    /** Recort the worst antiSlack */
    double maxAntiSlack = 0;

    /** Target cycleSlack */
    static double cycleSlack = 9;
    
    /** Free slack on all channels */
    static double globalFreeSlack = 0;

    /** Hierarchy weight */
    static double hierWeight = 1.01;

    /** Cost of "free" slack */
    static double costFreeSlack = 0.01;

    /** Allow non-integer numbuf variables? */
    static boolean allowNonInteger = false;

    /** Print channels with no buffers too */
    static boolean reportZeroBuffers = false;
    
    /** Print slacker_time's on subcells (NOTE: inaccurate if multiple instances) */
    static boolean reportSubcellTimes = false;
    
    /** Emit leaf info in slackResults */
    static boolean emitLeafResults = false;

    /** Check only? */
    static boolean checkOnly = false;

    /** Allow any antislack? */
    static boolean anyAntiSlack = false;

    /** Should we allow antislack everywhere? */
    static boolean allAntiSlack = false;

    /** Was there a non-zero slack on a channel without nonzeroSlack? */
    static boolean needsAnotherPass = false;

    /** Count number of integer indepenent variables. */
    static int numIntegerVariables = 0;
    
    /** Minimum latency per slack buffer */
    static double latencyPerBuffer = 1;
    
    /** Cycle-slack for internal handshake of slack buffers*/
    static double bufferCycleSlack = 5;

    /** Predicate to determine which instance_time to report */
    static UnaryPredicate reportInstanceTimes = null;

    /** Accumulate any instance_times to report */
    static Map<String,Double> instanceTimes = new TreeMap<String,Double>();

    /** Constructor */
    public Type(String name, boolean isLeaf) {
        this.name = name;
        this.isLeaf = isLeaf;
    }

    /** Compare Types by name */
    public int compareTo (Object b) {
        return name.compareTo(((Type) b).name);
    }

    /** Debugging printout */
    public void print() {
        System.out.println("(type " + name);
        System.out.println(" (inputs");
        for (int i=0; i<inPorts.size(); i++)
            System.out.println("  " + ((Channel) inPorts.get(i)));
        System.out.println(" )");
        System.out.println(" (outputs");
        for (int i=0; i<outPorts.size(); i++)
            System.out.println("  " + ((Channel) outPorts.get(i)));
        System.out.println(" )");
        System.out.println(" (channels");
        for (Iterator i=channels.iterator(); i.hasNext(); )
            System.out.println("  " + (Channel) i.next());
        System.out.println(" )");
        System.out.println(" (subcells");
        for (Iterator i=subcells.iterator(); i.hasNext(); )
            System.out.println("  " + (Subcell) i.next());
        System.out.println(" )");
        System.out.println(")");
    }

    /** Add or create a Channel */
    public Channel addChannel(Channel chan) {
        Channel canon = (Channel) channels.find(chan);
        if (canon!=null) return canon;
        channels.add(chan);
        return chan;
    }
    
    /** Add an input port */
    public Channel addInPort(Channel chan) {
        chan = addChannel(chan);
        inPorts.add(chan);
        return chan;
    }
    
    /** Add an output port */
    public Channel addOutPort(Channel chan) {
        chan = addChannel(chan);
        outPorts.add(chan);
        return chan;
    }
    
    /** Add a Subcell */
    public void addSubcell(Subcell cell) {
        subcells.add(cell);
    }

    /** Should we allow anti-slack on this channel? */
    private boolean allowAntiSlack(Channel chan) {
        return anyAntiSlack && (chan.allowAntiSlack() || allAntiSlack);
    }

    /** write problem.in to define energy, constraint, variables **/
    private int writeLPProblem(BufferedWriter bw)
        throws IOException {
        
        // recursively formulate problem
        MultiSet objectiveTerms = new MultiSet();
        List constraints = new ArrayList();
        List intVars = new ArrayList();
        List bounds = new ArrayList();
        int N = recursiveExport(objectiveTerms,constraints,bounds,intVars,0,1);
        
        // objective function
        bw.write("minimize\n");
        for (Iterator i = objectiveTerms.iterator(); i.hasNext(); ) {
            ObjectiveTerm term = (ObjectiveTerm) i.next();
            if(term.cost<0) 
                bw.write("  - " + (-term.cost) + " x" + term.index + "\n");
            else
                bw.write("  + " + term.cost + " x" + term.index + "\n");
        }

        // constraints
        bw.write("subject to\n");
        for (Iterator i = constraints.iterator(); i.hasNext(); ) {
            String str = (String) i.next();
            bw.write(str);
        }

        // bounds
        if (bounds.size()>0) {
            bw.write("bounds\n");
            for (Iterator i = bounds.iterator(); i.hasNext(); ) {
                String str = (String) i.next();
                bw.write(str);
            }
        }
        
        // integer variables
        numIntegerVariables = intVars.size();
        if (intVars.size()>0) {
            bw.write("integer\n");
            for (Iterator i = intVars.iterator(); i.hasNext(); ) {
                String str = (String) i.next();
                bw.write(str);
            }
        }

        // end
        bw.write("end\n");
        bw.close();
        if ((constraints.size()==0) || (objectiveTerms.size()==0)) return 0;
        return N;
    }

    /** Read results of linear programming solution */
    private double [] readLPResults(BufferedReader br, int N)
        throws IOException,InterruptedException {
        boolean ok = false;
        String line;
        double [] results = new double[N];
        while ((line=br.readLine()) != null) {
            String[] strs = line.trim().split("[\\s]+");
            if ((strs.length>=2) &&
                strs[0].equals("Status:") && strs[strs.length-1].equals("OPTIMAL"))
                ok = true;
            else if ((strs.length>=4) && strs[1].startsWith("x")) {
                int index = Integer.parseInt(strs[1].substring(1));
                try {
                    results[index] = Double.parseDouble(strs[2]);
                }
                catch (NumberFormatException e) {
                    results[index] = Double.parseDouble(strs[3]);
                }
            }
        }
        br.close();
        if (!ok) return null;
        return results;
    }
    
    /** Slack match using linear programming */
    public void slackMatch(String execSolve, String runDirectory) {
        boolean ok = false;
        if (isLeaf) return;
        maxAntiSlack = 0;
        needsAnotherPass = true;
        for (int pass = 1; needsAnotherPass; pass++) {
            needsAnotherPass = false;
            try {
                // write solve.in
                File fw = new File(runDirectory, "solve.in");

                BufferedWriter bw = new BufferedWriter(new FileWriter(fw));
                int N = writeLPProblem(bw);
                if (N==0) {
                    System.err.println("ERROR: empty MILP problem\n" +
                      "  There are no internal channels to add slack to!");
                    System.exit(1);
                }

                // execute solve
                OutputStream solve_out =
                    new FileOutputStream(runDirectory + "/solve.out");
                System.err.println("NOTE: GLPSOL  started  at " +
                                   new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));
                String command = execSolve + " -o solution.out solve.in";
                Exec.exec(command.split("[\\s]+"),
                          null, new File(runDirectory),
                          null, solve_out, System.err);
                solve_out.close();
                System.err.println("NOTE: GLPSOL  finished at " +
                                   new SimpleDateFormat("HH:mm:ss MM/dd/yyyy").format(new Date()));

                // read solution.out
                File fr = new File(runDirectory, "solution.out");
                BufferedReader br = new BufferedReader(new FileReader(fr));
                double [] results = readLPResults(br,N);
                if ((results==null) && checkOnly) {
                    System.err.println("ERROR: " + name + " not slack matched ");
                    System.exit(1);
                }
                else if ((results==null) && anyAntiSlack && allAntiSlack) {
                    System.err.println("ERROR: no feasible solution for " + name);
                    System.exit(1);
                }
                else if ((results==null) && !anyAntiSlack) {
                    anyAntiSlack     = true;
                    needsAnotherPass = true;
                }
                else if ((results==null) && !allAntiSlack) {
                    allAntiSlack     = true;
                    needsAnotherPass = true;
                }
                else {
                    int importN = recursiveImport(results,0,null);
                    Debug.assertTrue(N==importN);
                }
                System.out.println("Pass " + pass + ", " +
                                   N +" independent variables, " +
                                   numIntegerVariables + " integer variables" +
                                   (anyAntiSlack ? ", requires extra anti-slack" : ""));
            } catch (Exception e) {
                System.err.println("ERROR: can't read results: " + e);
                System.exit(1);
            }
        }
        System.out.println("");
    }
    
    /**
     * A class to recombine narrow channel directives into wide channel
     * directives.
     **/
    private class OutputDirective {
        /** A collection of all directives. **/
        private final Map<Pair<String,Channel>,String> dirs;

        /** A map from parent to children channels. **/
        private final MultiMap<String,Channel> children;

        OutputDirective() {
            dirs = new LinkedHashMap<Pair<String,Channel>,String>();
            children = new MultiMap<String,Channel>(
                    new HashMap<String,Collection<Channel>>(),
                    MultiMap.<Channel>arrayListFactory());
        }

        void init(final String dir, final Channel chan) {
            if (chan.parent != null) children.put(chan.parent, chan);
        }

        void add(final String dir, final Channel chan, final int value) {
            add(dir, chan, String.valueOf(value));
        }

        void add(final String dir, final Channel chan, final double value) {
            add(dir, chan, String.valueOf(value));
        }

        void add(final String dir, final Channel chan, final boolean value) {
            add(dir, chan, String.valueOf(value));
        }

        void add(final String dir, final Channel chan, final String value) {
            dirs.put(new Pair<String,Channel>(dir, chan), value);
        }

        private String get(final String dir, final Channel chan) {
            return dirs.get(new Pair<String,Channel>(dir, chan));
        }

        void write() {
            // which wide channel directive has been processed already
            final Map<Pair<String,String>,Boolean> doneWide =
                new HashMap<Pair<String,String>,Boolean>();

            for (Map.Entry<Pair<String,Channel>,String> entry :
                    dirs.entrySet()) {
                final Pair<String,Channel> p = entry.getKey();
                final String dir = p.getFirst();
                final Channel chan = p.getSecond();
                if (chan.parent != null) {
                    final Pair<String,String> key =
                        new Pair<String,String>(dir, chan.parent);
                    Boolean matched = doneWide.get(key);
                    if (matched == null) {
                        matched = Boolean.TRUE;
                        boolean first = true;
                        String last = null;
                        for (Channel child : children.get(chan.parent)) {
                            final String curr = get(dir, child);
                            if (first) {
                                first = false;
                                last = curr;
                            } else {
                                if (!ObjectUtils.equals(curr, last)) {
                                    matched = Boolean.FALSE;
                                    break;
                                }
                            }
                        }
                        doneWide.put(key, matched);
                        if (matched && last != null) {
                            // emit the wide directive
                            write(dir, chan.parent, last);
                        }
                    }
                    if (matched) {
                        // don't emit narrow directive
                        continue;
                    }
                }
                write(dir, chan.name, entry.getValue());
            }
        }

        private void write(final String dir, final String key,
                           final String val) {
            System.out.println("    " + dir + "(" + key + ") = " + val + ";");
        }
    }

    /** Report results of slackMatch */
    public void slackResults(boolean topType) {
        double minInputTime=0,maxOutputTime=0;
        boolean minInputValid=false,maxOutputValid=false;
        if (!topType && isLeaf && !emitLeafResults) return;
        final OutputDirective outputDir = new OutputDirective();
        System.out.println("Cell: " + name);

        for (Iterator i=channels.iterator(); i.hasNext(); ) {
            Channel chan = (Channel) i.next();
            for (String dir : new String[] { "num_buffers",
                                             "slacker_initial_tokens",
                                             "slacker_ignore",
                                             "slacker_time",
                                             "slacker_dont_touch" }) {
                outputDir.init(dir, chan);
            }
        }

        // all types have valid dynamic_slack or initial_tokens information
        for (Iterator i=channels.iterator(); i.hasNext(); ) {
            Channel chan = (Channel) i.next();
            if (chan.ignore) continue;
            boolean unaligned_port = (chan.srcTimeIndex<0) || (chan.dstTimeIndex<0);
            if (chan.numbuf>0 || reportZeroBuffers && !unaligned_port)
                outputDir.add("num_buffers", chan, chan.numbuf);
            if (chan.initialTokens!=0 && unaligned_port)
                outputDir.add("slacker_initial_tokens", chan,
                              chan.initialTokens);
        }

        // only topType or leaf cells have accurate slacker_time information
        if (topType || isLeaf || reportSubcellTimes) {
            for (int i=0; i<inPorts.size(); i++) {
                Channel chan = (Channel) inPorts.get(i);
                if (chan.ignore) {
                    outputDir.add("slacker_ignore", chan, chan.ignore);
                } else {
                    double time = (isLeaf ? chan.dstTimeOffset :
                                   (chan.srcTimeIndex>=0 ? chan.srcTime 
                                                         : chan.dstTime));
                    if (!minInputValid || (time<minInputTime)) {
                        minInputValid = true;
                        minInputTime  = time;
                    }
                    outputDir.add("slacker_time", chan, time);
                }
                if (chan.dontTouch) {
                    outputDir.add("slacker_dont_touch", chan, chan.dontTouch);
                }
            }
            for (int i=0; i<outPorts.size(); i++) {
                Channel chan = (Channel) outPorts.get(i);
                if (chan.ignore) {
                    outputDir.add("slacker_ignore", chan, chan.ignore);
                } else {
                    double time = (isLeaf ? chan.srcTimeOffset :
                                   (chan.dstTimeIndex>=0 ? chan.dstTime
                                                         : chan.srcTime));
                    if (!maxOutputValid || (time>maxOutputTime)) {
                        maxOutputValid = true;
                        maxOutputTime  = time;
                    }
                    outputDir.add("slacker_time", chan, time);
                }
                if (chan.dontTouch) {
                    outputDir.add("slacker_dont_touch", chan, chan.dontTouch);
                }
            }
            if (minInputValid && maxOutputValid)
                System.out.println("    // slack depth = " +
                                   (maxOutputTime - minInputTime));
            if (maxAntiSlack>0)
                System.out.println("    // maximum anti-slack = " +
                                   maxAntiSlack);
        }
        if (topType) {
            for (Map.Entry<String,Double> time : instanceTimes.entrySet()) {
                System.out.println("    // instance_time(" + time.getKey() +
                                   ") = " + time.getValue());
            }
        }
        outputDir.write();
        System.out.println();
    }

    private String append(final String prefix, final String last) {
        return prefix == null ? last : (prefix + "/" + last);
    }

    private void reportInstanceTime(final String instance, final double time) {
        if (instance != null && reportInstanceTimes != null &&
            reportInstanceTimes.evaluate(name)) {
            instanceTimes.put(instance, time);
        }
    }

    /** initialize totalFreeSlack of all channels in this Type */
    private void initializeTotalFreeSlack() {
        if (visited) return;
        for (Iterator i = channels.iterator(); i.hasNext(); ) {
            Channel chan = (Channel) i.next();
            chan.totalFreeSlack = chan.extraFreeSlack + 
                globalFreeSlack * chan.handshakes;
        }
    }

    /**
     * Propagate information from Subcell ports back to parent's
     * Channels.  Also create new time variables for ports with group
     * alignment.
     */
    private void attachChannelsToSubcell(Subcell cell) {
        for (int j=0; j<cell.inPorts.size() && j<cell.type.inPorts.size(); j++) {
            Channel chan = (Channel) cell.inPorts.get(j);
            Channel port = (Channel) cell.type.inPorts.get(j);
            if (port.alignment>=0) { // aligned port
                chan.dstTimeIndex  = port.srcTimeIndex;
                chan.dstTimeOffset = port.srcTimeOffset;
            }
            else { // collapse info for unaligned port
                chan.dstTimeIndex  = port.dstTimeIndex;
                chan.dstTimeOffset = port.dstTimeOffset;
                if (!visited) {
                    chan.totalFreeSlack += port.totalFreeSlack;
                    chan.initialTokens  += port.initialTokens;
                }
            }
            if (port.ignore) chan.ignore = true;
            if (port.dontTouch) chan.dontTouch = true;
        }
        for (int j=0; j<cell.outPorts.size() && j<cell.type.outPorts.size(); j++) {
            Channel chan = (Channel) cell.outPorts.get(j);
            Channel port = (Channel) cell.type.outPorts.get(j);
            if (port.alignment>=0) { // aligned port
                chan.srcTimeIndex  = port.dstTimeIndex;
                chan.srcTimeOffset = port.dstTimeOffset;
            }
            else { // collapse info for unaligned port
                chan.srcTimeIndex  = port.srcTimeIndex;
                chan.srcTimeOffset = port.srcTimeOffset;
                if (!visited) {
                    chan.totalFreeSlack += port.totalFreeSlack;
                    chan.initialTokens  += port.initialTokens;
                }
            }
            if (port.ignore) chan.ignore = true;
            if (port.dontTouch) chan.dontTouch = true;
        }
    }

    /** Assign src/dst Time Index/Offset for Ports of this Type */
    private int attachPortTimes(int baseIndex) {
        // handle subcell, aligned port time indices
        int newBaseIndex = baseIndex;
        for (int j=0; j<inPorts.size(); j++) {
            Channel port = (Channel) inPorts.get(j);
            if (isLeaf) port.dstTimeIndex = timeIndex;
            if (port.alignment>=0) {
                port.srcTimeIndex = baseIndex+port.alignment;
                if (port.srcTimeIndex+1>newBaseIndex)
                    newBaseIndex = port.srcTimeIndex+1;
            } else {
                port.srcTimeIndex = -1;
                port.srcTimeOffset = 0;
            }
        }
        for (int j=0; j<outPorts.size(); j++) {
            Channel port = (Channel) outPorts.get(j);
            if (isLeaf) port.srcTimeIndex = timeIndex;
            if (port.alignment>=0) {
                port.dstTimeIndex = baseIndex+port.alignment;
                if (port.dstTimeIndex+1>newBaseIndex)
                    newBaseIndex = port.dstTimeIndex+1;
            } else {
                port.dstTimeIndex = -1;
                port.dstTimeOffset = 0;
            }
        }
        baseIndex = newBaseIndex;

        // handle passthru ports by adding internal time index
        for (int j=0; j<inPorts.size(); j++) {
            Channel port = (Channel) inPorts.get(j);
            if (port.passthru>=0) {
                port.dstTimeIndex = baseIndex+port.passthru;
                port.dstTimeOffset = 0;
                if (port.dstTimeIndex+1>newBaseIndex)
                    newBaseIndex = port.dstTimeIndex+1;
            }
        }
        for (int j=0; j<outPorts.size(); j++) {
            Channel port = (Channel) outPorts.get(j);
            if (port.passthru>=0) {
                port.srcTimeIndex = baseIndex+port.passthru;
                port.srcTimeOffset = 0;
                if (port.srcTimeIndex+1>newBaseIndex)
                    newBaseIndex = port.srcTimeIndex+1;
            }
        }
        return newBaseIndex;
    }

    /**
     * Recursively formulate and export the objective function and
     * constraints in two lists of Strings.  Return accumulated number
     * of independent variables.
     */
    private int recursiveExport(MultiSet objectiveTerms, List constraints,
                                List bounds, List intVars, int baseIndex,
                                double weight)
        throws IOException {

        // pick timeIndex for leaf cells
        if (isLeaf) timeIndex = baseIndex++;

        // recursively process subcells, attach subcell information to channels
        initializeTotalFreeSlack();
        for (Iterator i = subcells.iterator(); i.hasNext(); ) {
            Subcell cell = (Subcell) i.next();
            baseIndex = cell.type.recursiveExport(objectiveTerms,constraints,
                                                  bounds,intVars,baseIndex,
                                                  weight*hierWeight);
            attachChannelsToSubcell(cell);
        }
        baseIndex = attachPortTimes(baseIndex);
        
        // process channels in this Type
        for (Iterator i = channels.iterator(); i.hasNext(); ) {
            Channel chan = (Channel) i.next();

            // skip channel if ignored or unaligned port
            if (chan.ignore) continue;
            if ((chan.srcTimeIndex<0) || (chan.dstTimeIndex<0)) continue;

            // channel properties
            boolean dontTouch = chan.dontTouch && !allAntiSlack || checkOnly;
            double cweight = weight * (chan.dontTouch ? DONT_TOUCH_WEIGHT : 1);

            // contribution to objective function
            if ((chan.numbufIndex<0) || (chan.numbufIndex==baseIndex)) { // per Type
                chan.numbufIndex = baseIndex++;
                if (chan.nonzeroSlack) intVars.add("  x" + chan.numbufIndex + "\n"); 
            }
            if (!dontTouch)
                ObjectiveTerm.addObjectiveTerm(chan.numbufIndex,
                                               cweight*chan.cost,objectiveTerms);

            // create slack variable
            chan.slackIndex = baseIndex++;

            // minimize freeSlack = cost * (slack / maxLatency - num_buffers)
            double maxLatency = latencyPerBuffer + (cycleSlack - bufferCycleSlack)/2;
            double cost = costFreeSlack*weight*chan.cost;
            ObjectiveTerm.addObjectiveTerm(chan.slackIndex,
                                           cost/maxLatency,objectiveTerms);
            if (!dontTouch)
                ObjectiveTerm.addObjectiveTerm(chan.numbufIndex,
                                               -cost,objectiveTerms);

            // minimize anti-slack
            chan.antiSlackIndex = baseIndex++;
            if (allowAntiSlack(chan))
                ObjectiveTerm.addObjectiveTerm(chan.antiSlackIndex,
                                               weight*chan.cost*1e4,objectiveTerms);

            // constrain channel time difference
            // slack - antiSlack = (dstTime+dstTimeOffset) -
            //                     (srcTime+srcTimeOffset+latency) +
            //                     initialTokens*cycleSlack
            constraints.add("  x" + chan.slackIndex +
                            (allowAntiSlack(chan) ? " - x" + chan.antiSlackIndex : "") +
                            (chan.dstTimeIndex==chan.srcTimeIndex ? "" :
                             " - x" + chan.dstTimeIndex + " + x" + chan.srcTimeIndex) + 
                            " = " +
                            (chan.dstTimeOffset - chan.srcTimeOffset - chan.latency +
                             chan.initialTokens*cycleSlack) +
                            "\n");

            // constrain minimum channel slack
            // slack >= latencyPerBuffer * numbuf
            constraints.add("  x" + chan.slackIndex + 
                            (dontTouch ? "" : " " + (-latencyPerBuffer) +
                             " x" + chan.numbufIndex) +
                            " >= 0\n");

            // constrain maximum channel slack
            // slack <= maxLatency * numbuf + freeSlack
            constraints.add("  x" + chan.slackIndex +
                            (dontTouch ? "": " " + (-maxLatency) +
                             " x" + chan.numbufIndex) +
                            " <= " + chan.totalFreeSlack + "\n");
        }
        visited = true; // no more per-Type processing will be done
        return baseIndex;
    }

    /**
     * Recursively process Types to import slack results.
     */    
    private int recursiveImport(double [] results, int baseIndex,
                                String instance) {

        // pick timeIndex for leaf cells
        if (isLeaf) {
            timeIndex = baseIndex++;
            reportInstanceTime(instance,results[timeIndex]);
        }

        // recursively process subcells
        for (Iterator i = subcells.iterator(); i.hasNext(); ) {
            Subcell cell = (Subcell) i.next();
            baseIndex = cell.type.recursiveImport(results,baseIndex,
                    append(instance, cell.name));
            attachChannelsToSubcell(cell);
        }
        baseIndex = attachPortTimes(baseIndex);

        // process channels in this Type
        for (Iterator i = channels.iterator(); i.hasNext(); ) {
            Channel chan = (Channel) i.next();

            // update channel times for debugging
            chan.dstTime = chan.dstTimeOffset;
            chan.srcTime = chan.srcTimeOffset;
            if (chan.dstTimeIndex>=0) chan.dstTime += results[chan.dstTimeIndex];
            if (chan.srcTimeIndex>=0) chan.srcTime += results[chan.srcTimeIndex];

            // skip channel if ignored or unaligned port
            if (chan.ignore) continue;
            if ((chan.srcTimeIndex<0) || (chan.dstTimeIndex<0)) continue;

            // import type-based variable numbuf
            if (chan.numbufIndex==baseIndex) baseIndex++; // per Type
            chan.numbuf = results[chan.numbufIndex];
            if (!chan.nonzeroSlack && !allowNonInteger && (chan.numbuf>0)) {
                chan.nonzeroSlack = true;
                needsAnotherPass  = true;
            }
            if (chan.dontTouch && chan.numbuf > 0) {
                System.out.println("WARNING: num_buffers(" + name + "/" +
                                   chan.name + ") = " + chan.numbuf +
                                   " on slacker_dont_touch channel");
            }

            // import instance-based variables slack, antiSlack, freeSlack
            chan.slackIndex = baseIndex++;
            chan.slack = results[chan.slackIndex];
            chan.antiSlackIndex = baseIndex++;
            if (allowAntiSlack(chan)) {
                chan.antiSlack = results[chan.antiSlackIndex];
                if (chan.antiSlack>0) {
                    if (chan.antiSlack>maxAntiSlack) maxAntiSlack=chan.antiSlack;
                    System.out.println("WARNING: anti_slack(" + name + "/" + chan.name +
                                       ") = " + chan.antiSlack);
                }
            }
        }
        return baseIndex;
    }
}
