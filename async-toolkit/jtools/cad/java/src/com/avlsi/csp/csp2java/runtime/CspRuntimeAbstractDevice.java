/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import java.io.File;
import java.io.FileInputStream;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.TreeMap;

import com.avlsi.csp.grammar.MemParser;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.WakeAt;
import com.avlsi.tools.sigscan.LoggedString;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.tsim.Arbiter;
import com.avlsi.tools.tsim.Arbiter.Alternative;
import com.avlsi.tools.tsim.Arbiter.Term;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.BufferedNodeChannel;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.NodeReadChannel;
import com.avlsi.tools.tsim.NodeWriteChannel;
import com.avlsi.tools.tsim.EmptyWaitSetException;
import com.avlsi.tools.tsim.Message;
import com.avlsi.tools.tsim.Statusable;
import com.avlsi.tools.tsim.Wait;
import com.avlsi.tools.tsim.Waitable;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.math.BigIntegerUtil;
import com.avlsi.util.text.NaturalStringComparator;

/**
 * Subclass of AbstractDevice which handles arrays of channels in the 
 * csp2java runtime.
 *
 * @author David Hilvert
 * @version $Name:  $ $Date$
 **/

public abstract class CspRuntimeAbstractDevice extends AbstractDevice {

    /** Array of input channels **/
    protected final ChannelInput[] in;

    /** Array of output channels **/
    protected final ChannelOutput[] out;

    /** Array of wide nodes */
    protected final WideNode[] wideNodes;

    /** Nodes to monitor for change when waiting for an event */
    protected final Node[] nodes;

    /** The arbitration mode to use for Arbiters. **/
    protected final int arbitrationMode;

    /** Map&lt;String, Arbiter&gt; from arbiter name to
     * <code>Arbiter</code>, used by nondeterministic selection statement
     * code.
     **/
    protected final HashMap<String,Arbiter> arbiters =
        new HashMap<String,Arbiter>();

    /** Handy random number generator. */
    private Random rand = new Random(1);

    /** Random number generator for use by arbiters. */
    protected final Random arbiterRandom = new Random(1);

    /** String indicating current location, for debugging */
    public volatile String whereAmI;
    
    /** Used to log whereAmI to SST **/
    protected LoggedString loggedWhereAmI;

    /** String indicating current channel operation, if any, for debugging */
    public volatile String currChanOp;

    /** Node transition responsible for waking me up **/
    private Node enablingNode;

    /** Number of times yield() has been called **/
    private volatile int yieldCount = 0;

    /** Maximum supported base conversion */
    private static final CspInteger MAX_BASE =
        new CspInteger(new BigInteger("36"));

    /** A scale factor to apply to digital delays **/
    private float digitalTau = 1.0f;

    /** Accumulated energy in fJ **/
    private CspInteger energy = CspInteger.ZERO;

    /**
     * A synchronized <code>List</code> of
     * <code>CspRuntimeAbstractDevice</code> created since the last reset.
     **/
    private static final List<CspRuntimeAbstractDevice> cspDevices =
        Collections.synchronizedList(
                new ArrayList<CspRuntimeAbstractDevice>());

    /**
     * The stack of the CSP program
     **/
    private final LinkedList<StackFrame> frames = new LinkedList<StackFrame>();

    /**
     * Used to detect if a channel is sent or received on multiple times in one
     * loop.
     **/
    private MultiMap<Statusable,String> usedChannels = null;


    /**
     * Class constructor.
     **/
    protected CspRuntimeAbstractDevice(String name,
            ChannelInput[] in,
            ChannelOutput[] out,
            WideNode[] wideNodes,
            boolean suppressOutput,
            int arbitrationMode) {
        super(name, suppressOutput);
        this.in = in;
        this.out = out;
        this.wideNodes = wideNodes;
        final HashSet/*<Node>*/ uniqNodes = new HashSet/*<Node>*/();
        for (int i = 0; i < wideNodes.length; ++i) {
            final Node[] components = wideNodes[i].getNodes();
            for (int j = 0; j < components.length; ++j) {
                uniqNodes.add(components[j]);
            }
        }
        this.nodes = (Node[]) uniqNodes.toArray(new Node[0]);
        this.arbitrationMode = arbitrationMode;

        for (int i = 0; i < in.length; ++i) setOwner(in[i]);
        for (int i = 0; i < out.length; ++i) setOwner(out[i]);

        cspDevices.add(this);
    }

    private void setOwner(final ChannelInput in) {
        if (in instanceof NodeReadChannel) {
            final ChannelInput wrapped = ((NodeReadChannel) in).unwrap();
            if (wrapped instanceof BufferedNodeChannel) {
                ((BufferedNodeChannel) wrapped).setOwner(this);
            }
        }
    }

    private void setOwner(final ChannelOutput out) {
        if (out instanceof NodeWriteChannel) {
            final ChannelOutput wrapped = ((NodeWriteChannel) out).unwrap();
            if (wrapped instanceof BufferedNodeChannel) {
                ((BufferedNodeChannel) wrapped).setOwner(this);
            }
        }
    }

    private void setChanOp(final Statusable chan, final String op) {
        currChanOp = chan.getName() + op;
    }

    private void resetChanOp() {
        currChanOp = null;
    }

    protected Term[] getTerm(final String deviceName, final String term,
                             final boolean isNegated) {
        return getTerms(deviceName, new String[] { term }, isNegated);
    }

    protected Term[] getTerms(final String deviceName, final String[] terms,
                              final boolean isNegated) {
        final Term[] result = new Term[terms.length];
        final DSim dsim = DSim.get();
        for (int i = 0; i < terms.length; ++i) {
            result[i] =
                new Term(dsim.findOrAddNode(deviceName + "." + terms[i]),
                         isNegated);
        }
        return result;
    }

    protected Term[][] flattenTerms(final Term[][][] terms) {
        final Collection result = new ArrayList();
        for (int i = 0; i < terms.length; ++i) {
            for (int j = 0; j < terms[i].length; ++j) {
                result.add(terms[i][j]);
            }
        }
        final Term[][] array = (Term[][]) result.toArray(new Term[0][0]);
        return array;
    }

    protected Term[] flattenTerms(final Term[][] terms) {
        final Collection result = new ArrayList();
        for (int i = 0; i < terms.length; ++i) {
            for (int j = 0; j < terms[i].length; ++j) {
                result.add(terms[i][j]);
            }
        }
        final Term[] array = (Term[]) result.toArray(new Term[0]);
        return array;
    }

    /**
     * Display a BigInteger message to stdout.
     *
     * @param message Message (of type BigInteger) to send.
     * @return CSP boolean true.
     **/
    protected CspInteger _print(CspInteger message) {
        System.out.println(getTime() + ": " + getFullname() +
                           ": 0x" + message.toString(16));
        return CspInteger.valueOf(-1);
    }

    /**
     * Display a string message to stdout.
     *
     * @param message Message (of type String) to send.
     * @return CSP boolean true.
     **/
    protected CspInteger _print(CspString message) {
        System.out.println(getTime() + ": " + getFullname() + ": " + message);
        return CspInteger.valueOf(-1);
    }

    /**
     * Display a tagged BigInteger message to stdout.  
     * 
     * @param tag Tag (of type BigInteger) associated with the message.
     * @param message Message (of type BigInteger) to send.
     * @return CSP boolean true.
     **/
    protected CspInteger _print(CspInteger tag, CspInteger message) {
        System.out.println(getTime() + ": " + getFullname() +
                           ": 0x" +  message.toString(16) + 
                           " (tag=0x" + tag.toString(16) + ")");
        return CspInteger.valueOf(-1);
    }

    /**
     * Display a tagged String message to stdout.  
     * 
     * @param tag Tag (of type BigInteger) associated with the message.
     * @param message Message (of type String) to send.
     * @return CSP boolean true.
     **/
    protected CspInteger _print(CspInteger tag, CspString message) {
        System.out.println(getTime() + ": " + getFullname() + ": " + message + 
                         " (tag=0x" + tag.toString(16) + ")");
        return CspInteger.valueOf(-1);
    }

    /**
     * Waits forever, will never return normally, only by throwing
     * InterruptedException.
     **/
    protected static void waitForever() throws InterruptedException {
        DigitalScheduler.get().waitForever();
    }

    /**
     * Waits until the event queue is empty.
     **/
    protected CspInteger _eventQueueIsEmpty() throws InterruptedException {
        DigitalScheduler.get().waitForEmptyEventQueue();
        updateTime(DigitalScheduler.get().getTime());
        return CspInteger.valueOf(-1);
    }

    /**
     * Sets the DSim warn and error state.  Useful for test environments.
     **/
    protected void _enableDSimErrors(CspInteger enableErrors)
        throws InterruptedException {
        DSim.get().setWarn(enableErrors.booleanValue());
        DSim.get().setError(enableErrors.booleanValue());
    }

    /**
     * Returns a random CspInteger with numBits
     **/
    protected CspInteger _random(CspInteger numBits) {
        return new CspInteger(new BigInteger(numBits.intValue(),rand));
    }

    /**
     * Seed the random number generator.
     **/
    protected CspInteger _srandom(CspInteger seed) {
        rand.setSeed(seed.longValue());
        arbiterRandom.setSeed(rand.nextLong());
        return CspInteger.valueOf(-1);
    }

    /**
     * Picks one of two integers based on a boolean, just like
     * C and Java's ?: operator.  (Except without the short-circuit
     * behavior.
     */
    protected CspInteger _choose(CspInteger b, CspInteger t, CspInteger f)
        throws InterruptedException {
        return b.booleanValue() ? t : f;
    }

    protected CspInteger _log2(CspInteger a) {
        return a.log2();
    }

    protected CspInteger _log4(CspInteger a) {
        return a.log4();
    }
    
    protected CspString _string(CspInteger num, CspInteger baseNum) {
        final int base;
        if (baseNum.compareTo(CspInteger.ONE) <= 0 ||
            baseNum.compareTo(MAX_BASE) > 0) {
            outerr("Invalid base " + baseNum + " at " + whereAmI + ".\n");
            base = 10;
        } else {
            base = baseNum.intValue();
        }
        return new CspString(num.toString(base));
    }

    /** Convert a CspInteger into ASCII string **/
    protected CspString _string(CspInteger num) {
        return new CspString(new String(num.toBigInteger().toByteArray(),
                                        Charset.forName("ISO-8859-1")));
    }

    /**
     * Returns true if all receives can complete on all channels in the
     * array <code>in</code>.  
     **/
    protected boolean probe(final CspChannelInArray1 in) {
        for (int i = in.getMaxIndex(); i >= in.getMinIndex(); i--) {
            final ChannelInput channel = in.get(CspInteger.valueOf(i));
            if (!probeReceive(channel))
                return false;
        }

        return true;
    }

    /**
     * Returns true if all sends can complete on all channels in the
     * array <code>in</code>.  
     **/
    protected boolean probe(final CspChannelOutArray1 out) {
        for (int i = out.getMaxIndex(); i >= out.getMinIndex(); i--) {
            final ChannelOutput channel = out.get(CspInteger.valueOf(i));
            if (!probeSend(channel))
                return false;
        }

        return true;
    }

    protected boolean probe(final ChannelInput in) {
        // AbstractDevice.probeReceive(ChannelInput) passes the current
        // device time to ChannelInput.probeReceive(time)
        return probeReceive(in);
    }

    protected boolean probe(final ChannelOutput out) {
        // AbstractDevice.probeSend(ChannelOutput) passes the current
        // device time to ChannelOutput.probeSend(time)
        return probeSend(out);
    }

    protected void send(final String cellName, final String location,
                        final ChannelOutput out, final BigInteger message)
        throws InterruptedException {
        final BigInteger numValues = out.getNumPossibleValues();
        if (message.compareTo(BigInteger.ZERO) < 0 ||
            message.compareTo(numValues) >= 0) {
            outerr("Warning: implicit non-power-of-2 posmod is deprecated: " +
                   message + " outside [0.." +
                   (numValues.subtract(BigInteger.ONE)) + "] at " + location);
        }
        send(out, message);
    }

    protected void send(final ChannelOutput out, BigInteger message)
        throws InterruptedException {
        final boolean isStatusable = out instanceof Statusable;
        if (isStatusable) {
            final Statusable status = (Statusable) out;
            setChanOp(status, "!" + message);
            if (usedChannels !=null) usedChannels.put(status, whereAmI);
        }
        super.send(out, message);
        if (isStatusable) resetChanOp();
    }

    protected Message receive(ChannelInput in) throws InterruptedException {
        final boolean isStatusable = in instanceof Statusable;
        if (isStatusable) {
            final Statusable status = (Statusable) in;
            setChanOp(status, "?");
            if (usedChannels !=null) usedChannels.put(status, whereAmI);
        }
        final Message m = super.receive(in);
        if (isStatusable) resetChanOp();
        return m;
    }

    protected BigInteger peek(final ChannelInput in)
        throws InterruptedException {
        if (in instanceof Statusable) setChanOp((Statusable) in, "#?");
        in.waitForMessage();
        if (in instanceof Statusable) resetChanOp();
        // AbstractDevice.probeValue(ChannelInput) takes care of updating the time
        return probeValue(in).getValue();
    }

    /**
     * Uses {@link com.avlsi.tools.tsim.Wait#select()} to wait,
     * but logs any EmptyWaitSetExceptions with outerr().
     **/
    public /*@ non_null @*/ Pair/*<Waitable,Long>*/ select2(
            final /*@ non_null @*/ Wait wait)
        throws InterruptedException {
        try {
            return wait.select2();
        } catch (EmptyWaitSetException e) {
            outerr("Empty wait set -- You have a [] whose guards never " +
                    "become true");
            throw e;
        }
    }

    protected void cleanup() {
        for (int i = 0; i < in.length; ++i) {
            in[i].destroy();
            in[i]=null;
        }
        for (int i = 0; i < out.length; ++i) {
            out[i].destroy();
            out[i]=null;
        }
        for (Arbiter arb : arbiters.values()) {
            arb.destroy();
        }
        arbiters.clear();
    }

    /**
     * Wait for the specified number of digital cycles.
     **/
    protected void _wait(final CspInteger delay) throws InterruptedException {
        wait(Math.round(delay.intValue() * digitalTau));
    }

    protected void yield() throws InterruptedException {
        ++yieldCount;
        wait(0);
    }

    public int getYieldCount() {
        return yieldCount;
    }

    public void resetYieldCount() {
        yieldCount = 0;
    }

    public synchronized Node getEnablingNode() {
        return enablingNode;
    }

    public synchronized void setEnablingNode(final Node node) {
        enablingNode = node;
    }

    protected Node[] getNodesWithDifferentValue(boolean on) {
        final Collection result = new ArrayList();
        for (int i = 0; i < nodes.length; ++i) {
            if (nodes[i].getValue() != (on ? Node.VALUE_1 : Node.VALUE_0))
                result.add(nodes[i]);
        }
        return (Node[]) result.toArray(new Node[0]);
    }

    protected CspInteger _stable(CspNode node) throws InterruptedException {
        return CspInteger.valueOf(node.stable() ? -1 : 0);
    }

    protected CspInteger _readHexInts(CspString filename, CspInteger n,
                                      CspArray data) {
        List<BigInteger> ints = new ArrayList<>();
        try (FileInputStream s = new FileInputStream(filename.toString())) {
            MemParser.parseFile(s, filename.toString(), ints);
        } catch (Exception e) {
            outerr("Can't read data from " + filename + " at " + whereAmI +
                   ": " + e.getMessage());
            // if (DSim.get().haltOnError) DSim.get().interrupt();
            return CspInteger.ZERO;
        }
        final int ni = n.getValue().intValueExact();
        final int len = Math.min(ints.size(), ni);
        for (int i = 0, idx = data.getMinIndex(); i < len; ++i, ++idx) {
            CspValue val = data.get(idx);
            val.setValue(new CspInteger(ints.get(i)));
        }
        return CspInteger.valueOf(len);
    }

    /**
     * Return the current device time.
     **/
    protected CspInteger _time() throws InterruptedException {
        return CspInteger.valueOf(getTime());
    }

    /**
     * Accumulate energy.
     *
     * @param val energy in fJ
     **/
    protected void _energy(CspInteger val) {
        energy = energy.add(val);
    }

    /**
     * Reset energy accumulator.
     **/
    public void resetEnergy() {
        energy = CspInteger.ZERO;
    }

    /**
     * Return energy in J.
     **/
    public double getEnergy() {
        return energy.toBigInteger().doubleValue() * 1e-15;
    }

    protected void _dumpOn() {}

    protected void _dumpOff() {}

    protected CspInteger _ord(final CspString s) {
        return new CspInteger(BigIntegerUtil.fromASCII(s.toString()));
    }

    protected CspString _chr(final CspInteger val) {
        return new CspString(
                new String(
                    CollectionUtils.reverse(val.toBigInteger().toByteArray())));
    }

    /**
     * A function to be run while ~_RESET.  By default it does nothing.  It
     * will be overriden if there is a CSP function called resetNodes().
     **/
    protected void resetNodes() throws InterruptedException { }

    protected void waitForReset() throws InterruptedException {
        Node resetNode = DSimUtil.getResetNode();
        if (resetNode == null) {
            synchronized(warnLock) {
                if (!outWarn) {
                    System.err.println("Warning: No reset node was found.");
                    outWarn = true;
                }
            }
            return;
        }

        Wait wait;
        
        wait = new Wait(null, null, null, new Node[]{resetNode});
        wait.select();

        try {
            resetNodes();
        } catch (CspNode.UnstableException e) {
            printUnstableException();
        }

        wait = new Wait(null, null, new Node[]{resetNode}, null);
        wait.select();
        new WakeAt(DSim.get().getTime() + 00, this).sleepTil();

        return;
    }

    protected void printUnstableException() {
        outerr("Reading from an unstable node at " + whereAmI + ".\n" +
               "Use [ stable(node) ] to ensure a node is stable before " +
               "reading from it.");
    }

    public static List<CspRuntimeAbstractDevice> getAllCspDevices() {
        return Collections.unmodifiableList(cspDevices);
    }

    public static void clearAllCspDevices() {
        cspDevices.clear();
    }

    public Collection<ChannelInput> getInputChannels() {
        return Collections.unmodifiableList(Arrays.asList(in));
    }

    public Collection<ChannelOutput> getOutputChannels() {
        return Collections.unmodifiableList(Arrays.asList(out));
    }

    protected void handleError(final String cellName, final String filename,
                               final int line, final int column) {
        outerr("Runtime error detected in " + cellName + " at " +
               filename + ":" + line + ":" + column);
        if (DSim.get().haltOnError) {
            DSim.get().interrupt();
        }
    }

    protected void handleAssert(final String whereAmI, final CspInteger expr,
                                final CspString mesg) {
        if (!expr.booleanValue()) {
            outerr("Assertion failed at " + whereAmI +
                   (mesg == null ? "" : ": " + mesg.toString()));
            if (DSim.get().haltOnError) {
                DSim.get().interrupt();
            }
        }
    }

    protected void setDigitalTau(final float digitalTau) {
        this.digitalTau = digitalTau;
    }

    protected CspRuntimeAbstractDevice getSelf() {
        return this;
    }

    protected String getLoggingScope() {
        StringBuilder buf = new StringBuilder();
        buf.append(getFullname());
        for (StackFrame frame :
                CollectionUtils.iterable(frames.descendingIterator())) {
            String funcName = frame.getFunctionName();
            if (funcName != null) {
                buf.append('.');
                buf.append(funcName);
            }
        }
        return buf.toString();
    }

    protected void enterFrame(final String funcName) {
        frames.addFirst(new StackFrame(funcName, whereAmI));
    }

    protected void leaveFrame() {
        frames.removeFirst();
    }

    public Collection<StackFrame> getFrames() {
        return Collections.unmodifiableList(frames);
    }

    /**
     * Returns the top frame in the stack, or <code>null</code> if the stack is
     * empty.  The stack may be empty if the device has not started the go()
     * method, or if it exited the go() method due to an exception.
     **/
    public StackFrame topFrame() {
        return frames.isEmpty() ? null : frames.getFirst();
    }

    protected CspInteger pack(final Packable structure) {
        final CspInteger packed = new CspInteger();
        structure.pack(packed, 0);
        return packed;
    }

    protected void unpack(final Packable structure, final CspInteger packed) {
        structure.unpack(packed, 0);
    }

    public void setSigscan(final Sigscan sigscan) {
        final Sigscan oldSigscan = getSigscan();
        super.setSigscan(sigscan);
        final Sigscan newSigscan = getSigscan();
        if (oldSigscan != newSigscan && newSigscan != null) {
            initWhereAmI();
        }
    }

    protected void initWhereAmI() {
        loggedWhereAmI = new LoggedString(getFullname(), "whereAmI",
                                          getSigscan(), getDebugOpts(), true);
    }

    protected void setWhereAmI(String whereAmI) {
        this.whereAmI = whereAmI;
        if (loggedWhereAmI != null) loggedWhereAmI.set(whereAmI, time);
    }

    protected void checkAlternative(final List alts,
                                    final CspSnoopingInterface snoop)
    throws InterruptedException {
        int numTrue = 0;
        for (Iterator i = alts.iterator(); i.hasNext(); ) {
            final Alternative alt = (Alternative) i.next();
            if (alt.getPredicate().evaluate()) numTrue++;
        }
        if (numTrue == 0) {
            snoop.onDeadlock("No guards true in non-deterministic selection",
                             whereAmI);
        } else if (numTrue > 1) {
            snoop.onArbitrate(
                "Multiple guards true in non-deterministic selection",
                whereAmI);
        }
    }

    protected void handleBoundsException(final CspArrayBoundsException e,
                                         final CspSnoopingInterface snoop)
    throws InterruptedException {
        if (snoop == null) {
            final String msg =
                "device " + getFullname() + ": " + e.getMessage();
            ExceptionPrettyPrinter.prettyMessage(msg, e.filename, e.line,
                                                 e.column + 1, System.err);
            DSim.get().interrupt();
            waitForever();
        } else {
            snoop.onOutOfBoundsAccess(
                e.getMessage(),
                new File(e.filename).getName() + ' ' + e.line + ':' + e.column);
        }
    }

    protected void arbitrate(final Arbiter arb, final String location)
        throws InterruptedException {
        try {
            arb.arbitrate();
        } catch (EmptyWaitSetException e) {
            final String context = e.getMessage();
            outerr("Empty wait set: waiting for condition " +
                   (context == null ? "" : "(" + context + ") ") +
                   "that can never become true at " + location);
            DSim.get().interrupt();
            waitForever();
        }
    }

    protected CspSnoopingInterface getProteusSnooper() {
        return new ProteusSnooper();
    }

    private class ProteusSnooper implements CspSnoopingInterface {
        private boolean firstLoop = true;

        public void onStart() {
            usedChannels = new MultiMap<Statusable,String>(
                new TreeMap<Statusable,Collection<String>>(
                    new Comparator<Statusable>() {
                        public int compare(Statusable s1, Statusable s2) {
                            return NaturalStringComparator.compareString(
                                s1.getName(), s2.getName());
                        }
                    }),
                MultiMap.<String>arrayListFactory());
        }

        public void onOutermostLoopStart() {
            if (!usedChannels.keySet().isEmpty()) {
                final String err = getErrorString(firstLoop);
                if (firstLoop) {
                    if (err.length() > 0) {
                        outerr("warning (synthesis): channel communication " + 
                               "before main loop: " + err);
                    }
                } else {
                    if (err.length() > 0) {
                        outerr("warning (synthesis): at most 1 send or " +
                               "receive operation allowed per channel per " +
                               "main loop iteration: " + err);
                    }
                }
                usedChannels.clear();
            }
            firstLoop = false;
        }

        public void onEnd() {}

        public void onDeadlock(String message, String whereAmI) {
            outerr("warning (synthesis): deadlock detected at " + whereAmI +
                   ": " + message);
        }

        public void onArbitrate(String message, String whereAmI) {
            outerr("warning (synthesis): arbitration detected at " + whereAmI +
                   ": " + message);
        }

        public void onOutOfBoundsAccess(String message, String whereAmI) {}

        private String getErrorString(boolean initial) {
            final StringBuilder sb = new StringBuilder();
            for (Statusable s : usedChannels.keySet()) {
                final Collection<String> locs = usedChannels.get(s);
                if ((initial && locs.size() > 0) || locs.size() > 1) {
                    sb.append(s.getName());
                    sb.append(s instanceof ChannelInput ? '?' : '!');
                    sb.append(" (");
                    boolean first = true;
                    for (String loc : locs) {
                        if (first) {
                            first = false;
                        } else {
                            sb.append(", ");
                        }
                        sb.append(loc);
                    }
                    sb.append(") ");
                }
            }
            return sb.toString();
        }
    }

    protected CspInteger _walltime() {
        return new CspInteger(BigInteger.valueOf(System.nanoTime()));
    }

    protected CspString _getArgValue(CspString arg, CspString def) {
        return def;
    }

    protected void _cover(final CspInteger expr) {
    }

    protected void _cover(final CspInteger expr, final CspString msg) {
    }
}
