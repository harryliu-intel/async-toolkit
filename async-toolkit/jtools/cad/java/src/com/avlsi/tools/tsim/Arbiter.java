/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import EDU.oswego.cs.dl.util.concurrent.LinkedQueue;

import com.avlsi.file.common.HierName;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
//import com.avlsi.util.functions.NullaryPredicate;

/**
 * <p> Arbitration linked between Java and PRS simulation. </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public final class Arbiter {

    /** Mode constant to indicate that cosimulation is not occuring.  **/
    public static final int NON_LINKED = 0;

    /**
     * Mode constant to indicate that this linked arbiter is either the
     * slave to the prs level arbiter or slave to a behavioral level
     * arbiter with mode <code>LINKED_MASTER</code>.
     **/
    public static final int LINKED_SLAVE = 1;

    /**
     * Mode constant to indicate that this arbiter is a master to another
     * behavioral level arbiter with mode <code>LINKED_SLAVE</code>.
     **/
    public static final int LINKED_MASTER = 2;

    /**
     * Array of <code>Alternative</code>s containing the conditions needing
     * arbitration.  May not be null.
     **/
    private final Alternative[] alts;

    /**
     * Mode constant.  One of {@link #NON_LINKED}, <code>LINKED_SLAVE</code>,
     * or <code>LINKED_MASTER</code>.
     **/
    private final int mode;

    /**
     * Input channels to be waited on if none of the behavioral guard
     * expressions are true.
     **/
    private final ChannelInput[] in;

    /**
     * Output channels to be waited on if none of the behavioral guard
     * expressions are true.
     **/
    private final ChannelOutput[] out;

    /**
     * Rising nodes to be waited on if none of the behavioral guard expressions
     * are true.
     **/
    private final Node[] up;

    /**
     * Falling nodes to be waited on if none of the behavioral guard expressions
     * are true.
     **/
    private final Node[] down;

    /**
     * Random number generator to pick among true alternatives when mode
     * is <code>LINKED_MASTER</code> or <code>NON_LINKED</code>.  Must be
     * non-null when <code>mode == LINKED_MASTER ||
     * mode == NON_LINKED</code>, otherwise, may be null or non-null.
     **/
    private final Random random;

    /**
     * May be null if <code>mode == NON_LINKED</code>.
     **/
    private final Linkage linkage;

    /**
     * The device that owns the arbiter.  Here solely so the arbiter
     * can update the device's time.  May be null.
     **/
    private final AbstractDevice device;

    /**
     * Constructs an arbiter.  
     * 
     * @param alts  Array of {@link Alternative}s
     * @param neutralState  the neutral state to which the arbiter must
     *     return after an alternative is selected
     * @param mode  Mode the arbiter operates in one of {@link #NON_LINKED},
     *     {@link #LINKED_SLAVE}, or {@link LINKED_MASTER}.
     * @param in  Input channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param out  Output channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param random  Random number generator.  May be null if
     *     <code>mode == LINKED_SLAVE</code>.
     *
     * @throws IllegalArgumentException  If arbitration mode is not valid.
     *
     * deprecated  Does not update the device's time, and it is
     *     impossible to do so when using this constructor.
     *     Use
     *     {@link #<init>(Alternative[],Linkage,int,ChannelInput[],ChannelOutput[],Random,device)}
     *     instead.
     **/
    public Arbiter(final Alternative[] alts, final Term[] neutralState,
            final int mode,
            final ChannelInput[] in, final ChannelOutput[] out,
            final Random random) {
        this(alts, makeLinkage(alts, neutralState, mode), mode,
             in, out, random);
    }

    /**
     * Constructs an arbiter.
     * 
     * @param alts  Array of {@link Alternative}s
     * @param linkage  The linkage information for the arbiter.
     *     May be null if <code>mode == NON_LINKED</code>.
     * @param mode  Mode the arbiter operates in one of {@link #NON_LINKED},
     *     {@link #LINKED_SLAVE}, or {@link LINKED_MASTER}.
     * @param in  Input channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param out  Output channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param random  Random number generator.  May be null if
     *     <code>mode == LINKED_SLAVE</code>.
     *
     * @throws IllegalArgumentException  If arbitration mode is not valid.
     *
     * deprecated  Does not update the device's time, and it is
     *     impossible to do so when using this constructor.
     *     Use
     *     {@link #<init>(Alternative[],Linkage,int,ChannelInput[],ChannelOutput[],Random,device)}
     *     instead.
     **/
    public Arbiter(final Alternative[] alts,
            final Linkage linkage,
            final int mode,
            final ChannelInput[] in, final ChannelOutput[] out,
            final Random random) {
        this(alts, linkage, mode, in, out, random, null);
    }

    /**
     * Constructs an arbiter.  This constructor will eventually be
     * deprecated.
     * 
     * @param alts  Array of {@link Alternative}s
     * @param linkage  The linkage information for the arbiter.
     *     May be null if <code>mode == NON_LINKED</code>.
     * @param mode  Mode the arbiter operates in one of {@link #NON_LINKED},
     *     {@link #LINKED_SLAVE}, or {@link LINKED_MASTER}.
     * @param in  Input channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param out  Output channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param random  Random number generator.  May be null if
     *     <code>mode == LINKED_SLAVE</code>.
     * @param device
     *        If non-null, the device whose time is to be updated by
     *        <code>initiateArbitration()</code> or
     *        <code>arbitrate()</code>.
     *
     * @throws IllegalArgumentException  If arbitration mode is not valid.
     **/
    public Arbiter(final Alternative[] alts,
            final Linkage linkage,
            final int mode,
            final ChannelInput[] in, final ChannelOutput[] out,
            final Random random,
            final AbstractDevice device) {
        this(alts, linkage, mode, in, out, null, null, random, device);
    }

    /**
     * Constructs an arbiter.  This constructor will eventually be
     * deprecated.
     * 
     * @param alts  Array of {@link Alternative}s
     * @param linkage  The linkage information for the arbiter.
     *     May be null if <code>mode == NON_LINKED</code>.
     * @param mode  Mode the arbiter operates in one of {@link #NON_LINKED},
     *     {@link #LINKED_SLAVE}, or {@link LINKED_MASTER}.
     * @param in  Input channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param out  Output channels to be waited on if none of the behavioral
     *   guards are true.  Typically, these are any channels that are probed
     *   in the guards.  May be null if <code>mode == LINKED_SLAVE</code>.
     * @param up  Rising nodes to be waited on if none of the behavioral
     *   guards are true.  Must be null unless <code>mode == NON_LINKED</code>.
     * @param down  Falling nodes to be waited on if none of the behavioral
     *   guards are true.  Must be null unless <code>mode == NON_LINKED</code>.
     * @param random  Random number generator.  May be null if
     *     <code>mode == LINKED_SLAVE</code>.
     * @param device
     *        If non-null, the device whose time is to be updated by
     *        <code>initiateArbitration()</code> or
     *        <code>arbitrate()</code>.
     *
     * @throws IllegalArgumentException  If arbitration mode is not valid.
     **/
    public Arbiter(final Alternative[] alts,
            final Linkage linkage,
            final int mode,
            final ChannelInput[] in, final ChannelOutput[] out,
            final Node[] up, final Node[] down,
            final Random random,
            final AbstractDevice device) {

        if (mode != NON_LINKED && mode != LINKED_SLAVE && mode != LINKED_MASTER)
            throw new IllegalArgumentException("Bad arbitration mode: " + mode);

        if (mode != LINKED_SLAVE) {
            if (random == null)
                throw new IllegalArgumentException("Random generator " +
                        "must be specified if mode != LINKED_SLAVE");
            if (in == null)
                throw new IllegalArgumentException("Input channels " +
                        "must be specified if mode != LINKED_SLAVE");
            if (out == null)
                throw new IllegalArgumentException("Output channels " +
                        "must be specified if mode != LINKED_SLAVE");
        }

        if (mode != NON_LINKED && linkage == null)
                throw new IllegalArgumentException("Linkage " +
                        "must be specified if mode != NON_LINKED");

        if (mode != NON_LINKED && (up != null || down != null)) {
            throw new IllegalArgumentException(
                "Linkage cannot be specified if arbitration involves nodes");
        }

        this.alts = alts;
        this.mode = mode;
        this.in = in;
        this.out = out;
        this.up = up;
        this.down = down;
        this.random = random;
        this.linkage = linkage;
        this.device = device;
    }

    /**
     * Constructs a <code>Linkage</code> from the terms in
     * an array of </code>Alternative</code>s and
     * <code>neutralState</code>.  For use by the old Arbiter
     * constructor.
     **/
    private static Linkage makeLinkage(
            final Alternative[] alts,
            final Term[] neutralState,
            final int arbitrationMode) {
        if (neutralState == null)
            return null;

        final Term[][] guardTerms = new Term[alts.length][];
        for (int i = 0; i < guardTerms.length; ++i) {
            guardTerms[i] = alts[i].getTerms();
            // Null out so we don't accidentally reference it
            alts[i].terms = null;
        }
        return new Linkage(guardTerms, neutralState, arbitrationMode);
    }

    /**
     * Constructs an arbiter, mode must be LINKED_SLAVE.
     * 
     * @param alts  Array of {@link Alternative}s
     * @param neutralState  the neutral state to which the arbiter must
     *     return after an alternative is selected
     * @param mode  Mode the arbiter operates in.  Must be
     *     {@link LINKED_SLAVE}.
     *
     * @throws IllegalArgumentException
     **/
    public Arbiter(final Alternative[] alts, final Term[] neutralState,
            final int mode) {
        this(alts, neutralState, mode, null, null, null);
    }

    /**
     * Returns the index <code>0 &lt;= index &amp;&amp;
     * index &lt; alts.length</code> of the alternative that was chosen.
     * If <code>mode == NON_LINKED || mode == LINKED_MASTER</code>,
     * then the alternative is chosen from a uniform distribution among 
     * all alternatives whose NullaryPredicate is true.  If
     * <code>mode == LINKED_SLAVE</code>, then the alternative is chosen
     * based on the terms in the node condition.  If
     * <code>mode == LINKED_MASTER</code>, then the nodes of the node
     * expression will be set to such that the node expression is true.
     *
     * @throws IllegalStateException  If more than one node condition is
     *     true and <code>mode == LINKED_SLAVE</code>.
     * @throws InterruptedException  If the thread is interrupted
     *     during a blocking operation such as a send, receive, or wait.
     **/
    public void arbitrate() throws InterruptedException { 

        final int chosenAlternative = initiateArbitration();

        // execute action of chosen alternative
        alts[chosenAlternative].getAction().execute();

        completeArbitration();
    }

    public static void removeFromWait(final Wait w, final Waitable wa) {
        // Try to disable the chosen channel as both a read and
        // write channel, because we don't know which it is.
        // If the channel is a BufferedChannel, it implements
        // both ChannelInput and ChannelOutput, but is only in
        // one set, so one of the disablings will fail silently.
        if (wa instanceof ChannelInput) {
            w.disableRead((ChannelInput) wa);
        }
        if (wa instanceof ChannelOutput) {
            w.disableWrite((ChannelOutput) wa);
        }
        if (wa instanceof Node) {
            final Node node = (Node) wa;
            switch (node.getValue()) {
              case Node.VALUE_0: w.disableDown(node);
                                 w.addUp(node);
                                 break;
              case Node.VALUE_1: w.disableUp(node);
                                 w.addDown(node);
                                 break;
              default: throw new AssertionError();
            }
        }
    }

    /**
     * Starts an arbitration, returns the index of the chosen alternative.
     *
     * <p> Use of this method is strongly discouraged except for
     * code automatically generated from csp.
     *
     * @throws IllegalStateException  If more than one node condition is
     *     true and <code>mode == LINKED_SLAVE</code>.
     * @throws InterruptedException  If the thread is interrupted
     *     during a blocking operation such as a send, receive, or wait.
     **/
    public int initiateArbitration() throws InterruptedException { 

        final List trueList = new ArrayList(alts.length);
        Wait conditionWait = null;

        do {
            // go through all of the alternatives, and pick one that's true.
            for (int i = 0; i < alts.length; ++i) {
                if (mode == LINKED_SLAVE) {
                    if (evaluateTerms(linkage.guardTerms[i]))
                        trueList.add(new Integer(i));
                } else {
                    if (alts[i].getPredicate().evaluate()) {
                        trueList.add(new Integer(i));
                    }
                }
            }

            if (trueList.size() == 0) {
                if (mode != LINKED_SLAVE ||
                    linkage.transitionQueue.isEmpty()) {
                    if (conditionWait == null) {
                        if (mode == LINKED_SLAVE)
                            conditionWait = makeWait(alts);
                        else // mode == NON_LINKED || mode == LINKED_MASTER
                            conditionWait =
                                WaitFactory.newWait(in, out, up, down);
                    }
                    final Pair/*<Waitable,Long>*/ p;
                    try {
                        p = conditionWait.select2();
                    } catch (EmptyWaitSetException e) {
                        throw new EmptyWaitSetException(
                                mode == LINKED_SLAVE ? "slave guards"
                                                     : "master guards",
                                e);
                    }
                    final long time = ((Long) p.getSecond()).longValue();
                    if (device != null)
                        device.updateTime(time);
                    final Waitable w = (Waitable) p.getFirst();
                    removeFromWait(conditionWait, w);
                }

                if (mode == LINKED_SLAVE &&
                    !linkage.transitionQueue.isEmpty())
                    linkage.dequeueTransitionAndUpdateState();
            }
        } while (trueList.size() == 0);

        // it is an error to have more than one expression true if we are a
        // slave
        if (mode == LINKED_SLAVE && trueList.size() > 1)
            throw new IllegalStateException("Non-exclusive node conditions");

        final int chosenIndex =
            trueList.size() > 1 ? random.nextInt(trueList.size()) : 0;
        final int chosenAlternative =
            ((Integer) trueList.get(chosenIndex)).intValue();

        // if we are a master, then set the terms to true
        if (mode == LINKED_MASTER)
            makeTermsTrue(linkage.guardTerms[chosenAlternative]);
        // XXX: To help find incorrect guard conditions, we would like
        //      to wait for the conditions to become true if we are
        //      a linked slave, but that doesn't work in the presence of
        //      inverted probes (~#X) because they will never become true
        //      if #X has already become true.
        // else if (mode == LINKED_SLAVE) {
        //     // if we are a slave, then wait for the predicate to become true
        //     final NullaryPredicate condition =
        //         alts[chosenAlternative].getPredicate();
        //     if (!condition.evaluate()) {
        //         // REVIEW: Do we really need to use a WaitFactory here?
        //         // Or are WaitFactories are only needed when deciding
        //         // which event came first?
        //         final Wait wait = WaitFactory.newWait(in, out);
        //         do {
        //             wait.select();
        //             wait.removeReady();
        //         } while (!condition.evaluate());
        //     }
        // }

        return chosenAlternative;
    }

    /**
     * Completes the arbitration by waiting for the neutral state condition
     * to become true or setting the neutral state condition to true 
     * as appropriate for mode.
     *
     * <p> Use of this method is strongly discouraged except for
     * code automatically generated from csp.
     **/
    public void completeArbitration() throws InterruptedException {
        // complete the arbitration
        if (mode == LINKED_MASTER) {
            // if we are a master, make the neutral condition true
            makeTermsTrue(linkage.neutralState);
        } else if (mode == LINKED_SLAVE) {
            // if we are a slave
            // wait for the neutral condition to become true
            Wait neutralWait = null;
            while (!evaluateTerms(linkage.neutralState)) {
                if (linkage.transitionQueue.isEmpty()) {
                    // REVIEW: Do we really need to use a WaitFactory?
                    if (neutralWait == null)
                        neutralWait = makeWait(linkage.neutralState);
                    try {
                        neutralWait.select();
                    } catch (EmptyWaitSetException e) {
                        throw new EmptyWaitSetException("neutral state", e);
                    }
                    neutralWait.removeReady();
                }

                if (!linkage.transitionQueue.isEmpty())
                    linkage.dequeueTransitionAndUpdateState();
            }
        }
    }

    /**
     * Appends the node to the <code>upList</code> if it is not negated,
     * and the <code>dnList</code> if it is negated.
     *
     * @param terms  Array of {@link Term}s that we are trying to make true.
     * @param upList List of nodes that must transition up for the terms
     *     to become true
     * @param dnList List of nodes that must transition down for the terms
     *     to become true
     **/
    private void appendToWaitLists(final Term[] terms,
            final List upList,
            final List dnList) {
        for (int i = 0; i < terms.length; ++i)
            (terms[i].isNegated() ? dnList : upList).add(terms[i].getNode());
    }

    /**
     * Makes a wait that will wait for any node in the terms of the
     * alternatives to switch in a direction that will help the 
     * expression become more true.
     *
     * @param terms  Array of {@link Alternative}s whose terms we are trying
     *     to make true.
     *
     * @return The wait object.
     **/
    private Wait makeWait(final Alternative[] alts) {
        final List upList = new ArrayList();
        final List dnList = new ArrayList();

        for (int i = 0; i < linkage.guardTerms.length; ++i)
            appendToWaitLists(linkage.guardTerms[i], upList, dnList);

        return makeWait(upList, dnList);
    }

    /**
     * Makes a wait that waits for any node in the terms to switch in a
     * direction that will help the expression become more true.
     *
     * @param terms  Array of {@link Terms}s we are trying to make true.
     *
     * @return The wait object.
     **/
    private Wait makeWait(final Term[] terms) {
        final List upList = new ArrayList(terms.length);
        final List dnList = new ArrayList(terms.length);

        appendToWaitLists(terms, upList, dnList);

        return makeWait(upList, dnList);
    }

    /**
     * Makes a wait that will wait for the given nodes to transition up or
     * down.
     *
     * @param upList List of nodes that must transition up
     * @param dnList List of nodes that must transition down
     *
     * @return The wait object.
     **/
    private Wait makeWait(final List upList, final List dnList) {
        final Node[] up = (Node[]) upList.toArray(new Node[upList.size()]);
        final Node[] dn = (Node[]) dnList.toArray(new Node[dnList.size()]);
        return WaitFactory.newWait(null, null, up, dn);
    }

    /**
     * Sets the nodes such that the terms are true.
     **/
    private void makeTermsTrue(final Term[] terms)
        throws InterruptedException {
        for (int i = 0; i < terms.length; ++i) {
            final Term term = terms[i];
            final byte value =
                term.isNegated() ? Node.VALUE_0 : Node.VALUE_1;
            final Node node = term.getNode();
            if (node.getValue() != value) {
                // System.err.println("Arbiter setting " + node +
                //                    " to " + value);
                node.scheduleImmediate(value);
            }
        }

        // Fix for bug 1189, wait for terms to actually become true.
        // Prevents one scheduled event from overwriting another
        // in case the device time doesn't advance between calls
        // to makeTermsTrue
        for (int i = 0; i < terms.length; ++i) {
            final Term term = terms[i];
            final Node [] node = new Node[]{term.getNode()};
            Node [] up, dn;
            if (term.isNegated()) {
                up = null;
                dn = node;
            } else {
                up = node;
                dn = null;
            }
            // REVIEW: Do we really need to use a WaitFactory?
            WaitFactory.newWait(null, null, up, dn).select();
        }
    }

    /**
     * Evaluate the nodes to determine if all the terms are true.
     **/
    private boolean evaluateTerms(final Term[] terms) {
        for (int i = 0; i < terms.length; ++i) {
            final byte nodeValue = linkage.getValue(terms[i].getNode());
            final byte neededValue =
                terms[i].isNegated() ? Node.VALUE_0 : Node.VALUE_1;

            if (nodeValue != neededValue)
                return false;
        }

        return true;
    }

    /**
     * Destroys the nodewatches associated with the arbiter if it was
     * constructed with the old constructor.
     **/
    public void destroy() {
        if (linkage != null && alts.length > 0 && alts[0].getTerms() != null)
            linkage.destroy();
    }

    /**
     * Encapsulates the behavioral level guard, the prs level guard node
     * condition, and the action to be performed if a guard is true.
     **/
    public static final class Alternative {

        /**
         * The predicate for the behavioral guard.  May not be null.
         **/
        private final NullaryPredicate behavioralGuard;

        /**
         * The and-terms of the node expression for prs-level cosimulation.
         * May be null if used with an arbiter with
         * <code>mode == NON_LINKED</code> or if the arbiter
         * is constructed with <code>linkage != null</code>.
         **/
        private Term[] terms;

        /**
         * The action to execute if this alternative is chosen.  May not be
         * null.
         **/
        private final Action action;

        /**
         * Class constructor.
         *
         * @param behavioralGuard  Predicate for the behavioral guard.
         *     May not be null.
         * @param terms  The and-terms of the node expression for prs-level
         *     cosimulation.  May be null if used with an arbiter with
         *     <code>mode == NON_LINKED</code> or if the arbiter
         *     is constructed with <code>linkage != null</code>.
         * @param action  The action to execute if this alternative is
         *     chosen.  May not be null.
         **/
        public Alternative(final NullaryPredicate behavioralGuard,
                final Term[] terms,
                final Action action) {
            this.behavioralGuard = behavioralGuard;
            this.terms = terms;
            this.action = action;
        }

        /**
         * Class constructor.  Constructs Alternative with null
         * <code>terms</code>.
         *
         * @param behavioralGuard  Predicate for the behavioral guard.
         *     May not be null.
         * @param action  The action to execute if this alternative is
         *     chosen.  May not be null.
         **/
        public Alternative(final NullaryPredicate behavioralGuard,
                final Action action) {
            this(behavioralGuard, null, action);
        }

        /**
         * Legacy constructor to support
         * <code>com.avlsi.util.functions.NullaryPredicate</code>.
         **/
        public Alternative(
                final com.avlsi.util.functions.NullaryPredicate
                    behavioralGuard,
                final Term[] terms,
                final Action action) {
            this(new NullaryPredicateAdapter(behavioralGuard),
                 terms, action);
        }

        /**
         * Legacy constructor to support
         * <code>com.avlsi.util.functions.NullaryPredicate</code>.
         **/
        public Alternative(
                final com.avlsi.util.functions.NullaryPredicate
                    behavioralGuard,
                final Action action) {
            this(behavioralGuard, null, action);
        }

        /**
         * Returns the behavioral guard predicate.
         *
         * @return behavioral guard predicate
         **/
        public NullaryPredicate getPredicate() {
            return behavioralGuard;
        }

        /**
         * Returns the terms of the node expression for prs-level
         * cosimulation.
         *
         * @return the terms of the node expression for prs-level
         * cosimulation
         **/
        Term[] getTerms() {
            return terms;
        }

        /**
         * Returns the action to be performed if this alternative is chosen.
         *
         * @return action to be performed if this alternative is chosen
         **/
        Action getAction() {
            return action;
        }
    }

    /**
     * A term is a node and a boolean indicating whether or not it is
     * inverted.  Represents <code>n</code> or <code>~n</code>.
     * A <code>Term[]</code> should be interpreted with all of its terms
     * anded together.
     **/
    public static final class Term {

        /**
         * The node.
         **/
        private final Node node;

        /**
         * If the sense of the node is negated.
         **/
        private final boolean isNegated;

        /**
         * Class constructor.
         *
         * @param node  The node of the term
         * @param isNegated if the sense if the node is negated
         **/
        public Term(final Node node, final boolean isNegated) {
            this.node = node;
            this.isNegated = isNegated;
        }

        /**
         * Returns the node.
         *
         * @return the node
         **/
        Node getNode() {
            return node;
        }

        /**
         * Returns whether the sense of the node is negated
         *
         * @return whether the sense of the node is negated
         **/
        boolean isNegated() {
            return isNegated;
        }

        /**
         * Returns a string representation.
         **/
        public String toString() {
            return (isNegated() ? "~" : "") + getNode().getName();
        }
    }

    /**
     * An action taking to arguments, but presumably having side effects.
     **/
    public interface Action {
        /** Perform the operation.  **/
        void execute() throws InterruptedException;
    }

    /**
     * Nullary predicate that can throw <code>InterruptedException</code>.
     * 
     * @see com.avlsi.util.functions.NullaryPredicate
     **/
    public interface NullaryPredicate {
        /**
         * Evaluate the predicate.
         **/
        boolean evaluate() throws InterruptedException;
    }

    private static final class NullaryPredicateAdapter
        implements NullaryPredicate {

        private final com.avlsi.util.functions.NullaryPredicate predicate;
        
        private NullaryPredicateAdapter(
                final /*@ non_null @*/
                com.avlsi.util.functions.NullaryPredicate predicate) {
            this.predicate = predicate;
        }

        public boolean evaluate() throws InterruptedException {
            return predicate.evaluate();
        }
    }

    /**
     * Class to encapsulate information on how the arbiter is linked
     * with prs.
     **/
    public static final class Linkage {

        /**
         * The terms for the neutral-state of the arbiter.  
         * May not be null.
         **/
        private final Term[][] guardTerms;

        /**
         * The terms for the neutral-state of the arbiter.  
         * May not be null.
         **/
        private final Term[] neutralState;

        /**
         * Map from node HierName to Byte representing the "current" value
         * of the node with that name.
         **/
        private final HashMap nodeValueMap;

        /**
         * All transitions are enqueued so that the arbiter doesn't miss
         * any.
         **/
        private final LinkedQueue transitionQueue;

        /**
         * The node watcher to enqueue transitions onto the
         * <code>transitionQueue</code>.  This is saved in a field 
         * because <code>destroy()</code> needs it to unregister
         * the watchers.
         **/
        private final NodeWatcher nodeWatcher;

        /**
         * The node watcher that sets the nodes to the neutral
         * state on reset.  Null if
         * <code>arbitrationMode != LINKED_SLAVE</code>.
         **/
        private final NodeWatcher resetNodeWatcher;

        /**
         * True if <code>destroy()</code> has been called.
         **/
        private boolean destroyed = false;

        /**
         * <pre><jml>
         *   public normal_behavior
         *     requires guardTerms != null;
         *     requires neutralState != null;
         * </jml></pre>
         **/
        public Linkage(final Term[][] guardTerms,
                       final Term[] neutralState,
                       final int arbitrationMode) {
            this.guardTerms = guardTerms;
            this.neutralState = neutralState;
            this.nodeValueMap = new HashMap();
            this.transitionQueue = new LinkedQueue();

            this.nodeWatcher = new NodeWatcher() {
                public void nodeChanged(Node node, long time) {
                    try {
                        transitionQueue.put(new Transition(node.getName(),
                                                           node.getValue()));
                    } catch (InterruptedException e) {
                        throw new AssertionError();
                    }
                }
            };

            if (arbitrationMode == LINKED_MASTER) {
                final Node resetNode = DSimUtil.getResetNode();

                // XXX: We have race conditions when the reset node
                // may be created later by nested prs sims
                // (ie AbstractPrsDevice)
                if (resetNode != null) {
                    this.resetNodeWatcher = new NodeWatcher() {
                        public void nodeChanged(Node node, long time) {
                            // set the nodes to the neutral state when
                            // we enter reset
                            if (node.getValue() == Node.VALUE_0) {
                                // System.err.println
                                //     ("Resetting arbiter nodes " +
                                //      "to neutral state");
                                makeNeutralStateTrue(neutralState);
                                // System.err.println("Done");
                            }
                        }
                    };
                    resetNode.addWatch(resetNodeWatcher);
                } else
                    this.resetNodeWatcher = null;
            } else {
                for (int i = 0; i < guardTerms.length; ++i)
                    registerWatcher(nodeValueMap, guardTerms[i], nodeWatcher);

                registerWatcher(nodeValueMap, neutralState, nodeWatcher);

                this.resetNodeWatcher = null;
            }
        }

        private void makeNeutralStateTrue(final Term[] terms) {
            for (int i = 0; i < terms.length; ++i) {
                final Term term = terms[i];
                final byte value =
                    term.isNegated() ? Node.VALUE_0 : Node.VALUE_1;
                final Node node = term.getNode();
                if (node.getValue() != value) {
                    // System.err.println("Arbiter setting " + node +
                    //                    " to " + value);
                    // setValueAndEnqueueDependents vs scheduleImmediate?
                    // node.setValueAndEnqueueDependents(value);
                    node.scheduleImmediate(value);
                    // System.err.println("New value: " + node);
                    // XXX: I believe we currently have a race condition
                    // because we are not sure that the neutral state is
                    // reached before the master sets it to something else.
                    // OTOH, I'm not sure this matters.  We don't care
                    // what value we set the nodes to, we're just doing
                    // this to prevent dsim from complaining about unstable
                    // nodes during reset.
                }
            }
        }

        /**
         * Registers a watcher for all nodes in <code>terms</code> if one
         * is not already registered, and records the current state of that
         * node in the <code>nodeValueMap</code>.
         **/
        private static void registerWatcher(final Map nodeValueMap,
                                            final Term[] terms,
                                            final NodeWatcher nodeWatcher) {
            for (int iterm = 0; iterm < terms.length; ++iterm) {
                final Node node = terms[iterm].getNode();
                final HierName name = node.getName();

                if (!nodeValueMap.containsKey(name)) {
                    node.addWatch(nodeWatcher);
                    nodeValueMap.put(name, new Byte(node.getValue()));
                }
            }
        }

        /**
         * Unregisters the <code>nodeWatcher</code> from all nodes referenced in
         * <code>terms</code>
         **/
        private void unregisterWatcher(final Term[] terms) {
            for (int iterm = 0; iterm < terms.length; ++iterm) {
                final Node node = terms[iterm].getNode();
                node.removeWatch(nodeWatcher);
            }
        }

        /**
         * Gets the value of the specified node from the
         * <code>nodeValueMap</code>.  
         **/
        private byte getValue(Node node) {
            return ((Byte) nodeValueMap.get(node.getName())).byteValue();
        }

        private void dequeueTransitionAndUpdateState() {
            try {
                // the transition queue must not be empty because we just
                // completed a wait
                Debug.assertTrue(!transitionQueue.isEmpty());
                final Transition t = (Transition) transitionQueue.take();
                nodeValueMap.put(t.getName(), new Byte(t.getValue()));
            } catch (InterruptedException e) {
                throw new AssertionError();
            }
        }

        /**
         * Removes the watchers that we have registered (if we are a 
         * <code>LINKED_SLAVE</code>, otherwise does nothing.
         **/
        public void destroy() {
            // TODO: somehow use weak references to avoid having to do this
            for (int i = 0; i < guardTerms.length; ++i)
                unregisterWatcher(guardTerms[i]);

            unregisterWatcher(neutralState);

            if (resetNodeWatcher != null) {
                final Node resetNode = DSimUtil.getResetNode();
                // the node might have already been destroyed
                if (resetNode != null) resetNode.removeWatch(resetNodeWatcher);
            }

            destroyed = true;
        }

        public void finalize() {
            // TODO: guard this in if (DEBUG)
            if (!destroyed) {
                System.err.println
                    ("You must call Arbiter.Linkage.destroy() " +
                     "to avoid memory leaks!");

                destroy();
            }
        }
    }

    /**
     * <code>Transition</code>s are enqueued on <code>transitionQueue</code>
     * to record the fact that a node has transitioned.
     **/
    private static final class Transition {
        private final HierName name;
        private final byte value;

        public Transition(final HierName name, final byte value) {
            this.name = name;
            this.value = value;
        }

        public HierName getName() {
            return name;
        }

        public byte getValue() {
            return value;
        }
    }

} // end of class Arbiter

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
