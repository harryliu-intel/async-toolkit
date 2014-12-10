/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.util.NoSuchElementException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import com.avlsi.util.debug.Debug;
import com.avlsi.tools.dsim.Event;
import com.avlsi.tools.dsim.Rule;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.tsim.Waitable;
import com.avlsi.util.container.TriTree;
import com.avlsi.file.common.HierName;

/**
 * DSimNode class
 * 
 * Representation of state and state-change for a particular node.
 * DNF.  This class only keeps track of the disjuncts.  The conjuncts
 * are kept track of by the *conjunct members in the {@link Rule}
 * class.
 * <p> 
 * Each node can be triggered by any one of a number of different rules
 * which are just boolean AND expressions. "A and not B and C".
 * <p>
 * Would perhaps be slightly clearer if state-change and current state were
 * kept in seperate entities.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public final class Node implements Event, Waitable {
    /** String constants for the possible logic values **/
    private static final String [] valueNames = { "0", "1", "U" };
    /** Numeric constants for the possible logic values **/
    public static final byte VALUE_0 = 0, VALUE_1 = 1, VALUE_U = 2;
    /** If true, then every node notifies the simulation on change. **/
    static boolean watchAll = false;
    /** Reference to the Singleton simulator **/
    private static DSim sim = DSim.get();
    
    /** Node that allows us to access our name. **/
    //MixNode label;
    private final HierName name;
    
    /** Trigger time */
    long eventTime;
    /** Rules we activate positively and negatively */
    private final Rule[][] rules = { new Rule[4], new Rule[4] };

    /**
     * The number of rules we have added.  
     *
     * <pre><jml>
     *   private constraint
     *     (\forall int i; 0 <= i && i < 2;
     *              (\num_of int j; 0 <= j && j < rules[i].length;
     *                       rules[i][j] != null)
     *              == numRules[i]);
     * </jml></pre>
     **/
    private final int[] numRules = { 0, 0 };

    /** number of disjuncts (rules) certainly driving us up and down.
     *  Note: this corresponds to pending, not current values.
     **/
    private final int count[] = { 0, 0 };
    /** number of disjuncts (rules) possibly driving us up and down.
     *  Note: this corresponds to pending, not current values.
     **/
    private final int uCount[] = { 0, 0 };

    /**
     * Beware: some of the fields are used by ASTA for different purposes:
     *   transitionCount - stores an index into the array of independent
     *                     variables
     *   flags - if set, bit RANDOM_BIT indicates arrival times are fixed
     *           if set, bit BREAKPT_BIT indicates this node has dependent rules
     **/

    /** number of transitions */
    int transitionCount;

    /** the capacitance associated with this node */
    float capacitance = 0;

    /** the output slew rate */
    float output_slew = Float.NaN;
    
    /** the source of the delay, either digital, estimated, or measured */
    byte delay_type = DSim.DIGITAL_TAU;

    /** "for critical path analysis".
     * saved to history.
     **/
    Event enabler;
    
    /** Current value */
    byte value = VALUE_U;
    /** Value after next transition */
    byte pending = VALUE_U;
    /** Tracing, etc. */
    // The watches can be accessed by multiple java threads at once.
    // Deciding to watch, or not watch a clock raising or lowering is 
    // a key example.  So we synchronize access to it, but not through
    // a Collections.SynchronizedCollection, as we also do not
    // want Iterators to fail.  The synchronization is done via this,
    // as nothing else uses it.
    private Collection watches = null;

    /** Tracing, etc. */
    private static final byte BREAKPT_BIT = 0x1;
    private static final byte UNSTAB_DN_BIT = 0x2;
    private static final byte UNSTAB_UP_BIT = 0x4;
    private static final byte RANDOM_BIT = 0x8;
    private static final byte GENERATION_BIT = 0x10;

    private byte flags = 0;
    // /** linkage with aspice ... is this nodes value controlled by */
    // private boolean hasDigitalFanin;

    /** Class constructor.  Creates an empty Node **/
    public Node(HierName name) {
        this.name = name;
    }
    /** Sets the container which represents our canonical name.
        The names are stored in the tree for space and searching efficiency. **/
    //public void setLabel(MixNode t) { label=t; }
    /** Returns our container object. **/
    //public MixNode getLabel() { return label; }
    /** Returns the Nodes canonical name from its TriTree node **/
    //public String getName() {
    //    if (label==null) { return null; }
    //    return label.getName();
    //}
    /** Returns the Nodes canonical name. **/
    public HierName getName() {
        return name;
    }

    /** Returns the number of possible logical values for a node. **/
    public static int getNumValues() { return valueNames.length; }
    /** converts an integer logical value into a corresponding string. **/
    public static String getNameForValue(int val) {
        if (val<0 || val>=getNumValues()) { return ""; }
        return valueNames[val];
    }
    /** Returns the value that matches name <code>s</code>, default is unknown.*/
    public static byte getValueForName(String s) {
        if (s!=null) {
            if (s.equals("0")) { return VALUE_0; }
            if (s.equals("1")) { return VALUE_1; }
        }
        return VALUE_U;
    }
    /** Returns a string representation of a pending change on the node **/
    public String getEventString() {
        return getName().getAspiceString()+" "+valueNames[value]+"->"+valueNames[pending]+" at "+eventTime;
    }
    
    /** Collect minimum and maximum slew across all rules */
    private final static float slewStat[] = new float[2];
    public static void resetSlewStat() {
        slewStat[0] = Float.POSITIVE_INFINITY;
        slewStat[1] = Float.NEGATIVE_INFINITY;
    }
    private static void updateSlewStat(final Node me) {
        final float slew = me.getSlew();
        slewStat[0] = Math.min(slew, slewStat[0]);
        slewStat[1] = Math.max(slew, slewStat[1]);
    }
    public static float[] getSlewStat() {
        return slewStat;
    }

    /* Event interface Methods... */
    /** Triggers this node as an event. **/
    public void fire() { 
        transitionCount++;
        setValueAndEnqueueDependents(pending); 
        updateSlewStat(this);
    }
    private int index=-1;
    /** Returns current index as set by the scheduler. **/
    public int getIndex() { return index; }
    /** Sets current index (by the scheduler). -1 indicates not scheduled. **/
    public void setIndex(int _index) { 
        index = _index;
    }
    /** Returns the next time that we should fire. **/
    public long getTime() { return eventTime; }
    /** Returns whether we should fire at a random time. **/
    public boolean isRandom() { return (flags & RANDOM_BIT) != 0; }
    /** sets whether we should fire at a random time. **/
    public void setRandom(boolean b) {
        if (b) {
            flags |= RANDOM_BIT;
        } else {
            flags &= ~RANDOM_BIT;
        }
    }

    public boolean getGeneration() { return (flags & GENERATION_BIT) != 0; }
    public void setGeneration(boolean b) {
        if (b) {
            flags |= GENERATION_BIT;
        } else {
            flags &= ~GENERATION_BIT;
        }
    }

    /**
     * Add another rule we might trigger.
     *
     * <pre><jml>
     *   private normal_behavior
     *     requires drule != null && 0 <= sense && sense < 2;
     *     ensures numRules[sense] == \old(numRules[sense]) + 1
     *          && numRules[1 - sense] == \old(numRules[1 - sense])
     *          && rules[sense][numRules[sense] - 1] == drule
     *          && (\forall int i; 0 <= i && i < \old(numRules[sense]);
     *                      rules[sense][i] == \old(rules[sense][i]))
     *          && (\forall int i; 0 <= i && i < numRules[1 - sense];
     *                      rules[sense][i] == \old(rules[1 - sense][i]));
     * </jml></pre>
     **/
    void addRule(Rule drule, int sense) {
        // The rule's counts are adjusted in its constructor.
        Debug.assertTrue(sense >= 0 && sense < 2);
        final int nr = numRules[sense];
        final Rule[] rs = rules[sense];

        if (nr == rs.length) {
            final Rule[] newRules = new Rule[2 * nr];
            System.arraycopy(rs, 0, newRules, 0, nr);
            rules[sense] = newRules;

            newRules[nr] = drule;
            numRules[sense] = nr + 1;
        } else {
            rs[nr] = drule;
            numRules[sense] = nr + 1;
        }
    }

    /** Remove from our watcher list. **/
    public synchronized boolean removeWatch(NodeWatcher w) {
        if (watches == null)
            return false;
        else
            return watches.remove(w);
    }

    /** Remove all watchers associated with this node **/
    public synchronized void removeAllWatchers(){
        watches = null;
    }
    
    /** Add to our watcher list, for change notifications. **/
    public synchronized boolean addWatch(NodeWatcher w) {
        if (watches == null)
            watches = new ArrayList/*<NodeWatcher>*/(1);
        // Only add if the watcher is not already registered
        return watches.contains(w) || watches.add(w);
    }
    /** Check presence in our watcher list. **/
    public synchronized boolean isWatcher(NodeWatcher w) {
        if (watches == null)
            return false;
        else
            return watches.contains(w);
    }

    /** Do we stop the simulation on change? **/
    public boolean getBreakpoint() { return (flags & BREAKPT_BIT) != 0; }

    /** Sets whether we stop the simulation on change. **/
    public void setBreakpoint(boolean b) {
        if (b) {
            flags |= BREAKPT_BIT;
        } else {
            flags &= ~BREAKPT_BIT;
        }
    }

    /** Do we care if node goes unstable (per pending direction)? **/
    public boolean isUnstable(int pending) {
        if      (pending==VALUE_0) return (flags & UNSTAB_DN_BIT) != 0;
        else if (pending==VALUE_1) return (flags & UNSTAB_UP_BIT) != 0;
        else return false;
    }

    /** Sets whether we care if node goes unstable (per direction). **/
    public void setUnstable(boolean updir) {
        if (updir) flags |= UNSTAB_UP_BIT;
        else       flags |= UNSTAB_DN_BIT;
    }

    /** Returns the current number of transitions this node has undergone. **/
    public int getTCount() { return transitionCount; }
    
    /** clears the transitionCount associated with node. **/
    public void clearTCount() { transitionCount =0; }

    /** sets the transitionCount associated with node. **/
    public void setTCount(int tcount) { transitionCount = tcount; }

    /** returns a short string representation of this Node. **/
    public String toString() { return getName().getAspiceString() + ":" + valueNames[value]; }
    /** Returns a list of Rules that we affect. Used for debugging. **/
    public List<Rule> getTargets() {
        List<Rule> ret = new ArrayList<Rule>();
        for (int i=0; i<2; i++) {
	    ret.addAll(getTargets(i));
        }
        return ret;
    }
    /** Returns a list of Rules that the input sense targets. **/
    public List<Rule> getTargets(int sense) {
        List<Rule> ret = new ArrayList<Rule>();
	int n = numRules[sense];
	Rule[] rs = rules[sense];
	for (int j = 0; j < n; j++) {
	    ret.add(rs[j]);
	}
        return ret;
    }
    /** Is the given Rule affected by us. Slow, used for debugging. **/
    public boolean targets(Rule targ) {
        for (int i=0; i<2; i++) {
            int n = numRules[i];
            Rule[] rs = rules[i];
            for (int j = 0; j < n; j++) {
                Rule r = rs[j];
                if (r==targ) { return true; }
            }
        }
        return false;
    }
    /** Returns which direction causes a given Rule to be activated. **/
    public int getSense(Rule targ) {
        int n = numRules[0];
        Rule[] rs = rules[0];
        for (int i = 0; i < n; ++i) {
            Rule r = rs[i];
            if (r==targ) { return 0; }
        }
        return 1;
    }
    /** List our rules. **/
    public String listRules() {
        String ret = getName().getAspiceString()+" ("
            +getNameForValue(value)+"->"+getNameForValue(pending)+", "
            +count[0]+"-, "+count[1]+"+, "
            +uCount[0]+"u-, "+uCount[1]+"u+)\n"; 
        for (Rule r : sim.getTargetingRules(this)) {
            ret = ret + r.getGuardList() +"\n";
        }
        return ret;
    }
    
    /**
     * Modify a node's value by external poking.
     * Adjusts count and possibly fire rules we trigger.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires 0 <= newValue && newValue <= 2;
     * </jml></pre>
     **/
    public void setValueAndEnqueueDependents(byte newValue) {
        int s;
        Debug.assertTrue(newValue >= 0 && newValue <=2);

        if (getBreakpoint()) { sim.interrupt(); }

        if (value==newValue) { return; }

        for (s=0; s<2; s++) {
            int n = numRules[s];
            Rule[] rs = rules[s];
            for (int j = 0; j < n; ++j) {
                Rule drule = rs[j];
                // compute old state
                drule.updateOldState(); // Why can't we just copy current state? 
                //if (drule.falseConjuncts>0)
                //    drule.oldState=Rule.RULE_OFF;
                //else if (drule.unknownConjuncts>0)
                //    drule.oldState=Rule.RULE_U;
                //else
                //    drule.oldState=Rule.RULE_ON;
            }
        }
        /*** update dependent rule and node counters ***/
        for (s=0; s<2; s++) {
            int n = numRules[s];
            Rule[] rs = rules[s];
            for (int j = 0; j < n; ++j) {
                Rule drule = rs[j];

                /*** reverse last transition on node ***/
                drule.reverseTransitions();
                //if (drule.falseConjuncts==0) {
                //    if (drule.unknownConjuncts==0) drule.target.dropCount(drule.dir);
                //    else drule.target.dropUCount(drule.dir);
                //}

                /*** update rule conjunct counts ***/
                switch (newValue) {
                    case VALUE_0:
                        drule.falseConjuncts+=s;
                        break;
                    case VALUE_1:
                        drule.falseConjuncts+=(1-s);
                        break;
                    case VALUE_U:
                        drule.unknownConjuncts++;
                        break;
                    default:
                        Debug.assertTrue(false);
                }

                switch (value) {
                    case VALUE_0:
                        drule.falseConjuncts-=s;
                        break;
                    case VALUE_1:
                        drule.falseConjuncts-=(1-s);
                        break;
                    case VALUE_U:
                        drule.unknownConjuncts--;
                        break;
                    default:
                        Debug.assertTrue(false);
                }

                /*** update counts of target node ***/
                drule.updateTargetCounts();
                //if (drule.falseConjuncts==0) {
                //    if (drule.unknownConjuncts==0) drule.target.bumpCount(drule.dir);
                //    else drule.target.bumpUCount(drule.dir);
                //}
            }
        }
        
        for (s=0; s<2; s++) {
            int n = numRules[s];
            Rule[] rs = rules[s];
            for (int j = 0; j < n; ++j) {
                Rule drule = rs[j];
                // compute drule.newState
                drule.updateNewState();
                //if (drule.falseConjuncts>0) { drule.newState=Rule.RULE_OFF; }
                //else if (drule.unknownConjuncts>0) { drule.newState=Rule.RULE_U; }
                //else { drule.newState=Rule.RULE_ON; }
            }
        }

        /*** set node value ***/
        value=newValue;

        /*** debugging printouts ***/
        synchronized (this) { // Do not want a stale iterator
            boolean toldDSim = false;
            if (watches != null) {
                for (Iterator i = watches.iterator(); i.hasNext();) {
                    NodeWatcher nw = (NodeWatcher)i.next();
                    if (nw==sim) { toldDSim = true; }
                    nw.nodeChanged(this, sim.getTime());
                }
            }
            if (!toldDSim && watchAll) { 
                sim.nodeChanged(this, sim.getTime());
            } 
        }

        /*** fire modified rules ***/
        boolean critical = false;
        for (s=0; s<2; s++) {
            int n = numRules[s];
            Rule[] rs = rules[s];
            for (int j = 0; j < n; ++j) {
                Rule drule = rs[j];
                if (drule.modified()) {
                    drule.fireRule(this);
                    critical = true;
                }
            }
        }

        /*** record history ***/
        if (sim.recordHistory(critical)) {
            sim.recordEvent(this, enabler, eventTime, transitionCount, newValue, getSlew(), getDelayType());
        }
    }

    /** Returns our current value (vs pending value). **/
    public /*@ pure @*/ byte getValue() { return value; }

    /** Notification that a Rule affecting this Node changed its state **/
    public void ruleFired(int delay, float slew, byte type, byte oldState,
            byte newState, Node lastEvent, Rule drule) {
        if (getBreakpoint()) { System.out.println("Rule fired on breakpointed node: "+getName().getAspiceString()); }
        /*** figure out whats happening to the node ***/
        if ((count[0]+uCount[0]>0)&& (count[1]+uCount[1]>0)) { /*** interfering ***/
            if ((oldState==Rule.RULE_OFF)&&(((index < 0)&&(value!=VALUE_U)) || 
                        (((index >=0)&&(pending!=VALUE_U))))) {
                /*** interfering ***/
                //sim.eventWarning(lastEvent,drule,"interfering");
                sim.interferenceWarning(lastEvent,drule);
                sim.scheduleEvent(this,VALUE_U,-1,false,lastEvent);
            }
        } else if ((count[0]>0) || (count[1]>0)) /*** driven ***/ {
            byte dir;
            boolean random = false;
            dir=(count[1]>0?(byte)1:(byte)0);
            if (delay==0) {
                // Magical "after 0" delay always fires instantly in
                // any timing mode.  This construct should be avoided
                // if possible, except in env bodies or in glitching
                // circuits like synchronizers.  --AML
                delay = -1;
            }
            else if (drule.isochronic && (sim.randomOrder == DSim.UNTIMED_RANDOM)) {
                // isochronic rules fire "instantly" in random
                // simulations intended to replace old "after 0"
                // notation for timing assumptions.
                delay = -1;
            }
            else if (sim.randomOrder != DSim.NO_RANDOM && !drule.absoluteDelay) {
                if (drule.timed || sim.randomOrder == DSim.TIMED_RANDOM) {
                    delay *= sim.randomDelay();
                } else {
                    random = true;
                    delay = 0;
                }
            }

            if (index < 0) { 
                if (value==dir); /*** vacuous ***/
                else if (value==VALUE_U) { 
                    /*** new event due to disappearance of interference ***/
                    sim.eventNotice(lastEvent,drule,"changes from U");
                    sim.scheduleEvent(this,dir,0,false,lastEvent);
                } else if (newState==Rule.RULE_ON) { 
                    /*** new event ***/
                    sim.eventNotice(lastEvent,drule,"scheduled normally");
                    setSlew(slew);
                    setDelayType(type);
                    sim.scheduleEvent(this,dir,delay,random,lastEvent);
                } 
            } else {
                if (pending==VALUE_U) { 
                    /*** new event due to disappearance of interference ***/
                    sim.eventNotice(lastEvent,drule,"changes from U");
                    sim.scheduleEvent(this,dir,0,false,lastEvent);
                } else if ((oldState==Rule.RULE_ON)&&(pending!=dir)) {
                    /*** instability ***/
                    if (!isUnstable(pending)) {
                        // unstable (set to 0 or 1)
                        sim.glitchWarning(lastEvent,drule);
                    }
                    sim.scheduleEvent(this,dir,-1,false,lastEvent);
                } else if ((newState==Rule.RULE_ON)&&(pending==dir)&&
                        sim.compareDelayToTime(delay, eventTime) < 0) {
                    /*** reschedule event ***/
                    sim.eventNotice(lastEvent,drule,"rescheduled earlier");
                    setSlew(slew);
                    setDelayType(type);
                    sim.scheduleEvent(this,dir,delay,random,lastEvent);
                }
            }
        } else /*** floating ***/ {
            if ((oldState==Rule.RULE_ON)&&(index >= 0)&&(pending!=VALUE_U)) {   
                /*** instability ***/
                if (!isUnstable(pending)) {
                    // unstable (set to U)
                    sim.unstabWarning(lastEvent,drule);
                }
                sim.scheduleEvent(this,VALUE_U,-1,false,lastEvent);
            } else if ((newState==Rule.RULE_U)&&(((uCount[0]>0)&&(value==VALUE_1)) ||
                        ((uCount[1]>0)&&(value==VALUE_0)))) {
                /*** set to U ***/
                sim.eventWarning(lastEvent,drule,"set to U");
                sim.scheduleEvent(this,VALUE_U,-1,false,lastEvent);
            }           
        }           
    }

    /**
     * Cause this node to be scheduled to change as soon as possible, after
     * the next running time.  Does not block.  
     * If you want it to actually happen immediately,
     * you can use setValueAndEnqueueDependents().
     **/
    public void scheduleImmediate(byte value) {
        sim.scheduleEvent(this, value, -1, false, null);
    }

    /**
     * Cause this node to be scheduled to change at time t.
     * Does not block.
     **/
    public void scheduleTime(byte value, long t, Node lastEvent) {
        long delay = t - sim.getTime();
        scheduleDelay(value, delay, lastEvent);
    }

    /**
     * Cause this node to be scheduled to change at time t.
     * Does not block.
     * Same as <code>scheduleTime(value, t, null)</code>.
     **/
    public void scheduleTime(byte value, long t) {
        scheduleTime(value, t, null);
    }

    /**
     * Cause this node to be scheduled delay timesteps in the future.
     * Does not block.
     **/
    public void scheduleDelay(byte value, long delay, Node lastEvent) {
        sim.scheduleEvent(this, value, delay, false, lastEvent);
    }

    /**
     * Cause this node to be scheduled delay timesteps in the future.
     * Does not block.
     * Same as <code>scheduleDelay(value, delay, null)</code>.
     **/
    public void scheduleDelay(byte value, long delay) {
        scheduleDelay(value, delay, null);
    }

    /**
     * Place this transition on the random queue.
     * Does not block.
     **/
    public void scheduleRandom(byte value, Node lastEvent) {
        sim.scheduleEvent(this, value, -1, true, lastEvent);
    }

    /**
     * Place this transition on the random queue.
     * Does not block.
     * Same as <code>scheduleRandom(value, null)</code>.
     **/
    public void scheduleRandom(byte value) {
        scheduleRandom(value, null);
    }

    /**
     * Calls <code>scheduleRandom(value)</code> if DSim's random
     * mode is <code>UNTIMED_RANDOM</code>, otherwise calls
     * <code>scheduleDelay</code>.
     * Does not block.
     **/
    public void scheduleDelayOrRandom(byte value, long delay) {
        if (sim.getRandom() == DSim.UNTIMED_RANDOM)
            scheduleRandom(value);
        else
            scheduleDelay(value, delay);
    }

    /**
     * Calls <code>scheduleRandom(value)</code> if DSim's random
     * mode is <code>UNTIMED_RANDOM</code>, otherwise calls
     * <code>scheduleTime</code>.
     * Does not block.
     **/
    public void scheduleTimeOrRandom(byte value, long t, Node lastEvent) {
        if (sim.getRandom() == DSim.UNTIMED_RANDOM)
            scheduleRandom(value, lastEvent);
        else
            scheduleTime(value, t, lastEvent);
    }

    /**
     * Calls <code>scheduleRandom(value)</code> if DSim's random
     * mode is <code>UNTIMED_RANDOM</code>, otherwise calls
     * <code>scheduleTime</code>.
     * Does not block.
     * Same as <code>scheduleTimeOrRandom(value, t, null)</code>.
     **/
    public void scheduleTimeOrRandom(byte value, long t) {
        scheduleTimeOrRandom(value, t, null);
    }

    /** Another rule is pulling us up or down based on <code>dir</code>. **/
    void bumpCount(int dir) { count[dir]++; }

    /** Another rule may be (undefined) pulling us up or down based on <code>dir</code>. **/
    void bumpUCount(int dir) { uCount[dir]++; }

    /** Another rule stopped pulling us up or down based on <code>dir</code>. **/
    void dropCount(int dir) { count[dir]--; }

    /** Another (undefined) rule stopped pulling us up or down based on <code>dir</code>. **/
    void dropUCount(int dir) { uCount[dir]--; }

    /**
     * Set the capacitance of this node.
     **/
    public void setCapacitance(final float cap) {
        this.capacitance = cap;
    }

    /**
     * Get the capacitance of this node.
     **/
    public float getCapacitance() {
        return capacitance;
    }

    /**
     * Set the output slew rate of this node.
     **/
    public void setSlew(final float slew) {
        this.output_slew = slew;
    }

    /**
     * Get the output slew rate of this node.
     **/
    public float getSlew() {
        return output_slew;
    }

    /**
     * For each Rule that depends on this Node, calls f with the
     * rule, sense, and node.
     */
    public void foreachRule(RuleFunc f) {
        for (int sense = 0; sense < 2; sense++)
            for (int i = 0; i < numRules[sense]; i++)
                f.accept(rules[sense][i], sense, this);
    }

    public interface RuleFunc {
        /**
         * User-supplied function passed to foreachRule().
         * @param r     one of the Rules that depends on the Node
         * @param sense 0 => inverted, 1 => noninverted
         * @param n     the Node (for convenience; you already know it)
         */
        void accept(Rule r, int sense, Node n);
    }

    /**
     * Set the delay type of this node.
     **/
    public void setDelayType(final byte type) {
        this.delay_type = type;
    }

    /**
     * Get the delay type of this node.
     **/
    public byte getDelayType() {
        return delay_type;
    }
}
