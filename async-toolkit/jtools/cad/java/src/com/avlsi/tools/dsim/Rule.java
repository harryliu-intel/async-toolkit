/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:cin:sw=4:expandtab
/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.math.QuadraticInterpolation;
import com.avlsi.tools.dsim.Node;

/**
 * Rule class.
 * 
 * Representation of rule in DNF (OR of ANDS.)
 * This just keeps track of the number of conjuncts.
 * The Disjuncts are represented in {@link Node} in the
 * <code>count</code>, <code>uCount</code> arrays.
 * <p>
 * Not terribly clear but each rule can either pull a node up or down.
 * To trigger a rule, it must have no false conjuncts, or unknown conjuncts.
 * i.e. All of its conjuncts must be nodes satisfying the test.
 * 
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public final class Rule {
    static final byte RULE_OFF = 0, RULE_ON = 1, RULE_U = 2;
    public static final int INVALID_MODEL = -1, LINEAR_MODEL = 0,
                            POLYNOMIAL_MODEL = 1;

    public static final byte MUST_BE_COVERED = 0,
                             IGNORE_COVERAGE = 1,
                             MUST_NOT_BE_COVERED = 2;

    int delay;
    final byte delay_type;
    final int dir;
    final Node target;
    private static final Pair[] NO_DATA = new Pair[0];
    private ArrayList<Pair[]> measuredDelay = null;

    // for debugging
    //final Node[] guards;
    //final int[] sense;
    //int totalConjuncts = 0;

    /**
     * Beware: some of the fields are used by ASTA for different purposes:
     *   falseConjuncts - stores instance count of a grayboxed rule
     *   transitionCount - stores the index into the table of interpolations
     **/

    int falseConjuncts = 0;
    int unknownConjuncts = 0;
    byte oldState, newState;

    /** For rule coverage */
    public int transitionCount = 0;
    public final byte coverageRequirement;

    /** 
     * Used for timing assumptions in synchronous circuitry.  Behaves
     * like TIMED_RANDOM even in UNTIMED_RANDOM simulations.
     **/
    final boolean timed;

    /**
     * Used for timing assumptions in asynchronous circuitry, fires
     * before any other rules only in UNTIMED_RANDOM simulations.
     **/
    final boolean isochronic;

    /**
     * Used for generating clock sources.  The specified delay is used
     * regardless of which random mode is selected.
     **/
    final boolean absoluteDelay;

    /**
     * Used for error checking.  An asserted rule should be excluded from
     * coverage and ASTA considerations.
     **/
    final boolean asserted;

    /*@
      @ instance invariant
//    @                        guards != null
//    @                        && (\forall int i;
//    @                                    0 <= i && i < guards.length;
//    @                                    guards[i] != null)
//    @                        &&
      @                        falseConjuncts >= 0
      @                        && unknownConjuncts >= 0
      @                        && target != null
      @                        && oldState >= RULE_OFF && oldState <= RULE_U
      @                        && newState >= RULE_OFF && newState <= RULE_U
      @                        && delay >= 0
      @                        && (dir == 0 || dir == 1);
      @*/

    /**
     * If <code>true</code>, use measure delay for this rule, if available.
     * Otherwise, do not use measure delay.
     **/
    boolean measureDelayOn;
    
    /**
     * Lower bound of timed jitter range.
     **/
    final float fastDelay;

    /**
     * Upper bound of timed jitter range.
     **/
    final float slowDelay;

    /**
     * Instance name of the cell that contains this production rule.
     **/
    final HierName prefix;

    /**
     * The canonical rule if ASTA grayboxing is enabled.
     **/
    private Rule canonRule;

    /** Converts just the PRS rule itself to a string. **/
    public String toPrettyString () {
        DSim sim = DSim.get();
        StringBuffer ret = new StringBuffer();
        boolean pastFirst = false;
        for (Node n : sim.getTargetingNodes(this)) {
            int sense = n.getSense(this);
            if (pastFirst) { ret.append(" & "); } 
            if (sense==0) { ret.append('~'); }
            ret.append(n.getName());
            pastFirst=true;
        }
        ret.append(" -> ");
        ret.append(target.getName());
        ret.append(dir>0?'+':'-');
        return ret.toString();
    }

    /** Returns a list of Node names that affect us with '~' to indicate direction **/
    public String getGuardList() {
        String ret = toPrettyString();
        ret = ret+" ("+Node.getNameForValue(oldState)+"->"+Node.getNameForValue(newState)
            +", "+unknownConjuncts+"U, "+falseConjuncts+"F)";
        return ret;
    }

    public void addMeasured(final Pair[] data, final int which) {
        if (measuredDelay == null) {
            measuredDelay = new ArrayList<Pair[]>(1);
        }
        for (int i = measuredDelay.size(); i <= which; ++i) {
            measuredDelay.add(NO_DATA);
        }
        measuredDelay.set(which, data);
    }

    private boolean hasMeasured() {
        return measuredDelay != null;
    }

    private Pair[] getMeasured(int index) {
        if (hasMeasured() && index < measuredDelay.size()) {
            return measuredDelay.get(index);
        } else {
            return NO_DATA;
        }
    }
    
    /**
     * Class constructor.  Creates a Rule to trigger the target node.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires guards != null
     *           && (\forall int i; 0 <= i && i < guards.length;
     *                       guards[i] != null)
     *           && sense != null
     *           && (\forall int i; 0 <= i && i < sense.length;
     *                       sense[i] == 0 || sense[i] == 1)
     *           && guards.length == sense.length
     *           && target != null
     *           && (dir == 0 || dir == 1)
     *           && delay >= 0;
     * </jml></pre>
     **/
    public Rule(Node[] guards, int[] sense, Node target, int dir, int delay,
                byte delay_type, boolean timed, float fastDelay,
                float slowDelay, boolean isochronic,
				boolean absoluteDelay, boolean asserted, Pair[] measuredDelay,
				byte coverageRequirement, HierName prefix) {
        Debug.assertTrue(guards.length == sense.length);
        int guardCount = guards.length;
        this.dir = dir;
        this.delay = delay;
        this.delay_type = delay_type;
        this.target = target;
        this.timed = timed;
        this.fastDelay = fastDelay;
        this.slowDelay = slowDelay;
        this.isochronic = isochronic;
        this.absoluteDelay = absoluteDelay;
        this.asserted = asserted;
        if (measuredDelay != null) addMeasured(measuredDelay, 0);
        this.coverageRequirement = coverageRequirement;
        this.prefix = prefix;
        this.measureDelayOn = true;
        // next few lines are for debuging
        //this.totalConjuncts = guardCount;
        //this.guards = guards;
        //this.sense = sense;
        //target.notifyTarget(this);

        for (int i = 0; i < guardCount; i++) {
            guards[i].addRule(this, sense[i]);
            int value = guards[i].getValue();
            if (value == Node.VALUE_U) {
                unknownConjuncts++;
            } else if (value != sense[i]) {
                falseConjuncts++;
            }
        }

        oldState = RULE_OFF;
        updateNewState();
        updateTargetCounts();

        if (modified()) {
            fireRule(null);
        }
    }
    /** Did our state change? **/
    public /*@ pure @*/ boolean modified() {
        return oldState != newState;
    }

    public Rule getCanonRule() {
        return canonRule;
    }

    public void setCanonRule(final Rule canonRule) {
        this.canonRule = canonRule;
    }

    /**
     * Notify target Node that our state has changed.
     * 
     * <pre><jml>
     *   public normal_behavior
     *     requires modified();
     * </jml></pre>
     **/
    public void fireRule(Node lastEvent) {
        Debug.assertTrue(modified());
        final int wait;
        final float slew;
        final byte type;
        final float fallbackSlew =
            lastEvent == null ? target.getSlew() : lastEvent.getSlew();
        if (!measureDelayOn || !hasMeasured()) {
            type = delay_type;
            wait = delay;
            slew = fallbackSlew;
        } else {
            float[] measured = new float[] { delay, fallbackSlew };
            type = getMeasured(lastEvent, measured) ? DSim.MEASURED_TAU
                                                    : delay_type;
            wait = Math.round(measured[0]);
            slew = measured[1];
        }

        if (coverageRequirement == MUST_NOT_BE_COVERED &&
            newState == RULE_ON && transitionCount == 0)
            System.out.println("Supposedly unused rule fired: " +
                               toPrettyString());

        // only count transitions between the on and off states
        if (oldState != RULE_U && newState != RULE_U)
            transitionCount++;

        target.ruleFired(wait, slew, type, oldState, newState, lastEvent, this);
    }       
    /** calcuate old state. **/
    public void updateOldState() {
        if (falseConjuncts>0) { oldState=RULE_OFF; } 
        else if (unknownConjuncts>0) { oldState=RULE_U; }
        else { oldState=RULE_ON; }
    }
    /** Calculate our new state after update **/
    public void updateNewState() {
        if (falseConjuncts>0) { newState=RULE_OFF; }
        else if (unknownConjuncts>0) { newState=RULE_U; }
        else { newState=RULE_ON; }
    }
    /** Remove last adjustement from target Node, so we can recalculate state. **/
    void reverseTransitions() {
        //if      ((falseConjuncts==0)&&(unknownConjuncts==0)) target.dropCount(dir);
        //else if ((falseConjuncts==0)&&(unknownConjuncts>0))  target.dropUCount(dir);
        if (falseConjuncts==0) {
            if (unknownConjuncts==0) target.dropCount(dir);
            else target.dropUCount(dir);
        }
    }
    /** Adjust taget node for current state. **/
    void updateTargetCounts() {
        //if      ((falseConjuncts==0)&&(unknownConjuncts==0)) target.bumpCount(dir);
        //else if ((falseConjuncts==0)&&(unknownConjuncts>0))  target.bumpUCount(dir);
        if (falseConjuncts==0) {
            if (unknownConjuncts==0) target.bumpCount(dir);
            else target.bumpUCount(dir);
        }
    }
    /** Verify that the state of the Rule is consistent with expectations. **/
    void verify() {
        Debug.assertTrue(falseConjuncts >= 0);
        Debug.assertTrue(unknownConjuncts >= 0);
        Debug.assertTrue(target != null);
        Debug.assertTrue(oldState >= RULE_OFF && oldState <= RULE_U);
        Debug.assertTrue(newState >= RULE_OFF && newState <= RULE_U);
        Debug.assertTrue(delay >= 0);
    }

    /** simple accessor, return target **/
    public Node target(){return target; }

    boolean getMeasured(final Node last, final float[] result) {
        if (!this.hasMeasured() || this.measuredDelay.isEmpty() || 
            last == null) return false;
        final Pair[] measuredDelay = getMeasured(0);
        for (int i = 0; i < measuredDelay.length; ++i) {
            final Object trigger = measuredDelay[i].getFirst();
            if (trigger == null || trigger.equals(last)) {
                final Pair p = (Pair) measuredDelay[i].getSecond();
                final int model = ((Integer) p.getFirst()).intValue();
                final Object data = p.getSecond();
                if (model == LINEAR_MODEL) {
                    linearInterpolate((float[]) data, last.getSlew(), result,
                                      last);
                } else if (model == POLYNOMIAL_MODEL) {
                    final Pair coeffs = (Pair) data;
                    polynomialInterpolate((float[]) coeffs.getFirst(),
                                          (float[]) coeffs.getSecond(),
                                          last.getSlew(), result);
                }
                return true;
            }
        }
        return false;
    }

    static QuadraticInterpolation[] getInterpolation(
            final Pair[] measuredDelay, final Node last, final Node target,
            int dir, double alpha, double minDeriv, double maxDeriv) {
        if (measuredDelay != null) {
            for (int i = 0; i < measuredDelay.length; ++i) {
                final Object trigger = measuredDelay[i].getFirst();
                if (trigger == null || trigger.equals(last)) {
                    final Pair p = (Pair) measuredDelay[i].getSecond();
                    final int model = ((Integer) p.getFirst()).intValue();
                    final float[] table = (float[]) p.getSecond();
                    if (model == LINEAR_MODEL) {
                        final int size = table.length / 3;
                        final double[] x = new double[size + 1];
                        final double[] delay1 = new double[size + 1];
                        final double[] delay2 = new double[size + 1];
                        int index = 0;
                        for (int j = 1; j < size + 1; ++j) {
                            x[j] = table[index++];
                            delay1[j] = table[index++] / 100;
                            delay2[j] = delay1[j] + table[index++];
                        }

                        // constrain the derivative of delay1 or delay2
                        boolean violated = false;
                        if (!Double.isNaN(maxDeriv)) {
                            violated |=
                                ProcessMeasuredDelay.constrainMaxDerivative(
                                    x, delay1, maxDeriv);
                            violated |=
                                ProcessMeasuredDelay.constrainMaxDerivative(
                                    x, delay2, maxDeriv);
                        }
                        if (!Double.isNaN(minDeriv)) {
                            violated |=
                                ProcessMeasuredDelay.constrainMinDerivative(
                                    x, delay1, minDeriv);
                            violated |=
                                ProcessMeasuredDelay.constrainMinDerivative(
                                    x, delay2, minDeriv);
                        }

                        if (violated) {
                            ProcessMeasuredDelay.printTable(
                                x, delay1, delay2,
                                "derivative limit constrained " +
                                last.getName() + (dir > 0 ? '-' : '+') +
                                " -> " + target.getName() +
                                (dir > 0 ? '+' : '-'));
                        }

                        // Add an additional data point to constrain
                        // extrapolation
                        x[0] = 0;
                        delay1[0] = delay1[1];
                        delay2[0] = delay2[1];

                        return
                            new QuadraticInterpolation[] {
                                new QuadraticInterpolation(x, delay1, alpha),
                                new QuadraticInterpolation(x, delay2, alpha)
                            };
                    } else if (model == POLYNOMIAL_MODEL) {
                        throw new RuntimeException(
                            "Polynomial model not supported");
                    }
                }
            }
        }
        return null;
    }

    QuadraticInterpolation[] getInterpolation(final Node last, double alpha,
                                              double minDeriv, double maxDeriv,
                                              final int which)
    {
        try {
            return getInterpolation(getMeasured(which), last, target, dir,
                                    alpha, minDeriv, maxDeriv);
        } catch (RuntimeException e) {
            throw new RuntimeException(
                "Polynomial model not supported: " + prefix + "/" +
                toPrettyString(), e);
        }
    }

    private void warnSlew(final Node last, final String s) {
        DSim.get().ui_out_verbose(
                "Slew interpolation warning:\n" + last.getName() + " -> " +
                target.getName() + (dir > 0 ? "+" : "-") + " " + s + "\n");
    }

    private void polynomialInterpolate(final float[] delayCoeff,
                                       final float[] slewCoeff,
                                       final float input,
                                       final float[] result) {
        result[0] = delayCoeff[0];
        result[1] = slewCoeff[0];
        final int small, index;
        final float[] big;
        if (delayCoeff.length < slewCoeff.length) {
            small = delayCoeff.length;
            big = slewCoeff;
            index = 1;
        } else {
            small = slewCoeff.length;
            big = delayCoeff;
            index = 0;
        }
        float powers = input;
        for (int i = 1; i < small; ++i, powers *= input) {
            result[0] += delayCoeff[i] * powers;
            result[1] += slewCoeff[i] * powers;
        }
        for (int i = small; i < big.length; ++i, powers *= input) {
            result[index] += big[i] * powers;
        }
    }

    private void linearInterpolate(final float[] table, final float input,
                                   final float[] result, final Node last) {
        final int l = table.length;
        assert l > 0 && l % 3 == 0;

        if (l == 3) {
            result[0] = table[1];
            result[1] = table[2];
        } else {
            int index = -1;
            if (input < table[0]) {
                index = 0;
                if (DSim.get().getVerbose())
                    warnSlew(last, "(" + input + " < " + table[0] + ")");
            } else if (input > table[l - 3]) {
                index = l - 6;
                if (DSim.get().getVerbose())
                    warnSlew(last, "(" + input + " > " + table[l - 3] + ")");
            } else {
                for (int i = 0; i < l - 3; i += 3) {
                    if (input >= table[i] && input <= table[i + 3]) {
                        index = i;
                        break;
                    }
                }
            }

            assert index >= 0;

            final float scale =
                (input - table[index]) / (table[index + 3] - table[index]);

            final float y1 = table[index + 1], y2 = table[index + 4],
                        z1 = table[index + 2], z2 = table[index + 5];
            result[0] = y1 == y2 ? y1 : y1 + (y2 - y1) * scale;
            result[1] = z1 == z2 ? z1 : z1 + (z2 - z1) * scale;
        }
    }

    public char getDirection() {
        return (dir>0 ? '+' : '-');
    }

    public void setMeasureStatus(final boolean on) {
        measureDelayOn = on;
    }
    
    public boolean getMeasureStatus() {
        return measureDelayOn;
    }

    public int setDelay(final int newDelay) {
        final int oldDelay = delay;
        delay = newDelay;
        return oldDelay;
    }

    public int getDelay() {
        return delay;
    }

    public float getFastDelay() {
        return fastDelay;
    }

    public float getSlowDelay() {
        return slowDelay;
    }

    public String toString() {
        Set<Node> guards = DSim.get().getTargetingNodes(this);
        return "Dsim rule(" +
            "delay=" + delay +
            ", direction=" + dir +
            ", target=" + target +
            ", guards=" + guards +
            ", falseConjuncts=" + falseConjuncts +
            ", unknownConjuncts=" + unknownConjuncts +
            // enable for debugging
            // ", totalConjuncts=" + totalConjuncts +
            ", oldState=" + oldState +
            ", newState=" + newState +
            ", timed=" + timed +
            ", isochronic=" + isochronic +
            ")";
    }
}
