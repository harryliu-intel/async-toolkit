/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.prs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.avlsi.file.common.HierName;
import com.avlsi.util.bool.AbstractAtomicBooleanExpression;
import com.avlsi.util.bool.AbstractBooleanExpression;
import com.avlsi.util.bool.AndBooleanExpression;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionVisitorInterface;
import com.avlsi.util.bool.BooleanUtils;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.test.AbstractTestCase;

/**
 * Class to represent a production rule.  Production rules have
 * four components:
 * <ol>
 *   <li> A left-hand side, a condition.
 *   <li> A right-hand side, a node that fires when the left-hand side
 *      condition is met.
 *   <li> A direction, either up or down, 
 *   <li> A number of time steps after which the rule will fire
 *      see {@link #getAfter}
 *   <li> <em>A boolean flag indicating whether or not the production
 *      rule is an "environmental" production rule, ie whether it
 *      was declared in an <code>env</code> block.</em> This is no
 *      longer relevant.  A ProductionRule in a PrsBlock is not
 *      "environmental", a ProductionRule derived from a statement in an
 *      AssertBlock is.
 * </ol>
 * <p>
 * There are three options for a production rule:
 * <ul>
 *   <li> unstable see {@link #isUnstable}
 *   <li> timed see {@link #isTimed}
 *   <li> isochronic see {@link #isIsochronic} and <a href="http://internal/bugzilla/show_bug.cgi?id=1643">Bug#1643</a>
 * </ul>
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ProductionRule {
    /**
     * The guard condition.
     **/
    private final BooleanExpressionInterface guard;

    /**
     * The target node.
     **/
    private final HierName target;

    /**
     * The power supply node.
     **/
    private final HierName powerSupply;

    /**
     * The direction (UP or DOWN) the target will fire when the guard condition
     * is true.
     **/
    private final int direction;

    /**
     * see {@link #isIsochronic}
     **/
    private final boolean isIsochronic;

    /**
     * see {@link #isUnstable}
     **/
    private final boolean isUnstableP;

    /**
     * see {@link #isMetastable}
     **/
    private final boolean isMetastableP;

    /**
     * see {@link #isTimed}
     **/
    private final boolean isTimedP;

    /**
     * see {@link #getAfter}
     **/
    private final int after;

    /**
     * see {@link #isAbsolute}
     **/
    private final boolean absolute;

    /**
     * see {@link #getFastDelay}
     **/
    private final float fastDelay;

    /**
     * see {@link #getSlowDelay}
     **/
    private final float slowDelay;

    /**
     * Direction constant indicating that the right-hand side goes down when
     * the left-hand side condition is true.
     **/
    public static final int DOWN = 0;

    /**
     * Direction constant indicating that the right-hand side goes up when
     * the left-hand side condition is true.
     **/
    public static final int UP = 1;

    /**
     * Class constructor.
     **/
    public ProductionRule(final BooleanExpressionInterface guard,
                          final HierName target,
                          final HierName powerSupply,
                          final int direction,
                          final boolean isIsochronic,
                          final boolean isUnstableP,
                          final boolean isMetastableP,
                          final boolean isTimedP,
                          final int after,
                          final boolean absolute) {
        this(guard, target, powerSupply, direction, isIsochronic,
             isUnstableP, isMetastableP, isTimedP, after, absolute,
             1, 1);
    }

    /**
     * Class constructor.
     **/
    public ProductionRule(final BooleanExpressionInterface guard,
                          final HierName target,
                          final HierName powerSupply,
                          final int direction,
                          final boolean isIsochronic,
                          final boolean isUnstableP,
                          final boolean isMetastableP,
                          final boolean isTimedP,
                          final int after,
                          final boolean absolute,
                          final float fastDelay,
                          final float slowDelay) {
        assert !(isUnstableP && isMetastableP) : "A rule cannot be unstable and metastable: " + guard + " -> " + target;
        assert direction == UP || direction == DOWN : "Unknown direction: " + direction;
        if (direction != DOWN && direction != UP)
            throw new IllegalArgumentException("bad direction:" + direction);

        this.guard = guard;
        this.target = target;
        this.powerSupply = powerSupply;
        this.direction = direction;
        this.isIsochronic = isIsochronic;
        this.isUnstableP = isUnstableP;
        this.isMetastableP = isMetastableP;
        this.isTimedP = isTimedP;
        this.after = after;
        this.absolute = absolute;
        this.fastDelay = fastDelay;
        this.slowDelay = slowDelay;
    }

    /**
     * Returns the left-hand side condition.
     **/
    public BooleanExpressionInterface getGuard() {
        return guard;
    }

    /**
     * Returns the right-hand side node.
     **/
    public HierName getTarget() {
        return target;
    }

    /**
     * Returns the power supply node.
     **/
    public HierName getPowerSupply() {
        return powerSupply;
    }

    /**
     * Returns the direction the right hand side will transition 
     * when the left hand side condition is true.
     **/
    public int getDirection() {
        return direction;
    }

    /**
     * Flag for special "isochronic" handling of this rule by DSim.
     **/
    public boolean isIsochronic() {
        return isIsochronic;
    }

    /**
     * Whether the node on the right hand side of the rule is guaranteed
     * not to be changed by another rule between the time this rule's
     * left hand side becomes true, and the right hand side is changed.
     **/
    public boolean isUnstable() {
        return isUnstableP;
    }

    /**
     * Whether the node on the right hand side of the rule should have a "lazy"
     * threshold for analog to digital conversation.
     * See <a href="http://internal/bugzilla/show_bug.cgi?id=2949">bug 2949</a>
     * for more details.
     **/
    public boolean isMetastable() {
        return isMetastableP;
    }

    /**
     * Means something to dsim, not sure what.
     **/
    public boolean isTimed() {
        return isTimedP;
    }

    /**
     * Whether the <code>after</code> delay is in terms of DSim units, or in
     * terms of absolute time.
     * See <a href="http://internal/bugzilla/show_bug.cgi?id=3026">bug 3026</a>
     * for more details.
     **/
    public boolean isAbsolute() {
        return absolute;
    }

    /**
     * @deprecated Return the number of time steps between the left
     * hand side becoming true and the right hand side transitioning.
     * This access method replaces -1 (that is, unspecified) with 100
     * for compatibility with older tools. We should eventually
     * migrate most tools to CellDelay instead. -- AML
     **/
    public int getAfterTimeSteps() {
        return (after >=0 ) ? after : 100;
    }

    /**
     * New way of getting after rule.  Returns -1 if after was not
     * explictly specified.  -- AML
     **/
    public int getAfter() {
        return after;
    }

    /**
     * Returns the lower bound of the time jitter range.
     **/
    public float getFastDelay() {
        return fastDelay;
    }

    /**
     * Returns the upper bound of the time jitter range.
     **/
    public float getSlowDelay() {
        return slowDelay;
    }

    /**
     * Returns production rule with transformed left hand side, opposite
     * direction, and otherwise the same.  The CNF of the left hand side 
     * must consist only of literals and negated literals.  The new left
     * hand side expression will be the same, but with the opposite sense
     * for each literal.
     * <p> Ie. a & b & ~c -&gt; d+ yields ~a & ~b & c -&gt; d-
     **/
    public ProductionRule cElementComplement(final HierName newPowerSupply) {
        return new ProductionRule(
                cElementComplement(getGuard(), true),
                getTarget(), newPowerSupply,
                oppositeDirection(getDirection()),
                isIsochronic(), isUnstable(), isMetastable(), 
                isTimed(), getAfter(), isAbsolute());
    }

    /**
     *
     **/
    private static BooleanExpressionInterface cElementComplement(
            final BooleanExpressionInterface be,
            final boolean nonNegatedP) {
        /** @review denney  This is kinda nasty due to design of
         * boolean stuff.  How should we improve it? **/

        if (be instanceof AbstractAtomicBooleanExpression) {
            // handle variable
            return be.negated();
        } else {
            final boolean sense;
            final Collection terms;
            if (be instanceof AndBooleanExpressionInterface) {
                final AndBooleanExpressionInterface abe
                    = (AndBooleanExpressionInterface) be;
                sense = abe.getSense();
                terms = abe.getConjuncts();
            } else if (be instanceof OrBooleanExpressionInterface) {
                final OrBooleanExpressionInterface obe
                    = (OrBooleanExpressionInterface) be;
                sense = obe.getSense();
                terms = obe.getDisjuncts();
            } else {
                throw new AssertionFailure("unknown boolean expression"
                        + " type: " + be.getClass().getName());
            }

            // handle true/false
            if (terms.size() == 0)
                return be.negated();

            final Collection newTerms = new ArrayList();
            for (final Iterator iTerm = terms.iterator(); iTerm.hasNext();) {
                final BooleanExpressionInterface subBe
                    = (BooleanExpressionInterface) iTerm.next();

                newTerms.add(cElementComplement(subBe, nonNegatedP == sense));
            }

            if (nonNegatedP == sense)
                return new AndBooleanExpression(sense, newTerms);
            else 
                return new OrBooleanExpression(sense, newTerms);
        }
    }

    /**
     * Returns production rule with negated left hand side, opposite
     * direction, and otherwise the same.
     * <p> Ie. a & (b | c) -&gt; d+ yields ~(a & (b | c)) -&gt; d-
     **/
    public ProductionRule combinationalComplement(
            final HierName newPowerSupply) {
        return new ProductionRule(getGuard().negated(),
                getTarget(), newPowerSupply,
                oppositeDirection(getDirection()),
                isIsochronic(), isUnstable(), isMetastable(), isTimed(),
                getAfter(), isAbsolute());
    }

    /**
     * Returns a new ProductionRule with the HierNames on both
     * sides transformed by <code>f</code>.
     **/
    public ProductionRule mapNames(final UnaryFunction f) {
        return new ProductionRule(
            BooleanUtils.mapBooleanExpressionHierNames(
                getGuard(), f),
            (HierName) f.execute(getTarget()),
            (HierName) f.execute(getPowerSupply()),
            getDirection(), isIsochronic(), isUnstable(), isMetastable(),
            isTimed(), getAfter(), isAbsolute());
    }

    /**
     * Returns a new ProductionRule with the HierNames on both
     * sides transformed replaced with the canonical name
     * according to <code>aliases</code>.  <code>f</code>
     * must not return null.
     **/
    public ProductionRule canonicalizeNames(final AliasedSet aliases) {
        return mapNames(
            new UnaryFunction() {
                public Object execute(final Object o) {
                    final HierName n = (HierName) o;
                    final HierName cn =
                        (HierName) aliases.getCanonicalKey(n);
                    Debug.assertTrue(cn != null);
                    return cn;
                }});
    }

    /**
     * Executes f on each HierName mentioned in the production rule.
     **/
    public void foreachHierName(final UnaryAction f) {
        f.execute(getTarget());
        f.execute(getPowerSupply());
        BooleanUtils.foreachHierName(getGuard(), f);
    }

    /**
     * Verifies the inverse monotonicity property.
     *
     * Our prs expressions are only synthesizable if they are "inverse
     * monotonic". This just means that any input from a rule going from
     * high to low can only leave the target alone or make the target go
     * from low to high, and that any input going from low to high can
     * only leave the target alone, or make it go from high to low.
     *
     * @return whether this production rule satisfies the inverse monotonicity constraint.
     **/
    public boolean isInverseMonotonic() {
        MonotonicityChecker visitor = new MonotonicityChecker();
        guard.visitWith(visitor);
        return visitor.ok;
    }

    /** report rule boolean flags as a string suitable for csim/aspice output */
    public String flagsString() {
        return 
            (isTimed()      ? "timed "      : "") +
            (isIsochronic() ? "isochronic " : "") +
            (isUnstable()   ? "unstab "     : "") +
            (isMetastable() ? "metastab "   : "");
    }

    /** print a PRS in a readable and parseable format */
    public String toString() {
        return flagsString()
            + (getAfter()>=0 ? (isAbsolute() ? "after_ps " : "after ") + getAfter() + " " : "")
            + getGuard() + " -> \"" + getTarget() +"\""
            + (getDirection() == UP ? "+" : "-");
    }

    /**
     * Returns DOWN if passed UP, and vice versa.
     *
     * @throws IllegalArgumentException  If direction is not either
     *   <code>UP</code> or <code>DOWN</code>.
     **/
    public int oppositeDirection(final int direction) {
        if (direction == UP)
            return DOWN;
        else if (direction == DOWN)
            return UP;
        else
            throw new IllegalArgumentException("bad direction: " + direction);
    }

    private class MonotonicityChecker implements BooleanExpressionVisitorInterface {
        /** are all atomic terms processed so far acceptable? Also used for shortcutting. **/
        public boolean ok = true;
        /** current sense we are checking against. **/
        private boolean up = direction == UP;
        /**
         * Handle and expression.
         **/
        public void visit(AndBooleanExpressionInterface ae) {
            final boolean nonnegated=ae.getSense();
            // if this is a negated clause, treat all subclauses as if up is opposite.
            up ^= (!nonnegated);
            Collection c = ae.getConjuncts();
            for (Iterator i = c.iterator(); ok && i.hasNext(); ) {
                BooleanExpressionInterface b = (BooleanExpressionInterface)i.next();
                b.visitWith(this);
            }
            // restore for parent.
            up ^= (!nonnegated);
        }
        /**
         * Handle or expression.
         **/
        public void visit(OrBooleanExpressionInterface oe) {
            boolean nonnegated=oe.getSense();
            // if this is a negated clause, treat all subclauses as if up is opposite.
            up ^= (!nonnegated);
            Collection c = oe.getDisjuncts();
            for (Iterator i = c.iterator(); ok && i.hasNext(); ) {
                BooleanExpressionInterface b = (BooleanExpressionInterface)i.next();
                b.visitWith(this);
            }
            // restore for parent.
            up ^= (!nonnegated);
        }
        /**
         * Handle atomic expression.
         **/
        public void visit(HierNameAtomicBooleanExpression te) {
            boolean nonnegated=te.getSense();
            if (!up ^ nonnegated) {
                ok = false;
            }
        }
    }

    /**
     * Tests {@link
     * ProductionRule#cElementComplement(BooleanExpressionInterface,boolean)}
     **/
    public static final class TestCElementComplement
        extends AbstractTestCase {

        /**
         * Test cElementComplement, making sure that the
         * string representation of the complement
         * matches what is expected.  Obviously, this is suboptimal,
         * and a better boolean epression comparison method is needed.
         **/
        private void test(final BooleanExpressionInterface be,
                          final String expected) {
            final String got
                = cElementComplement(be, true).toUserVisibleString();

            assertTrue(got.equals(expected));
        }

        /**
         * Test cElementComplement.  The expected string
         * will have to be changed if the algorithm changes, 
         * or the formatting changes.  This sucks, but at least
         * we have automated tests.
         **/
        public void test() throws Throwable {
            final BooleanUtils u = new BooleanUtils();

            final BooleanExpressionInterface a = u.literal("a");
            final BooleanExpressionInterface b = u.literal("b");
            final BooleanExpressionInterface c = u.literal("c");
            final BooleanExpressionInterface d = u.literal("d");

            // test true, expect false
            test(u.t(), "~true");

            // test false, expect true
            test(u.f(), "~false");

            // test ~true, expect true
            test(u.not(u.t()), "true");

            // test ~false, expect false
            test(u.not(u.f()), "false");

            // test a, expect ~a
            test(a, "~\"a\"");

            // test ~a, expect a
            test(u.not(a), "\"a\"");

            // test a & b, expect ~a & ~b
            test(u.and(a, b), "(~\"a\"&~\"b\")");

            // test a | b, expect ~a & ~b
            test(u.or(a, b), "(~\"a\"&~\"b\")");

            // test ~(a & b), expect ~(~a | ~b)
            test(u.not(u.and(a, b)), "~(~\"a\"|~\"b\")");

            // test ~(a | b), expect ~(~a | ~b)
            test(u.not(u.or(a, b)), "~(~\"a\"|~\"b\")");

            // test ~(~a & b), expect ~(~~a | ~b)
            test(u.not(u.and(u.not(a), b)), "~(\"a\"|~\"b\")");

            // test ~(~a | b), expect ~(~~a | ~b)
            test(u.not(u.or(u.not(a), b)), "~(\"a\"|~\"b\")");

            // test (a & b) | (c & d), expect ~a & ~b & ~c & ~d
            test(u.or(u.and(a, b), u.and(c, d)),
                    "((~\"a\"&~\"b\")&(~\"c\"&~\"d\"))");

            // test ~(~(~a & b) | (~c & d)),
            // expect ~(~(~~"a"&~"b")|~~"c"|~"d")
            test(u.not(u.or(u.not(u.and(u.not(a), b)), u.and(u.not(c), d))),
                    "~(~(\"a\"&~\"b\")|(\"c\"|~\"d\"))");
        }

        public static void main(String[] args) {
            AbstractTestCase.testOne(new TestCElementComplement());
        }
    }
}
