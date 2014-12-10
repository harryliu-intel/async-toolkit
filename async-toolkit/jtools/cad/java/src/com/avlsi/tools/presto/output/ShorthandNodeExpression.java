/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public abstract class ShorthandNodeExpression implements NodeExpression {
    protected final NodeName[] names;
    protected final boolean reset;

    protected ShorthandNodeExpression(NodeName[] names, boolean reset) {
        this.names = names;
        this.reset = reset;
    }

    /**
     * Operator which should appear between the ith and i+1th node.
     * Note that the last operator is actually the operator between
     * the ith node and destination node (i. e. arrow operator), rather
     * than an "and" or "or" operator.
     */
    protected abstract String operator(int i, Direction preferredDirection);

    /**
     * Should there be a left paren before the ith node?
     */
    protected boolean parenBefore(int i) {
        return false;
    }

    /**
     * Should there be a right paren after the ith node?
     */
    protected boolean parenAfter(int i) {
        return false;
    }

    /**
     * This can be overridden for operators which can only be
     * written in one direction, like and2c2.
     */
    protected Direction overrideDirection(Direction preferredDirection) {
        return preferredDirection;
    }

    public LineAndSection[] evaluate(final NodeName dest,
                                     Direction preferredDirection_,
                                     Section upSection, Section downSection) {
        final Direction preferredDirection =
            overrideDirection(preferredDirection_);

        return new LineAndSection[] { new LineAndSection(new AbstractPrsLine() {
                public boolean isParameterized() {
                    for (int i = 0; i < names.length; i++)
                        if (names[i].isParameterized())
                            return true;
                    return dest.isParameterized();
                }

                private String resetPrefix() {
                    if (!reset)
                        return "";
                    return (preferredDirection == Direction.UP ?
                            "~_Reset | " : "_Reset & (");
                }

                private String resetSuffix() {
                    return (reset && preferredDirection == Direction.DOWN ?
                            ")" : "");
                }

                public String toStringWithIndex(int index) {
                    StringBuffer result = new StringBuffer(resetPrefix());
                    for (int i = 0; i < names.length; i++) {
                        if (parenBefore(i))
                            result.append('(');
                        if (preferredDirection == Direction.UP)
                            result.append('~');
                        result.append(names[i].toStringWithIndex(index));
                        if (parenAfter(i))
                            result.append(')');
                        if (i == names.length - 1)
                            result.append(resetSuffix());
                        result.append(' ');
                        result.append(operator(i, preferredDirection));
                        result.append(' ');
                    }
                    result.append(dest.toStringWithIndex(index));
                    result.append(preferredDirection.toString());
                    return result.toString();
                }

                public String toStringWithVariable(String var) {
                    StringBuffer result = new StringBuffer(resetPrefix());
                    for (int i = 0; i < names.length; i++) {
                        if (parenBefore(i))
                            result.append('(');
                        if (preferredDirection == Direction.UP)
                            result.append('~');
                        result.append(names[i].toStringWithVariable(var));
                        if (parenAfter(i))
                            result.append(')');
                        if (i == names.length - 1)
                            result.append(resetSuffix());
                        result.append(' ');
                        result.append(operator(i, preferredDirection));
                        result.append(' ');
                    }
                    result.append(dest.toStringWithVariable(var));
                    result.append(preferredDirection.toString());
                    return result.toString();
                }
            }, upSection) };
    }

    public abstract NodeExpression unparameterize(int index);
    public abstract NodeExpression canonicalize(NodeCanonicalizer c);

    protected class SHSimplification implements Simplification {
        boolean isGND(int n) {
            return (names[n] == Power.GND);
        }

        boolean isVdd(int n) {
            return (names[n] == Power.Vdd);
        }

        public boolean isSimpler() {
            for (int i = 0; i < names.length; i++)
                if (isGND(i) || isVdd(i))
                    return true;
            return false;
        }
    }

    public Simplification getSimplification() {
        return new SHSimplification();
    }

    public abstract NodeExpression applySimplification(Simplification simp);
    
    public int hashCode() {
        int hc = getClass().hashCode();
        for (int i = 0; i < names.length; i++) {
            hc += names[i].hashCode();
        }
        return (reset ? hc : ~hc);
    }

    public boolean equals(Object o) {
        if (o.getClass().equals(getClass())) {
            ShorthandNodeExpression that = (ShorthandNodeExpression) o;
            if (names.length != that.names.length || reset != that.reset)
                return false;
            for (int i = 0; i < names.length; i++)
                if (!names[i].equals(that.names[i]))
                    return false;
            return true;
        } else {
            return false;
        }
    }
}
