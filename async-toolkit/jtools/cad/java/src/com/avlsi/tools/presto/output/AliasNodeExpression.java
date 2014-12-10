/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

public class AliasNodeExpression implements NodeExpression {
    private final NodeName a;
    private final int delta;

    public AliasNodeExpression(NodeName a, int delta) {
        this.a = a;
        this.delta = delta;
    }

    public NodeName getNode() {
        return a;
    }

    /**
     * Returns a string which describes adding delta to something.
     * For example, will return "+5" if delta is 5, "-5" if
     * delta is -5, and "" (the empty string) if delta is 0.
     */
    private String signedDelta() {
        if (delta == 0)
            return "";
        String result = Integer.toString(delta);
        if (result.startsWith("-"))
            return result;
        else
            return "+" + result;
    }

    public LineAndSection[] evaluate(final NodeName dest,
                                     Direction preferredDirection,
                                     Section upSection, Section downSection) {
        return new LineAndSection[] { new LineAndSection(new AbstractPrsLine() {
                public boolean isParameterized() {
                    return (a.isParameterized() || dest.isParameterized());
                }

                public String toStringWithIndex(int index) {
                    return (dest.toStringWithIndex(index) + " = " +
                            a.toStringWithIndex(index+delta) + ";");
                }

                public String toStringWithVariable(String var) {
                    return (dest.toStringWithVariable(var) + " = " +
                            a.toStringWithVariable(var+signedDelta()) + ";");
                }
            }, (preferredDirection == Direction.UP ?
                upSection : downSection)) };
    }

    public NodeExpression unparameterize(int index) {
        return new AliasNodeExpression(a.unparameterize(index+delta), 0);
    }

    public NodeExpression canonicalize(NodeCanonicalizer c) {
        return new AliasNodeExpression(c.canonicalize(a), delta);
    }

    public Simplification getSimplification() {
        // an alias cannot be simplified
        return new Simplification() {
                public boolean isSimpler() {
                    return false;
                }
            };
    }

    public NodeExpression applySimplification(Simplification simp) {
        // an alias cannot be simplified
        return this;
    }

    public int hashCode() {
        return a.hashCode() + (a.isParameterized() ? delta : 0);
    }

    public boolean equals(Object o) {
        if (o instanceof AliasNodeExpression) {
            AliasNodeExpression x = (AliasNodeExpression) o;
            return (a.equals(x.a) &&
                    (!a.isParameterized() || delta == x.delta));
        } else {
            return false;
        }
    }
}
