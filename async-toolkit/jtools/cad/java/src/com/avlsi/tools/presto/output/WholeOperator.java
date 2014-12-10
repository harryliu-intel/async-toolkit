/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * A WholeOperator represents a pair of half-operators.  (Duh!)
 * The destination and NodeExpression might be parameterized, which is
 * why we need to store an index in the WholeOperator.
 */

public class WholeOperator {
    public final NodeName dest;
    public final NodeExpression expr;
    public final int index;
    public final Direction preferredDirection;
    public final Section upSection;
    public final Section downSection;

    // The most general constuctor.
    public WholeOperator(NodeName dest, NodeExpression expr, int index,
                         Direction preferredDirection,
                         Section upSection, Section downSection) {
        this.dest = dest;
        this.expr = expr;
        this.index = index;
        this.preferredDirection = preferredDirection;
        this.upSection = upSection;
        this.downSection = downSection;
    }

    // Constructor without an index
    public WholeOperator(NodeName dest, NodeExpression expr,
                         Direction preferredDirection,
                         Section upSection, Section downSection) {
        this(dest, expr, -1, preferredDirection, upSection, downSection);
    }

    // Constructor with only one section
    public WholeOperator(NodeName dest, NodeExpression expr, int index,
                         Direction preferredDirection, Section upSection) {
        this(dest, expr, index, preferredDirection, upSection, upSection);
    }

    // Constructor with only one section and no index
    public WholeOperator(NodeName dest, NodeExpression expr,
                         Direction preferredDirection, Section upSection) {
        this(dest, expr, preferredDirection, upSection, upSection);
    }
}
