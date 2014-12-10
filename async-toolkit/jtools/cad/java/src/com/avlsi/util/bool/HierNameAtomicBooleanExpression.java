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

package com.avlsi.util.bool;

import java.util.ArrayList;
import java.util.Collection;

import com.avlsi.file.common.HierName;

/**
 * Base class for boolean expressions.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public class HierNameAtomicBooleanExpression
        extends AbstractAtomicBooleanExpression
        implements Comparable<HierNameAtomicBooleanExpression> {
    HierName name;
    
    /**
     * Constructor.
     **/
    public HierNameAtomicBooleanExpression(boolean nonNegated, HierName n) {
        super(nonNegated);
        name = n;
    }

    /* Satisfying interfaces */
    public BooleanExpressionInterface negated() {
        return new HierNameAtomicBooleanExpression(!sense, name);
    }

    /**
     * For simplification, etc.
     **/
    public boolean equals(Object obj) {
        return obj instanceof HierNameAtomicBooleanExpression
            && equals((HierNameAtomicBooleanExpression)obj);
    }

    public final boolean equals(HierNameAtomicBooleanExpression obj) {
        return (sense == obj.sense) && (name.equals(obj.name));
    }

    public int hashCode() {
        return name.hashCode();
    }

    public HierName getName() {
        return name;
    }

    public String toString() {
        return toUserVisibleString();
    }

    //
    // implementing BooleanExpressionInterface
    //
    public String toUserVisibleString() {
        final StringBuffer sb = new StringBuffer();
        if (!sense)
            sb.append("~");
        sb.append('"').append(name.getAspiceString()).append('"');
        return sb.toString();
    }

    public void visitWith(BooleanExpressionVisitorInterface visitor) {
        visitor.visit(this);
    }

    public int compareTo(HierNameAtomicBooleanExpression o) {
        int x = getName().compareTo(o.getName());
        if (x != 0) return x;

        if (getSense() == o.getSense()) return 0;
        else return o.getSense() ? -1 : 1;
    }
}
