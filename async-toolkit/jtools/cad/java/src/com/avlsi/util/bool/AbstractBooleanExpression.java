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

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Collectors;

/**
 * Base class for boolean expressions.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public abstract class AbstractBooleanExpression implements BooleanExpressionInterface {
    /**
     * If this flag is false, then the entire expression has the negated
     * sense.
     **/
    final boolean sense;

    public abstract BooleanExpressionInterface negated();

    /**
     * converts this expression to DNF.
     **/
    public abstract OrBooleanExpressionInterface DNFForm();

    /**
     * converts this expression to CNF.
     **/
    public abstract AndBooleanExpressionInterface CNFForm();

    /**
     * Class constructor.
     * @param sense   non-negated sense flag.  If false, expression
     *   is negated.
     **/
    protected AbstractBooleanExpression(final boolean sense) {
        this.sense = sense;
    }

    /**
     * Returns the sense of the expression, <code>false</code> for
     * negated, <code>true</code> for non-negated.
     **/
    public boolean getSense() {
        return sense;
    }

    /**
     * Helper for AndBooleanExpression /
     * OrBooleanExpression.toUserVisibleString.
     *
     * @param terms  the list of terms
     * @param unit  value if there are no terms in expression,
     *     ie "true" / "false"
     * @param op  the operation, ie '&', '|'
     **/
    String toUserVisibleString(
            final Collection<BooleanExpressionInterface> terms,
            final String unit, final char op) {
        final StringBuilder sb = new StringBuilder();

        if (!getSense()) {
            sb.append("~");
        }

        if (terms.isEmpty()) {
            sb.append(unit);
        } else {
            sb.append(terms.stream()
                           .map(t -> t.toUserVisibleString())
                           .collect(Collectors.joining(Character.toString(op),
                                                       "(", ")")));
        }

        return sb.toString();
    }


    public static Collection<BooleanExpressionInterface>
    negateTerms(Collection<BooleanExpressionInterface> terms) {
        return terms.stream()
                    .map(e -> e.negated())
                    .collect(Collectors.toList());
    }

    static boolean oneLevel(final Collection<BooleanExpressionInterface> terms) {
        return terms.stream()
                    .allMatch(e -> e instanceof AbstractAtomicBooleanExpression);
    }
}
