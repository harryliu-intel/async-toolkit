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
import java.util.Iterator;

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
    String toUserVisibleString(final Collection terms,
                               final String unit,
                               final char op) {
        final StringBuffer sb = new StringBuffer();

        if (!sense)
            sb.append("~");

        final Iterator i = terms.iterator();
        if (!i.hasNext())
            sb.append(unit);
        else {
            sb.append('(');

            boolean first = true;
            while (i.hasNext()) {
                if (first)
                    first = false;
                else
                    sb.append(op);

                sb.append(((BooleanExpressionInterface) i.next())
                        .toUserVisibleString());
            }

            sb.append(')');
        }

        return sb.toString();
    }


    public static Collection negateTerms(Collection terms) {
            ArrayList rv = new ArrayList(terms.size());
            Iterator i=terms.iterator();
            while (i.hasNext()) {
                AbstractBooleanExpression e = (AbstractBooleanExpression)
                    i.next();
                rv.add(e.negated());
            }
            return rv;
    }
}
