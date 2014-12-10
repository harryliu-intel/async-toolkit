/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Function call expression, represents <code> f(x, y) </code>.
 *
 * <p> This class is flexible enough to handle arrays of functions, or
 * functions returning functions, but none of those are allowed
 * by the CSP language at this point. </p>
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class FunctionCallExpression
    extends AbstractASTNode
    implements ExpressionInterface {

    /**
     * Expression for function to call.  May not be null.
     **/
    private final ExpressionInterface functionExpr;

    /**
     * List of {@link ExpressionInterface}s for actual parameters to 
     * function.  May not be null.
     **/
    private final List<ExpressionInterface> actuals;

    /**
     * Class constructor.  Actual parameter list starts empty.
     *
     * @param functionExpr  expression for function to call, not null
     **/
    public FunctionCallExpression(final ExpressionInterface functionExpr) {
        this.functionExpr = functionExpr;
        this.actuals = new ArrayList<ExpressionInterface>();
    }

    /**
     * Returns the function expression.
     *
     * @return expression for function to call, not null
     **/
    public ExpressionInterface getFunctionExpression() {
        return functionExpr;
    }

    /**
     * Returns an iterator over the acutal parameters
     *
     * @return Iterator of {@link ExpressionInterface}s over the
     *     actual parameters.  May not be null.
     **/
    public Iterator<ExpressionInterface> getActuals() {
        return actuals.iterator();
    }

    /**
     * Adds an actual parameter to the list.
     *
     * @param expr  expression for actual parameter, not null
     **/
    public void addActual(final ExpressionInterface expr) {
        actuals.add(expr);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitFunctionCallExpression(this);
    }
}
