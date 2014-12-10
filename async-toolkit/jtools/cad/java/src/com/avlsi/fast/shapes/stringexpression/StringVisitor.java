/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;


import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;

/**
   Visitor interface used for StringExpressions.
 */
public interface StringVisitor {
    void constant(final String constValue);

    void variable(final String varName);

    void concatOperator(final StringExpressionCollection terms);

    void subExpressionReference(final StringVariableDictionary subBindings,
                                final StringExpression subExpression);
}
