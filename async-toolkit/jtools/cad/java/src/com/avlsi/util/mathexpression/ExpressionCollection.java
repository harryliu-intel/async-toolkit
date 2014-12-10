/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;

import com.avlsi.util.mathexpression.ExpressionCollectionIterator;

/**
   Iterface to a collection of expressions.
   @see com.avlsi.util.mathexpression.MathExpression
 */
public interface ExpressionCollection {
    /**
       Gets an iterator that will enumerate all the expressions
       in the collection.
       @return An iterator that will enumerate all the expressions
       in the collection.
     */
    ExpressionCollectionIterator getIterator();

    /**
       Determins if all the members of the collection are constant.
       @return true if all the expressions in the collection
       have constant values.
     */
    boolean allMembersAreConstant( );

    /**
       @return The number of expressions in the collection.
    */
    int size();

}
