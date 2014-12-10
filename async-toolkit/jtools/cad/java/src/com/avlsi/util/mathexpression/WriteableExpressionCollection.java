/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;


import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.WriteableExpressionCollectionIterator;

/**
   Iterface to a collection of expressions.
   @see com.avlsi.util.mathexpression.MathExpression
 */
public interface WriteableExpressionCollection extends ExpressionCollection {
    /**
       Gets an iterator that will enumerate all the expressions
       in the collection.  The returned iterator is capable of removing and
       adding expressions to the collection.
       @return An iterator that will enumerate all the expressions
       in the collection.
     */
    WriteableExpressionCollectionIterator getWriteableIterator();

    /**
       Adds an expression to the collection.
       @param expToAdd Expression to add.
    */
    void addExpression( MathExpression expToAdd );

    /**
       Removes all the expression from the collection.
     */
    void clear() ;

}
