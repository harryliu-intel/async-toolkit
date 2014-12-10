/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;


import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollectionIterator;

/**
   Iterface to a collection of expressions.
   @see com.avlsi.fast.shapes.stringexpression.StringExpression
 */
public interface WriteableStringExpressionCollection extends StringExpressionCollection {
    /**
       Gets an iterator that will enumerate all the expressions
       in the collection.  The returned iterator is capable of removing and
       adding expressions to the collection.
       @return An iterator that will enumerate all the expressions
       in the collection.
     */
    WriteableStringExpressionCollectionIterator getWriteableIterator();

    /**
       Adds an expression to the collection.
       @param expToAdd Expression to add.
    */
    void addExpression( StringExpression expToAdd );

    /**
       Removes all the expression from the collection.
     */
    void clear() ;

}
