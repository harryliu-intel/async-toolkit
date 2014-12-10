/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;

import com.avlsi.util.mathexpression.MathExpression;

import com.avlsi.util.mathexpression.ExpressionCollectionIterator;

/**
   Interface to an iterator that iterates throw the expression
   stored in a ExpressionCollection.
   @see com.avlsi.util.mathexpression.WriteableExpressionCollection
 */
public interface WriteableExpressionCollectionIterator 
    extends ExpressionCollectionIterator
{

    /**
       Inserts the specified expression into the collection.
       The new expression is inserted before the implicit
       cursor: a subsequent call to next would be unaffected, and
       a subsequent call to previous would return the new expression.
     */
    void add( MathExpression Exp );

    /**
       Removes the last expression returned from next or previous
       from the collection of expressions.  This call can only be made
       once per call to next or previous.  It can only be called if
       WriteableExpressionCollectionIterator.add has not been called
       after the last call to next or previous.
     */
    void remove();
    
}
