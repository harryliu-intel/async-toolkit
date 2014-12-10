/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;

import com.avlsi.fast.shapes.stringexpression.StringExpression;

import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;

/**
   Interface to an iterator that iterates over the expression
   stored in a StringExpressionCollection.
   @see com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection
 */
public interface WriteableStringExpressionCollectionIterator 
    extends StringExpressionCollectionIterator
{

    /**
       Inserts the specified expression into the collection.
       The new expression is inserted before the implicit
       cursor: a subsequent call to next would be unaffected, and
       a subsequent call to previous would return the new expression.
     */
    void add( StringExpression Exp );

    /**
       Removes the last expression returned from next or previous
       from the collection of expressions.  This call can only be made
       once per call to next or previous.  It can only be called if
       WriteableStringExpressionCollectionIterator.add has not been called
       after the last call to next or previous.
     */
    void remove();
    
}
