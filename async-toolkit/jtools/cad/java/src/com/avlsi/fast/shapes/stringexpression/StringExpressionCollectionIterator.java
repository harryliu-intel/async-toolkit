/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;

import java.util.NoSuchElementException;

import com.avlsi.fast.shapes.stringexpression.StringExpression;

/**
   Interface to an iterator that iterates throw the expression
   stored in a StringExpressionCollection.
   @see com.avlsi.fast.shapes.stringexpression.StringExpressionCollection
 */
public interface StringExpressionCollectionIterator {

    /**
       Gets the next expression in the iteration.
       @exception NoSuchElementException Thrown when there
       is not another expression in the iteration.
       @return The next expression in the iteration of the
       expression collection.

       @throws NoSuchElementException
       
     */
    StringExpression next();

    /**
       Gets the previous expression in the iteration.
       @exception NoSuchElementException Thrown when there
       is not a previous expression in the iteration.
       @return The previous expression in the iteration of the
       expression collection.

       @throws NoSuchElementException
       
     */
    StringExpression previous();

    /**
       Determines if there is another expression in the iteration
       of the expression collection.
       @return true if there is another expression in the
       iterator of the expression collection.
     */
    boolean hasNext();

    /**
       Determines if there is a previous expression in the iteration
       of the expression collection.
       @return true if there is a previous expression in the
       iterator of the expression collection.
     */
    boolean hasPrevious();

}
