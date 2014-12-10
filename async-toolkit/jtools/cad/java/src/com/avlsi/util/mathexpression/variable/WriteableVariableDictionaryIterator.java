/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.variable.WriteableVariable ;

import java.util.NoSuchElementException;

/**
   Iterator interface for iterating over variable bindings
   in a WriteableVariableDictionary.
   @see WriteableVariableDictionary
 */
public interface WriteableVariableDictionaryIterator {
    /**
       Returns the next variable in the dictionary if there is one.
       @exception NoSuchElementException  Thrown when there is not
       another variable in the dictionary.
       @return The next variable in the VariableDictionary that
       created the iterator.
      
     */
    WriteableVariable next( );
    /**
       Determines if there is another variable in the dictionary.
       @return true if thare is another variable in the dictionary
     */
    boolean hasNext( );

    /**
       Removes the last variable returned by the iterator from
       the dictionary.
     */
    void remove( );

}
