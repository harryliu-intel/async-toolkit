/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariable;

import java.util.NoSuchElementException;

/**
   Iterator interface for iterating over string variable bindings
   in a WriteableStringVariableDictionary.
   @see WriteableStringVariableDictionary
 */
public interface WriteableStringVariableDictionaryIterator {
    /**
       Returns the next string variable in the dictionary if there is one.
       @exception NoSuchElementException  Thrown when there is not
       another string variable in the dictionary.
       @return The next string variable in the VariableDictionary that
       created the iterator.
       @throws NoSuchElementException
     */
    WriteableStringVariable next( );
    /**
       Determines if there is another string variable in the dictionary.
       @return true if thare is another string variable in the dictionary
     */
    boolean hasNext( );

    /**
       Removes the last string variable returned by the iterator from
       the dictionary.
     */
    void remove( );
}
