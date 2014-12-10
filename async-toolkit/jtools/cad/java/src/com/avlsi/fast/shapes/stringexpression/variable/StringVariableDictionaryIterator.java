/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariable;

import java.util.NoSuchElementException;

/**
   Iterator interface for iterating over variable bindings
   in a StringVariableDictionary.
   @see StringVariableDictionary
 */
public interface StringVariableDictionaryIterator {
   /**
       Returns the next string variable in the dictionary if 
       there is one.
       @exception NoSuchElementException  Thrown when there is not
       another string variable in the dictionary.
       @return The next string variable in the StringVariableDictionary that
       created the iterator.

       @throws NoSuchElementException
      
     */
    StringVariable next();
    /**
       Determines if there is another string variable in the dictionary.
       @return true if thare is another string variable in the dictionary
     */
    boolean hasNext();
}
