/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;

import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariableDictionaryIterator;

import com.avlsi.fast.shapes.stringexpression.StringExpression;

/**
   Interface to a variable dictionary that can be changed.
 */
public interface WriteableStringVariableDictionary 
    extends StringVariableDictionary {
     /**
       Binds a StringExpression to a string variable with the specified name.
       @param VariableName The name of the string variable to bind the specified
       expression to.
       @param Value The new value of the string variable.  
       @return false when there was already a string variable with the same
       name.
     */
    boolean bindVariable( final String VariableName, 
			  final StringExpression Value );

     /**
       Unbinds a string variable with the specified name.
       @param VariableName The name of the string variable to unbind.
     */
    void unBindVariable( final String VariableName );

    /**
       Returns an iterator that will iterate through all the string variables in 
       the dictionary.
       @return An iterator that can be used to enumerate all the string variables
       in the dictionary.
     */
    WriteableStringVariableDictionaryIterator getWriteableIterator();

}
