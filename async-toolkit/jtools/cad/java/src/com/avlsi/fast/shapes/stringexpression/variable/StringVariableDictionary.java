/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionaryIterator;

import com.avlsi.fast.shapes.stringexpression.StringExpression;


/**
   Interface to a dictionary of string variable values.
 */
public interface StringVariableDictionary {
    
    /**
       Returns the value of a string variable with the specified name.
       Variable names are case sunsitive.
       @param VariableName Name of the string variable you want the value of.
       @return The value of the string variable if the variable exists in the
       dictionary, null otherwise.  The returned StringExpression can
       contain references to other StringExpressions.
     */
    StringExpression getVariableValue( final String VariableName );

    /**
       Returns an iterator that will iterate through all the string variables in
       the dictionary.
       @return An iterator that can be used to enumerate all the string 
       variables in the dictionary.
     */
    StringVariableDictionaryIterator getIterator();

}
