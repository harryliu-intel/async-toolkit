/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.MathExpression;

import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;



/**
   Interface to a dictonary of variable values.
 */
public interface VariableDictionary {
    /**
       Returns the value of a variable with the specified name.
       Variable names are case sensitive.
       @param VariableName Name of the variable you want the value of.
       @return The MathExpression that is the value of the variable.
               The returned math exression may or may not have
               contain references to other variables.  If there
	       is no variable with the specified name then null is returned.
     */
    MathExpression getVariableValue( final String VariableName );

    /**
       Returns an iterator that will iterate through all the variables in 
       the dictionary.
       @return An iterator that can be used to enumerate all the variables
       in the dictionary.
     */
    VariableDictionaryIterator getIterator();

}
