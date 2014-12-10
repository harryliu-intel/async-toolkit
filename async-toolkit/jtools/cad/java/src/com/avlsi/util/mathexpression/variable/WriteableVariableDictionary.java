/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.MathExpression;

import com.avlsi.util.mathexpression.variable.WriteableVariableDictionaryIterator;


/**
   Interface to a variable dictionary that can be changed.
 */
public interface WriteableVariableDictionary extends VariableDictionary {
    /**
       Binds a MathExpression to a variable with the specified name.
       @param VariableName The name of the variable to bind the specified
       expression to.
       @param Exp The new value of the variable.  The expression may reference
       other variable.
       @return false when there was already a variable with the same
       name.
     */
    boolean bindVariable( final String variableName, 
			  final MathExpression exp );
    
    /**
       Returns an iterator that will iterate through all the variables in 
       the dictionary.
       @return An iterator that can be used to enumerate all the variables
       in the dictionary.
     */
    WriteableVariableDictionaryIterator getWriteableIterator();

    void unBindVariable( final String variableName );

}
