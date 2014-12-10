/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;



/**
   Exception thrown by the GetConstantValue method of MathExpression,
   when the expression did not evaluate to a constant value.
   @see MathExpression
 */
public class NotAConstantValueException extends Exception {
    protected String[] m_UnboundVariables;
    /**
       @param UnboundVariables An array of strings where
       each element of the string is the name of a variable
       that did not have a value when the expression was evaluated.
     */
    public NotAConstantValueException( String[] UnboundVariables ) {
	m_UnboundVariables = UnboundVariables;
    }

    /**
       @return An array of strings where
       each element of the string is the name of a variable
       that did not have a value when the expression was evaluated.
    */
    public String[] getUnboundVariables() {
	return m_UnboundVariables;
    }
}
