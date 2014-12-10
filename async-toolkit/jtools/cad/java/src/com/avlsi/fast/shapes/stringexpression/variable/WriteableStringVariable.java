/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariable;

import com.avlsi.fast.shapes.stringexpression.StringExpression;

/**
   Interface to a string variable.
 */
public interface WriteableStringVariable extends StringVariable {
    
    /**
       Sets the value of the string variable to the specified string.
       @param NewValue The new value of the variable.
     */
    void setValue( StringExpression NewValue );

}
