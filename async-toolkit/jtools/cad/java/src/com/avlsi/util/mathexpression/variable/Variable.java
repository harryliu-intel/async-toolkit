/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Inteface to a variable.
 */
public interface Variable {
    /**
       Retrieves the name of the variable.
       @return The name of the variable.
     */
    String getName( ) ;
    /**
       Retrieves the value of the variable
       @return A MathExpression which is the value of the variable.
       The MathExpression may use variables.
     */
    MathExpression getValue( );
}
