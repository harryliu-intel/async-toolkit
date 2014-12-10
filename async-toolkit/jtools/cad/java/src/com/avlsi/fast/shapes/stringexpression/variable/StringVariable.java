/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.StringExpression;



/**
   Interface to a string variable.
 */
public interface StringVariable {
    /**
       Retrieves the name of the variable.
       @return The name of the variable.
     */
    String getName( );
    /**
       Retrieves the value of the variable.
       @return A string which is the value of the variable.
     */
    StringExpression getValue( );
}
