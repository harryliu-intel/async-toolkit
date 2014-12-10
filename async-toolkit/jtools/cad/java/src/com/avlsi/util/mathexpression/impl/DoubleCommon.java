/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.NotAConstantValueException;


import java.math.BigDecimal;

/**
   Root class of all implementations of MathExpression that use
   doubles.
 */
public abstract class DoubleCommon implements MathExpression {
    
    public BigDecimal getConstantValueAsBigDecimal( ) 
    throws NotAConstantValueException {
	return new BigDecimal( getConstantValue() ) ;
    }

}
