/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import com.avlsi.util.mathexpression.impl.DoubleCommon;
import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.Visitor;
import java.io.PrintWriter;



/**
   Default implementation of MathExpression that implements
   a constant value.
   
   
 */
public class Constant extends DoubleCommon {

    protected double m_Val;

    /**
       Construct a constant value math expression object
       with the specified value.
       @param val Value of the constant.  The specified
       BigDecimal object will be referenced by the constructed object,
       thus changes to the specified object will change the value
       of the constructed object.
     */
    public Constant( double val ) {
	m_Val = val;
    }

    public MathExpression evaluate( VariableDictionary VarBindings ) {
	return this;
    }

    public boolean isConstant( ) {
	return true;
    }
    
    public double getConstantValue( ) {
	return m_Val;
    }

    public String[] getVariableNames() {
	return new String[0];
    }
    
    public void accept( Visitor v ) {
	v.constant( m_Val );
    }
	
    public boolean equals(Object obj) {
        if (obj instanceof Constant) {
            Constant o = (Constant) obj;
            return m_Val == o.m_Val;
        } else
            return false;
    }
}
