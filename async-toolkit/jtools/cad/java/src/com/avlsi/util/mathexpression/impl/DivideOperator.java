/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.util.mathexpression.Visitor;

import com.avlsi.util.mathexpression.variable.VariableDictionary;

import com.avlsi.util.mathexpression.impl.DoubleOperatorCommon;

/**
   Default implmentation of the divide operator.
 */
public class DivideOperator extends DoubleOperatorCommon {

    protected MathExpression m_Dividend;
    protected MathExpression m_Divisor;

    /**
       Construct a divide operator.
       @param dividend The dividend.
       @param divisor The divisor.
     */
    public DivideOperator( MathExpression dividend,
			   MathExpression divisor ) {
	m_Dividend = dividend;
	m_Divisor = divisor;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new DivideOperator( m_Dividend.evaluate( varBindings ),
				      m_Divisor.evaluate( varBindings ) );
	}
	if ( ret.isConstant() ) {
	    try {
		ret = makeConstantExpression( ret.getConstantValue() );
	    }
	    catch( NotAConstantValueException e ){
		Debug.assertTrue( false, 
			      "Caught NotAConstantValueException " +
			      "even though I called isConstant first." );
	    }
	}
	return ret;
    }

    public boolean isConstant() {
	return ( m_Dividend.isConstant() && m_Divisor.isConstant() );
    }

    public double getConstantValue() throws NotAConstantValueException {
	return m_Dividend.getConstantValue() / m_Divisor.getConstantValue();
    }

    public String[] getVariableNames() {
	return getReferencedVariableNames( m_Dividend, m_Divisor );
    }

    public void accept( Visitor v ) {
	v.divideOperator( m_Dividend, m_Divisor );
    }

    public boolean equals(Object obj) {
        if (obj instanceof DivideOperator) {
            DivideOperator o = (DivideOperator) obj;
            return m_Dividend.equals(o.m_Dividend) &&
                   m_Divisor.equals(o.m_Divisor);
        } else
            return false;
    }
}
