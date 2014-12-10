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
   Default implmentation of the negation operator.
 */
public class NegationOperator extends DoubleOperatorCommon {

    protected MathExpression m_x;

    /**
       Construct a negation operator expression.
       @param x Expression to negate.
     */
    public NegationOperator( MathExpression x ) {
	m_x = x;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new NegationOperator( m_x.evaluate( varBindings ) );
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
	return m_x.isConstant() ;
    }

    public double getConstantValue() throws NotAConstantValueException {
	return -( m_x.getConstantValue() );
    }

    public String[] getVariableNames() {
	return m_x.getVariableNames();
    }

    public void accept( Visitor v ) {
	v.negationOperator( m_x );
    }

    public boolean equals(Object obj) {
        if (obj instanceof NegationOperator) {
            NegationOperator o = (NegationOperator) obj;
            return m_x.equals(o.m_x);
        } else
            return false;
    }
}
