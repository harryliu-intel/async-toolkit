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
   Default implmentation of minus operator.
 */
public class MinusOperator extends DoubleOperatorCommon {

    protected MathExpression m_Left;
    protected MathExpression m_Right;

    /**
       Construct a minus operator.
       @param left The expression right will be subtracted from.
       @param right The expression that will be substracted from left.
     */
    public MinusOperator( MathExpression left,
			  MathExpression right ) {
	m_Left = left;
	m_Right = right;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new MinusOperator( m_Left.evaluate( varBindings ),
				     m_Right.evaluate( varBindings ) );
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
	return ( m_Left.isConstant() && m_Right.isConstant() );
    }

    public double getConstantValue() throws NotAConstantValueException {
	return m_Left.getConstantValue() - m_Right.getConstantValue();
    }

    public String[] getVariableNames() {
	return getReferencedVariableNames( m_Left, m_Right );
    }

    public void accept( Visitor v ) {
	v.minusOperator( m_Left, m_Right );
    }

    public boolean equals(Object obj) {
        if (obj instanceof MinusOperator) {
            MinusOperator o = (MinusOperator) obj;
            return m_Left.equals(o.m_Left) && m_Right.equals(o.m_Right);
        } else
            return false;
    }
}
