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
   Default implmentation of the less than operator.
 */
public class LessThanOperator extends DoubleOperatorCommon {

    protected final MathExpression m_a;
    protected final MathExpression m_b;
    protected final MathExpression m_TrueResult;
    protected final MathExpression m_FalseResult;

    /**
       Construct a less than operator.
       @param a The expression a.
       @param b The expression b.
       @param trueResult The result of the operator if a is
       less than b.
       @param falseResult The result of the operator if a is
       not less than b.
     */
    public LessThanOperator( MathExpression a,
			     MathExpression b,
			     MathExpression trueResult,
			     MathExpression falseResult ) {
	m_a = a;
	m_b = b;
	m_TrueResult = trueResult;
	m_FalseResult = falseResult;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new LessThanOperator( m_a.evaluate( varBindings ),
					m_b.evaluate( varBindings ),
					m_TrueResult.evaluate( varBindings ),
					m_FalseResult.evaluate( varBindings ) );
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
	boolean ret = false;
	
	if ( ( m_a.isConstant() ) && ( m_b.isConstant() ) ) {
	    try {
		if ( m_a.getConstantValue() < m_b.getConstantValue() ) {
		    ret = m_TrueResult.isConstant();
		}
		else {
		    ret = m_FalseResult.isConstant();
		}
	    }
	    catch( NotAConstantValueException e ){
		Debug.assertTrue( false, 
			      "Caught NotAConstantValueException " +
			      "even though I called isConstant first." );
	    }
	}
	return ret;
    }

    public double getConstantValue() throws NotAConstantValueException {
	double ret ;
	if ( m_a.getConstantValue() < m_b.getConstantValue() ) {
	    ret = m_TrueResult.getConstantValue() ;
	}
	else {
	    ret = m_FalseResult.getConstantValue();
	}
	return ret;
    }

    public String[] getVariableNames() {
	return getReferencedVariableNames( m_a, 
					   m_b,
					   m_TrueResult,
					   m_FalseResult );
    }

    public void accept( Visitor v ) {
	v.lessThanOperator( m_a, m_b, m_TrueResult, m_FalseResult );
    }

    public boolean equals(Object obj) {
        if (obj instanceof LessThanOperator) {
            LessThanOperator o = (LessThanOperator) obj;
            return m_a.equals(o.m_a) && m_b.equals(o.m_b) &&
                   m_TrueResult.equals(o.m_TrueResult) &&
                   m_FalseResult.equals(o.m_FalseResult);
        } else
            return false;
    }
}
