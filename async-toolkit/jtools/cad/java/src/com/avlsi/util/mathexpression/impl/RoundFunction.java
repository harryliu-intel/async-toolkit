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
   Default implmentation of the round function.
 */
public class RoundFunction extends DoubleOperatorCommon {

    protected MathExpression m_x;

    /**
       Construct a call to the round funciton.
       @param x Expression to round to an integer.
     */
    public RoundFunction( MathExpression x ) {
	m_x = x;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new RoundFunction( m_x.evaluate( varBindings ) );
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
	return java.lang.Math.floor( m_x.getConstantValue() + 0.5d );
    }

    public String[] getVariableNames() {
	return m_x.getVariableNames();
    }

    public void accept( Visitor v ) {
	v.roundFunction( m_x );
    }

    public boolean equals(Object obj) {
        if (obj instanceof RoundFunction) {
            RoundFunction o = (RoundFunction) obj;
            return m_x.equals(o.m_x);
        } else
            return false;
    }
}
