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
   Default implmentation of the arc cosine function.
 */
public class ArccosFunction extends DoubleOperatorCommon {

    protected MathExpression m_x;

    /**
       Construct a call to the arc cosine function.
       @param x An expression that evaluates to
       a number between -1 and 1.
     */
    public ArccosFunction( MathExpression x ) {
	m_x = x;
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    ret = new ArccosFunction( m_x.evaluate( varBindings ) );
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
	return java.lang.Math.acos( m_x.getConstantValue() );
    }

    public String[] getVariableNames() {
	return m_x.getVariableNames();
    }

    public void accept( Visitor v ) {
	v.arccosFunction( m_x );
    }

    public boolean equals(Object obj) {
        if (obj instanceof ArccosFunction) {
            ArccosFunction o = (ArccosFunction) obj;
            return m_x.equals(o.m_x);
        } else 
            return false;
    }
}
