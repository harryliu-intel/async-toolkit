/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import java.math.BigDecimal;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.util.mathexpression.Visitor;

import com.avlsi.util.mathexpression.variable.VariableDictionary;


/**
   Default implementation of a variable reference.
 */
public class VariableReference implements MathExpression {

    protected final String m_VarName;

    /**
       Consturcts a reference to a variable with the specified name.
       @param varName Name of the variable to reference.
     */
    public VariableReference( String varName ) {
	m_VarName = varName ;
    }

    public MathExpression evaluate( VariableDictionary VarBindings ) {
	MathExpression ret = VarBindings.getVariableValue( m_VarName );
	if ( ret == null ) {
	    ret = this;
	}
	return ret;
    }

    public boolean isConstant( ) { 
	return false;
    }

    public BigDecimal getConstantValueAsBigDecimal( ) 
	throws NotAConstantValueException {

	throw new NotAConstantValueException( getVariableNames() );

    }

    public double getConstantValue( ) throws NotAConstantValueException {
	throw new NotAConstantValueException( getVariableNames() );
    }

    public String[] getVariableNames( ) {
	String[] ret = new String[1];
	ret[0] = m_VarName;
	return ret;
    }

    public void accept( Visitor v ) {
	v.variable( m_VarName );
    }

    public boolean equals(Object obj) {
        if (obj instanceof VariableReference) {
            VariableReference o = (VariableReference) obj;
            return m_VarName.equals(o.m_VarName);
        } else
            return false;
    }
}
