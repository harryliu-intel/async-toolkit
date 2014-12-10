/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringVisitor;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;




/**
   Default implementation of StringExpression that implements
   a constant value.
 */
public class Constant implements StringExpression {

    protected String m_Val;

    /**
       Construct a constant value string expression object
       with the specified value.
       @param val Value of the constant.  The specified String object will be
       referenced by the constructed object, thus changes to the specified
       object will change the value of the constructed object.
     */
    public Constant( final String val ) {
	m_Val = val;
    }

    public StringExpression evaluate( StringVariableDictionary VarBindings ) {
	return this;
    }

    public boolean isConstant( ) {
	return true;
    }
    
    public String getConstantValue( ) {
	return m_Val;
    }

    public String[] getVariableNames() {
	return new String[0];
    }
    
    public void accept(StringVisitor v) {
        v.constant(m_Val);
    }
}
