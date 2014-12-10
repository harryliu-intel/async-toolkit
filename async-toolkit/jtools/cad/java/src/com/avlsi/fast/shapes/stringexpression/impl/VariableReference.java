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

import com.avlsi.util.mathexpression.NotAConstantValueException;


/**
   Default implementation of a variable reference.
 */
public class VariableReference implements StringExpression {

    protected final String m_VarName;

    /**
       Consturcts a reference to a variable with the specified name.
       @param varName Name of the variable to reference.
     */
    public VariableReference( String varName ) {
        m_VarName = varName ;
    }

    public StringExpression evaluate( StringVariableDictionary VarBindings ) {
        StringExpression ret = VarBindings.getVariableValue( m_VarName );
        if ( ret == null ) {
            ret = this;
        }
        return ret;
    }

    public boolean isConstant( ) {
        return false;
    }

    public String getConstantValue( ) throws NotAConstantValueException {
        throw new NotAConstantValueException( getVariableNames() );
    }

    public String[] getVariableNames( ) {
        String[] ret = new String[1];
        ret[0] = m_VarName;
        return ret;
    }

    public void accept(StringVisitor v) {
        v.variable(m_VarName);
    }
}
