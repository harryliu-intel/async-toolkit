/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;

import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.StringVisitor;


/**
   Iterface to a string expression.
 */
public interface StringExpression {

    /**
       Evaluates the string expression using the specified
       variable bindings.
       @param VarBindings Variable bindings to use when
       evaluating the expression.
       @return An expression equivalent to this expression,
       except that all variables references that could be resolved
       with the specified variable dictionary are replaced with the
       variables value in the dictionary.  Any variable references
       in this expression not resolved by the variable dictionary
       will also be in the returned expression.
     */
    StringExpression evaluate( StringVariableDictionary VarBindings );
    
    /**
       Determines if the result of the expression is a constant.
       @return true if the expressions evaluates to a constant.
     */
    boolean isConstant( );

    /**
       If the expression is evaluates to a constant, this
       method will return the evaluation of the expression as
       a String.
       @return The result of the expression if the expression
       evaluates to a constant result.
       @exception NotAConstantValueException Thrown when the expression
       does not evaluate to a constant result.
     */
    String getConstantValue() throws NotAConstantValueException;

    /**
       Returns an array of strings where each string in the array
       is the name of a variable referenced by this expression.
       @return An array of strings where each string in the array
       is the name of a variable referenced by this expression.
     */
    String[] getVariableNames( );

    /**
       Calls the appropriate method in the visistor interface given
       the implementation of this interface.  This method will
       not call accept on any referenced math expression directly,
       it is up to the implementation of the visitor interface
       to visit any referenced math expression.
       @param v StringVisistor to call into.
     */
    void accept(StringVisitor v);
}
