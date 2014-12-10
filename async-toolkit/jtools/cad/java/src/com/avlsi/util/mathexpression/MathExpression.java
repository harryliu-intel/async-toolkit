/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;

import com.avlsi.util.mathexpression.variable.VariableDictionary;

import com.avlsi.util.mathexpression.NotAConstantValueException;

import com.avlsi.util.mathexpression.Visitor;


import java.io.PrintWriter;

import java.math.BigDecimal;

/**
   Interface to a mathematical expression.

   This interface provides two different methods
   for getting the value of a constant expression.
   One method will return the constant value as a
   BigDecimal, the other as a double.

   Implementations can choose whether or not
   they want to use BigDecimal.  If they choose to
   use doubles instead of BigDecimals they will never ask
   a referenced expression for a BigDecimal and if someone
   asks them for a BigDecimal they will just make one
   on the fly.  Mixing and matching implementations that
   use BigDecimals and doubles may lead to trouble, but
   I think you should always have at least as much precision
   as double allows.  Implementations that only use
   double should never execute any BigDecimal code, which
   means performance of doulbe only implementation should
   not be affected by the ability of other implementations
   to use BigDecimal.  If an implementation decides
   to use BigDecimals then it will just ask BigDecimal
   to convert itself to a double when someone asks for
   the value as a double.  The Visitor interface has two
   methods for reporting constants one for BigDecimal and one
   for double.  Implementors of visitor will just have to deal
   with both cases.

   @see com.avlsi.util.mathexpression.Visitor
 */
public interface MathExpression {
    
    /**
       Evaluates the math expression using the specified
       variable bindings.
       @param varBindings Variable bindings to use when
       evaluating the expression.
       @return An expression equivalent to this expression,
       except that all variables references that could be resolved
       with the specified variable dictionary are replaced with the
       variables value in the dictionary.  Any variable references
       in this expression not resolved by the variable dictionary
       will also be in the returned expression.
     */
    MathExpression evaluate( VariableDictionary varBindings );
    
    /**
       Determines if the result of the expression is a constant.
       @return true if the expressions evaluates to a constant.
     */
    boolean isConstant( );
    
    /**
       If the expression is evaluates to a constant, this
       method will return the evaluation of the expression as
       a BigDecimal.
       @return The result of the expression if the expression
       evaluates to a constant result.  The returned BigDecimal
       object should not be modified.  Modifications of the
       returned object will have undefined side affects.
       Fortunately it appears that the BigDecimal class
       is read only.
       @exception NotAConstantValueException Thrown when the expression
       does not evaluate to a constant result.
     */
    BigDecimal getConstantValueAsBigDecimal( ) throws NotAConstantValueException;

    /**
       If the expression is evaluates to a constant, this
       method will return the evaluation of the expression as
       a double.
       @return The result of the expression if the expression
       evaluates to a constant result. 
       @exception NotAConstantValueException Thrown when the expression
       does not evaluate to a constant result.
     */
    double getConstantValue( ) throws NotAConstantValueException;

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
       @param v Visistor to call into.
     */
    void accept( Visitor v );

}
