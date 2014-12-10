/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;


import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.variable.VariableDictionary;

import java.math.BigDecimal;

/**
   Visitor interface used for MathExpressions.
   You can pass an instance of an object that implements
   this interface to the accept method of MathExpression.
 */
public interface Visitor {

    /**
       Called by implementations of MathExpression that implement
       constant values using a BigDecimal.
       @param constValue The value of the constant in the expression.
     */
    void constant( final BigDecimal constValue );
    /**
       Called by implementations of MathExpression that implement
       constant values using a double.
       @param constValue The value of the constant in the expression.
     */
    void constant( final double constValue );
    /**
       Called by implementations of MathExpression that implement
       variables references.
       @param varName The name of the referenced variable.  The
       name of the variable is case sensitive.
     */
    void variable( final String varName );

    /**
       Called by implementations of MathExpression that implement
       the negation operator.
       @param x The expression that is being negated.
     */
    void negationOperator( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the plus operator.
       @param terms A collection of expressions that the plus
       operator is adding together.
     */
    void plusOperator( final ExpressionCollection terms );
    /**
       Called by implementations of MathExpression that implement
       the minus operator.  a - b
       @param a The expression that b is being subtracted from.
       @param b The expression being subtracted from a.
     */
    void minusOperator( final MathExpression a, 
			final MathExpression b );
    /**
       Called by implementations of MathExpression that implement
       the divide operator.
       @param dividend The expression being divided by divisor.
       @param divisor  The expression that dividend is being divided by.
     */
    void divideOperator( final MathExpression dividend, 
			 final MathExpression divisor );

    /**
       Called by implementations of MathExpression that implement
       the times operator.
       @param terms A collection of expressions that are being multiplied.
     */
    void timesOperator( final ExpressionCollection terms );

    /**
       Called by implementations of MathExpression that implement
       the modulo operator.
       @param dividend The expression being divided by divisor.
       @param divisor  The expression that dividend is being divided by.
     */
    void modOperator( final MathExpression dividend,
		      final MathExpression divisor );
 
    /**
       Called by implementations of MathExpression that implement
       the exponent function.  The result of the function
       will be e raised to the power of the result of the expression
       x.
       @param x The expression x.
     */
    void expFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the natural log function.  The result of the function
       will be the logorithm of the result of the expression x.
       @param x The expression x.
     */
    void logFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the sine function.  The result of the function
       will be the sine of the result of the expression x.
       The result of expression x is assumed to be in radians.
       @param x The expression whose result is the number of radians to
       take the sine of.
     */
    void sinFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the cosine function.  The result of the function
       will be the cosine of the result of the expression x.
       The result of expression x is assumed to be in radians.
       @param x The expression whose result is the number of radians to
       take the cossine of.
     */
    void cosFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the tangent function.  The result of the function
       will be the tangent of the result of the expression x.
       The result of expression x is assumed to be in radians.
       @param x The expression whose result is the number of radians to
       take the tangent of.
     */
    void tanFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the arc sine function.  The result of the function
       will be the arc sine of the result of the expression x.
       The result of the arc sine function will be in radians.
       @param x The expression x.
     */
    void arcsinFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the arc cosine function.  The result of the function
       will be the arc cosine of the result of the expression x.
       The result of the arc cosine function will be in radians.
       @param x The expression x.
     */
    void arccosFunction( final MathExpression x );

   /**
       Called by implementations of MathExpression that implement
       the arc tangent function.  The result of the function
       will be the arc tangent of the result of the expression x.
       The result of the arc tangent function will be in radians.
       @param x The expression x.
     */
    void arctanFunction( final MathExpression x );
    
    /**
       Called by implementations of MathExpression that implement
       the ceiling function.  The result of the ceiling function
       is the smallest integer not less than the result of the
       expression x.
       @param x The expression x.
     */
    void ceilFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the round function.  The result of the round function
       is the closest integer to the result of the expression x.
       If the expression x evaluates to a number that equidistant
       to two integers the larger is the result.  The round
       function is equivalent to floor( x + 0.5 ).
       @param x The expression x.
     */
    void roundFunction( final MathExpression x );

     /**
       Called by implementations of MathExpression that implement
       the floor function.  The result of the floor function
       is the largest integer not greater than the result of the
       expression x.
       @param x The expression x.
     */
    void floorFunction( final MathExpression x );

    /**
       Called by implementations of MathExpression that implement
       the less than operator.  The lessThan operator compares the result
       of expression a to the result of expression b.  If the result of expression
       a is less than the result of expression b then the result of
       the less than operator will be the result of the trueResult expression,
       otherwise the result of the less than operator will be the
       result of the falseResult expression.
       @param a Expression a.
       @param b Expression b.
       @param trueResult The result of the operator if the result
       of expression a is less than the result of expression b.
       @param falseResult The result of the operator if the result
       of expression a is not less than the result of expression b.
     */
    void lessThanOperator( final MathExpression a,
			   final MathExpression b,
			   final MathExpression trueResult,
			   final MathExpression falseResult );

    /**
       Called by implementations of MathExpression that implement
       the summation operator.  The value of the index variable
       is incremented by the integer 1.  The initial value of the
       index variable is the result of the indexFirstValue expression.
       The sum continues until the value of the index variable is
       greater than the result of the indexLastValue expression.
       The first value of the index variable that is greater than
       the result of the indexLastValue expression will not be used
       to generate a term in the summation.  The indexFirstValue expression
       and the indexLastValue expression do not need to evaluate to integers.
       @param indexName The name of the variable referenced by the
       MathExpression specified in termExpression that changes for
       each term in the summation.
       @param indexFirstValue An expression that evaluates to the first
       value of the index for the summation.
       @param indexLastValue An expression that evaluates to the last
       value of the index for the summation.
     */
    void sumOperator( final String indexName,
		      final MathExpression indexFirstValue,
		      final MathExpression indexLastValue,
		      final MathExpression termExpression );

    /**
       Called by implementations of MathExpression that implement
       a reference to a sub-expression.  Variables used in the
       sub-expression can be rebound to variables in the referencing
       expression.
       @param subBindings Dictionary that binds variables used in the
       sub-expression to constants and variables in the referencing expression.
       @param subExpression The sub-expression.
     */
    void subExpressionReference( final VariableDictionary subBindings,
				 final MathExpression subExpression );

}
