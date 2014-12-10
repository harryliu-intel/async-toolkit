/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression;


import java.math.BigDecimal;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.WriteableExpressionCollection;

import com.avlsi.util.mathexpression.variable.VariableDictionary;

/**
   Factory interface to be implemented by all collections of implementations
   of the MathExpression interface.  This interface provides methods
   to make all the types of operators mentioned in the visitor interface.
 */
public interface MathExpressionFactory {
    
    /**
       Method to make an empty collection of expressions.
       @return An empty collection of expressions.
     */
    WriteableExpressionCollection makeExpressionCollection( ) ;

    /**
       Makes an expression that evaluates to the specified
       constant value.
       @param constValue The value the returned expression will
       evaluate to.
       @return An expression that evaluates to the specified
       constant value.  The returned expression's isConstant method
       should return true.
     */
    MathExpression makeConstant( final double constValue );
    
    /**
       Makes an expression that evaluates to the specified
       constant value.
       @param constValue The value the returned expression will
       evaluate to.
       @return An expression that evaluates to the specified
       constant value.  The returned expression's isConstant method
       should return true.
     */
    MathExpression makeConstant( final BigDecimal constValue );
    
    /**
       Makes a reference to the specified variable.
       @param varName The name of the variable to reference.
       @return An expression that is a reference to the specified variable.
     */
    MathExpression makeVariableReference( final String varName );

    /**
       Makes an expression that will evaluate to the negation of
       the specified expression.
       @param x The expression to be negated in the resulting expression.
       @return An expression that is the negation of the specified
       expression x.
     */
    MathExpression makeNegationOperator( final MathExpression x );

    /**
       Makes an expression that is the sum of the expression in the
       specified collection of expressions.
       @param terms The collection of expressions to be added together in
       the returned expresion.
       @return An expression that is the sum of the expressions in the
       collection.
     */
    MathExpression makePlusOperator( final ExpressionCollection terms );

    /**
       Makes an expression that is the difference between the specified
       expressions a and b.
       @param a The expression a.
       @param b The expression b.
       @return An expression that evaluates to a minus b.
     */
    MathExpression makeMinusOperator( final MathExpression a,
				      final MathExpression b );

    /**
       Makes an expression that is the quotient of the dividend expression
       divided by the divisor expression.
       @param dividend The dividend expression.
       @param divisor The divisor expression.
       @return An expression that is the quotient of the dividend
       expression divided by the divisor expression.
     */
    MathExpression makeDivideOperator( final MathExpression dividend,
				       final MathExpression divisor );

    /**
       Makes an expression that is the product of the expressions in
       the specified collection of expressions.
       @param terms The collection of expressions to be multiplied together
       in the returned expression.
       @return An expresison that is the product of all the expressions
       in the specified collection of expressions.
     */
    MathExpression makeTimesOperator( final ExpressionCollection terms );

    /**
       Makes an expression that is the modulus of the dividend expresison
       divided by the divisor expression.
       @param dividend The dividend expresison.
       @param divisor The divisor expression.
       @return An expression that is the modulues of the dividend expression
       divided by the divisor expression.
     */
    MathExpression makeModOperator( final MathExpression dividend,
				    final MathExpression divisor );

    /**
       Makes an expression that is e raised to the power of the specified
       expression x.
       @param x The expression x.
       @return An expression that is e raised to the power of the
       specified expression x.
     */
    MathExpression makeExpFunction( final MathExpression x );

    /**
       Makes an expression that is the natural logarithm of
       the specified expression x.
       @param x The expression x.
       @return An expression that is the natural logarithm of
       the specified expression x.
     */
    MathExpression makeLogFunction( final MathExpression x );
    
    /**
       Make an expression that is the sine of an angle
       specified by the specified expression x in radians.
       @param x An expression, x, that evaluates to an angle in
       radians.
       @return An expression that is the sine of an angle
       specified by the specified expression x in radians.
     */
    MathExpression makeSinFunction( final MathExpression x );
    
    /**
       Make an expression that is the cosine of an angle
       specified by the specified expression x in radians.
       @param x An expression, x, that evaluates to an angle in
       radians.
       @return An expression that is the cosine of an angle
       specified by the specified expression x in radians.
     */
    MathExpression makeCosFunction( final MathExpression x );
    
    /**
       Make an expression that is the tangent of an angle
       specified by the specified expression x in radians.
       @param x An expression, x, that evaluates to an angle in
       radians.
       @return An expression that is the tangent of an angle
       specified by the specified expression x in radians.
     */
    MathExpression makeTanFunction( final MathExpression x );

    /**
       Make an expression that is the arc sine of the specified
       expression x.
       @param x The expression x.
       @return An expression that is the arc sine of the specified
       expression x.
     */
    MathExpression makeArcsinFunction( final MathExpression x );
    
    /**
       Make an expression that is the arc cosine of the specified
       expression x.
       @param x The expression x.
       @return An expression that is the arc cosine of the specified
       expression x.
     */
    MathExpression makeArccosFunction( final MathExpression x );
    
    /**
       Make an expression that is the arc tagent of the specified
       expression x.
       @param x The expression x.
       @return An expression that is the arc tangent of the specified
       expression x.
     */
    MathExpression makeArctanFunction( final MathExpression x );

    /**
       Makes an expression that evaluates to the smallest integer
       greater than the value of the specified expression x.
       @param x The expression x.
       @return An expression that evaluates to the smallest integer
       greater than the value of the specified expression x.
     */
    MathExpression makeCeilFunction( final MathExpression x );
    
    /**
       Makes an expression that evaluates to the closest integer
       to the value of the specified expression x.  If two
       integers are equally close to the value of the specified
       expression x, then the larger integer is the result of this
       expression.
       @param x The expression x.
       @return An expression that evaluates to the closest integer
       to the value of the specified expression x.
     */
    MathExpression makeRoundFunction( final MathExpression x );

    /**
       Makes an expression that evaluates to the largest integer
       less than the value of the specified expression x.
       @param x The expression x.
       @return An expression that evaluates to the greatest integer
       less than the value of the specified expression x.
     */
    MathExpression makeFloorFunction( final MathExpression x );

    /**
       Makes an expression that evaluates to the true result expression if
       the result of the expression a is less than the result of expression b and
       to the false result expression of the result of the expression a is not
       less than the result of the expression b.
       @param a The expression a
       @param b The expression b
       @param trueResult The true result expression
       @param falseResult The false result expression
       @return An expression that evaluates to the true result expression
       if a is less than b and the false result expression of a is greater
       than or equal to b.
     */
    MathExpression makeLessThanOperator( final MathExpression a,
					 final MathExpression b,
					 final MathExpression trueResult,
					 final MathExpression falseResult );

    /**
       Makes an expression that is the sum of the values of the term expression
       for each value of the index variable between the first value
       of the index and the last value of the index inclusive that is 
       1.0 greater than the previous value of the index variable.
       @param indexName The name of the index variable.
       @param indexFirstValue An expression that evaluates to
       the first value of the index variable.
       @param indexLastValue An expression that evaluates to
       the last value of the index variable.  If the difference
       between the first value and the last value is not an integer,
       then the last index value will be less that the result of the
       expression specified for the last value of the index variable.
       @param termExpression An expression for each term in the sumation.
       @return  An expression that is the sum of the values of the term 
       expression for each value of the index variable between the first value
       of the index and the last value of the index inclusive that is 
       1.0 greater than the previous value of the index variable.
     */
    MathExpression makeSumOperator( final String indexName,
				    final MathExpression indexFirstValue,
				    final MathExpression indexLastValue,
				    final MathExpression termExpression );

    /**
       Makes an expression that is a reference to another expression.
       @param subBindings A variable dictionary that defines some set
       of the variables used in the subExpression in terms of other expressions.
       @param subExpression The referenced expression.
       @return An expression that is a reference to another expression.
     */
    MathExpression makeSubExprRef( final VariableDictionary subBindings,
				   final MathExpression subExpression );



}
