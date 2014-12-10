/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;


import java.math.BigDecimal;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.MathExpressionFactory;
import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.WriteableExpressionCollection;

import com.avlsi.util.mathexpression.variable.VariableDictionary;


import com.avlsi.util.mathexpression.impl.ArccosFunction;
import com.avlsi.util.mathexpression.impl.ArcsinFunction;
import com.avlsi.util.mathexpression.impl.ArctanFunction;
import com.avlsi.util.mathexpression.impl.CeilFunction;
import com.avlsi.util.mathexpression.impl.Constant;
import com.avlsi.util.mathexpression.impl.CosFunction;
import com.avlsi.util.mathexpression.impl.DivideOperator;
import com.avlsi.util.mathexpression.impl.ExpFunction;
import com.avlsi.util.mathexpression.impl.FloorFunction;
import com.avlsi.util.mathexpression.impl.LessThanOperator;
import com.avlsi.util.mathexpression.impl.MinusOperator;
import com.avlsi.util.mathexpression.impl.ModOperator;
import com.avlsi.util.mathexpression.impl.NegationOperator;
import com.avlsi.util.mathexpression.impl.PlusOperator;
import com.avlsi.util.mathexpression.impl.RoundFunction;
import com.avlsi.util.mathexpression.impl.SinFunction;
import com.avlsi.util.mathexpression.impl.SubExpressionReference;
import com.avlsi.util.mathexpression.impl.SumOperator;
import com.avlsi.util.mathexpression.impl.TanFunction;
import com.avlsi.util.mathexpression.impl.TimesOperator;
import com.avlsi.util.mathexpression.impl.VariableReference;
import com.avlsi.util.mathexpression.impl.WriteableExpressionCollectionImpl;


public class MathExpressionFactoryImpl implements MathExpressionFactory {


    public WriteableExpressionCollection makeExpressionCollection( ) {
	return new WriteableExpressionCollectionImpl();
    }

    public MathExpression makeConstant( final double constValue ) {
	return new Constant( constValue );
    }

    public MathExpression makeConstant( final BigDecimal constValue ) {
	return makeConstant( constValue.doubleValue() );
    }

    public MathExpression makeVariableReference( final String varName ) {
	return new VariableReference( varName );
    }

    public MathExpression makeNegationOperator( final MathExpression x ) {
	return new NegationOperator( x );
    }

    public MathExpression makePlusOperator( final ExpressionCollection terms ) {
	return new PlusOperator( terms );
    }

    public MathExpression makeMinusOperator( final MathExpression a,
					     final MathExpression b ) {
	return new MinusOperator( a, b );
    }

    public MathExpression makeDivideOperator( final MathExpression dividend,
					      final MathExpression divisor ) {
	return new DivideOperator( dividend, divisor ) ;
    }

    public MathExpression makeTimesOperator( final ExpressionCollection terms ) {
	return new TimesOperator( terms );
    }

    public MathExpression makeModOperator( final MathExpression dividend,
			    final MathExpression divisor ) {
	return new ModOperator( dividend, divisor ) ;
    }

    public MathExpression makeExpFunction( final MathExpression x ) {
	return new ExpFunction( x );
    }
    
    public MathExpression makeLogFunction( final MathExpression x ) {
	return new LogFunction( x );
    }

    public MathExpression makeSinFunction( final MathExpression x ) {
	return new SinFunction( x );
    }

    public MathExpression makeCosFunction( final MathExpression x ) {
	return new CosFunction( x );
    }

    public MathExpression makeTanFunction( final MathExpression x ) {
	return new TanFunction( x );
    }

    public MathExpression makeArcsinFunction( final MathExpression x ) {
	return new ArcsinFunction( x );
    }

    public MathExpression makeArccosFunction( final MathExpression x ) {
	return new ArccosFunction( x );
    }

    public MathExpression makeArctanFunction( final MathExpression x ) {
	return new ArctanFunction( x );
    }

    public MathExpression makeCeilFunction( final MathExpression x ) {
	return new CeilFunction( x );
    }

    public MathExpression makeRoundFunction( final MathExpression x ) {
	return new RoundFunction( x );
    }

    public MathExpression makeFloorFunction( final MathExpression x ) {
	return new FloorFunction( x );
    }

    public MathExpression makeLessThanOperator( final MathExpression a,
						final MathExpression b,
						final MathExpression trueResult,
						final MathExpression falseResult ){
	return new LessThanOperator( a, b, trueResult, falseResult );
    }

    public MathExpression makeSumOperator( final String indexName,
					   final MathExpression indexFirstValue,
					   final MathExpression indexLastValue,
					   final MathExpression termExpression ) {
	return new SumOperator( indexName, 
				indexFirstValue,
				indexLastValue,
				termExpression );
    }
    
    public MathExpression makeSubExprRef( final VariableDictionary subBindings,
					  final MathExpression subExpression ) {
	return new SubExpressionReference( subExpression, subBindings );
    }

    

    


}
