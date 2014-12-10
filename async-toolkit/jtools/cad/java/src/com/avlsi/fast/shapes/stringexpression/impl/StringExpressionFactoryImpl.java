/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;


import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionFactory;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;


import com.avlsi.fast.shapes.stringexpression.impl.Constant;
import com.avlsi.fast.shapes.stringexpression.impl.ConcatOperator;
import com.avlsi.fast.shapes.stringexpression.impl.VariableReference;
import com.avlsi.fast.shapes.stringexpression.impl.SubExpressionReference;
import com.avlsi.fast.shapes.stringexpression.impl.WriteableStringExpressionCollectionImpl;


public class StringExpressionFactoryImpl implements StringExpressionFactory {


    public WriteableStringExpressionCollection makeExpressionCollection( ) {
	return new WriteableStringExpressionCollectionImpl();
    }

    public StringExpression makeConstant( final String constValue ) {
	return new Constant( constValue );
    }

    public StringExpression makeVariableReference( final String varName ) {
	return new VariableReference( varName );
    }

    public StringExpression makeConcatOperator( final StringExpressionCollection terms ) {
	return new ConcatOperator( terms );
    }
    
    public StringExpression makeSubExprRef( final StringVariableDictionary subBindings,
					    final StringExpression subExpression ) {
	return new SubExpressionReference( subExpression, subBindings );
    }

}
