/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import java.util.Arrays;
import java.util.TreeSet;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.util.mathexpression.Visitor;

import com.avlsi.util.mathexpression.variable.Variable;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;
import com.avlsi.util.mathexpression.variable.VariableUtil;

import com.avlsi.util.mathexpression.impl.DoubleOperatorCommon;

import com.avlsi.util.mathexpression.variable.impl.WriteableVariableDictionaryImpl;

/**
   Default implmentation of a sub expression reference.
 */
public class SubExpressionReference extends DoubleOperatorCommon {

    protected MathExpression m_SubExpression;
    protected VariableDictionary m_Bindings;

    /**
       Construct a sub expression reference.
       
     */
    public SubExpressionReference( MathExpression subExpression,
				   VariableDictionary bindings ) {
	m_SubExpression = subExpression;
	m_Bindings = bindings; 
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    WriteableVariableDictionary newSubDict = 
		new WriteableVariableDictionaryImpl();

	    VariableUtil.evaluateVariableValues( m_Bindings,
						 varBindings,
						 newSubDict );

	    WriteableVariableDictionary dictCopy =
		new WriteableVariableDictionaryImpl( varBindings );

	    VariableDictionaryIterator iter = newSubDict.getIterator();

	    while ( iter.hasNext() ) {
		Variable curr = iter.next() ;
		dictCopy.unBindVariable( curr.getName() );
	    }

	    ret = 
		new SubExpressionReference( m_SubExpression.evaluate( dictCopy ),
					    newSubDict );
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
	String[] var_names_in_sub_expr = m_SubExpression.getVariableNames();

        int num_var_names_in_sub_expr = var_names_in_sub_expr.length;

	int i = 0;

	boolean ret = true;

	while ( ( i < num_var_names_in_sub_expr  ) && ( ret ) ) {

	    MathExpression val = 
		m_Bindings.getVariableValue( var_names_in_sub_expr[i] );

	    if ( val == null ) {
		ret = false;
	    }
	    else {
		ret = val.isConstant();
	    }

	    ++i;
	    
	}

	return ret;
	
    }

    public double getConstantValue() throws NotAConstantValueException {
	return m_SubExpression.evaluate( m_Bindings ).getConstantValue();
    }

    public String[] getVariableNames() {
	if ( ! ( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();

	    String[] var_names_in_sub_expr = m_SubExpression.getVariableNames();
	    
	    int num_var_names_in_sub_expr = var_names_in_sub_expr.length;
	    
	    int i ;
	    
	    for ( i = 0 ; i < num_var_names_in_sub_expr ; ++i ) {
		MathExpression val =
		    m_Bindings.getVariableValue( var_names_in_sub_expr[i] );
		
		if ( val == null ) {
		    varSet.add( var_names_in_sub_expr[i] );
		}
		else {
		    varSet.addAll( Arrays.asList( val.getVariableNames() ) );
		}
	    }
	    return ( String[] ) varSet.toArray();

	}
	else { 
	    return new String[0];
	}
    }

    public void accept( Visitor v ) {
	v.subExpressionReference( m_Bindings,
				  m_SubExpression );
    }

}
