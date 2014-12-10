/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import java.util.Arrays;
import java.util.TreeSet;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.mathexpression.NotAConstantValueException;

import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringVisitor;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariable;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionaryIterator;
import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableUtil;

import com.avlsi.fast.shapes.stringexpression.impl.OperatorCommon;

import com.avlsi.fast.shapes.stringexpression.variable.impl.WriteableStringVariableDictionaryImpl;

/**
   Default implmentation of a sub expression reference.
 */
public class SubExpressionReference extends OperatorCommon {

    protected StringExpression m_SubExpression;
    protected StringVariableDictionary m_Bindings;

    /**
       Construct a sub expression reference.
       
     */
    public SubExpressionReference( StringExpression subExpression,
				   StringVariableDictionary bindings ) {
	m_SubExpression = subExpression;
	m_Bindings = bindings; 
    }

    public StringExpression evaluate( StringVariableDictionary varBindings ) {
	StringExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {
	    WriteableStringVariableDictionary newSubDict = 
		new WriteableStringVariableDictionaryImpl();

	    StringVariableUtil.evaluateVariableValues( m_Bindings,
						 varBindings,
						 newSubDict );

	    WriteableStringVariableDictionary dictCopy =
		new WriteableStringVariableDictionaryImpl( varBindings );

	    StringVariableDictionaryIterator iter = newSubDict.getIterator();

	    while ( iter.hasNext() ) {
		StringVariable curr = iter.next() ;
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

	    StringExpression val = 
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

    public String getConstantValue() throws NotAConstantValueException {
	return m_SubExpression.evaluate( m_Bindings ).getConstantValue();
    }

    public String[] getVariableNames() {
	if ( ! ( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();

	    String[] var_names_in_sub_expr = m_SubExpression.getVariableNames();
	    
	    int num_var_names_in_sub_expr = var_names_in_sub_expr.length;
	    
	    int i ;
	    
	    for ( i = 0 ; i < num_var_names_in_sub_expr ; ++i ) {
		StringExpression val =
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

    public void accept(StringVisitor v) {
        v.subExpressionReference(m_Bindings, m_SubExpression);
    }
}
