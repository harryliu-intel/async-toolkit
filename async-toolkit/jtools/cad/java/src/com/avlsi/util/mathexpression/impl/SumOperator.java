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

import com.avlsi.util.mathexpression.variable.impl.WriteableVariableDictionaryImpl;

import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;

import com.avlsi.util.mathexpression.impl.DoubleOperatorCommon;

/**
   Default implmentation of the summation operator.
 */
public class SumOperator extends DoubleOperatorCommon {

    protected final String m_IndexName;
    protected final MathExpression m_IndexFirstValue;
    protected final MathExpression m_IndexLastValue;
    protected final MathExpression m_TermExpression;

    /**
       Construct a divide operator.
       @param dividend The dividend.
       @param divisor The divisor.
     */
    public SumOperator( final String indexName,
			final MathExpression indexFirstValue,
			final MathExpression indexLastValue,
			final MathExpression termExpression ) {
	
	m_IndexName = indexName;
	m_IndexFirstValue = indexFirstValue;
	m_IndexLastValue = indexLastValue;
	m_TermExpression = termExpression;
    
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else {

	    WriteableVariableDictionary myVarDict = 
		new WriteableVariableDictionaryImpl( varBindings );

	    myVarDict.unBindVariable( m_IndexName );
	    
	    ret = new SumOperator( m_IndexName,
				   m_IndexFirstValue.evaluate( varBindings ),
				   m_IndexLastValue.evaluate( varBindings ),
				   m_TermExpression.evaluate( myVarDict ) );
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
	boolean ret = false;
	
	if ( m_IndexFirstValue.isConstant() && m_IndexLastValue.isConstant() ){
	    String[] referencedVars = m_TermExpression.getVariableNames();
	    
	    if ( referencedVars.length <= 1 ) {
		if ( referencedVars.length > 0 ) {
		    ret = m_IndexName.compareTo( referencedVars[0] ) == 0;
		}
		else {
		    ret = true;
		}
	    }
	}
	return ret;
    }

    public double getConstantValue() throws NotAConstantValueException {
	double ret = 0;
	double first_index = m_IndexFirstValue.getConstantValue();
	double index = first_index;
	double last_index = m_IndexLastValue.getConstantValue() ;
	WriteableVariableDictionaryImpl varDict = 
	    new WriteableVariableDictionaryImpl();

	while( index < last_index ) {
	   varDict.bindVariable( m_IndexName, 
				 makeConstantExpression( index ) );
	   
	   ret += m_TermExpression.evaluate( varDict ).getConstantValue();

	   ++index;

	   varDict.unBindVariable( m_IndexName );
	}

	return ret;
    }

    public String[] getVariableNames() {
	if ( !( isConstant() ) ) { 
	    TreeSet varSet = new TreeSet();
	    

	    if ( ! ( m_TermExpression.isConstant() ) ) {
		varSet.add( m_IndexName );
		varSet.addAll( 
			      Arrays.asList( 
					    m_TermExpression.getVariableNames() 
					    ) 
			      );
		varSet.remove( m_IndexName );
	    }

	    if ( ! ( m_IndexFirstValue.isConstant() ) ) {
		varSet.addAll(
			      Arrays.asList(
					    m_IndexFirstValue.getVariableNames()
					    )
			      );
	    }
	    if ( ! ( m_IndexFirstValue.isConstant() ) ) {
		varSet.addAll(
			      Arrays.asList(
					    m_IndexLastValue.getVariableNames()
					    )
			      );
	    }

	    return ( String[] ) varSet.toArray() ;
	   

	}
	else {
	    return new String[0];
	}
    }

    public void accept( Visitor v ) {
	v.sumOperator( m_IndexName, 
		       m_IndexFirstValue, 
		       m_IndexLastValue, 
		       m_TermExpression );
    }

    public boolean equals(Object obj) {
        if (obj instanceof SumOperator) {
            SumOperator o = (SumOperator) obj;
            return m_IndexName.equals(o.m_IndexName) &&
                   m_IndexFirstValue.equals(o.m_IndexFirstValue) &&
                   m_IndexLastValue.equals(o.m_IndexLastValue) &&
                   m_TermExpression.equals(o.m_TermExpression);
        } else
            return false;
    }
}
