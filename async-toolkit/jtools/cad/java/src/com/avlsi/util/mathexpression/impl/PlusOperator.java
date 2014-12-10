/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import java.util.Collection;
import java.util.Iterator;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.ExpressionCollectionIterator;
import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.util.mathexpression.Visitor;
import com.avlsi.util.mathexpression.WriteableExpressionCollection;
import com.avlsi.util.mathexpression.WriteableExpressionCollectionIterator;

import com.avlsi.util.mathexpression.variable.VariableDictionary;

import com.avlsi.util.mathexpression.impl.DoubleOperatorCommon;
import com.avlsi.util.mathexpression.impl.WriteableExpressionCollectionImpl;

/**
   Default implementation of the plus operator.
   Adds together a collection of terms.
   If one of the terms is a plus operator it
   is removed from the collection of terms and replaced
   by its terms.
   If there is ever more than one constant value in the collection,
   all the constant values are removed from the collection, added
   together and a single constant term is added to the end of the collection.
 */
public class PlusOperator extends DoubleOperatorCommon {
    
    protected WriteableExpressionCollection m_Terms ;

    private void Construct( int PredictedNumTerms ) {
	m_Terms = new WriteableExpressionCollectionImpl( PredictedNumTerms ) ;
    }

    protected void addTerm( MathExpression NewTerm ) {
	m_Terms.addExpression( NewTerm ) ;
    }

    protected void flattenChildPlusOperators( ) {
	WriteableExpressionCollectionIterator iter = 
	    m_Terms.getWriteableIterator() ;
	
	while( iter.hasNext() ) {
	    MathExpression currExp = iter.next() ;
	    
	    if ( currExp instanceof PlusOperator ) {
		iter.remove() ;
		
		PlusOperator childPlus = ( PlusOperator ) currExp ;
		
		ExpressionCollection innerTerms = childPlus.getTerms() ;
		
		ExpressionCollectionIterator inner_iter = 
		    innerTerms.getIterator() ;
		
		while( inner_iter.hasNext() ) {
		    MathExpression innerExp = inner_iter.next();
		    iter.add( innerExp ) ;
		}
	    }   
	}
    }

    protected void combineConstants( ) {
	WriteableExpressionCollectionIterator iter = 
	    m_Terms.getWriteableIterator() ;
	double accumulator = 0 ;

	while( iter.hasNext() ) {
	    MathExpression currExp = iter.next() ;
	    
	    if ( currExp.isConstant() ){
		try {
		    accumulator += currExp.getConstantValue() ;
		}
		catch( NotAConstantValueException e ) {
		    Debug.assertTrue( false, 
				  "Caught NotAConstantValueException " +
				  "even though I called isConstant first." );
		}
		iter.remove() ;
	    }
	}

	if ( accumulator != 0 ) {
	    addTerm( makeConstantExpression( accumulator ) ) ;
	}
    }

    protected ExpressionCollection getTerms(){
	return m_Terms;
    }

    protected PlusOperator( WriteableExpressionCollection terms ) {
	m_Terms = terms;
	flattenChildPlusOperators() ;
	combineConstants() ;
    }

    /**
       Construct a sum of the terms in the specified collection.
       Each entry in the collection must implement the MathExpression
       iterface.
       @see com.avlsi.util.mathexpression.MathExpression
       @param Terms Collection of terms that are to be added together.
     */
    public PlusOperator( Collection Terms ) {
	Iterator iter;
	Construct( Terms.size() ) ;

	iter = Terms.iterator() ;

	while( iter.hasNext() ) {
	    MathExpression currExp = ( MathExpression ) iter.next() ;

	    addTerm( currExp ) ;
	    
	}

	flattenChildPlusOperators() ;
	combineConstants() ;
    }
    
    /**
       Construct a sum of the terms in the specified collection.
       Each entry in the collection must implement the MathExpression
       iterface.
       @see com.avlsi.util.mathexpression.MathExpression
       @param Terms Collection of terms that are to be added together.
     */
    public PlusOperator( ExpressionCollection terms ) {
	Construct( 4 );
	ExpressionCollectionIterator iter;
	iter = terms.getIterator();
	
	while( iter.hasNext() ) {
	    MathExpression currExp = iter.next() ;
	    addTerm( currExp );
	}
	
	flattenChildPlusOperators() ;
	combineConstants();
    }

    public MathExpression evaluate( VariableDictionary varBindings ) {
	
MathExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else { 
	    WriteableExpressionCollection newTerms = 
		new WriteableExpressionCollectionImpl( m_Terms.size() ) ;
	    
	    ExpressionCollectionIterator iter = m_Terms.getIterator() ; 
	
	    while( iter.hasNext() ) {
		MathExpression currExp = iter.next() ;
		newTerms.addExpression( currExp.evaluate( varBindings ) ) ;
	    }
	    ret = new PlusOperator( newTerms ) ;
	}
	if ( ret.isConstant() ) {
	    try {
		ret = makeConstantExpression( ret.getConstantValue() ) ;
	    }
	    catch( NotAConstantValueException e ){
		Debug.assertTrue( false, 
			      "Caught NotAConstantValueException " +
			      "even though I called isConstant first." );
	    }
	}
	return ret;
    }

    public boolean isConstant( ) {
	return m_Terms.allMembersAreConstant() ;
    }

    

    public double getConstantValue( ) throws NotAConstantValueException {
	if ( ! ( isConstant() ) ) {
	    throw new NotAConstantValueException( getVariableNames() );
	}
	Debug.assertTrue( m_Terms.size() == 1,
		      "PlusOperator constructors should make sure" +
		      "that when adding constants everything gets" +
		      "collapsed into a single term" );
	return m_Terms.getIterator().next().getConstantValue();
    }

    public String[] getVariableNames( ) {
	return getReferencedVariableNames( m_Terms ); 
    }

    public void accept( Visitor v ) {
	v.plusOperator( getTerms() );
    }

    public boolean equals(Object obj) {
        if (obj instanceof PlusOperator) {
            PlusOperator o = (PlusOperator) obj;
            return m_Terms.equals(o.m_Terms);
        } else
            return false;
    }
}
