/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import java.util.Collection;
import java.util.Iterator;

import com.avlsi.util.debug.Debug;

import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringVisitor;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollectionIterator;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;

import com.avlsi.fast.shapes.stringexpression.impl.OperatorCommon;
import com.avlsi.fast.shapes.stringexpression.impl.WriteableStringExpressionCollectionImpl;

/**
   Default implementation of the concat operator.
   Concatenates together a collection of terms.  If one of the terms is a
   concat operator it is removed from the collection of terms and replaced by
   its terms.
   If there is ever more than one constant value in the collection, all the
   constant values are removed from the collection, concatenated together and a
   single constant term is added to the end of the collection.
 */
public class ConcatOperator extends OperatorCommon {
    
    protected WriteableStringExpressionCollection m_Terms ;

    private void Construct( int PredictedNumTerms ) {
	m_Terms = new WriteableStringExpressionCollectionImpl( PredictedNumTerms ) ;
    }

    protected void addTerm( StringExpression NewTerm ) {
	m_Terms.addExpression( NewTerm ) ;
    }

    protected void flattenChildConcatOperators( ) {
	WriteableStringExpressionCollectionIterator iter = 
	    m_Terms.getWriteableIterator() ;
	
	while( iter.hasNext() ) {
	    StringExpression currExp = iter.next() ;
	    
	    if ( currExp instanceof ConcatOperator ) {
		iter.remove() ;
		
		ConcatOperator childPlus = ( ConcatOperator ) currExp ;
		
		StringExpressionCollection innerTerms = childPlus.getTerms() ;
		
		StringExpressionCollectionIterator inner_iter = 
		    innerTerms.getIterator() ;
		
		while( inner_iter.hasNext() ) {
		    StringExpression innerExp = inner_iter.next();
		    iter.add( innerExp ) ;
		}
	    }   
	}
    }

    protected void combineConstants( ) {
	WriteableStringExpressionCollectionIterator iter = 
	    m_Terms.getWriteableIterator() ;
	String accumulator = "" ;

	while( iter.hasNext() ) {
	    StringExpression currExp = iter.next() ;
	    
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
	    } else {
                if (!accumulator.equals("")) {
                    iter.previous();
                    iter.add(makeConstantExpression(accumulator));
                    accumulator = "";
                }
            }
	}

	if (!accumulator.equals("")) {
	    iter.add( makeConstantExpression( accumulator ) ) ;
	}
    }

    protected StringExpressionCollection getTerms(){
	return m_Terms;
    }

    protected ConcatOperator( WriteableStringExpressionCollection terms ) {
	m_Terms = terms;
	flattenChildConcatOperators() ;
	combineConstants() ;
    }

    /**
       Construct a concatenation of the terms in the specified collection.
       Each entry in the collection must implement the StringExpression
       iterface.
       @see com.avlsi.fast.shapes.stringexpression.StringExpression
       @param Terms Collection of terms that are to be concatenated together.
     */
    public ConcatOperator( Collection Terms ) {
	Iterator iter;
	Construct( Terms.size() ) ;

	iter = Terms.iterator() ;

	while( iter.hasNext() ) {
	    StringExpression currExp = ( StringExpression ) iter.next() ;

	    addTerm( currExp ) ;
	    
	}

	flattenChildConcatOperators() ;
	combineConstants() ;
    }
    
    /**
       Construct a concatenation of the terms in the specified collection.
       Each entry in the collection must implement the StringExpression
       iterface.
       @see com.avlsi.fast.shapes.stringexpression.StringExpression
       @param Terms Collection of terms that are to be concatenated together.
     */
    public ConcatOperator( StringExpressionCollection terms ) {
	Construct( 4 );
	StringExpressionCollectionIterator iter;
	iter = terms.getIterator();
	
	while( iter.hasNext() ) {
	    StringExpression currExp = iter.next() ;
	    addTerm( currExp );
	}
	
	flattenChildConcatOperators() ;
	combineConstants();
    }

    public StringExpression evaluate( StringVariableDictionary varBindings ) {
	
        StringExpression ret;
	if ( isConstant() ) {
	    ret = this;
	}
	else { 
	    WriteableStringExpressionCollection newTerms = 
		new WriteableStringExpressionCollectionImpl( m_Terms.size() ) ;
	    
	    StringExpressionCollectionIterator iter = m_Terms.getIterator() ; 
	
	    while( iter.hasNext() ) {
		StringExpression currExp = iter.next() ;
		newTerms.addExpression( currExp.evaluate( varBindings ) ) ;
	    }
	    ret = new ConcatOperator( newTerms ) ;
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

    public String getConstantValue( ) throws NotAConstantValueException {
	if ( ! ( isConstant() ) ) {
	    throw new NotAConstantValueException( getVariableNames() );
	}
	Debug.assertTrue( m_Terms.size() == 1,
		      "ConcatOperator constructors should make sure" +
		      "that when adding constants everything gets" +
		      "collapsed into a single term" );
	return m_Terms.getIterator().next().getConstantValue();
    }

    public String[] getVariableNames( ) {
	return getReferencedVariableNames( m_Terms ); 
    }

    public void accept(StringVisitor v) {
        v.concatOperator(m_Terms);
    }
}
