/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import java.util.Arrays;
import java.util.Iterator;
import java.util.TreeSet;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.impl.Constant;
import com.avlsi.util.mathexpression.impl.DoubleCommon;

import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.ExpressionCollectionIterator;


/**
   Superclass of all the default imlementations of all the operators that
   use doubles.
 */
public abstract class DoubleOperatorCommon extends DoubleCommon {
    /**
       Method called by the default implementations of the operators
       to construct a constant value during simplications.
     */
    protected MathExpression makeConstantExpression( double val ) {
	return new Constant( val );
    }

    protected String[] getReferencedVariableNames( ExpressionCollection expCol) {
	if ( !( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();
	    
	    ExpressionCollectionIterator iter = expCol.getIterator();
	    
	    while ( iter.hasNext() ) {
		MathExpression currExp = iter.next() ;
		if ( ! ( currExp.isConstant() ) ) {
		    varSet.addAll( Arrays.asList( currExp.getVariableNames() ) );
		}
	    }
	    return ( String[] ) varSet.toArray(new String[0]);
	}
	else {
	    return new String[0];
	}
    }

    protected String[] getReferencedVariableNames( MathExpression a,
						   MathExpression b ) {
	if ( !( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();
	    
	    if ( ! ( a.isConstant() ) ) {
		varSet.addAll( Arrays.asList( a.getVariableNames() ) );
	    }
	    if ( ! ( b.isConstant() ) ) {
		varSet.addAll( Arrays.asList( b.getVariableNames() ) );
	    }
	    return ( String[] ) varSet.toArray();
	}
	else {
	    return new String[0];
	}
    }

    protected String[] getReferencedVariableNames( MathExpression a,
						   MathExpression b,
						   MathExpression c,
						   MathExpression d ) {
	if ( !( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();
	    
	    if ( ! ( a.isConstant() ) ) {
		varSet.addAll( Arrays.asList( a.getVariableNames() ) );
	    }
	    if ( ! ( b.isConstant() ) ) {
		varSet.addAll( Arrays.asList( b.getVariableNames() ) );
	    }
	    if ( ! ( c.isConstant() ) ) {
		varSet.addAll( Arrays.asList( c.getVariableNames() ) );
	    }
	    if ( ! ( d.isConstant() ) ) {
		varSet.addAll( Arrays.asList( d.getVariableNames() ) );
	    }
	    return ( String[] ) varSet.toArray();
	}
	else {
	    return new String[0];
	}
    }


}
