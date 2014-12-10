/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import java.util.Arrays;
import java.util.Iterator;
import java.util.TreeSet;

import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.impl.Constant;

import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;


/**
   Superclass of all the default imlementations of all the operators that
   use doubles.
 */
public abstract class OperatorCommon implements StringExpression {
    /**
       Method called by the default implementations of the operators
       to construct a constant value during simplications.
     */
    protected StringExpression makeConstantExpression( final String val ) {
	return new Constant( val );
    }

    protected String[] getReferencedVariableNames( StringExpressionCollection expCol) {
	if ( !( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();
	    
	    StringExpressionCollectionIterator iter = expCol.getIterator();
	    
	    while ( iter.hasNext() ) {
		StringExpression currExp = iter.next() ;
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

    protected String[] getReferencedVariableNames( StringExpression a,
						   StringExpression b ) {
	if ( !( isConstant() ) ) {
	    TreeSet varSet = new TreeSet();
	    
	    if ( ! ( a.isConstant() ) ) {
		varSet.addAll( Arrays.asList( a.getVariableNames() ) );
	    }
	    if ( ! ( b.isConstant() ) ) {
		varSet.addAll( Arrays.asList( b.getVariableNames() ) );
	    }
	    return ( String[] ) varSet.toArray(new String[0]);
	}
	else {
	    return new String[0];
	}
    }

}
