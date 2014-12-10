/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import com.avlsi.util.mathexpression.WriteableExpressionCollection;
import com.avlsi.util.mathexpression.WriteableExpressionCollectionIterator;
import com.avlsi.util.mathexpression.ExpressionCollectionIterator;
import com.avlsi.util.mathexpression.MathExpression;


import java.util.List;

import java.util.ArrayList;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
   Default implementation of WriteableExpressionCollection.
 */
public class WriteableExpressionCollectionImpl 
    implements WriteableExpressionCollection {

    ///The list of expressions in the collection.
    protected List m_Storage;

    protected boolean m_IsCachedIsConstantDirty;
    protected boolean m_CachedIsConstant;

    /**
       Utility private method to create the list used to store
       the collection.
       @param PredictedNumItems A guess as to how many expressions
       will end up in the collection.
     */
    private void Construct( final int PredictedNumItems ) {
	m_Storage = new ArrayList( PredictedNumItems );
	m_IsCachedIsConstantDirty = false;
	m_CachedIsConstant = true;
    }

    protected final void updateCachedIsConstant( ) {
	if ( isCachedIsConstantDirty() ) {
	    ExpressionCollectionIterator iter = getIterator();
	    m_CachedIsConstant = true;
	    while ( ( m_CachedIsConstant ) && ( iter.hasNext() ) ) {
		MathExpression currExp = iter.next();
		m_CachedIsConstant = currExp.isConstant() ;
	    }
	    m_IsCachedIsConstantDirty = false;
	}
    }

    protected final void cachedIsConstantIsDirty() {
	m_IsCachedIsConstantDirty = true;
    }

    protected final boolean isCachedIsConstantDirty() {
	return m_IsCachedIsConstantDirty;
    }

    protected final void notConstant() {
	m_CachedIsConstant = false;
    }

    /**
       Default constructor.  Allocates space for four expressions.
     */
    public WriteableExpressionCollectionImpl( ) {
	Construct( 4 );
    }

    /**
       Constructor that allows the caller to communicate their best guess
       as to how many expression will end up in the collection.
       @param PredictedNumExpression A guess as to how many expressions
       will end up in the collection.
     */
    public WriteableExpressionCollectionImpl( final int PredictedNumExpressions ) {
	Construct( PredictedNumExpressions );
    }

   
    public ExpressionCollectionIterator getIterator() {
	return new ExpressionCollectionIterator() {
		private ListIterator m_Iter = m_Storage.listIterator();
		

		public MathExpression next() {
		    return ( MathExpression ) m_Iter.next() ;
		}

		public MathExpression previous() {
		    return ( MathExpression ) m_Iter.previous() ;
		}
		
		public boolean hasNext() {
		    return m_Iter.hasNext() ;
		}
		
		public boolean hasPrevious() {
		    return m_Iter.hasPrevious();
		}
	    };
    }

    public boolean allMembersAreConstant() {
	updateCachedIsConstant( );
	return m_CachedIsConstant;
    }

    public int size() {
	return m_Storage.size();
    }
   
    public WriteableExpressionCollectionIterator getWriteableIterator() {
	return new WriteableExpressionCollectionIterator() {
		private ListIterator m_Iter = m_Storage.listIterator();

		public MathExpression next() {
		    return ( MathExpression ) m_Iter.next() ;
		}

		public MathExpression previous() {
		    return ( MathExpression ) m_Iter.previous() ;
		}
		
		public boolean hasNext() {
		    return m_Iter.hasNext() ;
		}
		
		public boolean hasPrevious() {
		    return m_Iter.hasPrevious();
		}

		public void add( MathExpression exp ) {
		    m_Iter.add( exp ) ;
		    if ( ! ( isCachedIsConstantDirty() ) ) {
			if ( allMembersAreConstant() ) {
			    if ( ! ( exp.isConstant() ) ) {
				notConstant();
			    }
			}
		    }
		}

		public void remove( ) {
		    cachedIsConstantIsDirty(); 
		    m_Iter.remove();
		}
	    };
    }

    public void addExpression( MathExpression expToAdd ) {
	m_Storage.add( expToAdd );
	if ( ! ( isCachedIsConstantDirty() ) ) {
	    if ( allMembersAreConstant() ) {
		if ( ! ( expToAdd.isConstant() ) ) {
		    notConstant();
		}
	    }
	}
    }

    public void clear() {
	m_Storage.clear();
    }

    public boolean equals(Object obj) {
        if (obj instanceof WriteableExpressionCollectionImpl) {
            WriteableExpressionCollectionImpl o =
                (WriteableExpressionCollectionImpl) obj;
            return m_Storage.equals(o.m_Storage);
        } else
            return false;
    }
}
