/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;

import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.StringExpression;

/**
   Default implementation of WriteableStringExpressionCollection.
 */
public class WriteableStringExpressionCollectionImpl 
    implements WriteableStringExpressionCollection {

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
	    StringExpressionCollectionIterator iter = getIterator();
	    m_CachedIsConstant = true;
	    while ( ( m_CachedIsConstant ) && ( iter.hasNext() ) ) {
		StringExpression currExp = iter.next();
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
    public WriteableStringExpressionCollectionImpl( ) {
	Construct( 4 );
    }

    /**
       Constructor that allows the caller to communicate their best guess
       as to how many expression will end up in the collection.
       @param PredictedNumExpression A guess as to how many expressions
       will end up in the collection.
     */
    public WriteableStringExpressionCollectionImpl( final int PredictedNumExpressions ) {
	Construct( PredictedNumExpressions );
    }

   
    public StringExpressionCollectionIterator getIterator() {
	return new StringExpressionCollectionIterator() {
		private ListIterator m_Iter = m_Storage.listIterator();
		

		public StringExpression next() {
		    return ( StringExpression ) m_Iter.next() ;
		}

		public StringExpression previous() {
		    return ( StringExpression ) m_Iter.previous() ;
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
   
    public WriteableStringExpressionCollectionIterator getWriteableIterator() {
	return new WriteableStringExpressionCollectionIterator() {
		private ListIterator m_Iter = m_Storage.listIterator();

		public StringExpression next() {
		    return ( StringExpression ) m_Iter.next() ;
		}

		public StringExpression previous() {
		    return ( StringExpression ) m_Iter.previous() ;
		}
		
		public boolean hasNext() {
		    return m_Iter.hasNext() ;
		}
		
		public boolean hasPrevious() {
		    return m_Iter.hasPrevious();
		}

		public void add( StringExpression exp ) {
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

    public void addExpression( StringExpression expToAdd ) {
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

    
}
