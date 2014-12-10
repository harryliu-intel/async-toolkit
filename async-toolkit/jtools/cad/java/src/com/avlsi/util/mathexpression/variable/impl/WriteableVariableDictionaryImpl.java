/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable.impl;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;
import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionaryIterator;
import com.avlsi.util.mathexpression.variable.Variable;
import com.avlsi.util.mathexpression.variable.WriteableVariable;

import java.util.Map;

import java.util.HashMap;

import java.util.NoSuchElementException;
import java.util.Iterator;

/**
   Default implementation of WriteableVariableDictionary interface.
 */
public class WriteableVariableDictionaryImpl 
    implements WriteableVariableDictionary {
    
    protected Map storage;

    public final MathExpression putVariable( final String varName,
						final MathExpression val ) {
	return ( MathExpression ) storage.put( varName, val );
    }

    /**
       Default constructor.  Uses a HashMap for storage.
     */
    public WriteableVariableDictionaryImpl( ) {
	storage = new HashMap( );
    }

    public WriteableVariableDictionaryImpl( VariableDictionary vDict ) {
	this();
	
	VariableDictionaryIterator iter = vDict.getIterator();
	
	while ( iter.hasNext() ) {
	    Variable currVar = iter.next() ;
	    putVariable( currVar.getName(), currVar.getValue() );
	}
    }
	    

    /**
       Dictionary will use specified map for storage.
       @param MapToUseForStorage Object implementing java.util.Map that the
       dictionary should use for storage.  You should not reference
       this map after using this constructor.
     */
    public WriteableVariableDictionaryImpl( Map MapToUseForStorage ) {
	storage = MapToUseForStorage;
    }

    public MathExpression getVariableValue( final String VariableName ) {
	return ( MathExpression ) storage.get( VariableName ) ;
    }

    public VariableDictionaryIterator getIterator() {
	return new VariableDictionaryIterator() {
		private Iterator m_Iter = storage.entrySet().iterator();
		
		public Variable next() {
		    Map.Entry CurrEntry = (Map.Entry) m_Iter.next();
		    String VarName = 
			( String ) CurrEntry.getKey();
		    MathExpression VarVal = 
			( MathExpression ) CurrEntry.getValue();
		    
		    return new WriteableVariableImpl( VarName );
		    
		}

		public boolean hasNext() {
		    return m_Iter.hasNext() ;
		}

	    };
    }

    public boolean bindVariable( final String VariableName, 
				 final MathExpression Exp ) {
	MathExpression PrevValue = putVariable( VariableName, Exp );
	if ( PrevValue != null ) {
	    putVariable( VariableName, PrevValue ) ;
	}
	return PrevValue == null ;
    }
    
    public WriteableVariableDictionaryIterator getWriteableIterator() {
	return new WriteableVariableDictionaryIterator() {
		private Iterator m_Iter = storage.entrySet().iterator();
		
		public WriteableVariable next( )  {
		    Map.Entry CurrEntry = (Map.Entry) m_Iter.next();
		    String varName = 
			( String ) CurrEntry.getKey();
		    MathExpression VarVal = 
			( MathExpression ) CurrEntry.getValue();
		    
		    return new WriteableVariableImpl( varName );
		    
		}

		public boolean hasNext( ) {
		    return m_Iter.hasNext();
		}

		public void remove( ) {
		    m_Iter.remove();
		}
	    };
    }

    public void unBindVariable( final String variableName ) {
	storage.remove( variableName );
    }

    /**
       Default implementation of WriteableVariable.
     */
    private class WriteableVariableImpl implements WriteableVariable {

        protected final String m_Name;

        /**
           @param Name The name of the variable.
           @param parent A pointer to the dictionary that instantiated this
           variable.
         */
        public WriteableVariableImpl( final String name ) {
            m_Name = name;
        }

        public String getName( ) {
            return m_Name;
        }

        public MathExpression getValue( ){
            return getVariableValue( m_Name );
        }

        public void setValue( MathExpression exp ) {
            putVariable( m_Name, exp );
        }
    }
			    
}
