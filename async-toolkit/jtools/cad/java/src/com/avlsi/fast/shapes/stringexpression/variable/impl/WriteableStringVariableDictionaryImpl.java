/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable.impl;

import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;

import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionaryIterator;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariableDictionaryIterator;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariable;
import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariable;

/**
   Default implementation of WriteableStringVariableDictionary interface.
 */
public class WriteableStringVariableDictionaryImpl 
    implements WriteableStringVariableDictionary {
    
    protected Map storage;

    public final StringExpression putVariable( final String varName,
						     final StringExpression val ) {
	return ( StringExpression ) storage.put( varName, val );
    }

    /**
       Default constructor.  Uses a HashMap for storage.
     */
    public WriteableStringVariableDictionaryImpl( ) {
	storage = new HashMap( );
    }

    public WriteableStringVariableDictionaryImpl( StringVariableDictionary vDict ) {
	this();
	
	StringVariableDictionaryIterator iter = vDict.getIterator();
	
	while ( iter.hasNext() ) {
	    StringVariable currVar = iter.next() ;
	    putVariable( currVar.getName(), currVar.getValue() );
	}
    }
	    

    /**
       Dictionary will use specified map for storage.
       @param MapToUseForStorage Object implementing java.util.Map that the
       dictionary should use for storage.  You should not reference
       this map after using this constructor.
     */
    public WriteableStringVariableDictionaryImpl( Map MapToUseForStorage ) {
	storage = MapToUseForStorage;
    }

    public StringExpression getVariableValue( final String StringVariableName ) {
	return ( StringExpression ) storage.get( StringVariableName ) ;
    }

    public StringVariableDictionaryIterator getIterator() {
	return new StringVariableDictionaryIterator() {
		private Iterator m_Iter = storage.entrySet().iterator();
		
                /** @throws NoSuchElementException **/
		public StringVariable next() {
		    Map.Entry CurrEntry = (Map.Entry) m_Iter.next();
		    String VarName = 
			( String ) CurrEntry.getKey();
		    StringExpression VarVal = 
			( StringExpression ) CurrEntry.getValue();
		    
		    return new WriteableStringVariableImpl( VarName );
		    
		}

		public boolean hasNext() {
		    return m_Iter.hasNext() ;
		}

	    };
    }

    public boolean bindVariable( final String StringVariableName, 
				       final StringExpression Exp ) {
	StringExpression PrevValue = putVariable( StringVariableName, Exp );
	if ( PrevValue != null ) {
	    putVariable( StringVariableName, PrevValue ) ;
	}
	return PrevValue == null ;
    }
    
    public WriteableStringVariableDictionaryIterator getWriteableIterator() {
	return new WriteableStringVariableDictionaryIterator() {
		private Iterator m_Iter = storage.entrySet().iterator();
		
                /** @throws NoSuchElementException **/
		public WriteableStringVariable next( )  {
		    Map.Entry CurrEntry = (Map.Entry) m_Iter.next();
		    String varName = 
			( String ) CurrEntry.getKey();
		    StringExpression VarVal = 
			( StringExpression ) CurrEntry.getValue();
		    
		    return new WriteableStringVariableImpl( varName );
		    
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
       Default implementation of WriteableStringVariable.
     */
    private class WriteableStringVariableImpl implements WriteableStringVariable {

        protected final String m_Name;

        /**
           @param Name The name of the variable.
         */
        public WriteableStringVariableImpl( final String name ) {
            m_Name = name;
        }

        public String getName( ) {
            return m_Name;
        }

        public StringExpression getValue( ) {
            return getVariableValue( m_Name );
        }

        public void setValue( StringExpression exp ) {
            putVariable( m_Name, exp );
        }
    }
}
