/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.variable;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionaryIterator;
import com.avlsi.fast.shapes.stringexpression.variable.WriteableStringVariableDictionary;

public class StringVariableUtil {
    /**
     * This class should not be instantiated.
     **/
    private StringVariableUtil() { }

    /**
       Determines if all the values of the variables defined in the specified 
       dictionary have constant values.
       @param d The variable dictionary.
       @return true if all the values of the variables defined in the dictionary
       have constant values.
     */
    public static boolean allVariablesAreConstants( final StringVariableDictionary d ) {
	StringVariableDictionaryIterator iter = d.getIterator() ;
	boolean ret = true;

	while ( ( iter.hasNext() ) && ( ret ) ) {
	    StringVariable curr = iter.next() ;
	    ret = curr.getValue().isConstant();
	}
	return ret;
    }


    /**
       Evaluates the StringExpression that are the values of the variables
       defined in src_dict, using the bindings in src_bindings.  Stores
       the resulting values in dest_dict.
       @param src_dict The dictionary with the variables whose
       values are to be evaluated using src_bindings.
       @param src_bindings The bindings that are to be used to evaluate
       the values of variables in src_dict.
       @param dest_dict The dictionary into which the resulting
       variable values are written into.
       @exception IllegalArgumentException when src_dict and src_bindings
       are the same dictionary.
     */
    public static 
	void evaluateVariableValues( final StringVariableDictionary src_dict,
				     final StringVariableDictionary src_bindings,
				     WriteableStringVariableDictionary dest_dict ) {

	if ( src_dict == src_bindings ) {
	    throw new IllegalArgumentException() ;
	}

	StringVariableDictionaryIterator iter = src_dict.getIterator();

	while ( iter.hasNext() ) {
	    StringVariable curr = iter.next() ;
	    dest_dict.unBindVariable( curr.getName() );
	    dest_dict.bindVariable( curr.getName(),
				    curr.getValue().evaluate( src_dict ) );
	}
    }
}
