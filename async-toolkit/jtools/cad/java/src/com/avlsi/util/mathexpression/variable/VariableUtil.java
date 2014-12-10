/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;

public class VariableUtil {
    /**
     * This class should not be instantiated.
     **/
    private VariableUtil() { }

    /**
       Determines if all the values of the variables defined in the specified 
       dictionary have constant values.
       @param d The variable dictionary.
       @return true if all the values of the variables defined in the dictionary
       have constant values.
     */
    public static boolean allVariablesAreConstants( final VariableDictionary d ) {
        VariableDictionaryIterator iter = d.getIterator() ;
        boolean ret = true;

        while ( ( iter.hasNext() ) && ( ret ) ) {
            Variable curr = iter.next() ;
            ret = curr.getValue().isConstant();
        }
        return ret;
    }


    /**
       Evaluates the MathExpression that are the values of the variables
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
        void evaluateVariableValues( final VariableDictionary src_dict,
                                     final VariableDictionary src_bindings,
                                     WriteableVariableDictionary dest_dict ) {

        if ( src_dict == src_bindings ) {
            throw new IllegalArgumentException() ;
        }

        VariableDictionaryIterator iter = src_dict.getIterator();

        while ( iter.hasNext() ) {
            Variable curr = iter.next() ;
            dest_dict.unBindVariable( curr.getName() );
            dest_dict.bindVariable( curr.getName(),
                                    curr.getValue().evaluate( src_dict ) );
        }
    }
}
