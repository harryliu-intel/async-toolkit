/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;


import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;

import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;

/**
   Factory interface to be implemented by all collections of implementations
   of the StringExpression interface.  This interface provides methods
   to make all the types of operators mentioned in the visitor interface.
 */
public interface StringExpressionFactory {
    
    /**
       Method to make an empty collection of expressions.
       @return An empty collection of expressions.
     */
    WriteableStringExpressionCollection makeExpressionCollection( ) ;

    /**
       Makes an expression that evaluates to the specified
       constant value.
       @param constValue The value the returned expression will
       evaluate to.
       @return An expression that evaluates to the specified
       constant value.  The returned expression's isConstant method
       should return true.
     */
    StringExpression makeConstant( final String constValue );
    
    /**
       Makes a reference to the specified variable.
       @param varName The name of the variable to reference.
       @return An expression that is a reference to the specified variable.
     */
    StringExpression makeVariableReference( final String varName );

    /**
       Makes an expression that is the concatenation of the expressions in the
       specified collection of expressions.
       @param terms The collection of string expressions to be concatened
       together in the returned expression.
       @return An expression that is the concatenation of all the expressions
       in the specified collection of expressions.
     */
    StringExpression makeConcatOperator( final StringExpressionCollection terms);

    /**
       Makes an expression that is a reference to another expression.
       @param subBindings A variable dictionary that defines some set
       of the variables used in the subExpression in terms of other expressions.
       @param subExpression The referenced expression.
       @return An expression that is a reference to another expression.
     */
    StringExpression makeSubExprRef( final StringVariableDictionary subBindings,
				     final StringExpression subExpression );

}
