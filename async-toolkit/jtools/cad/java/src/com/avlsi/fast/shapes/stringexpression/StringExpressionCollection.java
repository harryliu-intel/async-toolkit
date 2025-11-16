// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression;

import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;

/**
   Iterface to a collection of expressions.
   @see com.avlsi.fast.shapes.stringexpression.StringExpression
 */
public interface StringExpressionCollection {
    /**
       Gets an iterator that will enumerate all the expressions
       in the collection.
       @return An iterator that will enumerate all the expressions
       in the collection.
     */
    StringExpressionCollectionIterator getIterator();

    /**
       Determins if all the members of the collection are constant.
       @return true if all the expressions in the collection
       have constant values.
     */
    boolean allMembersAreConstant( );

    /**
       @return The number of expressions in the collection.
    */
    int size();

}
