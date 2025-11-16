// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable;

import com.avlsi.util.mathexpression.MathExpression;

/**
   Interface to a variable.
 */
public interface WriteableVariable extends Variable {
    
    /**
       Sets the value of the variable to the specified expression.
       @param Exp The new value for the variable.  This MathExpression
       may contain references to other variables.
     */
    void setValue( MathExpression Exp );

}
