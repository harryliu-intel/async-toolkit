// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/util/functions/UnaryPredicate.java#4 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.util.functions;

/**
 * Represents a function mapping nothing to boolean.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date: 2002/02/23 $
 **/
public interface NullaryPredicate {
    boolean evaluate();
}
