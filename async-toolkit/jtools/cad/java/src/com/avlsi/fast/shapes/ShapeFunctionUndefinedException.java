// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes;


public class ShapeFunctionUndefinedException extends Exception {
    public ShapeFunctionUndefinedException(final String func) {
        super("ShapeFunction " + func + " undefined.");
    }
}
