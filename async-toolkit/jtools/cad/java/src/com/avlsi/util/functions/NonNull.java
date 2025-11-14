// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.util.functions;

/**
 * A UnaryPredicate which returns true if the argument is non-null.
 * Useful for filtering nulls out of a list.
 */
public class NonNull implements UnaryPredicate {
    public boolean evaluate(Object a) {
        return (a != null);
    }
}
