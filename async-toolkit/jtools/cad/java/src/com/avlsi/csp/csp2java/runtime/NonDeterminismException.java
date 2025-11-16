// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2java.runtime;

/**
 * Exception thrown by CSP runtime if non-determinism occurs in a deterministic
 * statement.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class NonDeterminismException extends RuntimeException {
    /**
     * Class constructor.
     **/
    public NonDeterminismException(final String message) {
        super(message);
    }
}
