// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast;

/**
 * Exception thrown by {@link EnvBlock} for duplicated named environments.
 **/
public final class NamedEnvironmentRedeclaredException extends Exception {
    public NamedEnvironmentRedeclaredException(final String detail) {
        super(detail);
    }

    public String toString() {
        return "NamedEnvironmentRedeclaredException: "
            + getMessage();
    }
}
