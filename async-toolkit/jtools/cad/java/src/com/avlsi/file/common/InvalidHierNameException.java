// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.common;

/**
 * Exception throws for invalied HierNames.
 *
 * @see HierName
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class InvalidHierNameException extends Exception {
    InvalidHierNameException() {
        super();
    }

    InvalidHierNameException(final String detail) {
        super(detail);
    }

    InvalidHierNameException(final String detail, final Throwable cause) {
        super(detail, cause);
    }
}
