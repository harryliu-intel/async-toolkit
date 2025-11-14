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

package com.avlsi.csp.ast;

/**
 * Abstract base class for Exceptions thrown by visitors.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class VisitorException extends Exception {
    public VisitorException(final String message) {
        super(message);
    }
    public VisitorException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
