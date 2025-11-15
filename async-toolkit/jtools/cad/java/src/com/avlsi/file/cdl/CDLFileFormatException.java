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

package com.avlsi.file.cdl;

/**
 * Class to represent exceptions in parsing of .cdl files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CDLFileFormatException extends Exception {
    public CDLFileFormatException(final String message) {
        super(message);
    }

    public CDLFileFormatException(String message, Throwable cause) {
        super(message, cause);
    }

    public CDLFileFormatException(Throwable cause) {
        super(cause);
    }
}
