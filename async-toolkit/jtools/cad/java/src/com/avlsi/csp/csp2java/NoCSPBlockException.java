// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/csp/csp2java/SemanticException.java#2 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.csp.csp2java;

/**
 * Exception thrown if cell has no csp block.
 *
 * @author Kim Wallmark
 * @version $Revision: #2 $ $Date: 2002/02/23 $
 **/
public class NoCSPBlockException extends SemanticException {
    /**
     * Class constructor.
     **/
    public NoCSPBlockException() {
        super("No CSP block found");
    }
}
