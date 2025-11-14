// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

/**
 * Thrown to indicate that a cosimulation could not be carried out because
 * no specified behavior was found.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class NoBehaviorFoundException extends Exception {

    /**
     * Constructs a <code>NoSuchCellException</code> indicating
     * that the specified cell could not be found.
     **/
    public NoBehaviorFoundException(final String message) {
        super(message);
    }
}
