// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.coverage;

/**
 * To be thrown in simple cases such as where a user tries to
 * instantiate an object from a string with the wrong format. Used by
 * classes including ParseRange, ParsePosition, ProbeInfo,
 * Monitor.RegisteredType. 
 *
 * XXX: There should be a standard class for this, but I couldn't find
 * it.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class ParseException extends Exception {
    public ParseException(final String message) {
        super(message);
    }
}
