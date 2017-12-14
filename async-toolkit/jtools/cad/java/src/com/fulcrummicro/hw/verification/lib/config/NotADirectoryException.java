/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.lib.config;

public class NotADirectoryException extends Exception {

    private static final long serialVersionUID = -141608299549585117L;

    public NotADirectoryException() {
        this("");
    }

    public NotADirectoryException(String msg) {
        super(msg);
    }

}
