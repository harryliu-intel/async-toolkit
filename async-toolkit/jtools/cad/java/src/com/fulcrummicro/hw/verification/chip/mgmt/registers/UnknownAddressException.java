/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.fulcrummicro.hw.verification.chip.mgmt.registers;

/* Used in RegisterInfo */
public class UnknownAddressException extends Exception {

    private static final long serialVersionUID = 3405707352841364841L;

    public UnknownAddressException(String message) {
        super(message);
    }

}

