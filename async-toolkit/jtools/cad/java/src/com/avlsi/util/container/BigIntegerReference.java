// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.math.BigInteger;

/**
 * <code>BigInteger</code> reference implementation.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class BigIntegerReference
    implements BigIntegerReferenceInterface {

    private BigInteger value;

    public BigIntegerReference() {
        this.value = null;
    }

    public BigIntegerReference(final BigInteger value) {
        this.value = value;
    }

    public void setBigInteger(BigInteger value) {
        this.value = value;
    }

    public BigInteger getBigInteger() {
        return value;
    }
}
