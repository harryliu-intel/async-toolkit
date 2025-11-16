// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

/**
 * Net type used by {@link NetSink} and {@link NetSource}.
 * 
 * @see NetSink
 * @see NetSource
 **/
public final class NetType {
    /**
     * This class should not be instantiated.
     **/
    private NetType() { }

    /** 0: half-operator and transistor. */
    public static final int HALF_OPERATOR_TRANSISTOR = 0;
    /** 1: cell-type, only used during global net generation **/
    public static final int CELL = 1;
    /** 2: capacitive load, for primary output **/
    public static final int CAPACITIVE_LOAD = 2;
}
