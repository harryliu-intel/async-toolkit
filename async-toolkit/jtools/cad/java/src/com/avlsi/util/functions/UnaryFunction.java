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

package com.avlsi.util.functions;

/**
 * Represents a function mapping from Object to Object.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
@FunctionalInterface
public interface UnaryFunction<A,Z> {
    Z execute(A a);
}
