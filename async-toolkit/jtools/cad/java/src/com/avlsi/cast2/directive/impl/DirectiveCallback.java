// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast.impl.Environment;

/**
 * API used by the directives factory to resolve parameters in a block specific
 * way.
 **/
public interface DirectiveCallback {
    /**
     * Resolves a block specific parameter given the specified environment.
     * @param type type of the parameter
     * @param value value of the parameter
     * @param env Environment containing any loop variables
     * @return If the parameter is of the correct type, whatever is necessary
     * to the consumer of directives should be returned, otherwise
     * <code>null</code>.
     **/
    Object resolve(String type, String value, Environment env);
}
