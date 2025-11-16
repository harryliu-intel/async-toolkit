// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.util;

public interface DirectiveActionFilter {
    /**
     * Intercept calls to the given <code>DirectiveActionInterface</code>, do
     * processing on the arguments of the call, and optionally forward the
     * call.
     *
     * @param action The underlying <code>DirectiveActionInterface</code>
     * @return a new <code>DirectiveActionInterface</code> that does the
     * appropriate manipulations.
     **/
    DirectiveActionInterface filter(DirectiveActionInterface action);
}
