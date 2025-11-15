// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.DirectiveVisitor;
import com.avlsi.cast2.directive.impl.TokenInfo;

public class DirectiveConditional implements DirectiveStatement {
    private final String guard;
    private final DirectiveStatement[] body;
    private final TokenInfo info;

    public DirectiveConditional(final String guard,
                                final DirectiveStatement[] body,
                                final TokenInfo info) {
        this.guard = guard;
        this.body = body;
        this.info = info;
    }

    public void visit(final DirectiveVisitor v) {
        v.conditional(guard, body, info);
    }
}
