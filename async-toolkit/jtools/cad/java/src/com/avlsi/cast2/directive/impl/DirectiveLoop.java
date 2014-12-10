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

public class DirectiveLoop implements DirectiveStatement {
    private final String var, from, to;
    private final DirectiveStatement[] body;
    private final TokenInfo info;

    public DirectiveLoop(String var, String from, String to,
                         final DirectiveStatement[] body,
                         final TokenInfo info) {
        this.var = var;
        this.from = from;
        this.to = to;
        this.body = body;
        this.info = info;
    }

    public void visit(final DirectiveVisitor v) {
        v.loop(var, from, to, body, info);
    }
}
