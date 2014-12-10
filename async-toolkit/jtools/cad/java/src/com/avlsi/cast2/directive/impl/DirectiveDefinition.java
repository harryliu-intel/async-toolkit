/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.DirectiveVisitor;
import com.avlsi.cast2.directive.impl.TokenInfo;

public class DirectiveDefinition implements DirectiveStatement {
    private final String key, parameter, val;
    private final TokenInfo info;

    public DirectiveDefinition(String key, String val, TokenInfo info) {
        this(key, null, val, info);
    }

    public DirectiveDefinition(String key, String parameter, String val,
                               TokenInfo info) {
        this.key = key;
        this.parameter = parameter;
        this.val = val;
        this.info = info;
    }

    public void visit(final DirectiveVisitor v) {
        if (parameter == null) {
            v.definition(key, val, info);
        } else {
            v.definition(key, parameter, val, info);
        }
    }
}
