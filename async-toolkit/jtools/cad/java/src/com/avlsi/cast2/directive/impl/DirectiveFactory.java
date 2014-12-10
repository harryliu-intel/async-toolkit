/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.util.HashMap;
import java.util.Map;

import com.avlsi.util.container.Pair;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.TokenInfo;

/**
   Used by the parser to create directives.
 */
public class DirectiveFactory implements DirectiveFactoryInterface {
    public DirectiveStatement makeConditional(String guard, 
                                              final DirectiveStatement[] body,
                                              TokenInfo info) {
        return new DirectiveConditional(guard, body, info);
    }
    public DirectiveStatement makeLoop(String var, String to, String from,
                                       final DirectiveStatement[] body,
                                       TokenInfo info) {
        return new DirectiveLoop(var, to, from, body, info);
    }

    public DirectiveStatement makeDefinition(String key, String val,
                                             TokenInfo info) {
        return new DirectiveDefinition(key, val, info);
    }

    public DirectiveStatement makeDefinition(String key, String parameter,
                                             String val, TokenInfo info) {
        return new DirectiveDefinition(key, parameter, val, info);
    }
}
