/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.cast2.directive.impl.DirectiveStatement;
import com.avlsi.cast2.directive.impl.TokenInfo;

/**
 * Used by the parser to create directives.
 **/
public interface DirectiveFactoryInterface {
    /**
     * Return a DirectiveStatement that represents an if-statement.
     **/
    DirectiveStatement makeConditional(String guard,
                                       final DirectiveStatement[] body,
                                       TokenInfo info);

    /**
     * Return a DirectiveStatement that represents a loop.
     **/
    DirectiveStatement makeLoop(String var, String from, String to,
                                final DirectiveStatement[] body,
                                TokenInfo info);

    /**
     * Return a DirectiveStatement that assigns a value to a parameterized
     * directive.
     **/
    DirectiveStatement makeDefinition(String key, String parameter, String val,
                                      TokenInfo info);

    /**
     * Return a DirectiveStatement that assigns a value to a directive.
     **/
    DirectiveStatement makeDefinition(String key, String val, TokenInfo info);
}
