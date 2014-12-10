/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

import java.io.StringReader;

import antlr.ANTLRException;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.cast2.directive.DirectiveConstants;

public class CoSimSpecCallback implements DirectiveCallback {
    private static CoSimSpecCallback singleton = null;
    private CoSimSpecCallback() { }
    public static CoSimSpecCallback getInstance() {
        if (singleton == null) singleton = new CoSimSpecCallback();
        return singleton;
    }
    public Object resolve(String type, String value, Environment env) {
        assert type.equals(DirectiveConstants.COSIM_SPEC_TYPE);
        final CoSimLexer lexer = new CoSimLexer(new StringReader(value));
        final CoSimParser parser = new CoSimParser(lexer);
        try {
            return parser.cosimSpecDirective();
        } catch (antlr.ANTLRException e) {
            return null;
        } catch (IllegalArgumentException e) {
            return null;
        }
    }
}
