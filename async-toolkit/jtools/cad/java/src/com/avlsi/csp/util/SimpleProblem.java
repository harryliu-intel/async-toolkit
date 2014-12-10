/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.util;

import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.io.Printable;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;

public abstract class SimpleProblem implements Problem {
    protected final String errorCode;
    protected final ParseRange pr;
    protected final Object[] args;
    public SimpleProblem(final String errorCode, final ParseRange pr,
                         final Object[] args) {
        this.errorCode = errorCode;
        this.pr = pr;
        this.args = args;
    }
    public ParseRange getParseRange() {
        return pr;
    }
    public String getCode() {
        return errorCode;
    }
    public Object[] getArguments() {
        return args;
    }
    public abstract String getMessage();
    public void printMessage(Printable out) {
        ExceptionPrettyPrinter.prettyMessage(getMessage().trim(),
                                             pr.start.filename,
                                             pr.start.line,
                                             pr.start.column + 1,
                                             out);
    }
}
