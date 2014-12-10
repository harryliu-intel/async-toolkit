/*
 * Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.csp.util;

import com.avlsi.csp.ast.AbstractASTNodeInterface;
import com.avlsi.csp.ast.VisitorException;
import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import java.io.PrintStream;

public class VisitorExceptionWithLocation extends VisitorException {
    private final ParseRange pr;

    public VisitorExceptionWithLocation(final String message, ParseRange pr) {
        super(message);
        this.pr = pr;
    }

    public VisitorExceptionWithLocation(final String message,
                                        AbstractASTNodeInterface node) {
        this(message, node.getParseRange());
    }

    public ParseRange getParseRange() {
        return pr;
    }

    public void printSelf(PrintStream out) {
        ExceptionPrettyPrinter.prettyMessage(super.getMessage().trim(),
                                             pr.start.filename,
                                             pr.start.line, pr.start.column+1,
                                             out);
    }

    public String getMessage() {
        String msg = super.getMessage();
        if (msg.endsWith("\n")) {
            return pr.fullString() + ":\n" + msg;
        } else {
            return msg + " at " + pr.fullString();
        }
    }
}
