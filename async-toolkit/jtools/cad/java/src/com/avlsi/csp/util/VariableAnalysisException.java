/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.util;

/**
 * Class for exceptions thrown by {@link VariableAnalyzer#getResults}.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class VariableAnalysisException extends Exception {
    VariableAnalysisException(String s) {
        super(s);
    }
    VariableAnalysisException(String s, Throwable cause) {
        super(s, cause);
    }
}
