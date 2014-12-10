package com.avlsi.csp.util;

import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.io.Printable;

public interface Problem {
    ParseRange getParseRange();
    String getCode();
    String getMessage();
    void printMessage(Printable pw);
    Object[] getArguments();
}
