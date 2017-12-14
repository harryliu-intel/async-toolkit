/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.lib.config;

import java.util.HashMap;
import java.util.Map;

public class TestNamespace {
    static public void main(String args[]) {
        Namespace n = new Namespace();
        Namespace m = new Namespace();
        n.put("a.b","x0");
        n.put("a.c","x1");
        n.put("a.f","x9");
        n.put("b.d","x2");
        n.put("d","x5");
        assert(n.get("a.b")=="x0");
        assert(n.get("a.c")=="x1");
        assert(n.get("a.f")=="x9");
        assert(n.get("b.d")=="x2");
        assert(n.get("d")=="x5");

        m.put("b","x3");
        m.put("c","x4");
        m.put("e","x6");
        assert(m.get("b")=="x3");
        assert(m.get("c")=="x4");
        assert(m.get("e")=="x6");

        n.merge("a",m);
        n.remove("d");
        n.merge("d",m);
        assert(n.get("a.b")=="x3");
        assert(n.get("a.c")=="x4");
        assert(n.get("a.e")=="x6");
        assert(n.get("a.f")=="x9");
        assert(n.get("d.b")=="x3");
        assert(n.get("d.c")=="x4");
        assert(n.get("d.e")=="x6");

        assert false : "passed";
        System.err.println("you must turn assertions on for this test to work");
    }
}
