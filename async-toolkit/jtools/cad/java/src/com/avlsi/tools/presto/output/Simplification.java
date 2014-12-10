/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * A Simplification represents a way that a NodeExpression can be made
 * simpler.  (By removing references to Vdd and GND, for example.)
 * Simplification is mostly an opaque data type which can be used to
 * store anything the particular NodeExpression implementation wants to
 * store.  The only public method is isSimpler(), which indicates whether
 * the Simplification will actually do anything.  (If it returns false,
 * the Simplification is a no-op.)  The reason for Simplification objects
 * is because you need to look at the NodeExpression when it is
 * unparameterized and canonicalized, in order to determine how to
 * simplifying it, but we want to actually apply the simplification to
 * the original NodeExpression which may be parameterized or have
 * non-canonical node names.  Therefore, getSimplification() and
 * applySimplification() are separate methods, with the Simplification
 * object to communicate between them.
 */

public interface Simplification {
    boolean isSimpler();
}
