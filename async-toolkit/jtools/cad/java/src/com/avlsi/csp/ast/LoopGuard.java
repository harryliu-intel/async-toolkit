/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.csp.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Loop guards, <code>&lt; sep i : M..N : stmt &gt;</code>.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class LoopGuard extends AbstractLoop
    implements GuardedCommandInterface {

    /** Guards are in a deterministic construct.  **/
    public static final int BOX = 0;

    /** Guards are in a non-deterministic construct.  **/
    public static final int COLON = 1;

    /** List of guard commands, not null.  **/
    private final List /*<GuardedCommandInterface>*/ guards;

    /**
     * Class constructor.
     *
     * @param indexVar loop index variable, not null
     * @param range minimum and maximum of range for loop index, not null
     * @param separator  separator code, one of {@see #BOX},
     *     {@see #COLON}
     **/
    public LoopGuard(final String indexVar,
            final Range range,
            final int separator) {
        super(indexVar, range, separator);
        this.guards = new ArrayList();
    }

    /**
     * Add a guard.
     *
     * @param guard guard to be added
     **/
    public void addGuard(final GuardedCommandInterface guard) {
        guards.add(guard);
    }

    /**
     * Returns a list of guards.
     *
     * @return a list of {@link GuardedCommandInterface}, not null
     **/
    public List getGuards() {
        return Collections.unmodifiableList(guards);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitLoopGuard(this);
    }
}
