/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Represents a list of modes at which simulation should be attempted.
 * <p>
 * Represents the <code>mode_list<code>alternative of the rule
 * <code>levelspec</code> in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ModeListLevelSpec implements LevelSpecInterface,
                                                AcceptorInterface {
    /**
     * List of modes.  May not be null.
     **/
    private final ModeList modeList;

    /**
     * Class constructor.
     *
     * @param modeList  list of modes.  May not be null
     **/
    public ModeListLevelSpec(final ModeList modeList) {
        this.modeList = modeList;
    }

    /**
     * Returns list of modes.
     * @return list of modes, not null
     **/
    public ModeList getModeList() {
        return modeList;
    }

    public String toString() {
        return modeList.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitModeListLevelSpec(this);
    }
}
