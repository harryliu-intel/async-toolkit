/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Represents the level and method for the simulation of a cell.
 * <p>
 * Represents the <code>level ...</code>alternative of the rule
 * <code>levelspec</code> in the <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CoSimLevelSpec implements LevelSpecInterface,
                                             AcceptorInterface {
    /**
     * Level at which the cell should be simulated.  May not be negative.
     **/
    private final int level;

    /**
     * Method in which the cell should be simulated.  May not be null.
     **/
    private final CoSimSpecList coSimSpecList;

    /**
     * Class constructor.
     *
     * @param level  Level at which the cell should be simulated.  May not
     *   be negative.
     * @param coSimSpecList  Method in which the cell should be simulated.
     *   May not be null.
     *
     * @throws IllegalArgumentException  If <code>level</code> is negative.
     **/
    public CoSimLevelSpec(
            final int level,
            final CoSimSpecList coSimSpecList) {
        if (level < 0)
            throw new IllegalArgumentException
                ("level must be non-negative: " + level);

        this.level = level;
        this.coSimSpecList = coSimSpecList;
    }

    /**
     * Returns the level at which the cell should be simulated.
     * @return level at which the cell should be simulated, non-negative
     **/
    public int getLevel() {
        return level;
    }

    /**
     * Returns the method in which the cell should be simulated.
     * @return method in which the cell should be simulated, not null
     **/
    public CoSimSpecList getCoSimSpecList() {
        return coSimSpecList;
    }

    public String toString() {
        return level + coSimSpecList.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitCoSimLevelSpec(this);
    }
}
