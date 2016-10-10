/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Represents a list of cosimulation modes.
 * <p>
 * Represents rule <code>mode_list</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ModeList implements AcceptorInterface {
    /**
     * List of modes in which to attempt simulation.  May not be null.
     **/
    private final Mode[] modes;

    /**
     * Class constructor.
     *
     * @param modes  Modes in which to attempt simulation.
     *   May not be null.
     **/
    public ModeList(final Mode[] modes) {
        // make sure that each mode is specified at most once
        final Set modeSet = new HashSet();
        for (int i = 0; i < modes.length; ++i) {
            final Mode mode = modes[i];
            if (modeSet.contains(mode))
                throw new IllegalArgumentException("Duplicate mode: " + mode);

            modeSet.add(mode);
        }

        this.modes = modes;
    }

    /**
     * Returns an <code>Iterator&lt;Mode&gt;</code> of the
     *   modes  in which to attempt simulation.
     * @return an <code>Iterator&lt;Mode&gt;</code> of the
     *   methods in which to attempt simulation, not null.
     **/
    public Iterator<Mode> iterator() {
        return Collections.unmodifiableList(Arrays.asList(modes)).iterator();
    }

    /**
     * Returns an <code>Stream&lt;Mode&gt;</code> of the
     *   modes  in which to attempt simulation.
     * @return an <code>Stream&lt;Mode&gt;</code> of the
     *   methods in which to attempt simulation, not null.
     **/
    public Stream<Mode> stream() {
        return Stream.of(modes);
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        boolean firstP = true;

        for (int i = 0; i < modes.length; ++i) {
            if (firstP)
                firstP = false;
            else
                sb.append(',');
            sb.append(modes[i].toString());
        }

        return sb.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitModeList(this);
    }
}
