/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

/**
 * A type-safe key/value pair of {@link Symbol} and {@link Value}
 * representing names accessable in an {@link Environment} and their
 * value.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class EnvironmentEntry {
    private final Symbol name;
    private final Value value;

    EnvironmentEntry(Symbol name, Value value) {
        this.name = name;
        this.value = value;
    }

    public Symbol getName() {
        return name;
    }

    public Value getValue() {
        return value;
    }

    public String toString() {
        return getClass().getName() + '[' + name + ':' + value + ']';
    }
}
