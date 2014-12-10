/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/**
 * This is a type-safe enumeration which represents the two directions
 * a node can move in: up and down.
 */

public abstract class Direction {
    private Direction() {}
    public abstract Direction opposite();
    public static final Direction UP = new Direction() {
            public String toString() { return "+"; }
            public Direction opposite() { return DOWN; }
        };
    public static final Direction DOWN = new Direction() {
            public String toString() { return "-"; }
            public Direction opposite() { return UP; }
        };
}
