/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.complete;

import java.util.Collection;

public class Node {
    public static final int ACTIVE_HIGH = 1;
    public static final int ACTIVE_LOW = -1;
    public static final int UNKNOWN_SENSE = 0;

    public static final int NO_RAIL = -1;

    /** must have 0 children */
    public static final int TERMINAL = 0;

    /** must have 1 or more children (if 1 child, actually an inverter)
     * if ACTIVE_HIGH, actually a nand */
    public static final int NOR = 1;

    /** 2 children = C2
     * 3 children = AND2C2 (first two children are anded together)
     * 4 children = 2AND2C2 */
    public static final int CELEMENT = 2;

    private final int type;
    private final String name;
    private final int[] arrayIndices;
    private final int rail;
    private final int tier;
    private final Object cookie;
    private final Node[] children;

    /** Returns the type of this node: TERMINAL, NOR, or CELEMENT */
    public int getType() {
        return type;
    }

    /** Returns the sense of this node: ACTIVE_HIGH or ACTIVE_LOW. */
    public int getSense() {
        return ((tier & 1) == 0) ? ACTIVE_HIGH : ACTIVE_LOW;
    }

    /** Returns the base name of the channel.  This is just
     * alphanumeric (plus underscores.)  "null" indicates an
     * anonymous node, which are legal in some places but not
     * in others. */
    public String getName() {
        return name;
    }

    /** Returns the array index of the channel.  If the channel
     * is a scalar channel, the returned array is of length zero.
     * If the channel is a one-dimensional array, the returned
     * array is of length 1, with the one element being the array
     * index.  For multi-dimensional channels, the returned array
     * has one element for each dimension, specifying the index
     * for that dimension, reading left-to-right. */
    public int[] getArrayIndices() {
        return arrayIndices;
    }

    /** If this node is part of a channel, then this is the rail number.
     * (The part after the dot.)  If this node is not part of a channel,
     * returns NO_RAIL. */
    public int getRail() {
        return rail;
    }

    /** Returns the tier of this node.  (Number of transitions from
     * the starting point, where 0 is the starting point.) */
    public int getTier() {
        return tier;
    }

    /** Returns the cookie of this node.  As far the the completion code
     * is concerned, cookies are arbitrary and have no meaning.
     * The template code uses them to keep track of which section
     * (input, output, ack) a production rule goes in. */
    public Object getCookie() {
        return cookie;
    }

    /** Returns the number of children. */
    public int size() {
        return children.length;
    }

    /** Returns the specified child node. */
    public Node get(int idx) {
        return children[idx];
    }

    private static final Node[] ZERO_LENGTH_NODE_ARRAY = new Node[0];

    /* Construct a new terminal Node. */
    public Node(String name, int[] arrayIndices, int rail, int tier,
                Object cookie) {
        this.type = TERMINAL;
        this.name = name;
        this.arrayIndices = arrayIndices;
        this.rail = rail;
        this.tier = tier;
        this.cookie = cookie;
        this.children = ZERO_LENGTH_NODE_ARRAY;
    }

    /* Construct a new nonterminal node from an array. */
    public Node(int type, String name, int[] arrayIndices, int tier,
                Object cookie, Node[] children) {
        this.type = type;
        this.name = name;
        this.arrayIndices = arrayIndices;
        this.rail = NO_RAIL;
        this.tier = tier;
        this.cookie = cookie;
        this.children = children;
    }

    /* Construct a new nonterminal node from a collection. */
    public Node(int type, String name, int[] arrayIndices, int tier,
                Object cookie, Collection children) {
        this(type, name, arrayIndices, tier, cookie,
             (Node[]) children.toArray(ZERO_LENGTH_NODE_ARRAY));
    }
}
