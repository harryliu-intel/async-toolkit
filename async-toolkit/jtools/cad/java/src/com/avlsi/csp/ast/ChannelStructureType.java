/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Class for CSP structure types of channels
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class ChannelStructureType extends Type {
    /**
     * Name of the structure.
     **/
    private String name;

    private final Map/*<String,Type>*/ members;

    public ChannelStructureType (final String name) {
        this.name = name;
        this.members = new HashMap/*<String,Type>*/();
    }

    public int dimension() {
        return 0;
    }

    /**
     * Get the name of the structure.
     *
     * @return name of the structure
     **/
    public String getName() {
        return name;
    }

    public void addMember(final /*@ non_null @*/ String name,
                          final /*@ non_null @*/ Type type) {
        members.put(name, type);
    }

    public Type getMemberType(final /*@ non_null @*/ String name) {
        return (Type) members.get(name);
    }

    /**
     * Returns an unmodifiable map of the members and their types.
     *
     * @return members of the structure
     **/
    public Map/*<String,Type>*/ getMembers() {
        return Collections.unmodifiableMap(members);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitChannelStructureType(this);
    }

    public String toString() {
        return "defchan " + name;
    }
}
