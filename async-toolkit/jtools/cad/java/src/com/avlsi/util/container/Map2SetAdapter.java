/*
 * Copyright 2005 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 **/

package com.avlsi.util.container;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;

/**
 * Lets you make a Set out of a Map, for those rare cases (like
 * IdentityHashMap) where there is not already a Set version.
 *
 * @author Patrick Pelletier
 */

public class Map2SetAdapter extends AbstractSet {
    private final Object something = new Object();
    private final Map map;

    public Map2SetAdapter(Map map) {
        this.map = map;
    }

    public int size() {
        return map.size();
    }

    public Iterator iterator() {
        return map.keySet().iterator();
    }

    public boolean add(Object o) {
        return (map.put(o, something) == null);
    }

    public void clear() {
        map.clear();
    }
}
