/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.HashMap;
import java.util.Collection;
import java.util.Map;

public class HashCounter<E> extends Counter<E> {
    private final Map<E,Integer> map;

    public HashCounter() {
        this(new HashMap<E,Integer>());
    }

    public HashCounter(final Map<E,Integer> map) {
        this.map = map;
    }
    
    public void add(E e, int count) {
        if(map.containsKey(e)) {
            map.put(e, map.get(e) + count);
        } else {
            map.put(e, count);
        }
    }
  
    public int getCount(Object e) {
        Integer count = map.get(e);
        return count == null ? 0 : count;
    }

    public Collection<E> elements() {
        return map.keySet();
    }

}
