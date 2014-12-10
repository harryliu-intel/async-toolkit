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

public class HashCounter extends Counter {
    private final Map map;

    public HashCounter() {
        this(new HashMap());
    }

    public HashCounter(final Map map) {
        this.map = map;
    }
    
    public void add(Object o, int count) {
        if(map.containsKey(o)) {
            Integer i = (Integer)map.get(o);
            map.put(o, new Integer(i.intValue()+count) );
        }
        else {
            map.put(o, new Integer(count));
        }
    }
  
    public int getCount(Object o) {
        Integer i = (Integer)map.get(o);
        return (i==null ? 0 : i.intValue());
    }

    public Collection elements() {
        return map.keySet();
    }

}
