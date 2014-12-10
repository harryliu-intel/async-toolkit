package com.avlsi.csp.util;

import java.util.IdentityHashMap;
import java.util.Map;

public class UniqueLabel {
    private final Map map;
    public UniqueLabel() {
        this(new IdentityHashMap());
    }
    public UniqueLabel(final Map map) {
        this.map = map;
    }
    public int getLabel(final Object o) {
        Integer i = (Integer) map.get(o);
        if (i == null) {
            i = new Integer(map.size());
            map.put(o, i);
        }
        return i.intValue();
    }
}
