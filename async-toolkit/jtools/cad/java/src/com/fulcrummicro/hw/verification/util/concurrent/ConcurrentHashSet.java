package com.fulcrummicro.hw.verification.util.concurrent;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class ConcurrentHashSet<T> implements Set<T> {

    private final ConcurrentMap<T, Boolean> map;

    public ConcurrentHashSet() {
        map = new ConcurrentHashMap<T, Boolean>();
    }

    public boolean add(T o) {
        return this.map.put(o, Boolean.TRUE) == null;
    }

    public boolean addAll(Collection<? extends T> c) {
        Iterator<? extends T> it;
        boolean modified = false;

        for (it = c.iterator(); it.hasNext(); )
            modified |= this.add(it.next());
        return modified;
    }

    public void clear() {
        this.map.clear();
    }

    public boolean contains(Object o) {
        return this.map.containsKey(o);
    }

    public boolean containsAll(Collection<?> c) {
        return this.map.keySet().containsAll(c);
    }

    public boolean isEmpty() {
        return this.map.isEmpty();
    }

    public Iterator<T> iterator() {
        return this.map.keySet().iterator();
    }

    public boolean remove(Object o) {
        return this.map.remove(o) != null;
    }

    public boolean removeAll(Collection<?> c) {
        Iterator<?> it;
        boolean modified = false;

        for (it = c.iterator(); it.hasNext(); )
            modified |= this.remove(it.next());
        return modified;
    }

    public boolean retainAll(Collection<?> c) {
        Iterator<?> it;
        boolean modified = false;

        for (it = this.iterator(); it.hasNext(); ) {
            if (!c.contains(it.next())) {
                it.remove();
                modified = true;
            }
        }
        return modified;
    }

    public Object[] toArray() {
        return this.map.keySet().toArray();
    }

    @SuppressWarnings("hiding")
    public <T> T[] toArray(T[] a) {
        return this.map.keySet().toArray(a);
    }

    public int size() {
        return this.map.size();
    }

}
