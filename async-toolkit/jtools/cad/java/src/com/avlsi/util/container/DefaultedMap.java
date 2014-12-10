package com.avlsi.util.container;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class DefaultedMap<K,V> implements Map<K,V> {
    private final V defValue;
    private final Map<K,V> map;
    public DefaultedMap(final V defValue, final Map<K,V> map) {
        this.defValue = defValue;
        this.map = map;
    }

    public void clear() {
        map.clear();
    }

    public boolean containsKey(Object key) {
        return map.containsKey(key);
    }

    public boolean containsValue(Object val) {
        return map.containsValue(val);
    }

    public Set<Map.Entry<K,V>> entrySet() {
        return map.entrySet();
    }

    public boolean equals(Object o) {
        if (o instanceof DefaultedMap) {
            DefaultedMap that = (DefaultedMap) o;
            return ObjectUtils.equals(this.defValue, that.defValue) &&
                   ObjectUtils.equals(this.map, that.map);
        } else {
            return false;
        }
    }

    public V get(Object key) {
        final V result = map.get(key);
        return result == null && !map.containsKey(key) ? defValue : result;
    }

    public int hashCode() {
        return ObjectUtils.hashCode(map) + ObjectUtils.hashCode(defValue);
    }

    public boolean isEmpty() {
        return map.isEmpty();
    }

    public Set<K> keySet() {
        return map.keySet();
    }

    public V put(K key, V val) {
        return map.put(key, val);
    }

    public void putAll(Map<? extends K, ? extends V> m) {
        map.putAll(m);
    }

    public V remove(Object key) {
        return map.remove(key);
    }

    public int size() {
        return map.size();
    }

    public Collection<V> values() {
        return map.values();
    }
}
