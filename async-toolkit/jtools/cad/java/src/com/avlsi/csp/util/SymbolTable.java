/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

public class SymbolTable<V> {
    private final Map<String,LinkedList<V>> table;
    private final LinkedList<Set<String>> history;
    private Set<String> scope;

    public SymbolTable() {
        table = new HashMap<String,LinkedList<V>>();
        history = new LinkedList<Set<String>>();
        scope = new HashSet<String>();
    }

    public void enterScope() {
        history.addFirst(scope);
        scope = new HashSet<String>();
    }

    public void leaveScope() {
        for (String s : scope) table.get(s).removeFirst();
        scope = history.removeFirst();
    }

    public V lookup(final String s) {
        final LinkedList<V> l = table.get(s);
        return l == null || l.isEmpty() ? null : l.getFirst();
    }

    public V lookupCurrent(final String s) {
        return scope.contains(s) ? lookup(s) : null;
    }

    public boolean bind(final String s, final V v) {
        if (!scope.add(s)) return false;

        LinkedList<V> l = table.get(s);
        if (l == null) {
            l = new LinkedList<V>();
            table.put(s, l);
        }
        l.addFirst(v);
        return true;
    }
}
