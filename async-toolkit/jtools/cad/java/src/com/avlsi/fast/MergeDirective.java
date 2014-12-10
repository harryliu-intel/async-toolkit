/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Iterator;

import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;

/**
 * DirectiveBlock that represents the merging of 2 DirectiveBlocks.
 * Essentially, requests will be forwarded to one DirectiveBlock, and if
 * unsuccessful, to the other DirectiveBlock.
 **/

public class MergeDirective extends DirectiveBlock {
    private DirectiveInterface child;
    private Map valueCache;

    /**
     * Constructs a MergeDirective from 2 DirectiveBlocks.
     * @param parent Containing "fall-through" directives, consulted second.
     * @param child Containing overriding directives, consulted first.
     **/
    public MergeDirective(DirectiveInterface parent, DirectiveInterface child) {
        super(parent);
        this.child = child;
        this.valueCache = null;
    }

    public Object getDefaultValue(String key, String memberType) throws UnknownDirectiveException {
        try {
            return child.getDefaultValue(key, memberType);
        } catch (UnknownDirectiveException e) {
            return directives.getDefaultValue(key, memberType);
        }
    }

    public Map getValues(String key, String memberType) throws UnknownDirectiveException {
        Map p = null;
        try {
            p = directives.getValues(key, memberType);
        } catch (UnknownDirectiveException e) {
            return child.getValues(key, memberType);
        }

        Map c = null;
        try {
            c = child.getValues(key, memberType);
        } catch (UnknownDirectiveException e) {
            return p;
        }

        if (valueCache == null) valueCache = new HashMap();

        final Pair k = new Pair(key, memberType);
        Map result = (Map) valueCache.get(k);
        if (result == null) {
            result = new LinkedHashMap();
            result.putAll(p);
            result.putAll(c);
            valueCache.put(k, result);
        }
        return result;
    }

    public Object lookup(String key) throws UnknownDirectiveException {
        try {
            if (child.containsDirective(key)) return child.lookup(key);
        } catch (UnknownDirectiveException e) { }
        return directives.lookup(key);
    }

    public boolean containsDirective(String key) throws UnknownDirectiveException {
        return child.containsDirective(key) || directives.containsDirective(key);
    }

    public Object lookup(String key, String memberType, Object parameter) throws UnknownDirectiveException {
        try {
            Object o = child.lookup(key, memberType, parameter);
            if (o != child.getDefaultValue(key, memberType)) return o;
        } catch (UnknownDirectiveException e) { }
        return directives.lookup(key, memberType, parameter);
    }

    public boolean isKey(String key) {
        return child.isKey(key) || directives.isKey(key);
    }

    public boolean isKey(String key, String memberType) {
        return child.isKey(key, memberType) || directives.isKey(key, memberType);
    }
    
    public Iterator noparamEntryIterator() {
        HashMap mergeMap = new HashMap();
        for(Iterator i=super.noparamEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            mergeMap.put( entry.getKey(), entry.getValue() );
        }
        for(Iterator i=child.noparamEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            mergeMap.put( entry.getKey(), entry.getValue() );
        }
        
        return mergeMap.entrySet().iterator();
    }

    public Iterator paramEntryIterator() {
        HashMap mergeMap = new HashMap();
        for(Iterator i = super.paramEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            mergeMap.put(entry.getKey(), new HashMap((Map) entry.getValue()));
        }
        for(Iterator i = child.paramEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            Object key = entry.getKey();
            Map m = (Map) mergeMap.get(key);
            if (m == null) {
                m = new HashMap();
                mergeMap.put(key, m);
            }
            m.putAll((Map) entry.getValue());
        }
        
        return mergeMap.entrySet().iterator();
    }  
}
