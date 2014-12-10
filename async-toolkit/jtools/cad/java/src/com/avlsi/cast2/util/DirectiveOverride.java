/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Iterator;

import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.util.container.Pair;

/**
 * Redirect all operations related to a particular directive to another
 * directive interface.
 **/
public class DirectiveOverride implements DirectiveInterface {
    private final DirectiveInterface original, replacement;
    private final String which;
    public DirectiveOverride(final DirectiveInterface original,
                             final DirectiveInterface replacement,
                             final String which) {
        assert which != null && original != null && replacement != null;
        this.original = original;
        this.replacement = replacement;
        this.which = which;
    }

    public Object getDefaultValue(final String key, final String memberType)
    throws UnknownDirectiveException {
        return which.equals(key) ? replacement.getDefaultValue(key, memberType)
                                 : original.getDefaultValue(key, memberType);
    }

    public Map getValues(final String key, final String memberType)
    throws UnknownDirectiveException {
        return which.equals(key) ? replacement.getValues(key, memberType)
                                 : original.getValues(key, memberType);
    }

    public Object lookup(final String key) throws UnknownDirectiveException {
        return which.equals(key) ? replacement.lookup(key)
                                 : original.lookup(key);
    }

    public boolean containsDirective(final String key)
    throws UnknownDirectiveException {
        return which.equals(key) ? replacement.containsDirective(key)
                                 : original.containsDirective(key);
    }

    public Object lookup(final String key, final String memberType,
                         final Object parameter)
    throws UnknownDirectiveException {
        return which.equals(key) ? replacement.lookup(key, memberType, parameter)
                                 : original.lookup(key, memberType, parameter);
    }

    public boolean isKey(final String key) {
        return which.equals(key) ? replacement.isKey(key)
                                 : original.isKey(key);
    }

    public boolean isKey(final String key, final String memberType) {
        return which.equals(key) ? replacement.isKey(key, memberType)
                                 : original.isKey(key, memberType);
    }

    public Iterator paramEntryIterator() {
        final Collection result = new ArrayList();
        for (Iterator i = original.paramEntryIterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair p = (Pair) entry.getKey();
            if (!p.getFirst().equals(which)) result.add(entry);
        }
        for (Iterator i = replacement.paramEntryIterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair p = (Pair) entry.getKey();
            if (p.getFirst().equals(which)) result.add(entry);
        }
        return result.iterator();
    }

    public Iterator noparamEntryIterator() {
        final Collection result = new ArrayList();
        for (Iterator i = original.noparamEntryIterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            if (!entry.getKey().equals(which)) result.add(entry);
        }
        for (Iterator i = replacement.noparamEntryIterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            if (entry.getKey().equals(which)) result.add(entry);
        }
        return result.iterator();
    }
}
