/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;
import java.util.StringTokenizer;

import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LoopEnvironment;
import com.avlsi.cast.impl.Range;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.directive.impl.DirectiveVisitor;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.directive.impl.TokenInfo;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;

public class DirectiveImpl implements DirectiveVisitor {
    public interface ErrorHandler {
        void error(String message);
    }

    private ErrorHandler handler;
    private String block;
    private Environment current;
    private Map paramDefinition, noparamDefinition;

    public DirectiveImpl(String block, Environment base) {
        this(block, base, null);
    }

    public DirectiveImpl(String block, Environment base, ErrorHandler handler) {
        this.block = block;
        this.handler = handler;
        paramDefinition = new HashMap();
        noparamDefinition = new HashMap();
        current = base;
    }

    private void error(final String s) {
        if (handler == null) {
            System.err.println("SYNTAX: " + s);
        } else {
            handler.error(s);
        }
    }
    
    /** only report unknown directives once per cell */
    private Set<String> unknown = new HashSet<String>();
    private void unknownDirective(final String key, final TokenInfo info) {
        if (unknown.add(key)) {
            error("Unknown directive " + key + " [" + info + "]!");
        }
    }

    public void conditional(String guard, DirectiveStatement[] body,
                            TokenInfo info) {
        final DirectiveCallback cb = DirectiveTable.lookupCallback(block, DirectiveConstants.BOOLEAN_TYPE);
        if (cb == null) {
            error("Cannot resolve if guard [" + info + "]!");
            return;
        }

        final Boolean resolved = (Boolean) cb.resolve(DirectiveConstants.BOOLEAN_TYPE, guard, current);
        if (resolved == null) {
            error("Cannot resolve if guard [" + info + "]!");
            return;
        }

        if (resolved.booleanValue()) {
            for (int i = 0; i < body.length; ++i) {
                body[i].visit(this);
            }
        }
    }

    public void loop(String var, String from, String to,
                     DirectiveStatement[] body, TokenInfo info) {
        var = var.trim();

        DirectiveCallback cb =
            DirectiveTable.lookupCallback(block, DirectiveConstants.INT_TYPE);
        if (cb == null) {
            error("Cannot resolve loop bounds [" + info + "]!");
            return;
        }

        Integer resolvedFrom =
            (Integer) cb.resolve(DirectiveConstants.INT_TYPE, from, current);
        if (resolvedFrom == null) {
            error("Cannot resolve loop bounds [" + info + "]!");
            return;
        }

        Integer resolvedTo;
        if (to == null) {
            resolvedTo = new Integer(resolvedFrom.intValue() - 1);
            resolvedFrom = new Integer(0);
        } else {
            resolvedTo =
                (Integer) cb.resolve(DirectiveConstants.INT_TYPE, to, current);
            if (resolvedTo == null) {
                error("Cannot resolve loop bounds [" + info + "]!");
                return;
            }
        }

        Range r = new Range(resolvedFrom.intValue(), resolvedTo.intValue());
        Symbol sym = Symbol.create(var);
        for (Range.Iterator ri = r.iterator(); ri.hasNext(); ) {
            final int i = ri.next();
            Environment old = current;
            current = new LoopEnvironment(old, sym, IntValue.valueOf(i));
            for (int l = 0; l < body.length; l++) {
                body[l].visit(this);
            }
            current = old;
        }
    }

    private Object resolve(String type, String str) {
        if (DirectiveTable.isArray(type)) {
            return resolve(DirectiveTable.deArray(type), parseArray(str));
        } else {
            DirectiveCallback cb = DirectiveTable.lookupCallback(block, type);
            if (cb == null) return null;
            return cb.resolve(type, str, current);
        }
    }

    private Object resolve(String type, String[] str) {
        if (str == null) return null;
        ArrayList result = new ArrayList(str.length);
        for (int i = 0; i < str.length; i++) {
            final Object obj = resolve(type, str[i]);
            if (obj == null) return null;
            result.add(obj);
        }
        return Collections.unmodifiableList(result);
    }

    private String[] parseArray(String s) {
        s = s.trim();
        if (!s.startsWith("{") || !s.endsWith("}")) return null;
        s = s.substring(s.indexOf('{') + 1, s.lastIndexOf('}'));
        final ArrayList list = new ArrayList();
        StringBuffer buf = new StringBuffer();
        int parens = 0;
        boolean inString = false;
        for (int i = 0; i < s.length(); ++i) {
            final char c = s.charAt(i);
            if (c == '[' || c == '(' || c == '{') {
                ++parens;
            } else if (c == ']' || c == ')' || c == '}') {
                --parens;
            } else if (c == '"') {
                inString = !inString;
            }
            if (c == ',' && parens == 0 && !inString) {
                list.add(buf.toString());
                buf = new StringBuffer();
            } else {
                buf.append(c);
            }
        }
        if (buf.length() > 0) {
            list.add(buf.toString());
        }
        if (list.size() == 0) return null;
        else return (String[]) list.toArray(new String[0]);
    }

    public void definition(String key, String val, TokenInfo info) {
        Pair p = DirectiveTable.lookupDirective(block, key);
        if (p == null) {
            unknownDirective(key,info);
            return;
        }
        String valueType = (String) p.getFirst();

        final Object value = resolve(valueType, val);

        if (value == null) {
            error("Expecting type " + valueType + ", found " + val + " [" +
                  info + "]!");
            return;
        }
        noparamDefinition.put(key, value);
    }

    public void definition(String key, String parameter, String val,
                           TokenInfo info) {
        Triplet[] candidates = DirectiveTable.lookupParameterizedDirective(block, key);
        if (candidates == null) {
            unknownDirective(key,info);
            return;
        }
        for (int i = 0; i < candidates.length; i++) {
            String memberType = (String) candidates[i].getFirst();
            final Object param = resolve(memberType, parameter);
            if (param == null) {
                error("Expecting type " + memberType + ", found " + parameter +
                      " [" + info + "]!");
                return;
            }

            String valueType = (String) candidates[i].getSecond();

            final Object value = resolve(valueType, val);

            if (value == null) {
                error("Expecting type " + valueType + ", found " + val + " [" +
                      info + "]!");
                return;
            }

            Map m = (Map) paramDefinition.get(new Pair(key, memberType));
            if (m == null) {
                m = new HashMap();
                paramDefinition.put(new Pair(key, memberType), m);
            }
            m.put(param, value);
        }
    }

    public DirectiveInterface getDirectiveInterface() {
        return new Impl(block, paramDefinition, noparamDefinition);
    }

    static class Impl implements DirectiveInterface {
        private final String block;
        private final Map paramDefinition, noparamDefinition;
        public Impl(final String block, final Map paramDefinition,
                    final Map noparamDefinition) {
            this.block = block;
            this.paramDefinition = paramDefinition;
            this.noparamDefinition = noparamDefinition;
        }

        private Triplet findMemberType(String key, String memberType)
        throws UnknownDirectiveException {
            Triplet[] candidates = DirectiveTable.lookupParameterizedDirective(block, key);
            if (candidates == null) throw new UnknownDirectiveException(key);
            for (int i = 0; i < candidates.length; i++) {
                String mType = (String) candidates[i].getFirst();
                if (memberType.equals(mType))
                    return candidates[i];
            }
            throw new UnknownDirectiveException(key + ":" + memberType);
        }

        public Object getDefaultValue(String key, String memberType)
        throws UnknownDirectiveException {
            Triplet info = findMemberType(key, memberType);
            return info.getThird();
        }

        public Map getValues(String key, String memberType)
        throws UnknownDirectiveException {
            final Triplet info = findMemberType(key, memberType);
            final Map result = (Map) paramDefinition.get(new Pair(key, memberType));
            return result == null ? Collections.EMPTY_MAP : result;
        }

        public Object lookup(String key) throws UnknownDirectiveException {
            if (noparamDefinition.containsKey(key)) {
                return noparamDefinition.get(key);
            } else {
                Pair p = DirectiveTable.lookupDirective(block, key);
                if (p == null) throw new UnknownDirectiveException(key);
                return p.getSecond();
            }
        }

        public boolean containsDirective(String key) throws UnknownDirectiveException {
            return noparamDefinition.containsKey(key);
        }

        public Object lookup(String key, String memberType, Object parameter) throws UnknownDirectiveException {
            Triplet info = findMemberType(key, memberType);
            Map map = (Map) paramDefinition.get(new Pair(key, memberType));
            if (map == null) return info.getThird();
            Object ret = map.get(parameter);
            if (ret == null) return info.getThird();
            else return ret;
        }

        public boolean isKey(String key) {
            return DirectiveTable.lookupDirective(block, key) != null;
        }

        public boolean isKey(String key, String memberType) {
            return DirectiveTable.lookupParameterizedDirective(block, key) != null;
        }

        public Iterator noparamEntryIterator() {
            return noparamDefinition.entrySet().iterator();
        }

        public Iterator paramEntryIterator() {
            return paramDefinition.entrySet().iterator();
        }
    }
}
