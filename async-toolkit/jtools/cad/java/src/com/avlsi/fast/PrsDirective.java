/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast.impl.AlintFaninValue;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast.impl.Value;
import static com.avlsi.cast2.directive.DirectiveConstants.ALINT_SCENARIO_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.INSTANCE_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.NODE_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.HALFOP_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.DEEP_HALFOP_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.WIDE_CHANNEL_TYPE;
import static com.avlsi.cast2.directive.DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.directive.impl.DirectiveImpl;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.debug.Debug;

/**
 * Class used to bring up directives from a inlined or flattened cell.  When a
 * cell is flattened in the PRS block, all its directives must be propagated
 * upwards, with the name of the flattened instance prepended to all nodes and
 * half operators used in the directive block inside the flattened cell.
 **/

public class PrsDirective extends DirectiveBlock {
    /**
     * Constructs a PrsDirective based on an empty DirectiveInterface.
     **/
    public PrsDirective(final String type) {
        this(type, (new DirectiveImpl(type, new LocalEnvironment())).getDirectiveInterface());
    }

    /**
     * Constructs a PrsDirective from the DirectiveInterface to the current
     * cell.
     * @param me Containing directives for the current cell.
     **/
    public PrsDirective(final String type, final DirectiveInterface me) {
        super(new PrefixDirective(type, me));
    }

    /**
     * Add the directives of an inlined cell.
     **/
    public void addInstance(final HierName instance,
                            final DirectiveInterface directives) {
        ((PrefixDirective) this.directives).addInstance(instance, directives);
    }

    private static class PrefixDirective implements DirectiveInterface {
        /**
         * A map from instance name prefixes to the corresponding directives
         * blocks of inlined cells.
         **/
        private final Map<HierName,DirectiveInterface> blocks;

        /**
         * The block type
         **/
        private final String type;

        /**
         * The base directive interface.
         **/
        private final DirectiveInterface base;

        private Map<Pair<String,String>,Map> valuesCache = null;

        public PrefixDirective(final String type,
                               final DirectiveInterface base) {
            this.type = type;
            this.base = base;
            this.blocks = new HashMap<HierName,DirectiveInterface>();
        }

        /**
         * The block type
         **/
        public void addInstance(final HierName instance,
                                final DirectiveInterface directives) {
            blocks.put(instance, directives);
        }

        public Object getDefaultValue(String key, String memberType)
            throws UnknownDirectiveException {
            return base.getDefaultValue(key, memberType);
        }

        private abstract static class Prefix {
            protected HierName prefix;

            public abstract Object process(final Object o);

            public void setPrefix(final HierName prefix) {
                this.prefix = prefix;
            }
        }

        private static class PrefixNode extends Prefix {
            public Object process(final Object o) {
                return HierName.prefixName(prefix, (HierName) o);
            }
        }

        private static class PrefixHalfop extends Prefix {
            public Object process(final Object o) {
                final Pair p = (Pair) o;
                final HierName node = (HierName) p.getFirst();
                return new Pair(HierName.prefixName(prefix, node), p.getSecond());
            }
        }

        private static class PrefixChannel extends Prefix {
            public Object process(final Object o) {
                return prefix.getAsString('.') + '.' + (String) o;
            }
        }

        private static class PrefixAlintScenario extends Prefix {
            public Object process(final Object o) {
                if (o == null) return o;

                final TupleValue tv = (TupleValue) o;
                final Collection<Value> result = new ArrayList<Value>();
                for (Iterator i = tv.getIterator(); i.hasNext(); ) {
                    final AlintFaninValue afv = (AlintFaninValue) i.next();
                    result.add(
                        afv.newInstanceName(
                            HierName.prefixName(prefix, afv.getNode())));
                }
                return new TupleValue(result.toArray(new Value[0]));
            }
        }

        private static class PrefixArray extends Prefix {
            private final Prefix element;
            public PrefixArray(final Prefix element) {
                this.element = element;
            }
            public void setPrefix(final HierName prefix) {
                element.setPrefix(prefix);
            }
            public Object process(final Object o) {
                if (o == null) return o;

                final Collection c = (Collection) o;
                final ArrayList result = new ArrayList();
                for (Iterator i = c.iterator(); i.hasNext(); ) {
                    result.add(element.process(i.next()));
                }
                return Collections.unmodifiableList(result);
            }
        }

        private interface Extract {
            Pair getParts(final Object o, final Set prefixes);
        }

        private static Pair getParts(final HierName n, final Set prefixes) {
            final HierName head = n.head();
            final HierName tail = n.tail();
            return tail != null && prefixes.contains(head) ?
                       new Pair(head, tail) : null;
        }

        private static class ExtractNode implements Extract {
            public Pair getParts(final Object o, final Set prefixes) {
                return PrefixDirective.getParts((HierName) o, prefixes);
            }
        }

        private static class ExtractHalfop implements Extract {
            public Pair getParts(final Object o, final Set prefixes) {
                final Pair p = (Pair) o;
                final Pair r = PrefixDirective.getParts(((HierName) p.getFirst()), prefixes);
                return r == null ? null :
                       new Pair(r.getFirst(), new Pair(r.getSecond(), p.getSecond()));
            }
        }

        private static class ExtractChannel implements Extract {
            public Pair getParts(final Object o, final Set prefixes) {
                final HierName h;
                try {
                    h = HierName.makeHierName((String) o, '.');
                } catch (InvalidHierNameException e) {
                    throw new RuntimeException("Cannot make HierName:" + o, e);
                }
                return PrefixDirective.getParts(h, prefixes);
            }
        }

        /**
         * How to inline the given type?
         **/
        private Prefix howPrefix(final String type) {
            if (DirectiveTable.isArray(type)) {
                final Prefix element = howPrefix(DirectiveTable.deArray(type));
                if (element == null) {
                    return null;
                } else {
                    return new PrefixArray(element);
                }
            }

            if (type.equals(NODE_TYPE) ||
                type.equals(INSTANCE_TYPE)) {
                return new PrefixNode();
            } else if (type.equals(HALFOP_TYPE) ||
                       type.equals(DEEP_HALFOP_TYPE)) {
                return new PrefixHalfop();
            } else if (type.equals(POSSIBLY_WIDE_CHANNEL_TYPE) ||
                       type.equals(WIDE_CHANNEL_TYPE)) {
                return new PrefixChannel();
            } else if (type.equals(ALINT_SCENARIO_TYPE)) {
                return new PrefixAlintScenario();
            } else {
                return null;
            }
        }

        /**
         * How to extract the prefix and suffix for a given type?
         **/
        private Extract howExtract(final String type) {
            if (type.equals(NODE_TYPE) ||
                type.equals(INSTANCE_TYPE)) {
                return new ExtractNode();
            } else if (type.equals(HALFOP_TYPE)) {
                return new ExtractHalfop();
            } else if (type.equals(POSSIBLY_WIDE_CHANNEL_TYPE) ||
                       type.equals(WIDE_CHANNEL_TYPE)) {
                return new ExtractChannel();
            } else {
                return null;
            }
        }

        private Triplet how(String key, String memberType) throws UnknownDirectiveException {
            final Triplet[] triplet =
                DirectiveTable.lookupParameterizedDirective(type, key);
            if (triplet == null) {
                throw new UnknownDirectiveException(key + ":" + memberType);
            }
            Triplet spec = null;
            for (int i = 0; i < triplet.length; ++i) {
                if (triplet[i].getFirst().equals(memberType)) {
                    spec = triplet[i];
                    break;
                }
            }
            if (spec == null) {
                throw new UnknownDirectiveException(key + ":" + memberType);
            }

            return new Triplet(howPrefix((String) spec.getFirst()),
                               howPrefix((String) spec.getSecond()),
                               howExtract((String) spec.getFirst()));
        }

        private Object augment(final Prefix prefix, final Object o) {
            if (prefix != null) {
                return prefix.process(o);
            } else {
                return o;
            }
        }

        /**
         * Augment the result map with key value pairs from an inlined cell,
         * prefixing things as necessary.
         **/
        private void augment(final Map result, final Map directive,
                             final Prefix member, final Prefix value) {

            if (member != null || value != null) {
                final Set entries = directive.entrySet();
                for (Iterator i = entries.iterator(); i.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final Object key = augment(member, entry.getKey());
                    final Object val = augment(value, entry.getValue());
                    result.put(key, val);
                }
            } else {
                result.putAll(directive);
            }
        }

        public Map getValues(String key, String memberType) throws UnknownDirectiveException {
            Map result = null;
            if (valuesCache == null) {
                valuesCache = new HashMap<Pair<String,String>,Map>();
            }
            final Pair<String,String> keyType =
                new Pair<String,String>(key, memberType);
            result = valuesCache.get(keyType);

            if (result != null) return result;

            final Triplet t = how(key, memberType);
            final Prefix member = (Prefix) t.getFirst();
            final Prefix value = (Prefix) t.getSecond();

            result = new HashMap(base.getValues(key, memberType));
            valuesCache.put(keyType, result);

            for (Map.Entry<HierName,DirectiveInterface> entry :
                    blocks.entrySet()) {
                final HierName instance = entry.getKey();
                final DirectiveInterface dir = entry.getValue();
                if (member != null) member.setPrefix(instance);
                if (value != null) value.setPrefix(instance);
                augment(result, dir.getValues(key, memberType), member, value);
            }

            return result;
        }

        public Object lookup(String key) throws UnknownDirectiveException {
            return base.lookup(key);
        }

        public boolean containsDirective(String key) throws UnknownDirectiveException {
            return base.containsDirective(key);
        }

        public Object lookup(String key, String memberType, Object parameter)
            throws UnknownDirectiveException {
            Object o = base.lookup(key, memberType, parameter);
            if (o != base.getDefaultValue(key, memberType)) return o;

            final Triplet t = how(key, memberType);
            final Prefix value = (Prefix) t.getSecond();
            final Extract extract = (Extract) t.getThird();
            HierName prefix = null;
            if (extract != null) {
                final Pair parts = extract.getParts(parameter, blocks.keySet());
                if (parts != null) {
                    prefix = (HierName) parts.getFirst();
                    final DirectiveInterface dir = blocks.get(prefix);
                    o = dir.lookup(key, memberType, parts.getSecond());
                }
            } else {
                for (Map.Entry<HierName,DirectiveInterface> entry :
                        blocks.entrySet()) {
                    final DirectiveInterface dir = entry.getValue();
                    o = dir.lookup(key, memberType, parameter);
                    if (o != dir.getDefaultValue(key, memberType)) {
                        prefix = entry.getKey();
                        break;
                    }
                }
            }

            if (value != null && prefix != null) {
                value.setPrefix(prefix);
                return augment(value, o);
            } else {
                return o;
            }
        }

        public boolean isKey(String key) {
            return base.isKey(key);
        }

        public boolean isKey(String key, String memberType) {
            return base.isKey(key, memberType);
        }

        public Iterator paramEntryIterator() {
            final HashMap result = new HashMap();

            for (Iterator i = base.paramEntryIterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                result.put(entry.getKey(), new HashMap((Map) entry.getValue()));
            }

            for (Map.Entry<HierName,DirectiveInterface> entry :
                    blocks.entrySet()) {
                final HierName instance = entry.getKey();
                final DirectiveInterface dir = entry.getValue();
                for (Iterator j = dir.paramEntryIterator(); j.hasNext(); ) {
                    final Map.Entry sentry = (Map.Entry) j.next();
                    final Pair keytype = (Pair) sentry.getKey();

                    final Triplet t;
                    try {
                        t = how((String) keytype.getFirst(),
                                (String) keytype.getSecond());
                    } catch (UnknownDirectiveException e) {
                        throw new RuntimeException(e);
                    }
                    final Prefix member = (Prefix) t.getFirst();
                    final Prefix value = (Prefix) t.getSecond();

                    if (member != null) member.setPrefix(instance);
                    if (value != null) value.setPrefix(instance);

                    Map sresult = (Map) result.get(keytype);
                    if (sresult == null) {
                        sresult = new HashMap();
                        result.put(keytype, sresult);
                    }

                    final Map values = (Map) sentry.getValue();
                    augment(sresult, values, member, value);
                }
            }

            return result.entrySet().iterator();
        }

        public Iterator noparamEntryIterator() {
            return base.noparamEntryIterator();
        }
    }
}
