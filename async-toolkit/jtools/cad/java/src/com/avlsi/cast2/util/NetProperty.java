package com.avlsi.cast2.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast2.directive.impl.DirectiveComparator;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.BinaryFunction;

/**
 * A class to help propagate properties on nets in a hierarchical design.  The
 * properties on a net can be specified in any point in the hierarchy.
 **/
public class NetProperty {
    public static class WireProperty implements Property {
        public interface DirectiveGetter {
            Object getDirective(CellInterface cell, HierName net, boolean port);
        }

        public static class WireDirective implements DirectiveGetter {
            private final String dir;
            private final Cadencize cad;
            private final BinaryFunction update;
            public WireDirective(final Cadencize cad, final String dir,
                                 final BinaryFunction update) {
                this.cad = cad;
                this.dir = dir;
                this.update = update;
            }
            public Object getDirective(final CellInterface cell,
                                       final HierName net, boolean port) {
                Object result = null;

                if (port) {
                    final Map top = DirectiveUtils.getTopLevelDirective(cell, dir, DirectiveConstants.NODE_TYPE);
                    final Map canon = DirectiveUtils.canonizeKey(cad.convert(cell).getLocalNodes(), top, update);
                    result = canon.get(net);
                }

                if (result == null) {
                    final Map subcell = DirectiveUtils.getSubcellDirective(cell, dir, DirectiveConstants.NODE_TYPE);
                    final Map canon = DirectiveUtils.canonizeKey(cad.convert(cell).getLocalNodes(), subcell, update);
                    result = canon.get(net);
                }

                return result;
            }
        }

        public static class EMDirective implements DirectiveGetter {
            private final String dir;
            private final Cadencize cad;
            private final BinaryFunction update;
            public EMDirective(final Cadencize cad, final String dir,
                               final BinaryFunction update) {
                this.cad = cad;
                this.dir = dir;
                this.update = update;
            }
            public Object getDirective(final CellInterface cell,
                                       final HierName net, boolean port) {
                Object result = null;

                if (result == null) {
                    final Map prs = DirectiveUtils.getPrsDirective(cell, dir, DirectiveConstants.NODE_TYPE);
                    final Map canon = DirectiveUtils.canonizeKey(cad.convert(cell).getLocalNodes(), prs, update);
                    result = canon.get(net);
                }

                if (result == null) {
                    final Map subcell = DirectiveUtils.getSubcellDirective(cell, dir, DirectiveConstants.NODE_TYPE);
                    final Map canon = DirectiveUtils.canonizeKey(cad.convert(cell).getLocalNodes(), subcell, update);
                    result = canon.get(net);
                }

                return result;
            }
        }

        Object value;
        final boolean fixed;
        final BinaryFunction update;
        private WireProperty(final Object value, final BinaryFunction update,
                             final boolean em) {
            this.value = value;
            this.fixed = value != null && !em;
            this.update = update;
        }

        public void absorbChildProperty(final Property other) {
            if (!fixed) {
                final Object oval = ((WireProperty) other).value;
                if (value == null) value = oval;
                else if (oval != null) {
                    value = update.execute(oval, value);
                }
            }
        }

        public Object getValue() {
            return value;
        }

        public String toString() {
            return value == null ? "null" : value.toString();
        }

        public static BinaryFunction MAX = new BinaryFunction() {
            public Object execute(final Object a, final Object b) {
                return ((Number) a).doubleValue() > ((Number) b).doubleValue() ?
                       a : b;
            }
        };

        public static BinaryFunction MIN = new BinaryFunction() {
            public Object execute(final Object a, final Object b) {
                return ((Number) a).doubleValue() < ((Number) b).doubleValue() ?
                       a : b;
            }
        };

        public static BinaryFunction OR = new BinaryFunction() {
            public Object execute(final Object a, final Object b) {
                return Boolean.valueOf(((Boolean) a).booleanValue() ||
                                       ((Boolean) b).booleanValue());
            }
        };

        public static PropertyFactory getFactory(final DirectiveGetter getter,
                                                 final BinaryFunction update) {
            return getFactory(getter, update, false);
        }

        public static PropertyFactory getFactory(final DirectiveGetter getter,
                                                 final BinaryFunction update,
                                                 final boolean em) {
            return new PropertyFactory() {
                public Property makeProperty(final CellInterface cell,
                                             final HierName net,
                                             final boolean port) {
                    return new WireProperty(getter.getDirective(cell, net, port), update, em);
                }
            };
        }
    }

    public interface Property {
        void absorbChildProperty(Property other);
        Object getValue();
    }

    public interface PropertyFactory {
        /**
         * Create a new property for the given cell.
         *
         * @param cell Cell to create the property for.
         * @param net Canonical name of the net
         * @param port Whether the property should be in a port context.
         **/
        Property makeProperty(CellInterface cell, HierName net, boolean port);
    }

    public static class Cache extends HashMap {
    }

    private final Cadencize cad;
    public NetProperty(final Cadencize cad) {
        this.cad = cad;
    }

    private static void update(final Map /*<HierName, Property>*/ map,
                               final CellInterface cell,
                               final HierName net,
                               final Property property) {
        assert net != null && property != null;
        final Property existing = (Property) map.get(net);
        if (existing == null) {
            map.put(net, property);
        } else {
            existing.absorbChildProperty(property);
        }
    }

    public static Map /*<HierName, Property>*/
    getNodeProperties(final Cache cache, final CellInterface cell,
                      final Cadencize cad, final PropertyFactory propFactory) {
        final Map /*<HierName, Property>*/ result = new HashMap();
        final CadenceInfo cinfo = cad.convert(cell);
        final AliasedSet locals = cinfo.getLocalNodes();

        for (Iterator i = locals.getCanonicalKeys(); i.hasNext(); ) {
            final HierName local = (HierName) i.next();
            update(result, cell, local, propFactory.makeProperty(cell, local, false));
        }

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();

            final CellInterface subcell = (CellInterface) p.getSecond();
            final Map subcellResult =
                getPortProperties(cache, subcell, cad, propFactory);

            // Convert subcell names to canonical names in this cell
            final HierName inst = (HierName) p.getFirst();
            for (Iterator j = subcellResult.entrySet().iterator();
                 j.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) j.next();
                final HierName key =
                    HierName.prefixName(inst, (HierName) entry.getKey());
                final HierName local =
                    (HierName) locals.getCanonicalKey(key);

                assert local != null : "key = " + key;
                final Property prop = (Property) entry.getValue();
                update(result, cell, local, prop);
            }
        }

        return result;
    }

    public static Map /*<HierName, Property>*/
    getPortProperties(final Cache cache, final CellInterface cell,
                      final Cadencize cad, final PropertyFactory propFactory) {
        final Map /*<HierName, Property>*/ cached = (Map) cache.get(cell.getFullyQualifiedType());
        if (cached != null) return cached;

        final Map /*<HierName, Property>*/ result = new HashMap();
        final CadenceInfo cinfo = cad.convert(cell);
        final AliasedMap ports = cinfo.getPortNodes();
        final AliasedSet locals = cinfo.getLocalNodes();

        for (Iterator i = ports.getCanonicalKeys(); i.hasNext(); ) {
            final HierName port = (HierName) i.next();
            update(result, cell, port, propFactory.makeProperty(cell, port, true));
        }

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();

            final CellInterface subcell = (CellInterface) p.getSecond();
            final Map subcellResult =
                getPortProperties(cache, subcell, cad, propFactory);

            // Convert subcell names to canonical names in this cell
            final HierName inst = (HierName) p.getFirst();
            for (Iterator j = subcellResult.entrySet().iterator();
                 j.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) j.next();
                final HierName key =
                    HierName.prefixName(inst, (HierName) entry.getKey());
                final HierName canon = (HierName) locals.getCanonicalKey(key);

                assert canon != null : "key = " + key;

                // If canon is not a port node of this cell, its information
                // need not be propagated up any further
                if (ports.contains(canon)) {
                    final Property prop = (Property) entry.getValue();
                    update(result, cell, canon, prop);
                }
            }
        }

        cache.put(cell.getFullyQualifiedType(), result);
        return result;
    }
}
