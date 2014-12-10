/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.UnaryFunction;

/**
 * A class for storing non-hierarchical net attributes.  For example, the
 * <code>extra_delay</code> directive is attached to a net by specifying it
 * in the subcells block of the cell where the net becomes internal, but it
 * affects all production rules that drives the net, no matter how many
 * level of hierarchy away they are from where the directive is specified.
 * This class aggregates all attributes that need to be handled in a
 * net specific way.
 **/
public class InstanceData {
    /**
     * Represents an attribute.
     **/
    public interface AttributeInterface {
        /**
         * Get the value of the attribute for the given node.
         **/
        Object getAttribute(final HierName canon);

        /**
         * Return a new <code>AttributeInterface</code> that is applicable
         * to an instance of <code>subcell</code> with name
         * <code>instance</code> in the cell <code>cell</code>.
         **/
        AttributeInterface translate(final CellInterface cell,
                                     final CellInterface subcell,
                                     final AliasedSet cellAliases,
                                     final AliasedSet subcellAliases,
                                     final HierName instance);
    }

    /**
     * An attribute stored as a directive parameterized on nodes.
     **/
    public static class NodeDirective implements AttributeInterface {
        private final Map /*<HierName,Object>*/ directives;
        private final UnaryFunction /*<CellInterface,Map>*/ getter;
        private MultiMap/*<HierName,Pair<HierName,Object>>*/ cache;

        public NodeDirective(final CellInterface ci,
                             final AliasedSet aliases,
                             final UnaryFunction getter) {
            this.directives =
                DirectiveUtils.canonizeKey(aliases,
                                           (Map) getter.execute(ci));
            this.getter = getter;
        }
        public NodeDirective(final CellInterface ci,
                             final Map /*<HierName,Object>*/ directives,
                             final UnaryFunction getter) {
            this.directives = directives;
            this.getter = getter;
        }
        private NodeDirective(final Map directives,
                              final UnaryFunction getter) {
            this.directives = directives;
            this.getter = getter;
        }
        public Object getAttribute(final HierName canon) {
            return directives.get(canon);
        }
        public AttributeInterface translate(final CellInterface cell,
                                            final CellInterface subcell,
                                            final AliasedSet cellAliases,
                                            final AliasedSet subcellAliases,
                                            final HierName instance) {
            // in anticipation that translate will be called on all instances
            // of cell, group attributes by instance
            if (cache == null) attributeByInstance(cell, cellAliases);
            final Collection cached = cache.get(instance);
            final Map translated;
            if (cached == null) {
                translated = Collections.EMPTY_MAP;
            } else {
                translated = new LinkedHashMap();
                final int n = instance.getNumComponents();
                for (Iterator i = cached.iterator(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final HierName alias = (HierName) p.getFirst();
                    translated.put(alias.tail(n), p.getSecond());
                }
            }
            final Map newdir =
                augmentMap(translated,
                           (Map) getter.execute(subcell), subcellAliases);
            return new NodeDirective(newdir, getter);
        }
        private void attributeByInstance(final CellInterface cell,
                                         final AliasedSet cellAliases) {
            for (Iterator i = directives.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName key = (HierName) entry.getKey();
                final Object val = entry.getValue();
                final Iterator j = cellAliases.getAliases(key);
                if (j == null) continue;
                // iterate over all aliases, because otherwise aliases created
                // in ancestors will not be handled correctly in children
                while (j.hasNext()) {
                    final HierName alias = (HierName) j.next();
                    HierName parent = alias.getParent();
                    Pair p = null;
                    while (parent != null) {
                        if (cell.getSubcell(parent) != null) {
                            if (p == null) {
                                p = new Pair(alias, val);
                            }
                            if (cache == null) {
                                cache =
                                    new MultiMap(new HashMap(),
                                                 MultiMap.ARRAY_LIST_FACTORY);
                            }
                            cache.put(parent, p);
                            // could break here if we don't care about inlined
                            // subcells
                        }
                        parent = parent.getParent();
                    }
                }
            }
            if (cache == null) cache = MultiMap.EMPTY_MULTIMAP;
        }
    }

    /**
     * Special attribute for handling back annotated alint measured delay
     * directives in DSim.
     **/
    public static class MeasuredDirective implements AttributeInterface {
        final Map /*<HierName,Object>*/ directives;
        private final UnaryFunction /*<HierName,HierName>*/ canonizer;
        private final UnaryFunction /*<CellInterface,Map>*/ getter;
        private final HierName prefix;

        private static Map canonize(final HierName prefix, final Map map,
                                    final UnaryFunction canonizer) {
            // We assume that each target node in measured_delay directives
            // appears in only one cell.  The target node can be specified
            // using any of its aliases, however, all usage must be consistent.
            final Map canonized = new LinkedHashMap();
            for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName key = (HierName) entry.getKey();
                canonized.put(canonizer.execute(HierName.append(prefix, key)),
                              entry.getValue());
            }
            return canonized;
        }

        public MeasuredDirective(final CellInterface ci,
                                 final UnaryFunction canonizer,
                                 final UnaryFunction getter,
                                 final HierName prefix) {
            this(canonize(prefix, (Map) getter.execute(new Pair(prefix, ci)),
                          canonizer),
                 canonizer, getter, prefix);
        }
        private MeasuredDirective(final Map directives,
                                  final UnaryFunction canonizer,
                                  final UnaryFunction getter,
                                  final HierName prefix) {
            this.directives = directives;
            this.canonizer = canonizer;
            this.getter = getter;
            this.prefix = prefix;
        }
        public Object getAttribute(final HierName canon) {
            return directives.get(canon);
        }
        public AttributeInterface translate(final CellInterface cell,
                                            final CellInterface subcell,
                                            final AliasedSet cellAliases,
                                            final AliasedSet subcellAliases,
                                            final HierName instance) {
            final HierName newPrefix = HierName.append(prefix, instance);

            // add new directives from subcells
            directives.putAll(
                canonize(newPrefix,
                         (Map) getter.execute(new Pair(newPrefix, subcell)),
                         canonizer));

            // We keep 1 map for all measured_delay directives in all cells,
            // thus we do not transform the map, but merely pass it in to the
            // new instance
            return new MeasuredDirective(directives, canonizer, getter,
                                         newPrefix);
        }
    }

    /**
     * Directives that are parameterized on instance names, and are
     * multiplicative in nature down the instance hierarchy.
     **/
    public static class MultiplicativeDirective implements AttributeInterface {
        public static class Multiply implements BinaryFunction {
            private final Double def;
            public Multiply(final double def) {
                this(new Double(def));
            }
            public Multiply(final Double def) {
                this.def = def;
            }
            public Object execute(final Object a, final Object b) {
                final Number n1 = a == null ? def : (Number) a;
                final Number n2 = b == null ? def : (Number) b;
                return new Double(n1.doubleValue() * n2.doubleValue());
            }
        }

        private final Object current;
        private final Map /*<HierName,Object>*/ directives;
        private final UnaryFunction /*<CellInterface,Map>*/ getter;
        private final BinaryFunction /*<Object,Object,Object>*/ product;
        public MultiplicativeDirective(final Object current,
                                       final CellInterface ci,
                                       final UnaryFunction getter,
                                       final BinaryFunction product) {
            this(current, (Map) getter.execute(ci), getter, product);
        }
        private MultiplicativeDirective(final Object current,
                                        final Map directives,
                                        final UnaryFunction getter,
                                        final BinaryFunction product) {
            this.current = product.execute(current, directives.get(null));
            this.directives = directives;
            this.getter = getter;
            this.product = product;
        }
        public Object getAttribute(final HierName instance) {
            return instance == null ? current 
                                    : product.execute(directives.get(instance),
                                                      current);
        }
        public AttributeInterface translate(final CellInterface cell,
                                            final CellInterface subcell,
                                            final AliasedSet cellAliases,
                                            final AliasedSet subcellAliases,
                                            final HierName instance) {
            return new MultiplicativeDirective(getAttribute(instance),
                    (Map) getter.execute(subcell), getter, product);
        }
    }

    /**
     * A half operator directive is just syntactic sugar for 2 node
     * directives, one for the up direction, and one for the down
     * direction.
     **/
    public static class HalfOpDirective implements AttributeInterface {
        public static class Convert implements UnaryFunction {
            private final UnaryFunction getter;
            private final boolean up;
            public Convert(final boolean up, final UnaryFunction getter) {
                this.getter = getter;
                this.up = up;
            }
            public Object execute(final Object o) {
                final Map dirs = (Map) getter.execute(o);
                return up ? DirectiveUtils.getUps(dirs) :
                            DirectiveUtils.getDowns(dirs);
            }
        }

        final AttributeInterface up, down;

        public HalfOpDirective(final CellInterface ci,
                               final AliasedSet aliases,
                               final UnaryFunction getter) {
            this(new NodeDirective(ci, aliases, new Convert(true, getter)),
                 new NodeDirective(ci, aliases, new Convert(false, getter)));
        }
        public HalfOpDirective(final CellInterface ci,
                               final Map /*<HierName,Object>*/ updirs,
                               final Map /*<HierName,Object>*/ dndirs,
                               final UnaryFunction getter) {
            this(new NodeDirective(ci, updirs, new Convert(true, getter)),
                 new NodeDirective(ci, dndirs, new Convert(false, getter)));
        }
        public HalfOpDirective(final AttributeInterface up,
                               final AttributeInterface down) {
            this.up = up;
            this.down = down;
        }
        public Object getAttribute(final HierName canon) {
            return new Pair(up.getAttribute(canon),
                            down.getAttribute(canon));
        }
        public AttributeInterface translate(final CellInterface cell,
                                            final CellInterface subcell,
                                            final AliasedSet cellAliases,
                                            final AliasedSet subcellAliases,
                                            final HierName instance) {
            return new HalfOpDirective(
                    up.translate(cell, subcell, cellAliases, subcellAliases,
                                 instance),
                    down.translate(cell, subcell, cellAliases,
                                   subcellAliases, instance));
        }
    }

    public static class SubcellGetter implements UnaryFunction {
        protected final String dir, type;
        public SubcellGetter(final String dir, final String type) {
            this.dir = dir;
            this.type = type;
        }
        public Object execute(final Object o) {
            final CellInterface cell = (CellInterface) o;
            return DirectiveUtils.getSubcellDirective(cell, dir, type);
        }
    }

    /**
     * Certain directives, e.g., <code>cap</code> and
     * <code>estimated_delay</code>, are defined in both the subcells and
     * the prs block.  This will choose which block to get the directives
     * from based on whether if a cell is a leaf cell, assuming that leaf
     * cells have prs blocks, and midlevel cells have subcell blocks.
     **/
    public static class SubcellPrsGetter implements UnaryFunction {
        private final String dir, type;
        public SubcellPrsGetter(final String dir, final String type) {
            this.dir = dir;
            this.type = type;
        }
        public Object execute(final Object o) {
            final CellInterface cell = (CellInterface) o;
            return CellUtils.isLeaf(cell) ?
                DirectiveUtils.getPrsDirective(cell, dir, type) :
                DirectiveUtils.getSubcellDirective(cell, dir, type);
        }
    }

    public static class MeasuredGetter implements UnaryFunction {
        private final UnaryFunction getter;
        private final Processor proc;
        public interface Processor {
            Object process(CellInterface cell, Pair target, HierName trigger,
                           List value);
        }
        public MeasuredGetter(final UnaryFunction getter, final Processor proc)
        {
            this.getter = getter;
            this.proc = proc;
        }
        public Object execute(final Object o) {
            final Pair p = (Pair) o;
            final HierName prefix = (HierName) p.getFirst();
            final CellInterface cell = (CellInterface) p.getSecond();

            // Rewrap the directives returned by the underlying getter into a
            // more convenient form.
            final Map/*<Pair<HierName,Boolean>[],Float[]>*/ val =
                (Map) getter.execute(cell);
            final Map/*<Pair<HierName,Boolean>,List<Pair<HierName,float[]>>*/
                result = new LinkedHashMap();
            for (Iterator i = val.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final List key = (List) entry.getKey();

                final Pair target = (Pair) key.get(0);
                final HierName trigger = key.size() > 1 ?
                    (HierName) ((Pair) key.get(1)).getFirst() : null;
                final List value = (List) entry.getValue();
                final Object measured =
                    proc.process(cell, target, trigger, value);
                if (measured == null) continue;

                if (!result.containsKey(target)) {
                    result.put(target, new ArrayList());
                }

                ((List) result.get(target)).add(new Triplet(prefix, trigger,
                                                            measured));
            }
            return result;
        }
    }

    private final Map /*<String,AttributeInterface>*/ attributes;

    public InstanceData() {
        this(new HashMap());
    }

    private InstanceData(final Map attributes) {
        this.attributes = attributes;
    }

    private static Map augmentMap(final Map parent, final Map child,
                                  final AliasedSet aliases) {
        final Map result = DirectiveUtils.canonizeKey(aliases, parent);
        final Map canon = DirectiveUtils.canonizeKey(aliases, child);
        result.putAll(canon);
        return result;
    }

    /**
     * Given a map of nodes to values, returns a new map that only
     * contains entries relevent to the given subcell instance, with the
     * keys translated to the namespace of the subcell.
     * <code>n</code>, <code>m</code> and <code>i</code> are arbitrary
     * <code>HierName</code>s, then node <code>n</code> is relevant to
     * instance <code>i</code> if <code>n</code> and <code>i.m</code> are
     * aliased together in <code>aliases</code>, and the translated name of
     * <code>n</code> is <code>m</code>.
     **/
    private static Map /*<HierName, Object>*/
    translateMap(final AliasedSet aliases, final HierName instance,
                 final Map /*<HierName, Object>*/ old) {
        final Map /*<HierName, Object>*/ result = new LinkedHashMap();
        final int n = instance.getNumComponents();
        for (Iterator i = old.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final HierName key = (HierName) entry.getKey();
            final Iterator j = aliases.getAliases(key);
            if (j == null) continue;
            while (j.hasNext()) {
                final HierName alias = (HierName) j.next();
                if (alias.isChildOf2(instance)) {
                    result.put(alias.tail(n), entry.getValue());
                    // Cannot break here; otherwise aliases created in
                    // ancestors will not be handled correctly in children
                }
            }
        }
        return result;
    }

    /**
     * Returns a new <code>InstanceData</code> that is suitable for use when
     * processing <code>subcell</code>.
     *
     * @param cell Cell associated with this <code>InstanceData</code>
     * @param subcell Subcell to process
     * @param instance Instance name of the subcell
     * @param cad Where to find alias information
     **/
    public InstanceData translate(final CellInterface cell,
                                  final CellInterface subcell,
                                  final HierName instance,
                                  final Cadencize cad) {
        final AliasedSet cellAliases =
            cad.convert(cell).getLocalNodes();
        final AliasedSet subcellAliases =
            cad.convert(subcell).getLocalNodes();

        final Map newattr = new HashMap(attributes);

        for (Iterator i = newattr.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final AttributeInterface ai = (AttributeInterface) entry.getValue();
            entry.setValue(ai.translate(cell, subcell, cellAliases,
                                        subcellAliases, instance));
        }

        return new InstanceData(newattr);
    }

    public AttributeInterface put(final String name,
                                  final AttributeInterface attr) {
        return (AttributeInterface) attributes.put(name, attr);
    }

    public AttributeInterface get(final String name) {
        return (AttributeInterface) attributes.get(name);
    }

    public void updateAstaExtraDelay(final CellInterface cell,
                                     final AliasedSet aliases) {
        put(DirectiveConstants.ASTA_EXTRA_DELAY,
            new HalfOpDirective(cell, aliases,
                new SubcellGetter(DirectiveConstants.ASTA_EXTRA_DELAY,
                                  DirectiveConstants.HALFOP_TYPE)));
    }

    public void updateExtraDelay(final CellInterface cell,
                                 final AliasedSet aliases) {
        put(DirectiveConstants.EXTRA_DELAY,
            new HalfOpDirective(cell, aliases,
                new SubcellGetter(DirectiveConstants.EXTRA_DELAY,
                                  DirectiveConstants.HALFOP_TYPE)));
    }

    public void updateExtraDelay(final CellInterface cell,
                                 final Map<HierName,Float> updirs,
                                 final Map<HierName,Float> dndirs) {
        put(DirectiveConstants.EXTRA_DELAY,
            new HalfOpDirective(cell, updirs, dndirs,
                new SubcellGetter(DirectiveConstants.EXTRA_DELAY,
                                  DirectiveConstants.HALFOP_TYPE)));
    }

    public void updateExtraDelay(final CellInterface cell,
                                 final AliasedSet aliases,
                                 final UnaryFunction customize) {
        put(DirectiveConstants.EXTRA_DELAY,
            new HalfOpDirective(cell, aliases,
                (UnaryFunction) customize.execute(
                    new SubcellGetter(DirectiveConstants.EXTRA_DELAY,
                                      DirectiveConstants.HALFOP_TYPE))));
    }

    public void updateEstimatedDelay(final CellInterface cell,
                                     final AliasedSet aliases) {
        put(DirectiveConstants.ESTIMATED_DELAY,
            new HalfOpDirective(cell, aliases,
                new SubcellPrsGetter(DirectiveConstants.ESTIMATED_DELAY,
                                     DirectiveConstants.HALFOP_TYPE)));
    }

    public void updateEstimatedDelaySignoff(final CellInterface cell,
                                            final AliasedSet aliases) {
        put(DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
            new HalfOpDirective(cell, aliases,
                new SubcellPrsGetter(DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
                                     DirectiveConstants.HALFOP_TYPE)));
    }

    public void updateDelayBias(final CellInterface cell, final Double cur) {
        final Double one = new Double(1);
        put(DirectiveConstants.DELAYBIAS, new MultiplicativeDirective(cur, cell,
            new SubcellGetter(DirectiveConstants.DELAYBIAS,
                              DirectiveConstants.INSTANCE_TYPE) {
                public Object execute(final Object o) {
                    final Map result = new HashMap((Map) super.execute(o));
                    final CellInterface cell = (CellInterface) o;
                    result.put(null,
                        DirectiveUtils.getTopLevelDirective(cell, dir));
                    return result;
                }
            },
            new MultiplicativeDirective.Multiply(one)));
    }

    public void updateDelayBias(final CellInterface cell) {
        updateDelayBias(cell, 1.0);
    }

    public class MeasuredProcessor implements MeasuredGetter.Processor {
        private final float delayScale;
        private final int dataSet;
        public MeasuredProcessor(final float delayScale, final int dataSet) {
            this.delayScale = delayScale;
            this.dataSet = dataSet;
        }
        private String errorPrefix(final CellInterface cell, final Pair target,
                                   final HierName trigger) {
            final HierName node = (HierName) target.getFirst();
            final Boolean up = (Boolean) target.getSecond();
            return "In " + cell.getFullyQualifiedType() + " " +
                   (trigger == null ? "<any>" : trigger.toString()) +
                   " -> " + node + (up.booleanValue() ? "+" : "-");
        }
        private void E(final String s) {
            System.err.println(s);
        }
        private float[] toFloatArray(final List obj) {
            final float[] result = new float[obj.size()];
            for (int i = 0; i < obj.size(); ++i) {
                result[i] = ((Float) obj.get(i)).floatValue();
            }
            return result;
        }

        private Object linearModel(final CellInterface cell, final Pair target,
                                   final HierName trigger, final List array)
        {
            if (array.size() < 4 || array.size() % 2 != 0) {
                E(errorPrefix(cell, target, trigger) + ": First dimension is " +
                  array.size() + "; it must at least be 4 and even");
                return null;
            }

            final int index;
            final int totalSets = (array.size() - 2) / 2;
            if (dataSet >= 0 && dataSet < totalSets) {
                index = dataSet;
            } else {
                E(errorPrefix(cell, target, trigger) + ": Selected dataset " +
                  dataSet + " out of valid range [0.." + (totalSets - 1) + "]");
                index = 0;
            }

            final List inputSlew = (List) array.get(1);
            final List delay = (List) array.get(2 + index * 2);
            final List outputSlew = (List) array.get(3 + index * 2);
            if (inputSlew.size() != delay.size() ||
                inputSlew.size() != outputSlew.size()) {
                E(errorPrefix(cell, target, trigger) + ": Number of delay and output slew points are  " + delay.size() + " and " + outputSlew.size() + " respectively; number of input slew points is " + inputSlew.size());
                return null;
            }

            final float[] result = new float[inputSlew.size() * 3];
            for (int i = 0; i < inputSlew.size(); ++i) {
                result[i * 3] = ((Float) inputSlew.get(i)).floatValue();
                result[i * 3 + 1] =
                    ((Float) delay.get(i)).floatValue() * delayScale;
                result[i * 3 + 2] = ((Float) outputSlew.get(i)).floatValue();
            }
            return result;
        }
        private Object polynomialModel(final CellInterface cell,
                                       final Pair target,
                                       final HierName trigger,
                                       final List array) {
            if (array.size() % 2 == 0) {
                E(errorPrefix(cell, target, trigger) + ": First dimension is " +
                  array.size() + "; it must be odd");
                return null;
            }

            final int index;
            final int totalSets = (array.size() - 1) / 2;
            if (dataSet >= 0 && dataSet < totalSets) {
                index = dataSet;
            } else {
                E(errorPrefix(cell, target, trigger) + ": Selected dataset " +
                  dataSet + " out of valid range [0.." + (totalSets - 1) + "]");
                index = 0;
            }
            final float[] delayCoeff =
                toFloatArray((List) array.get(1 + index * 2));
            for (int i = 0; i < delayCoeff.length; ++i)
                delayCoeff[i] *= delayScale;

            final float[] slewCoeff =
                toFloatArray((List) array.get(2 + index * 2));

            return new Pair(delayCoeff, slewCoeff);
        }

        public Object process(final CellInterface cell, final Pair target,
                              final HierName trigger, final List value) {
            final List first = (List) value.get(0);

            Object result = null;
            final int model = Math.round(((Float) first.get(0)).floatValue());
            switch (model) {
              case Rule.LINEAR_MODEL:
                result = linearModel(cell, target, trigger, value);
                break;
              case Rule.POLYNOMIAL_MODEL:
                result = polynomialModel(cell, target, trigger, value);
                break;
              default:
                E(errorPrefix(cell, target, trigger) + ": " + model +
                  " is not a recognized model");
            }
            return result == null ? null : new Pair(new Integer(model), result);
        }
    }

    public void updateMeasuredDelay(final HierName prefix,
                                    final CellInterface cell,
                                    final UnaryFunction canonizer,
                                    final float delayScale,
                                    final int dataSet) {
        final UnaryFunction getter =
            new MeasuredGetter(
                new SubcellPrsGetter(
                    DirectiveConstants.MEASURED_DELAY,
                    DirectiveConstants.UNCHECKED_HALFOP_TYPE + "[]"),
                new MeasuredProcessor(delayScale, dataSet));

        final AttributeInterface up =
            new MeasuredDirective(cell, canonizer,
                                  new HalfOpDirective.Convert(true, getter),
                                  prefix);
        final AttributeInterface dn =
            new MeasuredDirective(cell, canonizer,
                                  new HalfOpDirective.Convert(false, getter),
                                  prefix);

        put(DirectiveConstants.MEASURED_DELAY, new HalfOpDirective(up, dn));
    }

    private void updateCap(final HierName prefix, final CellInterface cell,
                           final UnaryFunction canonizer, final String dir) {
        final UnaryFunction baseGetter =
            new SubcellPrsGetter(dir, DirectiveConstants.UNCHECKED_NODE_TYPE);
        final UnaryFunction getter =
            new UnaryFunction() {
                public Object execute(final Object o) {
                    final Pair p = (Pair) o;
                    final HierName prefix = (HierName) p.getFirst();
                    final CellInterface cell = (CellInterface) p.getSecond();
                    return baseGetter.execute(cell);
                }
            };

        final AttributeInterface measured =
            new MeasuredDirective(cell, canonizer, getter, prefix);
        put(dir, measured);
    }

    public void updateEstimatedCap(final HierName prefix,
                                   final CellInterface cell,
                                   final UnaryFunction canonizer) {
        updateCap(prefix, cell, canonizer, DirectiveConstants.ESTIMATED_CAP);
    }

    public void updateMeasuredCap(final HierName prefix,
                                  final CellInterface cell,
                                  final UnaryFunction canonizer) {
        updateCap(prefix, cell, canonizer, DirectiveConstants.MEASURED_CAP);
    }

    private Object getHalfopResult(final boolean up, final HierName canon,
                                   final String name, final Object def) {
        final Pair p = (Pair) get(name).getAttribute(canon);
        final Object result = up ? p.getFirst() : p.getSecond();
        return result == null ? def : result;
    }

    private float getHalfopResult(final boolean up, final HierName canon,
                                  final String name, final float def) {
        final Float result = (Float) getHalfopResult(up, canon, name, null);
        return result == null ? def : result.floatValue();
    }

    /**
     * Returns the <code>extra_delay</code> directive (from a subcells
     * block) associated with the given node.
     *
     * @param up <code>true</code> for up transition, <code>false</code>
     * for down transition.
     * @param canon Canonical name of the node
     * @return Value of the <code>extra_delay</code> directive for
     * <code>canon</code> transitioning in the <code>up</code> direction,
     * or 0 if no such directive has been specified.
     **/
    public float getExtraDelay(final boolean up, final HierName canon) {
        float astaExtraDelay = 0;
        if (get(DirectiveConstants.ASTA_EXTRA_DELAY) != null) {
            astaExtraDelay =
                getHalfopResult(up, canon, DirectiveConstants.ASTA_EXTRA_DELAY,
                                0);
        }
        return astaExtraDelay +
               getHalfopResult(up, canon, DirectiveConstants.EXTRA_DELAY, 0);
    }

    public Object getExtraDelay(final boolean up, final HierName canon,
                                final Object def) {
        Object astaExtraDelay = def;
        if (get(DirectiveConstants.ASTA_EXTRA_DELAY) != null) {
            astaExtraDelay =
                getHalfopResult(up, canon, DirectiveConstants.ASTA_EXTRA_DELAY,
                                def);
        }

        Object extraDelay =
            getHalfopResult(up, canon, DirectiveConstants.EXTRA_DELAY, def);

        if (astaExtraDelay == def && extraDelay == def) {
            return def;
        } else if (astaExtraDelay == def) {
            return extraDelay;
        } else if (extraDelay == def) {
            return astaExtraDelay;
        } else {
            return ((Float) extraDelay).floatValue() +
                   ((Float) astaExtraDelay).floatValue();
        }
    }

    public float getEstimatedDelay(final boolean up, final HierName canon) {
        return getHalfopResult(up, canon, DirectiveConstants.ESTIMATED_DELAY,
                               Float.NaN);
    }

    public float getEstimatedDelaySignoff(final boolean up,
                                          final HierName canon) {
        return getHalfopResult(up, canon,
                               DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
                               Float.NaN);
    }

    public Triplet[] getMeasuredDelay(final boolean up, final HierName canon) {
        final Pair dir =
            (Pair) get(DirectiveConstants.MEASURED_DELAY).getAttribute(canon);
        final List result = (List) (up ? dir.getFirst() : dir.getSecond());
        return result == null ? null : (Triplet[]) result.toArray(new Triplet[0]);
    }

    public Map getMeasuredDelay(final boolean up) {
        final HalfOpDirective halfopDir =
            (HalfOpDirective) get(DirectiveConstants.MEASURED_DELAY);
        final MeasuredDirective dirs = up ? (MeasuredDirective) halfopDir.up
                                          : (MeasuredDirective) halfopDir.down;
        return dirs == null ? null
                            : Collections.unmodifiableMap(dirs.directives);
    }

    /**
     * Return the measured_cap directive on the specified node in F.  The
     * measured_cap directive is actually specified in fF.
     **/
    public float getMeasuredCap(final HierName canon) {
        final Float result =
            (Float) get(DirectiveConstants.MEASURED_CAP).getAttribute(canon);
        return result == null ? Float.NaN : (result.floatValue() * 1e-15f);
    }

    public float getEstimatedCap(final HierName canon) {
        final Float result =
            (Float) get(DirectiveConstants.ESTIMATED_CAP).getAttribute(canon);
        return result == null ? Float.NaN : (result.floatValue() * 1e-15f);
    }

    public void dumpMeasuredDelay() {
        final HalfOpDirective halfopDir =
            (HalfOpDirective) get(DirectiveConstants.MEASURED_DELAY);
        final MeasuredDirective up = (MeasuredDirective) halfopDir.up;
        final MeasuredDirective dn = (MeasuredDirective) halfopDir.down;
        if (up != null) {
            System.err.println("up: " + up.directives);
        }
        if (dn != null) {
            System.err.println("dn: " + dn.directives);
        }
    }

    public float getDelayBias(final HierName instance) {
        final Number result =
            (Number) get(DirectiveConstants.DELAYBIAS).getAttribute(instance);
        return result.floatValue();
    }

    public String toString() {
        return attributes.toString();
    }
}
