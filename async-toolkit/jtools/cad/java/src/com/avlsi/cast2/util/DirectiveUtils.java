/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast2.util;

import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.HashSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.function.Function;

import com.avlsi.cast2.directive.impl.DirectiveComparator;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.layout.LayerCallback;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cosim.ChannelTimingInfo;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.BinaryFunction;

/**
 * Class to hold functions operating on the maps returned by the
 * directive parser.
 * @author Aaron Denney
 * @version $Date$
 **/

public final class DirectiveUtils {
    /**
     * Static class.  Unconstructable.
     * @deprecated
     **/
    private DirectiveUtils() {
    }

    public static Object gruntDirective(BlockInterface b,
                                        String directive) {
        final DirectiveBlock db = getDirectiveBlock(b);
        if (db == null) {
            return DirectiveTable.lookupDirective(b.getType(), directive).getSecond();
        } else {
            Object o = null;
            assert db.isKey(directive) : "Directive " + directive +
                                         " not defined in " + b.getType() +
                                         " block";
            try {
                o = db.lookup(directive);
            } catch (UnknownDirectiveException e) {
                System.err.println(directive + " not registered?");
            }
            return o;
        }
    }

    /**
     * Does the grunt work to get a nonparametrized directive attached to the
     * cell.
     *
     * @param cell Cell to look in
     * @param directive Name of directive to look up
     *
     * @return The value of the directive.  If this cell (and its supercells) do not
     * set this directive, the default value (which may be null) is returned.
     **/
    public static Object getTopLevelDirective(CellInterface cell, String directive) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(cellBlock, directive);
    }

    /**
     * Does the grunt work to get a parametrized directive attached to the
     * given block.
     *
     * @param b block to look for directives block in.
     * @param directive Name of directive to look up.
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map gruntDirective(BlockInterface b, String directive,
                                     String type) {
        Map m = Collections.EMPTY_MAP;
        final DirectiveBlock db = getDirectiveBlock(b);
        if (db == null) { return m; } // no directive block.
        assert db.isKey(directive, type) : "Directive " + directive + "(" +
                                           type + ") not defined in " +
                                           b.getType() + " block";
        try {
            m = db.getValues(directive, type);
        } catch (UnknownDirectiveException e) {
            System.err.println(directive + "<"+type+"> not registered?");
        }
        return m;
    }

    /**
     * Gets a parameterized directive that can be defined in multiple blocks.
     *
     * @param blocks An list of BlockInterfaces, eqach one overriding the next
     * @param directive Name of directive to look up
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map getMultipleBlockDirective(List blocks,
                                                String directive,
                                                String type) {
        final Map map = new HashMap();
        for(Iterator i = blocks.iterator(); i.hasNext();) {
            final BlockInterface block = (BlockInterface) i.next();
            map.putAll(gruntDirective(block, directive, type));
        }
        return map;
    }

    public static BlockInterface getUniqueBlock(BlockInterface block,
                                                String type ) {

        BlockIterator subBlockIter = block.iterator(type);

        Debug.assertTrue(subBlockIter.hasNext(), "No " + type + " block exists!");
        BlockInterface b = subBlockIter.next();
        Debug.assertTrue(!subBlockIter.hasNext(), "Multiple " + type + " blocks?");

        return b;
    }

    public static Object getBlockDirective(CellInterface cell, String block,
                                           String directive) {
        return gruntDirective(getUniqueBlock(cell.getBlockInterface(), block),
                              directive);
    }

    public static Map getBlockDirective(CellInterface cell, String block,
                                        String directive, String type) {
        return gruntDirective(getUniqueBlock(cell.getBlockInterface(), block),
                              directive, type);
    }

    /**
     * Does the grunt work to get a parametrized directive attached to the
     * cell.
     *
     * @param cell Cell to look in
     * @param directive Name of directive to look up
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map getTopLevelDirective(CellInterface cell, String directive, String type) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(cellBlock, directive, type);
    }

    /**
     * Does the grunt work to get a parametrized directive attached to the prs
     * block.
     *
     * @param cell Cell to look in for directives attached to prs block.
     * @param directive Name of directive to look up
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map getPrsDirective(CellInterface cell, String directive, String type) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.PRS), directive, type);
    }

    public static Object getPrsDirective(CellInterface cell, String directive) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.PRS), directive);
    }

    /**
     * Does the grunt work to get a parametrized directive attached to the
     * subcell block.
     *
     * @param cell Cell to look in for directives attached to subcell block.
     * @param directive Name of directive to look up
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map getSubcellDirective(CellInterface cell, String directive, String type) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.SUBCELL), directive, type);
    }

    public static Object getSubcellDirective(CellInterface cell, String directive) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.SUBCELL), directive);
    }

    /**
     * Does the grunt work to get a parametrized directive attached to the
     * a cell specified in the env block.
     *
     * @param cell Cell to look in for directives attached to subcell block.
     * @param directive Name of directive to look up
     * @param type type directive is parameterized on.
     *
     * @return A (possibly empty) map having all the non-default values that have been set.
     **/
    public static Map getEnvDirective(CellInterface cell, String directive, String type) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.ENV), directive, type);
    }

    public static Object getEnvDirective(CellInterface cell, String directive) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.ENV), directive);
    }

    public static Map getCspDirective(CellInterface cell, String directive,
                                      String type) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.CSP),
                              directive, type);
    }

    public static Object getCspDirective(CellInterface cell, String directive) {
        BlockInterface cellBlock = cell.getBlockInterface();
        return gruntDirective(getUniqueBlock(cellBlock,BlockInterface.CSP),
                              directive);
    }

    public static Map getJavaDirective(CellInterface cell, String directive,
                                       String type) {
        return getBlockDirective(cell, BlockInterface.JAVA, directive, type);
    }

    /**
     * Returns <code>true</code> if there is a directive block associated with
     * block of type <code>blk</code> in <code>cell</code>, and if
     * <code>directive</code> parameterized on <code>type</code> is a valid
     * directive in that directive block.
     **/
    public static boolean containsDirective(CellInterface cell,
                                            String blk,
                                            String directive,
                                            String type) {
        final BlockInterface cellBlock = cell.getBlockInterface();
        final BlockInterface selected =
            blk.equals(BlockInterface.CELL) ? cellBlock
                                            : getUniqueBlock(cellBlock, blk);
        final DirectiveBlock db = getDirectiveBlock(selected);
        return db != null && db.isKey(directive, type);
    }

    /**
     * Given a Map from <x> to Boolean, give back
     * the set of those that are set to TRUE.
     *
     * Also verify that all values are Boolean.
     **/
    public static Set getExplicitTrues(Map m, Set s) {
        Iterator i = m.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry e = (Map.Entry) i.next();
            if (Boolean.TRUE.equals((Boolean)e.getValue())) {
                s.add(e.getKey());
            }
        }
        return s;
    }

    public static Set getExplicitTrues(Map m) {
        return getExplicitTrues(m, new HashSet());
    }

    /**
     * Given a Map from <x> to Boolean, give back
     * the set of those that are set to FALSE.
     *
     * Also verify that all values are Boolean.
     **/
    public static Set getExplicitFalses(Map m) {
        Set s = new HashSet();
        Iterator i = m.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry e = (Map.Entry) i.next();
            if (Boolean.FALSE.equals((Boolean)e.getValue())) {
                s.add(e.getKey());
            }
        }
        return s;
    }

    /**
     * Given a Map from <x> to Integer, give back
     * the set of those that are set to non-zero.
     *
     * Also verify that all values are Integer.
     **/
    public static Set getExplicitNonZeroIntegers(Map m) {
        Set s = new HashSet();
        Iterator i = m.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry e = (Map.Entry) i.next();
            Integer x = (Integer) e.getValue();
            if ((x!=null) && (x.intValue()>0)) {
                s.add(e.getKey());
            }
        }
        return s;
    }

    /*
     * HalfOperators are returned as (HierName, Boolean) pairs,
     * with the Boolean possibly being null to specify both up and Down.  See PrsCallback.
     */

    /**
     * Extracts the kvps whose keys are up or both up and down
     * from a map of <HalfOperator,X> into a map of <HierName,X>.
     **/
    public static Map getUps(Map m) {
        Map rv = new LinkedHashMap();
        Iterator i = m.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry e = (Map.Entry) i.next();
            Pair p = (Pair) e.getKey();
            Boolean direction = (Boolean) p.getSecond();
            HierName h = (HierName) p.getFirst();

            if (Boolean.TRUE.equals(direction) || direction == null) {
                rv.put(h,e.getValue());
            }
        }

        return rv;
    }

    /**
     * Extracts the kvps whose keys are down or both up and down
     * from a map of <HalfOperator,X> into a map of <HierName,X>.
     **/
    public static Map getDowns(Map m) {
        Map rv = new LinkedHashMap();
        Iterator i = m.entrySet().iterator();
        while (i.hasNext()) {
            Map.Entry e = (Map.Entry) i.next();
            Pair p = (Pair) e.getKey();
            Boolean direction = (Boolean) p.getSecond();
            HierName h = (HierName) p.getFirst();

            if (Boolean.FALSE.equals(direction) || direction == null) {
                rv.put(h,e.getValue());
            }
        }

        return rv;
    }

    /**
     * Return a new map whose keys are those of m,
     * but canonicalized according to a.
     *
     * @param a The aliased set determining canonicalness.
     * @param m The map whose keys are to be canonized.
     *
     * @return A new map with the specified properties.
     **/
    public static Map canonizeKey(AliasedSet a, Map m) {
        return canonizeKey(a, m, REPLACE);
    }

    public static BinaryFunction REPLACE = new BinaryFunction() {
        public Object execute(final Object a, final Object b) {
            return b;
        }
    };

    public static BinaryFunction MIN = new BinaryFunction() {
        public Object execute(final Object a, final Object b) {
            return ((Comparable) a).compareTo(b) <= 0 ? a : b;
        }
    };

    public static BinaryFunction MAX = new BinaryFunction() {
        public Object execute(final Object a, final Object b) {
            return ((Comparable) a).compareTo(b) >= 0 ? a : b;
        }
    };

    public static Map canonizeKey(AliasedSet a, Map m, BinaryFunction merge) {
        Map rv = new LinkedHashMap();
        Iterator i = m.keySet().iterator();
        while (i.hasNext()) {
            Object o = i.next();
            Object x = a.getCanonicalKey(o);
            Object key = x == null ? o : x;

            Object value = m.get(o);
            if (rv.containsKey(key)) value = merge.execute(rv.get(key), value);

            rv.put(key, value);
        }
        return rv;
    }

    /**
     * Return a new set whose members are those of s,
     * but canonicalized according to a.
     *
     * @param a The aliased set determining canonicalness.
     * @param s The set whose members are to be canonized.
     *
     * @return A new set with the specified properties.
     **/
    public static Set canonize(AliasedSet a, Set s) {
        Set rv = new HashSet();
        Iterator i = s.iterator();
        while (i.hasNext()) {
            Object o = i.next();
            Object x = a.getCanonicalKey(o);
            if (x == null) {
                rv.add(o);
            } else {
                rv.add(x);
            }
        }
        return rv;
    }

    /*
     * Return a <code>DirectiveInterface</code> that has values of type
     * <code>NODE_TYPE</code> and <code>HALFOP_TYPE</code> canonicalized
     * according to the given <code>AliasedSet</code>.
     *
     * @param alias An <code>AliasedSet</code> containing alias information.
     * @param db The <code>DirectiveInterface</code> to canonicalize.
     *
     * @return a canonicalized <code>DirectiveInterface</code>.
     */
    /*
    // XXX: Perhaps some day.
    public static DirectiveInterface canonize(final AliasedSet alias,
                                              final DirectiveInterface db) {

        return new DirectiveInterface() {
            public Object getDefaultValue(String key, String memberType)
                throws UnknownDirectiveException {
                return alias.getDefaultValue(key, memberType);
            }

            public Map getValues(String key, String memberType)
                throws UnknownDirectiveException {
            }

            public Object lookup(String key) throws UnknownDirectiveException {
            }

            public boolean containsDirective(String key)
                throws UnknownDirectiveException {
            }

            public Object lookup(String key, String memberType,
                                 Object parameter)
                throws UnknownDirectiveException {
            }

            public boolean isKey(String key) {
            }

            public boolean isKey(String key, String memberType) {
            }

            public Iterator paramEntryIterator() {
            }

            public Iterator noparamEntryIterator() {
            }
        };
    }
    */

    /**
     * Looks up h in map m returning the value of the Float that corresponds
     * to key h.  If h is not in the map returns nokey.
     *
     * @param h The HierName to look up.
     * @param m The map to look it up in.
     * @param nokey The return value if h is not in the map.
     **/
    public static float getFromFloatMap(HierName h, Map m, float nokey) {
        float rv = nokey; 
        if (m != null) {
            Float x = (Float) m.get(h);
            if (x != null) {
                rv = x.floatValue();
            }
        }
        return rv;
    }

    /**
     * Return the directive block associated with a block.
     *
     * @param block Where to get the directive block.
     * @return The directive block associated with the given block, or
     * <code>null</code> if there is no directive block.
     **/
    public static DirectiveBlock getDirectiveBlock(BlockInterface block) {
        final BlockIterator bi = block.iterator(BlockInterface.DIRECTIVE);
        if(!bi.hasNext()) return null;

        final DirectiveBlock db = (DirectiveBlock) bi.next();
        assert !bi.hasNext() : "Multiple directive blocks attached to " + block.getType() + " block?";
        return db;
    }

    private static class CompareDirective implements DirectiveActionInterface {
        private static class Exception extends RuntimeException {
            public Exception(final String message) {
                super(message);
            }

            public Exception(final String message, final Throwable cause) {
                super(message, cause);
            }
        }

        private final DirectiveBlock target;
        private final DirectiveComparator test;

        public CompareDirective(final DirectiveBlock target,
                                final DirectiveComparator test) {
            this.target = target;
            this.test = test;
        }

        public void doUnParameterizedDirective(BlockInterface block,
                                               DirectiveBlock db,
                                               String directive,
                                               Object value,
                                               String valueType)
        throws IOException {
            final Object v;
            try {
                v = target.lookup(directive);
            } catch (UnknownDirectiveException e) {
                throw new Exception("Invalid directive: " + directive, e);
            }
            if (!test.compare(block.getType(), valueType, value, v))
            {
                throw new Exception("Values are not equivalent: " + v + " vs " + value);
            }
        }

        public void doParameterizedDirectiveValue(BlockInterface block,
                                                  DirectiveBlock db,
                                                  String directive,
                                                  Object parameter,
                                                  Object value,
                                                  String parameterType,
                                                  String valueType)
        throws IOException {
            final Object v;
            try {
                v = target.lookup(directive, parameterType, parameter);
            } catch (UnknownDirectiveException e) {
                throw new Exception("Invalid directive: " + directive, e);
            }
            if (!test.compare(block.getType(), valueType, value, v))
            {
                throw new Exception("Values are not equivalent: " + v + " vs " + value);
            }
        }

        public void doParameterizedDirectiveType(BlockInterface block,
                                                 DirectiveBlock db,
                                                 String directive,
                                                 String parameterType,
                                                 String valueType)
        throws IOException { }

        public void doBlockInterface(BlockInterface block) throws IOException {
        }
    }

    public static boolean equalDirectiveBlock(final BlockInterface block1,
                                              final DirectiveActionFilter f1,
                                              final BlockInterface block2,
                                              final DirectiveActionFilter f2,
                                              final DirectiveComparator comp) {
        return equalDirectiveBlock(block1, getDirectiveBlock(block1), f1,
                                   block2, getDirectiveBlock(block2), f2,
                                   comp);
    }

    public static boolean equalDirectiveBlock(final BlockInterface block1,
                                              final DirectiveBlock db1,
                                              final DirectiveActionFilter f1,
                                              final BlockInterface block2,
                                              final DirectiveBlock db2,
                                              final DirectiveActionFilter f2,
                                              final DirectiveComparator comp) {
        if ((db1 == null || db2 == null) && db1 != db2) return false;
        try {
            final DirectiveWalker walker1 =
                new DirectiveWalker(f2.filter(new CompareDirective(db2, comp)));
            walker1.walk(block1, db1);

            final DirectiveWalker walker2 =
                new DirectiveWalker(f1.filter(new CompareDirective(db1, comp)));
            walker2.walk(block2, db2);
        } catch (UnknownDirectiveException e) {
            return false;
        } catch (IOException e) {
            throw new AssertionError("Cannot happen.");
        } catch (CompareDirective.Exception e) {
            return false;
        }
        return true;
    }

    private static HierName makeHierName(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot create HierName from " + s, e);
        }
    }

    public static Object parseDirective(final String type, final String val) {
        if (type.equals(DirectiveConstants.INT_TYPE)) {
            return Integer.valueOf(val);
        } else if (type.equals(DirectiveConstants.FLOAT_TYPE)) {
            return Float.valueOf(val);
        } else if (type.equals(DirectiveConstants.DOUBLE_TYPE)) {
            return Double.valueOf(val);
        } else if (type.equals(DirectiveConstants.BOOLEAN_TYPE)) {
            return Boolean.valueOf(val);
        } else if (type.equals(DirectiveConstants.STRING_TYPE)) {
            return val;
        } else if (type.equals(DirectiveConstants.NODE_TYPE)) {
            return makeHierName(val);
        } else if (type.equals(DirectiveConstants.HALFOP_TYPE)) {
            return makeHierName(val);
        } else if (type.equals(DirectiveConstants.CHANNEL_TYPE)) {
            return makeHierName(val);
        } else if (type.equals(DirectiveConstants.INSTANCE_TYPE)) {
            return makeHierName(val);
        } else if (type.equals(DirectiveConstants.LAYER_TYPE)) {
            return LayerCallback.getInstance().resolve(DirectiveConstants.LAYER_TYPE, val, null);
        } else {
            return null;
        }
    }

    public static Map scaleFloatMap(final Map spec, 
                                    final float scale) {
        assert spec != null;
        if (scale == 1) return spec;
        final LinkedHashMap result = new LinkedHashMap();
        for (Iterator i = spec.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            result.put(entry.getKey(),
                       new Float(((Float) entry.getValue()).floatValue() * scale));
        }
        return result;
    }

    /**
     * Scales the <code>ntpc_spec</code> directives specified in an environment
     * cell appropriately by the value specified by the
     * <code>ntpc_scaling</code> top-level directive.
     *
     * @param cell Top level cell containing the environment
     * @param spec A map of nodes to ntpc_spec.
     * @return A map of nodes to ntpc_spec, taking into account ntpc_scaling.
     * If ntpc_scaling is 1, then <code>spec</code> will be returned.
     **/
    public static Map scaleNtpcSpec(final CellInterface cell,
                                    final Map spec ) {
        final float scale = 
            ((Float) getTopLevelDirective
             (cell,
              DirectiveConstants.NTPC_SCALING)).floatValue();
        return scaleFloatMap(spec, scale);
    }

    /**
     * Scales the <code>ntpc_spec</code> directives specified in an environment
     * cell appropriately by the value specified by the
     * <code>ntpc_scaling_signoff</code> top-level directive.
     *
     * @param cell Top level cell containing the environment
     * @param spec A map of nodes to ntpc_spec.
     * @return A map of nodes to ntpc_spec, taking into account ntpc_scaling_signoff.
     * If ntpc_scaling_signoff is 1, then <code>spec</code> will be returned.
     **/
    public static Map scaleNtpcSpecSignoff(final CellInterface cell,
                                           final Map spec ) {
        final float scale = 
            ((Float) getTopLevelDirective
             (cell,
              DirectiveConstants.NTPC_SCALING_SIGNOFF)).floatValue();
        return scaleFloatMap(spec, scale);
    }

    /**
     * Gets the value of a half-operator directive for the specified cell,
     * node, and direction.
     **/
    public static Object getHalfOpDirectiveValue(
            final /*@ non_null @*/ CellInterface cell,
            final /*@ non_null @*/ String directive,
            final /*@ non_null @*/ HierName nodeName,
            final boolean direction,
            final /*@ non_null @*/ Cadencize cadencize) {
        final Map/*<Pair<HierName,Boolean>,Object>*/ halfOpMap =
            getTopLevelDirective(cell, directive,
                                 DirectiveConstants.HALFOP_TYPE);
        final Map/*<HierName,Object>*/ nodeMap;
        if (direction)
            nodeMap = getUps(halfOpMap);
        else
            nodeMap = getDowns(halfOpMap);
        final AliasedSet/*<HierName>*/ localNodes =
            cadencize.convert(cell).getLocalNodes();
        final Map/*<HierName,Object>*/ canonicalNodeMap =
            canonizeKey(localNodes, nodeMap);
        return canonicalNodeMap.get(nodeName);
    }
    
    public static String dumpDirectiveBlock(BlockInterface bi) {
        final StringBuilder buf = new StringBuilder();
        for (BlockIterator it = bi.iterator(BlockInterface.DIRECTIVE);
             it.hasNext(); ) {
            final DirectiveBlock dirBlock = (DirectiveBlock) it.next();
            for (Iterator entryIter = dirBlock.paramEntryIterator();
                 entryIter.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) entryIter.next();
                buf.append(entry.getKey() + " => " + entry.getValue() + "\n");
            }
        }
        return buf.toString();
    }

    private static void propagateWireDirective(final CellInterface cell,
                                              final NetProperty.Cache cache,
                                              final String dir,
                                              final BinaryFunction update,
                                              final Cadencize cad) {
        final Map result = NetProperty.getNodeProperties(cache, cell, cad, NetProperty.WireProperty.getFactory(new NetProperty.WireProperty.WireDirective(cad, dir, update), update));
        final DirectiveSource src = new DirectiveSource(BlockInterface.SUBCELL);
        for (Iterator i = result.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final NetProperty.Property property =
                (NetProperty.Property) entry.getValue();
            if (property.getValue() != null) {
                src.definition(dir, DirectiveConstants.NODE_TYPE, entry.getKey(), property.getValue());
            }
        }
        final BlockInterface subcell =
            cell.getBlockInterface().iterator(BlockInterface.SUBCELL).next();
        final DirectiveInterface old = getDirectiveBlock(subcell);
        final DirectiveInterface replace = src.getDirectiveInterface();
        final DirectiveInterface nevv = old == null ? replace :
            new DirectiveOverride(old, replace, dir);
        final BlockIterator it = subcell.iterator(BlockInterface.DIRECTIVE);
        if (it.hasNext()) {
            it.next();
            it.set(new DirectiveBlock(nevv));
        } else {
            it.add(new DirectiveBlock(nevv));
        }
    }
    public static void propagateWireDirective(final CellInterface cell,
                                              final Cadencize cad) {
        propagateWireDirective(cell, new NetProperty.Cache(), DirectiveConstants.WIREWIDTH, NetProperty.WireProperty.MAX, cad);
        propagateWireDirective(cell, new NetProperty.Cache(), DirectiveConstants.WIRESPACE, NetProperty.WireProperty.MAX, cad);
        propagateWireDirective(cell, new NetProperty.Cache(), DirectiveConstants.WIRELENGTH, NetProperty.WireProperty.MIN, cad);
        propagateWireDirective(cell, new NetProperty.Cache(), DirectiveConstants.WIRESPAN, NetProperty.WireProperty.MIN, cad);
        propagateWireDirective(cell, new NetProperty.Cache(), DirectiveConstants.RESET_NET, NetProperty.WireProperty.OR, cad);
    }

    public static ChannelTimingInfo getTiming(final CellInterface cell,
                                              final String channel) {
        return getTiming(cell, channel, 1);
    }

    public static ChannelTimingInfo getTiming(final CellInterface cell,
                                              final String channel,
                                              final Float defaultCycleTime) {
        return getTiming(cell, BlockInterface.CSP, channel, 1, defaultCycleTime);
    }

    public static ChannelTimingInfo getTiming(final CellInterface cell,
                                              final String channel,
                                              final int defStage) {
        return getTiming(cell, BlockInterface.CSP, channel, defStage);
    }
    
    public static ChannelTimingInfo getTiming(final CellInterface cell,
                                              final String block,
                                              final String channel,
                                              final int defStages) {
        return getTiming(cell, block, channel, defStages, null);

   }

    public static ChannelTimingInfo getTiming(final CellInterface cell,
                                              final String block,
                                              final String channel,
                                              final int defStages,
                                              final Float defaultCycleTime) {
        final Function<String,Object> getWideDir = dir ->
            getBlockDirective(cell, block, dir, DirectiveConstants.WIDE_CHANNEL_TYPE)
                   .get(channel);
        final Function<String,Object> getTopPosWideDir = dir ->
            getTopLevelDirective(cell, dir, DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE)
                   .get(channel);

        // find dynamic_slack, a scalar quantity that describes the maximum
        // number of tokens in a channel while still cycling at cycle_time
        final Integer dynslack = (Integer) getWideDir.apply(DirectiveConstants.DYNAMIC_SLACK);

        // find cycle_time
        final Float defaultct = (defaultCycleTime == null) ?
            (Float) getTopLevelDirective(cell, DirectiveConstants.CYCLE_TIME) : defaultCycleTime;
        final Float ct = (Float) getTopPosWideDir.apply(DirectiveConstants.CYCLE_TIME);
        final float cycleTime =
            ct == null ? defaultct.floatValue() : ct.floatValue();

        // find cycle_time_in
        final Float cti = (Float) getWideDir.apply(DirectiveConstants.CYCLE_TIME_IN);
        final float cycleTimeIn = cti == null ? cycleTime : cti.floatValue();

        // find cycle_time_out
        final Float cto = (Float) getWideDir.apply(DirectiveConstants.CYCLE_TIME_OUT);
        final float cycleTimeOut = cto == null ? cycleTime : cto.floatValue();

        // find latency_per_slack 
        final Float defaultlps = (Float) getBlockDirective(cell, block, DirectiveConstants.LATENCY_PER_SLACK);
        final Float lps_t = (Float) getWideDir.apply(DirectiveConstants.LATENCY_PER_SLACK);
        final float latencyPerSlack =
            lps_t == null ? defaultlps.floatValue() : lps_t.floatValue();

        // find slack
        final int slack;
        final Integer _slack = (Integer) getWideDir.apply(DirectiveConstants.SLACK);
        final Integer _stages = (Integer) getWideDir.apply(DirectiveConstants.STAGES);
        final Float sps = (Float) getBlockDirective(cell, block, DirectiveConstants.SLACK_PER_STAGE);
        final int stages = _stages == null ? defStages : _stages.intValue();
        final float dynamicCycleTime;
        if (_slack == null) {
            if (dynslack == null) {
                slack = (int) Math.ceil(stages * sps.floatValue());
                dynamicCycleTime = cycleTime;
            } else {
                slack = dynslack.intValue();
                // find cycle_time after dynamic_slack adjustment
                dynamicCycleTime = cycleTime / dynslack.intValue();
            }
        } else {
            slack = _slack.intValue();
            if (_stages != null) {
                System.err.println("WARNING: Both slack and stages specified for channel " + channel + " in " + cell.getFullyQualifiedType() + "; using slack directive");
            }
            dynamicCycleTime = cycleTime;
            if (dynslack != null) {
                System.err.println("WARNING: Both slack and dynamic_slack specified for channel " + channel + " in " + cell.getFullyQualifiedType() + "; ignoring dynamic_slack directive");
            }
        }

        final Integer _internalSlack = (Integer) getWideDir.apply(DirectiveConstants.INTERNAL_SLACK);
        final int internalSlack = _internalSlack == null ? 0 : _internalSlack;

        // find forward latency
        final float latency;
        final Float _latency = (Float) getWideDir.apply(DirectiveConstants.LATENCY);
        if (_latency == null) {
            if (slack == 0) {
                latency = 0;
            } else {
                final Float lps = (Float) getBlockDirective(cell, block, DirectiveConstants.LATENCY_PER_STAGE);
                latency = lps.floatValue() * stages;
            }
        } else {
            latency = _latency.floatValue();
        }

        if (slack == 0 && latency > 0) {
            System.err.println("WARNING: Non-zero latency ignored on slack zero channel " + channel + " in " + cell.getFullyQualifiedType());
        }

        // find up transition and down transistion delays in DSim units
        final Float _defaultUpDelay = (Float) getTopLevelDirective(cell, DirectiveConstants.DEFAULT_UP_DELAY);
        final Float _defaultDnDelay = (Float) getTopLevelDirective(cell, DirectiveConstants.DEFAULT_DN_DELAY);
        final float defaultUpDelay = _defaultUpDelay.floatValue() / 100;
        final float defaultDnDelay = _defaultDnDelay.floatValue() / 100;

        // find scale factor for data-enable and enable-data latencies
        final float fbScale = cycleTimeIn >= 10 ? 1 : cycleTimeIn / 10;
        final float bfScale = cycleTimeOut >= 10 ? 1 : cycleTimeOut / 10;

        // find data-enable latency
        final float fbLatency;
        final Float _fbLatency = (Float) getWideDir.apply(DirectiveConstants.FB);
        if (_fbLatency == null) {
            fbLatency = 3;
        } else {
            fbLatency = _fbLatency.floatValue();
        }

        final Float _fbNeutral = (Float) getWideDir.apply(DirectiveConstants.FB_NEUTRAL);
        final float fbNeutral =
            Math.max(0, _fbNeutral == null ?
                            fbScale * (fbLatency + defaultUpDelay - 1)
                          : _fbNeutral.floatValue());

        final Float _fbValid = (Float) getWideDir.apply(DirectiveConstants.FB_VALID);
        final float fbValid =
            Math.max(0, _fbValid == null ?
                            fbScale * (fbLatency + defaultDnDelay - 1)
                          : _fbValid.floatValue());

        // find enable-data latency
        final Float _bfLatency = (Float) getWideDir.apply(DirectiveConstants.BF);
        final float bfLatency;
        if (_bfLatency == null) {
            bfLatency = 2 * bfScale;
        } else {
            bfLatency = _bfLatency.floatValue();
        }

        // find the conversion to DSim units
        final int timeUnit = ((Integer) getTopLevelDirective(cell, DirectiveConstants.TIME_UNIT)).intValue();

        return new ChannelTimingInfo() {
            public int getSlack() { return slack; }
            public int getInternalSlack() { return internalSlack; }
            public int getLatency() {
                return Math.round(latency * timeUnit);
            }
            public int getCycleTime() {
                return Math.round(dynamicCycleTime * timeUnit);
            }
            public int getDataNeutralEnableLatency() {
                return Math.round(fbNeutral * timeUnit);
            }
            public int getDataValidEnableLatency() {
                return Math.round(fbValid * timeUnit);
            }
            public int getEnableDataLatency() {
                return Math.round(bfLatency * timeUnit);
            }
            public int getCycleTimeIn() {
                return Math.round(cycleTimeIn * timeUnit);
            }
            public int getCycleTimeOut() {
                return Math.round(cycleTimeOut * timeUnit);
            }
            public int getLatencyPerSlack() {
                return Math.round(latencyPerSlack * timeUnit);
            }
        };
    }

    public static enum IdleState { IDLE_0, IDLE_1, IDLE_UNKNOWN }

    private static IdleState getIdleState(final int s) {
        switch (s) {
          case 0: return IdleState.IDLE_0;
          case 1: return IdleState.IDLE_1;
          case 2: return IdleState.IDLE_UNKNOWN;
          default:
               throw new RuntimeException("Invalid idle_state constant: " + s);
        }
    }

    private static void getIdleState(final HierName prefix,
                                     final AliasedSet namespace,
                                     final Map<HierName,Integer> dir,
                                     final Map<HierName,IdleState> result) {
        for (Map.Entry<HierName,Integer> entry : dir.entrySet()) {
            final HierName canon = (HierName)
                namespace.getCanonicalKey(
                        HierName.append(prefix, entry.getKey()));
            result.put(canon, getIdleState(entry.getValue()));
        }
    }

    private static void getIdleState(final HierName prefix,
                                     final CellInterface cell,
                                     final AliasedSet namespace,
                                     final Map<HierName,IdleState> result) {
        final Map<HierName,Integer> dir = (Map<HierName,Integer>)
            getTopLevelDirective(cell, DirectiveConstants.IDLE_STATE,
                                 DirectiveConstants.NODE_TYPE);
        getIdleState(prefix, namespace, dir, result);
    }

    /**
     * Return idle_state directives as a map from canonical name to IdleState
     * enum.
     **/
    public static Map<HierName,IdleState> getIdleState(
            final CellInterface cell,
            final AliasedSet namespace) {
        final Map<HierName,IdleState> result =
            new HashMap<HierName,IdleState>();

        for (Iterator i = cell.getPortSubcellPairs(); i.hasNext(); ) {
            final Pair<HierName,CellInterface> p =
                (Pair<HierName,CellInterface>) i.next();
            getIdleState(p.getFirst(), p.getSecond(), namespace, result);
        }

        final Map<HierName,Integer> topDirs = (Map<HierName,Integer>)
            getTopLevelDirective(cell, DirectiveConstants.IDLE_STATE,
                                 DirectiveConstants.NODE_TYPE);
        final Map<HierName,Integer> prsDirs = (Map<HierName,Integer>)
            getPrsDirective(cell, DirectiveConstants.IDLE_STATE,
                            DirectiveConstants.NODE_TYPE);
        getIdleState(null, namespace, topDirs, result);
        getIdleState(null, namespace, prsDirs, result);

        return result;
    }
}
