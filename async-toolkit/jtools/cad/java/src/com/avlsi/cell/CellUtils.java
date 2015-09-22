/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

import java.io.IOException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.ports.*;
import com.avlsi.fast.metaparameters.*;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.NaturalStringComparator;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.text.StringUtil;

/**
 * Some useful utilities that doesn't really belong in CellInterface.
 **/
public final class CellUtils {
    /**
     * This class should not be instantiated.
     **/
    private CellUtils() { }

    public static boolean isWiring(final CellInterface cell) {
        if (cell.isNode() || cell.isChannel()) return true;

        if (cell.containsNetlist() || cell.containsVerilog()) return false;

        if (cell.containsCompletePrs() || cell.containsCompleteSubcells()) {
            if (cell.hasRealProductionRule()) return false;

            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (!isWiring(subcell)) return false;
            }
            return true;
        } else {
            return !cell.containsRunnableCsp();
        }
    }

    public static boolean isLeaf(final CellInterface cell) {
        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair pair = (Pair) i.next();
            final CellInterface subcell = (CellInterface) pair.getSecond();
            if (!isWiring(subcell)) return false;
        }
        return true;
    }

    public static boolean isSplittable(final CellInterface cell) {
        final Boolean splittable = (Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.SPLITTABLE);
        return splittable.booleanValue();
    }

    public static boolean isFixed(final CellInterface cell) {   
        return ((Boolean) DirectiveUtils.getTopLevelDirective(cell,
                    DirectiveConstants.FIXED_SIZE)).booleanValue();
    }

    public static boolean isFixedSize(final CellInterface cell) {   
        return isFixed(cell) && cell.containsNetlist();
    }
    
    public static Boolean isRouted(final CellInterface cell,
                                   final Boolean def) {
        final Boolean routed = (Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.ROUTED);
        return routed == null ? def : routed;
    }

    public static boolean isRouted(final CellInterface cell) {
        final Boolean routed = isRouted(cell, null);
        return routed == null ? isLeaf(cell) : routed.booleanValue();
    }

    public static boolean hasRouted(final CellInterface cell) {
        if (isRouted(cell, false)) {
            return true;
        }

        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair pair = (Pair) i.next();
            final CellInterface subcell = (CellInterface) pair.getSecond();
            if (hasRouted(subcell)) return true;
        }
        return false;
    }

    public static boolean isAstaBlackbox(final CellInterface cell) {
        Boolean bbox = (Boolean) DirectiveUtils.getTopLevelDirective(cell,
                DirectiveConstants.ASTA_BLACKBOX);
        return bbox == null ? false : bbox;
    }

    public static boolean isAstaGraybox(final CellInterface cell) {
        Boolean gbox = (Boolean) DirectiveUtils.getTopLevelDirective(cell,
                DirectiveConstants.ASTA_GRAYBOX);
        return gbox == null ? false : gbox;
    }

    public static boolean isDftChannel(final String type) {
        return type.equals("lib.serial.scan.ChanDft") ||
               type.equals("deprecated.lib.serial.scan.ChanDft") ||
               type.equals("lib.dft.channel.ChanDft");
    }

    public static boolean isDftChannel(final CellInterface cell,
                                       final String type) {
        return isDftChannel(type) &&
               !((Boolean) DirectiveUtils.getCspDirective(
                   cell, DirectiveConstants.DISABLE_CHANDFT_HANDLER)).booleanValue();
    }

    public static boolean isSramSerialChannel(final String type) {
        return type.equals("lib.sram.6T.channel.SramSerialChannel") ||
               type.equals("deprecated.lib.6T.channel.SramSerialChannel");
    }

    public static final String[] OLD_DFT_NODES =
        new String[] { "e", "d[0]", "d[1]", "d[2]" };

    public static final String[] NEW_DFT_NODES =
        new String[] { "C[0]", "C[1]", "C[2]", "C[3]",
                       "D.e", "D.d[0]", "D.d[1]" };

    public static CellInterface loadCell(final String castPath,
                                         final String cellName,
                                         final String castVersion)
        throws CastSyntaxException, CastSemanticException, IOException {
        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castPath), castVersion);
        return castParser.getFullyQualifiedCell(cellName);
    }

    /**
     * Checks if a cell refines directly from a cell that has the same name, up
     * to the last dot, i.e., returns true if A.B.C.D refines directly from
     * A.B.C.
     **/
    public static boolean isSubtype(final CellInterface cell) {
        final CellInterface parent = cell.getDirectRefinementParent();
        return cell.getModuleName().equals(parent.getFullyQualifiedType());
    }

    public static boolean isInternalEnv(final CellInterface cell) {
        return isLeaf(cell) && !cell.containsCompletePrs() &&
                               !cell.containsCompleteSubcells() &&
                               !cell.containsNetlist();
    }


    private static Integer FORWARD = new Integer(PortDefinition.FORWARD);
    private static Integer REVERSE = new Integer(PortDefinition.REVERSE);
    private static Integer BIDIRECTIONAL = new Integer(PortDefinition.BIDIRECTIONAL);

    /**
     * Return a map of all ports in the given cell in order (as specified in
     * CAST), and their directionality.  The names of the ports are Strings,
     * and if converted to HierNames, are compatible with names used by
     * Cadencize, which must be used to find all alias information.
     **/
    public static Map<String,Integer> markPorts(final CellInterface ci) {
        return markPorts(ci.getPortDefinitions());
    }

    public static Map<String,Integer> markPorts(
            final Iterator<PortDefinition> portDefinitions) {
        final Map<String,Integer> result = new LinkedHashMap<String,Integer>();
        (new MarkPort() {
            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                if (direction < 0) {
                    result.put(name, REVERSE);
                } else if (direction == 0) {
                    result.put(name, BIDIRECTIONAL);
                } else {
                    result.put(name, FORWARD);
                }
            }
        }).mark(portDefinitions);
        return result;
    }

    public static class MarkPort {
        public void mark(final CellInterface ci) {
            mark(ci.getPortDefinitions());
        }

        public void mark(final Iterator<PortDefinition> portDefinitions) {
            mark(portDefinitions, null);
        }

        public void mark(final Iterator<PortDefinition> portDefinitions,
                         final String prefix) {
            mark(portDefinitions, prefix, PortDefinition.FORWARD);
        }

        public void mark(final Iterator<PortDefinition> portDefinitions,
                         final String prefix, final int direction) {
            while (portDefinitions.hasNext()) {
                final PortDefinition p = portDefinitions.next();
                mark(p, prefix, direction);
            }
        }

        protected void mark(final PortDefinition p, final String prefix,
                            int direction) {
            assert p.getDirection() == PortDefinition.FORWARD ||
                   p.getDirection() == PortDefinition.REVERSE ||
                   p.getDirection() == PortDefinition.BIDIRECTIONAL ||
                   p.getDirection() == PortDefinition.NONE;

            // XXX: Use PortDefinition.getReverseDirection() instead
            if (p.getDirection() == PortDefinition.REVERSE)
                direction = -direction;
            else if (p.getDirection() == PortDefinition.BIDIRECTIONAL ||
                     p.getDirection() == PortDefinition.NONE)
                direction = 0;

            final String name;
            if (prefix == null)
                name = p.getName();
            else
                name = prefix + '.' + p.getName();

            mark(p.getType(), name, direction, false);
        }

        // ChannelType ------------------------------------------------------
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            final String newName =
                arrayName(channelType, name, inArray, channelType.isArrayed());
            if (channelType.isArrayed()) {
                final int width = channelType.getWidth();
                assert width > 0 : "Invalid width: " + width + " ChannelType = " + channelType;
                mark(channelType, newName, direction, width);
            } else {
                mark(channelType, newName, direction);
            }
        }

        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            mark(channelType.iterator(), name, direction);
        }

        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                mark(channelType, name + i + "]", direction);
            }
        }

        // StructureType ------------------------------------------------------
        protected void mark(final StructureType structureType,
                            final String name, final int direction,
                            final boolean inArray) {
            mark(structureType, arrayName(structureType, name, inArray, false),
                 direction);
        }

        protected void mark(final StructureType structureType,
                            final String name, final int direction) {
            mark(structureType.iterator(), name, direction);
        }
        
        // ArrayType ------------------------------------------------------
        protected void mark(final ArrayType arrayType, final String name,
                            final int direction, final boolean inArray) {
            mark(arrayType, arrayName(arrayType, name, inArray), direction);
        }

        protected void mark(final ArrayType arrayType, final String name,
                            final int direction) {
            final int min = arrayType.getMinIndex();
            final int max = arrayType.getMaxIndex();

            final PortTypeInterface arrayedType = arrayType.getArrayedType();
            for (int i = min; i <= max; ++i) {
                mark(arrayedType, name + i, direction, true);
            }
        }

        // NodeType ------------------------------------------------------
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            final String newName =
                arrayName(nodeType, name, inArray, nodeType.isArrayed());
            if (nodeType.isArrayed()) {
                final int width = nodeType.getWidth();
                assert width > 0 : "Invalid width: " + width + " NodeType = " + nodeType;
                mark(nodeType, newName, direction, width);
            } else {
                mark(nodeType, newName, direction);
            }
        }

        protected void mark(final NodeType nodeType, final String name,
                            final int direction) {
            /* Default does nothing */
        }

        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                mark(nodeType, name + i + "]", direction);
            }
        }

        protected String arrayName(final PortTypeInterface t,
                                   final String name,
                                   final boolean inArray,
                                   final boolean isArrayed) {
            return isArrayed ? name + (inArray ? "," : "[")
                             : inArray ? name + "]" : name;
        }

        protected String arrayName(final ArrayType t,
                                   final String name,
                                   final boolean inArray) {
            return name + (inArray ? "," : "[");
        }

        protected void mark(final PortTypeInterface t, final String name,
                            final int direction, final boolean inArray) {

            if (t instanceof ChannelType) {
                mark((ChannelType) t, name, direction, inArray);
            } else if (t instanceof StructureType) {
                mark((StructureType) t, name, direction, inArray);
            } else if (t instanceof ArrayType) {
                mark((ArrayType) t, name, direction, inArray);
            } else if (t instanceof NodeType) {
                mark((NodeType) t, name, direction, inArray);
            } else {
                throw new AssertionError("Unknown PortTypeInterface: " + t);
            }
        }
    }

    /**
     * Given a cell, return a new cell containing the given cell, and the
     * specified environment cell, with the ports between them correctly
     * connected.  For example,
     * <pre>
     * define A()(e1of1 +a; node -b) {
     *   ...
     *   env {
     *     digital {
     *       ...
     *     }
     *   }
     * }
     * </pre>
     * <code>getEnvWithCell(A, "digital", "newA", "myEnv", "myA")</code>
     * would return a cell that looks something like:
     * <pre>
     * define newA()() {
     *   subcells {
     *     myA A;
     *     myEnv A:digital;  // In CAST, you can't refer to the digital env
     *     myA.a.d = myEnv.a.d;
     *     myA.a.e = myEnv.a.e;
     *     myA.b = myEnv.b;
     *   }
     * }
     * </pre>
     *
     * @param cell Cell to process
     * @param envName Name of the environment
     * @param newName Name of the newly constructed cell
     * @param envInstance Name of the environment instance in the new cell
     * @param cellInstance Name of the <code>cell</code> instance in the new
     * cell
     *
     * @return a cell containing an instance of <code>cell</code>, and an
     * instance of the environment cell with name <code>envName</code>.
     **/
    public static CellImpl getEnvWithCell(final CellInterface cell,
                                          final String envName,
                                          final String newName,
                                          final String envInstance,
                                          final String cellInstance)
        throws NoSuchEnvironmentException {
        try {
            return getEnvWithCell(cell, envName, newName,
                                  HierName.makeHierName(envInstance, '.'),
                                  HierName.makeHierName(cellInstance, '.'));
        } catch (InvalidHierNameException e) {
            throw new AssertionError("Cannot construct HierName");
        }
    }

    public static CellImpl getEnvWithCell(final CellInterface cell,
                                          final String envName,
                                          final String newName,
                                          final HierName envInstance,
                                          final HierName cellInstance)
        throws NoSuchEnvironmentException {
        final CellInterface env = cell.getEnvironment(envName);
        final CellImpl result = new CellImpl(newName, cell.getModuleName(),
                                             CellImpl.SYNTHETIC_CELL);
        result.setHasCompleteSubcellsBlock();
        result.addSubcellPair(envInstance, env, false);
        result.addSubcellPair(cellInstance, cell, false);

        final Map ports = markPorts(cell);
        for (Iterator i = ports.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String s = (String) entry.getKey();
            final HierName port;
            try {
                port = HierName.makeHierName(s, '.');
            } catch (InvalidHierNameException e) {
                throw new AssertionError("Cannot construct HierName: " + s);
            }
            result.addConnection(HierName.append(envInstance, port),
                                 HierName.append(cellInstance, port));
        }
        return result;
    }

    /**
     * Given a cell type, returns a type that does not contain any
     * meta-parameters.
     *
     * @param type Cell type to process.
     * @return If <code>type</code> has metaparameters, return the base type
     * without metaparameters; otherwise return <code>type</code>.
     **/
    public static String getBaseType(final String type) {
        if (type.endsWith(")")) {
            final int lparen = type.lastIndexOf('(');
            if (lparen > 0) {
                return type.substring(0, lparen);
            }
        }
        return type;
    }

    /**
     * Calculate the hash of a string using a given digest algorithm, and
     * return the hash as a hex string.
     **/
    private static String hash(final String s, final String algorithm)
        throws NoSuchAlgorithmException {
        final MessageDigest hash = MessageDigest.getInstance(algorithm);
        return NumberFormatter.toHexString(hash.digest(s.getBytes()));
    }

    /**
     * Return the MD5 hash of a string.
     **/
    private static String md5(final String s) {
        try {
            return hash(s, "MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * If a cell name has a meta parameter list (the string in between the
     * first "(" and the last ")") that's longer than the specified number of
     * characters, replace it with a MD5 sum.
     **/
    public static String hashMetaParameters(final String name, 
                                             final int length) {
        final int first = name.indexOf('(');
        final int last = name.lastIndexOf(')');
        if (first < 0 || last < 0 || last - first < length) return name;
        final String byteString = md5(name.substring(first + 1, last));
        return name.substring(0, first) +
               "_" + byteString +
               name.substring(last + 1);
    }

    public static String hashMetaParameters(final String name) {
        return hashMetaParameters(name, 128);
    }

    /**
     * Regular expression for extracting the transistor type from a process
     * indepedent transistor name.
     **/
    private final static Pattern TRANSISTOR_TYPE_PATTERN =
        Pattern.compile("([NP])\\[(\\d+)\\]");

    /**
     * Given a transistor model name, attempt to extract the type.  A valid
     * model name matches the regular expression "[NP]\[\d+\]".
     *
     * @param type name of the transistor model
     * @return type of the transistor, as an Integer.  Or null if it is not a
     * valid model name.
     **/
    public static Integer extractTransistorNumber(final String type) {
        final Matcher m = TRANSISTOR_TYPE_PATTERN.matcher(type);
        return m.matches() ?
            new Integer((m.group(1).equals("N") ? "-" : "") + m.group(2)) :
            null;
    }

    /**
     * Given a process independent transistor model name, return a process
     * dependent transistor model name.  It extracts the transistor type from
     * the model name, and looks up the type in the translation table defined
     * by the <code>transistor_name</code> directive.
     *
     * @param cell cell where the transistor is located in
     * @param model process independent model name
     * @return process dependent model name if a valid translation exists, or
     * <code>model</code>.
     **/
    public static String getTransistorModelName(final CellInterface cell,
                                                final String model) {
        final Map names = DirectiveUtils.getTopLevelDirective(cell,
                DirectiveConstants.TRANSISTOR_NAME,
                DirectiveConstants.INT_TYPE);
        return getTransistorModelName(names, model);
    }

    /**
     * Given a process independent transistor names, and a translation table,
     * return the process dependent transistor name.
     *
     * @param names mapping from process independent transistor type to process
     * dependent name
     * @param model process indepedent transistor name
     * @return process dependent transistor name if a mapping exists in
     * <code>names</code>, otherwise return <code>model</code>.
     **/
    public static String getTransistorModelName(final Map names,
                                                final String model) {
        final Integer type = extractTransistorNumber(model);
        final String name = type == null ? null : (String) names.get(type);
        return name == null ? model : name;
    }

    /**
     * Returns N from a string of the form standard.channel.e1ofN, or
     * standard.channel.e1of(N).
     *
     * @return the number of data rails, or -1 if the channel type does not
     * begin with standard.channel.e1of.
     * @throws NumberFormatException if N cannot be parsed as an integer
     **/
    public static int extractN(final String channelType) {
        final String e1of = "standard.channel.e1of";
        if (!channelType.startsWith(e1of)) return -1;
        String remainder = channelType.substring(e1of.length());
        remainder = remainder.replace('(', ' ');
        remainder = remainder.replace(')', ' ');
        remainder = remainder.trim();

        return Integer.parseInt(remainder);
    }

    private static HierName toHier(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot create HierName from " + s, e);
        }
    }

    private static HierName toHierOrNull(final String s) {
        return s == null ? null : toHier(s);
    }

    public static class Channel implements Comparable<Channel> {
        private final HierName instance;
        private final String name;
        private final PortTypeInterface type;
        private final Channel parent;
        private final int index;
        private final int group;
        private final int direction;
        private final List<Channel> children;
        public Channel(final HierName instance, final String name,
                       final Channel parent, final PortTypeInterface type,
                       final int index, final int group, final int direction) {
            this.instance = instance;
            this.name = name;
            this.type = type;
            this.parent = parent;
            if (parent != null) parent.addChildren(this);
            this.index = index;
            this.group = group;
            this.direction = direction;
            this.children = new ArrayList<>();
        }
        public Channel(final HierName instance, final String name,
                       final Channel parent, final PortTypeInterface type,
                       final int index, final int direction) {
            this(instance, name, parent, type, index, -1, direction);
        }
        public HierName getInstance() {
            return instance;
        }
        public String getName() {
            return name;
        }
        public String getFullName() {
            return (instance == null ? "" : instance + ".") + name;
        }
        public PortTypeInterface getType() {
            return type;
        }
        public int getDirection() {
            return direction;
        }
        public Channel getParent() {
            return parent;
        }
        public int getIndex() {
            return index;
        }
        public int getGroup() {
            return group;
        }
        private void addChildren(final Channel child) {
            children.add(child);
        }
        public Collection<Channel> getChildren() {
            return Collections.unmodifiableList(children);
        }
        private int realDirection() {
            return getInstance() == null ? -getDirection() : getDirection();
        }
        public boolean isInput() {
            return realDirection() < 0;
        }
        public boolean isOutput() {
            return realDirection() > 0;
        }
        public boolean isBidirectional() {
            return realDirection() == 0;
        }
        public int compareTo(final Channel o) {
            int x = ObjectUtils.compare(getInstance(), o.getInstance());
            return x == 0 ? ObjectUtils.compare(getName(), o.getName()) : x;
        }
        public boolean equals(final Object other) {
            if (other instanceof Channel) {
                final Channel o = (Channel) other;
                return ObjectUtils.equals(getInstance(), o.getInstance()) &&
                       getName().equals(o.getName());
            } else {
                return false;
            }
        }
        public int hashCode() {
            return ObjectUtils.hashCode(getInstance()) + getName().hashCode();
        }
        public String toString() {
            return "instance: " + instance + " name: " + name + " type: " + type + " parent: [" + parent + "] index: " + index + " direction: " + direction + " group: " + group;
        }
    }

    public static class ChannelCreator extends MarkPort {
        private static ChannelType deArray(final ChannelType t) {
            return t.isArrayed() ?  new ChannelType(t.iterator(),
                                                    t.getTypeName(),
                                                    t.getNumValues())
                                 : t;
        }

        private static NodeType deArray(final NodeType t) {
            return t.isArrayed() ? new NodeType() : t;
        }

        protected Channel wide, narrow, node;
        private int nodeIndex;
        protected final HierName instance;
        private Channel result;
        public ChannelCreator(final HierName instance) {
            this.instance = instance;
            this.wide = null;
            this.narrow = null;
            this.nodeIndex = -1;
            this.result = null;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            if (channelType.isArrayed()) {
                wide = 
                    new Channel(instance,
                                arrayName(channelType, name, inArray, false),
                                null, channelType, -1, direction);
                setResult(wide);
            }
            super.mark(channelType, name, direction, inArray);
            wide = null;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            if (narrow == null) {
                narrow = new Channel(instance, name, wide,
                                     deArray(channelType), -1, direction);
                setResult(narrow);
            }
            nodeIndex = 0;
            for (Iterator i = channelType.iterator(); i.hasNext(); ++nodeIndex)
            {
                final PortDefinition p = (PortDefinition) i.next();
                mark(p, name, direction);
            }
            nodeIndex = -1;
            narrow = null;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                narrow = new Channel(instance, name + i + "]", wide,
                                     deArray(channelType), i, direction);
                setResult(narrow);
                mark(channelType, name + i + "]", direction);
            }
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            final boolean alone = narrow == null && nodeType.isArrayed();
            if (alone) {
                narrow = 
                    new Channel(instance,
                                arrayName(nodeType, name, inArray, false),
                                null, nodeType, -1, direction);
                setResult(narrow);
                nodeIndex = 0;
            }
            super.mark(nodeType, name, direction, inArray);
            if (alone) {
                narrow = null;
                nodeIndex = -1;
            }
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                mark(nodeType, name + i + "]", direction);
                if (nodeIndex >= 0) ++nodeIndex;
            }
            if (nodeIndex >= 0) --nodeIndex;
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction) {
            final int group = narrow == null ? -1
                                             : name.endsWith(".e") ? 0
                                                                   : 1;
            node = new Channel(instance, name, narrow, deArray(nodeType),
                               nodeIndex, group, direction);
            setResult(node);
        }
        protected void setResult(final Channel c) {
            if (result == null) result = c;
        }
        public Channel getResult() {
            return result;
        }
    }

    private static class ChannelConnection extends MarkPort {
        private static ChannelType deArray(final ChannelType t) {
            return t.isArrayed() ?  new ChannelType(t.iterator(),
                                                    t.getTypeName(),
                                                    t.getNumValues())
                                 : t;
        }

        private static NodeType deArray(final NodeType t) {
            return t.isArrayed() ? new NodeType() : t;
        }

        private Channel wide, narrow;
        private boolean realNarrow;
        private int nodeIndex;
        private final MultiMap nodeChannel;
        private final HierName instance;
        private final AliasedSet aliases;
        public ChannelConnection(final MultiMap nodeChannel,
                                 final HierName instance,
                                 final AliasedSet aliases) {
            this.nodeChannel = nodeChannel;
            this.instance = instance;
            this.wide = null;
            this.narrow = null;
            this.realNarrow = false;
            this.nodeIndex = -1;
            this.aliases = aliases;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final boolean inArray) {
            if (channelType.isArrayed())
                wide = 
                    new Channel(instance,
                                arrayName(channelType, name, inArray, false),
                                null, channelType, -1, direction);
            super.mark(channelType, name, direction, inArray);
            wide = null;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            if (narrow == null)
                narrow = new Channel(instance, name, wide,
                                     deArray(channelType), -1, direction);
            realNarrow = true;
            nodeIndex = 0;
            for (Iterator i = channelType.iterator(); i.hasNext(); ++nodeIndex)
            {
                final PortDefinition p = (PortDefinition) i.next();
                mark(p, name, direction);
            }
            nodeIndex = -1;
            narrow = null;
            realNarrow = false;
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                narrow = new Channel(instance, name + i + "]", wide,
                                     deArray(channelType), i, direction);
                mark(channelType, name + i + "]", direction);
            }
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final boolean inArray) {
            final boolean alone = narrow == null && nodeType.isArrayed();
            if (alone) {
                narrow = 
                    new Channel(instance,
                                arrayName(nodeType, name, inArray, false),
                                null, nodeType, -1, direction);
                nodeIndex = 0;
            }
            super.mark(nodeType, name, direction, inArray);
            if (alone) {
                narrow = null;
                nodeIndex = -1;
            }
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final int width) {
            for (int i = 0; i < width; ++i) {
                mark(nodeType, name + i + "]", direction);
                if (nodeIndex >= 0) ++nodeIndex;
            }
            if (nodeIndex >= 0) --nodeIndex;
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction) {
            final int group = realNarrow ? name.endsWith(".e") ? 0 : 1
                                         : -1;
            final Channel node =
                new Channel(instance, name, narrow, deArray(nodeType),
                            nodeIndex, group, direction);

            final HierName hier = HierName.prefixName(instance, toHier(name));
            final HierName canon = (HierName) aliases.getCanonicalKey(hier);
            assert canon != null : "Cannot find canonical name for " + hier;
            nodeChannel.put(canon, node);
        }
    }

    @FunctionalInterface
    public interface ChannelConnectionPredicate {
        boolean test(Channel out, Channel in,
                     AliasedSet/*<Channel>*/ connections);
    }

    private static void groupChannel(
        final AliasedSet/*<Channel>*/ connections,
        final Collection<Pair<Channel,Channel>> parts,
        final AliasedSet/*<Channel>*/ whole,
        final ChannelConnectionPredicate isConnected,
        final boolean useGroup) {

        for (Iterator i = connections.getCanonicalKeys(); i.hasNext(); ) {
            final Collection<Channel> input = new HashSet<>();
            final Collection<Channel> output = new HashSet<>();
            final Collection<Channel> inout = new HashSet<>();
            final Collection<Channel> childInput = new ArrayList<>();
            final Collection<Channel> childOutput = new ArrayList<>();
            final Collection<Channel> childInout = new ArrayList<>();
            final Object canon = i.next();
            for (Iterator j = connections.getAliases(canon); j.hasNext(); ) {
                final Channel chan = (Channel) j.next();
                (chan.isInput() ? childInput :
                    (chan.isOutput() ? childOutput : childInout)).add(chan);
                final Channel parent = chan.getParent();
                if (isChannelType(parent)) {
                    (parent.isInput() ? input :
                        (parent.isOutput() ? output : inout)).add(parent);
                }
            }

            // if there are inout channels, arbitrarily choose one as the
            // source, and make the rest into sinks
            for (Iterator j = childInout.iterator(); j.hasNext(); ) {
                final Channel chan = (Channel) j.next();
                final Collection<Channel> coll;
                if (childOutput.isEmpty()) {
                    childOutput.add(chan);
                    coll = output;
                } else {
                    childInput.add(chan);
                    coll = input;
                }

                final Channel parent = chan.getParent();
                if (isChannelType(parent) && parent.isBidirectional()) {
                    coll.add(parent);
                }
            }

            for (Iterator j = output.iterator(); j.hasNext(); ) {
                final Channel out = (Channel) j.next();
                for (Iterator k = input.iterator(); k.hasNext(); ) {
                    final Channel in = (Channel) k.next();

                    if (whole.areEquivalent(out, in)) {
                        continue;
                    }

                    boolean connected = true;

                    if (isConnected != null) {
                        connected = isConnected.test(out, in, connections);
                    } else if (out.getChildren().size() != in.getChildren().size()) {
                        connected = false;
                    } else if (useGroup) {
                        final Set<Channel> outChildren =
                            new HashSet<Channel>(out.getChildren());
                        for (Channel inChild : in.getChildren()) {
                            boolean found = false;
                            for (Iterator<Channel> l = outChildren.iterator();
                                 l.hasNext(); ) {
                                final Channel outChild = l.next();
                                if (inChild.getGroup() == outChild.getGroup() &&
                                    connections.areEquivalent(inChild,
                                                              outChild)) {
                                    found = true;
                                    l.remove();
                                    break;
                                }
                            }
                            if (!found) {
                                connected = false;
                                break;
                            }
                        }
                    } else {
                        final Iterator<Channel> outChildren =
                            out.getChildren().iterator();
                        final Iterator<Channel> inChildren =
                            in.getChildren().iterator();

                        while (outChildren.hasNext() &&
                               inChildren.hasNext() && connected) {
                            final Channel outChild = outChildren.next();
                            final Channel inChild = inChildren.next();
                            connected =
                                connections.areEquivalent(outChild, inChild);
                        }
                    }

                    if (connected) {
                        whole.makeEquivalent(out, in);
                    }
                }
            }

            if (parts != null) {
                for (Iterator j = childOutput.iterator(); j.hasNext(); ) {
                    final Channel out = (Channel) j.next();
                    final Channel outParent = out.getParent();
                    for (Iterator k = childInput.iterator(); k.hasNext(); ) {
                        final Channel in = (Channel) k.next();
                        final Channel inParent = in.getParent();
                        if (outParent == null || inParent == null || 
                            !whole.areEquivalent(outParent, inParent)) {
                            parts.add(new Pair<Channel,Channel>(out, in));
                        }
                    }
                }
            }
        }
    }

    public static Set getPortChannels(final CellInterface cell,
                                      final boolean flatten,
                                      final int parentLevels) {
        return getPortChannels(cell, new Cadencize(false, true), flatten,
                               parentLevels);
    }

    public static Set getPortChannels(final CellInterface cell,
                                      final Cadencize cad,
                                      final boolean flatten,
                                      final int parentLevels) {
        final MultiMap nodeChannel = new MultiMap();
        final AliasedSet locals =
            cad.convertChannels(cell, Collections.EMPTY_MAP);
        if (flatten) {
            final AliasedMap ports = cad.convert(cell).getPortNodes();
            (new ChannelConnection(nodeChannel, null, locals))
                .mark(new FilteringIterator(
                    flattenPortDefinitionsNodes(cell.getPortDefinitions()),
                    new PrsFilterNodes(ports)));
        } else {
            (new ChannelConnection(nodeChannel, null, locals)).mark(cell);
        }

        final Set results = new HashSet();
        for (Iterator i = nodeChannel.keySet().iterator(); i.hasNext(); ) {
            for (Iterator j = nodeChannel.get(i.next()).iterator();
                 j.hasNext(); ) {
                Channel channel = (Channel) j.next();
                int level = 0;
                for (int k = 0; k < parentLevels; ++k) {
                    final Channel parent = channel.getParent();
                    if (parent == null) break;
                    else channel = parent;
                }
                results.add(channel);
            }
        }

        return results;
    }

    /**
     * Convert a subcell representing a channel into a port definition.
     **/
    private static PortDefinition convertChannel(final CellInterface cell,
                                                 final HierName instance,
                                                 final int direction) {
        final PortTypeInterface portType;

        if (cell.isNode()) {
            portType = new NodeType();
        } else {
            final String moduleName = cell.getModuleName();
            final String typeName = cell.getType();
            // XXX: same code appears in CastTwoTree.g; should refactor
            if (isAsyncChannel(cell)) {
                portType = new ChannelType(cell.getPortDefinitions(),
                                           cell.getFullyQualifiedType(),
                                           CellUtils.getNumValues(cell));
            } else {
                portType = new StructureType(cell.getPortDefinitions(),
                                             cell.getFullyQualifiedType());
            }
        }

        return new PortDefinition(instance.getAsString('.'), portType,
                                  direction);
    }

    public static void getChannelConnection(final CellInterface cell,
                                            final Collection nodeConnections,
                                            final Collection narrowConnections,
                                            final Collection wideConnections,
                                            final Set nodeConverter,
                                            final Set narrowConverter,
                                            final Map flattenNodes) {
        getChannelConnection(cell, nodeConnections, narrowConnections,
                             wideConnections,
                             new AliasedSet(new NaturalOrderComparator()),
                             new AliasedSet(new NaturalOrderComparator()),
                             new AliasedSet(new NaturalOrderComparator()),
                             nodeConverter, narrowConverter, flattenNodes,
                             false);
    }

    private static boolean isChannelType(final Channel chan) {
        return chan != null && chan.getType() instanceof ChannelType;
    }

    private static class PrsFilterNodes implements UnaryPredicate {
        // filter out ports that are not used
        private final Set seen;
        private final AliasedMap ports;
        public PrsFilterNodes(final AliasedMap ports) {
            this.seen = new HashSet();
            this.ports = ports;
        }
        public boolean evaluate(final Object o) {
            final PortDefinition port = (PortDefinition) o;
            final HierName hport = toHier(port.getName());
            final Boolean used = (Boolean) ports.getValue(hport);
            return used.booleanValue() &&
                   seen.add(new Pair(ports.getCanonicalKey(hport),
                                     new Integer(port.getDirection())));
        }
    }

    public static AliasedSet
    getChannelConnection(final CellInterface cell,
                         final Collection nodeConnections,
                         final Collection narrowConnections,
                         final Collection wideConnections,
                         final AliasedSet nodeAliases,
                         final AliasedSet narrowAliases,
                         final AliasedSet wideAliases,
                         final Set nodeConverter,
                         final Set narrowConverter,
                         final Map flattenNodes,
                         final boolean internal) {
        return getChannelConnection(cell, nodeConnections, narrowConnections,
                wideConnections, nodeAliases, narrowAliases, wideAliases,
                nodeConverter, narrowConverter, flattenNodes, internal, false);
    }

    public static AliasedSet
    getChannelConnection(final CellInterface cell,
                         final Collection nodeConnections,
                         final Collection narrowConnections,
                         final Collection wideConnections,
                         final AliasedSet nodeAliases,
                         final AliasedSet narrowAliases,
                         final AliasedSet wideAliases,
                         final Set nodeConverter,
                         final Set narrowConverter,
                         final Map flattenNodes,
                         final boolean internal,
                         final boolean allowTwist) {
        return getChannelConnection(cell, nodeConnections, narrowConnections,
                wideConnections, nodeAliases, narrowAliases, wideAliases,
                nodeConverter, narrowConverter, flattenNodes, internal,
                allowTwist, new Cadencize(false), null);
    }

    public static AliasedSet
    getChannelConnection(final CellInterface cell,
                         final Collection nodeConnections,
                         final Collection narrowConnections,
                         final Collection wideConnections,
                         final AliasedSet nodeAliases,
                         final AliasedSet narrowAliases,
                         final AliasedSet wideAliases,
                         final Set nodeConverter,
                         final Set narrowConverter,
                         final Map flattenNodes,
                         final boolean internal,
                         final boolean allowTwist,
                         final Cadencize cad,
                         // a "verilog aware" Cadencize
                         Cadencize vcad) {
        return getChannelConnection(cell, nodeConnections, narrowConnections,
                wideConnections, nodeAliases, narrowAliases, wideAliases,
                null, null, null,
                nodeConverter, narrowConverter, flattenNodes, internal,
                allowTwist, cad, vcad);
    }

    public static <T> AliasedSet getChannelConnection(
            final CellInterface cell,
            final Collection<Pair<Channel,Channel>> nodeConnections,
            final Collection<Pair<Channel,Channel>> narrowConnections,
            final Collection<Pair<Channel,Channel>> wideConnections,
            final AliasedSet nodeAliases,
            final AliasedSet narrowAliases,
            final AliasedSet wideAliases,
            final ChannelConnectionPredicate nodeConnected,
            final ChannelConnectionPredicate narrowConnected,
            final ChannelConnectionPredicate wideConnected,
            final Set nodeConverter,
            final Set narrowConverter,
            final Map flattenNodes,
            final boolean internal,
            final boolean allowTwist,
            final Cadencize cad,
            // a "verilog aware" Cadencize
            Cadencize vcad) {
        final MultiMap nodeChannel = new MultiMap();
        final AliasedSet locals = cad.convertChannels(cell, flattenNodes);
        (new ChannelConnection(nodeChannel, null, locals)).mark(cell);

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            final HierName inst = (HierName) p.getFirst();
            if (subcell.isNode() || subcell.isChannel()) {
                if (internal)
                    (new ChannelConnection(nodeChannel, null, locals))
                        .mark(Collections.singletonList(
                                convertChannel(subcell, inst,
                                               PortDefinition.IN)).iterator());
            } else {
                if (flattenNodes.containsKey(inst)) {
                    final boolean prs = flattenNodes.get(inst) == Boolean.TRUE;
                    if (!prs || subcell.hasRealProductionRule()) {
                        // if verilog behavior, use "verilog aware" cadencize
                        final Cadencize realcad = prs ? cad :
                            (vcad == null ? (vcad = new Cadencize(false, true))
                                          : vcad);
                        final AliasedMap ports =
                            realcad.convert(subcell).getPortNodes();
                        (new ChannelConnection(nodeChannel, inst, locals))
                            .mark(new FilteringIterator(
                                flattenPortDefinitionsNodes(
                                    subcell.getPortDefinitions()),
                                new PrsFilterNodes(ports)));
                    }
                } else {
                    (new ChannelConnection(nodeChannel, inst, locals))
                        .mark(subcell);
                }
            }
        }

        for (Iterator i = nodeChannel.keySet().iterator(); i.hasNext(); ) {
            final Iterator j = nodeChannel.get(i.next()).iterator();
            final Object first = j.next();
            nodeAliases.add(first);
            while (j.hasNext()) {
                nodeAliases.makeEquivalent(first, j.next());
            }
        }

        groupChannel(nodeAliases, nodeConnections, narrowAliases,
                     nodeConnected, allowTwist);
        groupChannel(narrowAliases, narrowConnections, wideAliases,
                     narrowConnected, false);
        groupChannel(wideAliases, wideConnections, null, wideConnected, false);

        if (nodeConnections != null) {
            for (Iterator i = nodeConnections.iterator(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final Channel out = ((Channel) p.getFirst()).getParent();
                final Channel in = ((Channel) p.getSecond()).getParent();
                if (isChannelType(out)) nodeConverter.add(out);
                if (isChannelType(in)) nodeConverter.add(in);
            }
        }

        if (narrowConnections != null) {
            for (Iterator i = narrowConnections.iterator(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final Channel out = ((Channel) p.getFirst()).getParent();
                final Channel in = ((Channel) p.getSecond()).getParent();
                if (out != null) narrowConverter.add(out);
                if (in != null) narrowConverter.add(in);
            }
        }

        for (Iterator i = nodeConverter.iterator(); i.hasNext(); ) {
            final Channel chan = ((Channel) i.next()).getParent();
            if (chan != null) narrowConverter.add(chan);
        }

        return locals;
    }

    public static final int ARRAY_TYPE = 1;
    public static final int STRUCTURE_TYPE = 2;
    public static final int WIDE_CHANNEL_TYPE = 4;
    public static final int CHANNEL_TYPE = 8;
    public static final int WIDE_NODE_TYPE = 16;
    public static final int NODE_TYPE = 32;
    /**
     * Flatten and filter ports such that the kind of ports specified in the
     * <code>level</code> bitmap are the only kind of ports left in the result.
     *
     * TODO: Replace flattenPortDefinitions, and flattenPortDefinitionsNodes
     * with calls to this function
     **/
    public static Iterator<PortDefinition>
    flattenPorts(final Iterator ports, final int level) {
        return flattenPorts(ports, level, Collections.<String>emptySet());
    }

    public static Iterator<PortDefinition>
    flattenPorts(final Iterator ports, final int level,
                 final Set<String> keepStruct) {
        final Collection<PortDefinition> result =
            new ArrayList<PortDefinition>();
        (new MarkPort() {
            private ChannelType deArray(final ChannelType t) {
                return t.isArrayed() ?  new ChannelType(t.iterator(),
                                                        t.getTypeName(),
                                                        t.getNumValues())
                                     : t;
            }
            private NodeType deArray(final NodeType t) {
                return t.isArrayed() ? new NodeType() : t;
            }
            private int translate(final int direction) {
                if (direction < 0) {
                    return PortDefinition.REVERSE;
                } else if (direction == 0) {
                    return PortDefinition.BIDIRECTIONAL;
                } else {
                    return PortDefinition.FORWARD;
                }
            }
            private void finish(final String name, final PortTypeInterface t,
                                final int direction) {
                result.add(new PortDefinition(name, t, translate(direction)));
            }
            protected String arrayName(final PortTypeInterface t,
                                       final String name,
                                       final boolean inArray,
                                       final boolean isArrayed) {
                return super.arrayName(t, name, inArray, isArrayed &&
                    ((t instanceof ChannelType &&
                      (level & WIDE_CHANNEL_TYPE) == 0) ||
                     (t instanceof NodeType &&
                      (level & WIDE_NODE_TYPE) == 0)));
            }
            protected void mark(final StructureType structureType,
                                final String name, final int direction) {
                if ((level & STRUCTURE_TYPE) == 0 &&
                    !keepStruct.contains(structureType.getTag())) {
                    super.mark(structureType, name, direction);
                } else {
                    finish(name, structureType, direction);
                }
            }
            protected void mark(final ArrayType arrayType, final String name,
                                final int direction) {
                if ((level & ARRAY_TYPE) == 0) {
                    super.mark(arrayType, name, direction);
                } else {
                    finish(name, arrayType, direction);
                }
            }
            protected void mark(final ChannelType channelType,
                                final String name, final int direction) {
                if ((level & CHANNEL_TYPE) == 0) {
                    super.mark(channelType, name, direction);
                } else {
                    finish(name, deArray(channelType), direction);
                }
            }
            protected void mark(final ChannelType channelType,
                                final String name, final int direction,
                                final int width) {
                if ((level & WIDE_CHANNEL_TYPE) == 0) {
                    super.mark(channelType, name, direction, width);
                } else {
                    finish(name, channelType, direction);
                }
            }
            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                if ((level & NODE_TYPE) == 0) {
                    super.mark(nodeType, name, direction);
                } else {
                    finish(name, deArray(nodeType), direction);
                }
            }
            protected void mark(final NodeType nodeType, final String name,
                                final int direction, final int width) {
                if ((level & WIDE_NODE_TYPE) == 0) {
                    super.mark(nodeType, name, direction, width);
                } else {
                    finish(name, nodeType, direction);
                }
            }
         }).mark(ports);
        return result.iterator();
    }

    public static class FlattenPortDefinitions extends MarkPort {
        protected final Collection result;
        public FlattenPortDefinitions(final Collection result) {
            this.result = result;
        }
        protected int translate(final int direction) {
            if (direction < 0) {
                return PortDefinition.REVERSE;
            } else if (direction == 0) {
                return PortDefinition.BIDIRECTIONAL;
            } else {
                return PortDefinition.FORWARD;
            }
        }
        protected String arrayName(final PortTypeInterface t,
                                   final String name,
                                   final boolean inArray,
                                   final boolean isArrayed) {
            return super.arrayName(t, name, inArray, false);
        }
        protected void mark(final ChannelType channelType,
                            final String name, final int direction) {
            result.add(new PortDefinition(name, channelType,
                                          translate(direction)));
        }
        protected void mark(final ChannelType channelType,
                            final String name, final int direction,
                            final int width) {
            result.add(new PortDefinition(name, channelType,
                                          translate(direction)));
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction) {
            result.add(new PortDefinition(name, nodeType,
                                          translate(direction)));
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final int width) {
            result.add(new PortDefinition(name, nodeType,
                                          translate(direction)));
        }
    }

    public static Iterator flattenPortDefinitionsNodes(final Iterator ports) {
        final Collection result = new ArrayList();
        (new MarkPort() {
            private int translate(final int direction) {
                if (direction < 0) {
                    return PortDefinition.REVERSE;
                } else if (direction == 0) {
                    return PortDefinition.BIDIRECTIONAL;
                } else {
                    return PortDefinition.FORWARD;
                }
            }
            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                result.add(new PortDefinition(name, new NodeType(),
                                              translate(direction)));
            }
         }).mark(ports);
        return result.iterator();
    }

    /**
     * Check to see if the specified cell or if any of its refinement parents
     * satisfies the predicate <code>check</code>.  If the predicate is
     * satisfied by the specified cell, return <code>true</code>; otherwise,
     * recursively check the cell's refinement parents, and return whatever
     * value is returned by the recursive call.
     **/
    public static boolean matchRefinement(
            final CellInterface cell,
            final UnaryPredicate<CellInterface> check) {
        if (cell == null) return false;
        if (check.evaluate(cell)) return true;
        return matchRefinement(cell.getDirectRefinementParent(), check);
    }

    /**
     * Check to see if any of the inheritance parents of the specified cell
     * satisfies the predicate <code>check</code>.  If the predicate is
     * satisfied, return <code>true</code>; otherwise, return
     * <code>false</code>.
     **/
    public static boolean matchInheritance(
            final CellInterface cell,
            final UnaryPredicate<CellInterface> check) {
        return cell.getInheritedCells()
                   .anyMatch(attrib -> check.evaluate(attrib) ||
                                       matchInheritance(attrib, check));
    }

    /**
     * Return a <code>UnaryPredicate</code> object that when used to evaluate a
     * <code>CellInterface</code> object will return <code>true</code> if the
     * cell or any of its refinement or inheritance ancestors refines or
     * inherits a cell that satisfied the predicate <code>matcher</code>.
     **/
    public static UnaryPredicate<CellInterface> getTypeMatcher(
            final UnaryPredicate<CellInterface> matcher) {
        final UnaryPredicate<CellInterface> inheritanceCheck =
            c -> CellUtils.matchInheritance(c, matcher);
        return c -> CellUtils.matchRefinement(c, matcher) ||
                    CellUtils.matchRefinement(c, inheritanceCheck);
    }

    /**
     * Return a <code>UnaryPredicate</code> object that when used to evaluate a
     * <code>CellInterface</code> object will return <code>true</code> if the
     * cell or any of its refinement or inheritance ancestors refines or
     * inherits any of the cells listed in <code>candidates</code>, or
     * <code>false</code> otherwise.  The matching is done by type name, so
     * <code>candidates</code> should contain <code>String</code> objects.  If
     * any parameterization of a metaparameterized cell is to be matched, then
     * use just the base name of the cell, without the metaparameters.
     **/
    public static UnaryPredicate<CellInterface> getTypeMatcher(
            final Set<String> candidates) {
        return getTypeMatcher(
            c -> candidates.contains(c.getFullyQualifiedType()) ||
                 candidates.contains(
                         CellUtils.getBaseType(c.getFullyQualifiedType())));
    }

    public static UnaryPredicate<CellInterface> getTypeMatcher(
            final Collection<String> candidates) {
        return getTypeMatcher(new HashSet<String>(candidates));
    }


    public interface SizingEnvCreator {
        void getInternalEnv(final CellInterface cell,
                            final AliasedSet aliases,
                            final Map<HierName,CellInterface> subcells);
    }

    private static UnaryPredicate<CellInterface> INHERITS_LEAF =
        getTypeMatcher(Arrays.asList(
                    "standard.base.leaf",
                    "standard.process.imported_proteus_cell"));

    public static void verifySubcells(final CellInterface cell,
                                      final Cadencize cad,
                                      boolean checkE1ofN) {
        if (!cell.containsCompleteSubcells()) return;

        final CellInterface parent = cell.getDirectRefinementParent();
        if (parent != null && parent.containsCompleteSubcells()) {
            // presumably connections have already been verified in the parent
            return;
        }

        // don't check e1ofN connections instead a leaf cell, as fragments
        // often (harmlessly) violate this rule
        if (INHERITS_LEAF.evaluate(cell)) checkE1ofN = false;

        final AliasedSet aliases =
            cad.convertChannels(cell, Collections.EMPTY_MAP);

        final MultiMap<HierName,Channel> channels =
            new MultiMap<HierName,Channel>();

        new ChannelConnection(channels, null, aliases).mark(cell);

        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair<HierName,CellInterface> p =
                (Pair<HierName,CellInterface>) i.next();
            final CellInterface subcell = p.getSecond();
            if (subcell.isNode() || subcell.isChannel()) continue;
            new ChannelConnection(channels, p.getFirst(), aliases).mark(subcell);
        }

        TreeSet<String> errors = null;
        HashSet<HierName> e1ofNodes = null;
        for (HierName canon : channels.keySet()) {
            int in = 0, out = 0, group = 0, total = 0;;
            for (Channel c : channels.get(canon)) {
                if (c.isInput() || c.isBidirectional()) ++in;
                if (c.isOutput() || c.isBidirectional()) ++out;
                if (c.getGroup() == 0) ++group;
                ++total;
            }
            if (in != 0 && out == 0) {
                if (errors == null)
                    errors = new TreeSet<String>(
                                NaturalStringComparator.getInstance());
                errors.add(canon.toString());
            } else if (checkE1ofN && total == group && in != 1) {
                if (e1ofNodes == null) e1ofNodes = new HashSet<HierName>();
                e1ofNodes.add(canon);
            }
        }

        TreeSet<String> e1ofErrors = null;
        if (e1ofNodes != null) {
            channels.clear();
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair<HierName,CellInterface> p =
                    (Pair<HierName,CellInterface>) i.next();
                final CellInterface subcell = p.getSecond();
                if (subcell.isChannel()) {
                    new ChannelConnection(channels, p.getFirst(), aliases)
                        .mark(subcell);
                }
            }
            e1ofErrors =
                new TreeSet<String>(NaturalStringComparator.getInstance());
            for (HierName h : e1ofNodes) {
                final TreeSet<String> canon = new TreeSet<String>();
                for (Channel c : channels.get(h)) {
                    if (c.getGroup() >= 0) {
                        canon.add(c.getParent().getFullName());
                    } else {
                        canon.add(c.getInstance().toString());
                    }
                }
                e1ofErrors.add(canon.first());
            }
        }

        if (errors != null) {
            System.err.println(
                "WARNING: the following nets have sinks but no sources in " +
                cell.getFullyQualifiedType() + ":");
            for (String error : errors) {
                System.err.println("    " + error);
            }
        }
        if (e1ofErrors != null) {
            System.err.println(
                "WARNING: the following e1ofN channels are not " +
                "point-to-point in " + cell.getFullyQualifiedType() + ":");
            for (String error : e1ofErrors) {
                System.err.println("    " + error);
            }
        }
    }

    /**
     * Return the name of the implied port given the name the implied port
     * connects to in the instantiator, or <code>null</code> if such a port
     * does not exist.
     **/
    public static String getLocalImpliedName(final CellInterface ci,
                                             final String parentImpliedName) {
        for (Iterator i = ci.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition p = (PortDefinition) i.next();
            if (ci.isImpliedPort(p.getName())) {
                final String parent = ci.getParentImpliedPort(p.getName());
                if (parent.equals(parentImpliedName)) return p.getName();
            }
        }
        return null;
    }

    /**
     * Return the name of reset, or <code>null</code> if none found.  Reset is
     * either an implied node that would connect to _RESET in the parent, or an
     * implied node that would connect to _Reset in the parent.
     **/
    public static String getResetName(final CellInterface ci) {
        final String RESET = getLocalImpliedName(ci, "_RESET");
        final String Reset = getLocalImpliedName(ci, "_Reset");
        if (RESET != null) return RESET;
        else if (Reset != null) return Reset;
        else return null;
    }

    /**
     * Construct and return a leakage environment.  The environment aliases .e
     * of input e1ofN channels, and .C.0 of input ChanDft channels to reset.
     * Otherwise, it aliases all input nodes to ground.  If reset or ground
     * cannot be found, <code>null</code> is returned.
     **/
    public static CellInterface getLeakageEnv(final CellInterface parent) {
        final CellImpl result =
            new CellImpl(parent.getType() + "_leakageEnv",
                         parent.getModuleName(),
                         CellImpl.SYNTHETIC_CELL);

        result.setHasCompleteSubcellsBlock();

        for (Iterator i = parent.getPortSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            result.addSubcellPair((HierName) p.getFirst(),
                                  (CellInterface) p.getSecond(),
                                  true);
        }

        final HierName reset = toHierOrNull(CellUtils.getResetName(parent));
        final HierName gnd =
            toHierOrNull(CellUtils.getLocalImpliedName(parent, "GND"));

        for (Iterator i = parent.getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition pd = (PortDefinition) i.next();
            final String name = pd.getName();
            final int dir;
            if (parent.isImpliedPort(name)) {
                final String inParent = parent.getParentImpliedPort(name);
                result.addImpliedPortMapping(name, inParent);
                dir = pd.getDirection();
            } else {
                dir = pd.getReverseDirection();
            }
            result.addPortDefinition(new PortDefinition(name,
                                                        pd.getType(),
                                                        dir));
        }

        for (Iterator i = parent.getMetaParamDefinitions(); i.hasNext(); ) {
            final MetaParamDefinition mpd = (MetaParamDefinition) i.next();
            result.addMetaParamDefinition(mpd);
        }

        final Set<HierName> resets = new HashSet<HierName>();
        final Set<HierName> grounds = new HashSet<HierName>();
        final Set<HierName> implied = new HashSet<HierName>();
        (new CellUtils.MarkPort() {
            private boolean isImplied = false;
            private void add(Set<HierName> set, final String s) {
                if (isImplied) set = implied;
                set.add(toHier(s));
            }
            protected void mark(final PortDefinition p, final String prefix,
                                int direction) {
                if (prefix == null && parent.isImpliedPort(p.getName())) {
                    isImplied = true;
                }
                super.mark(p, prefix, direction);
                isImplied = false;
            }
            protected void mark(final StructureType structureType,
                                final String name, final int direction) {
                if (CellUtils.isDftChannel(structureType.getTag()) &&
                    direction < 0) {
                    add(resets, name + ".C.d[0]");
                }
                super.mark(structureType, name, direction);
            }
            protected void mark(final ChannelType channelType,
                                final String name,
                                final int direction) {
                if (direction > 0) {
                    add(resets, name + ".e");
                }
                super.mark(channelType, name, direction);
            }
            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                if (direction < 0) {
                    add(grounds, name);
                }
            }
        }).mark(parent);

        for (HierName h : resets) {
            if (reset == null) return null;
            result.addConnection(h, reset);
        }

        for (HierName h : grounds) {
            if (!resets.contains(h)) {
                if (gnd == null) return null;
                result.addConnection(h, gnd);
            }
        }

        return result;
    }

    public static Iterator<Pair<HierName,CellInterface>> filterSubcellPairs(
            final CellInterface cell,
            final UnaryPredicate<CellInterface> filter) {
        return new FilteringIterator<Pair<HierName,CellInterface>>(
                (Iterator<Pair<HierName,CellInterface>>) cell.getSubcellPairs(),
                new UnaryPredicate<Pair<HierName,CellInterface>>() {
                    public boolean evaluate(
                        final Pair<HierName,CellInterface> p) {
                        return filter.evaluate(p.getSecond());
                    }
                });
    }

    /**
     * Move array indexing to the end of string, e.g., a[0]_[1]b to a_b[0][1].
     * Nested array indexing not supported.
     **/
    private static String moveBrackets(final String s) {
        StringBuilder nonbracket = null;
        StringBuilder bracket = null;
        final int len = s.length();
        int start = 0;
        while (start < len) {
            final int left = s.indexOf('[', start);
            if (left >= 0) {
                final int right = s.indexOf(']', left + 1);
                if (right >= 0) {
                    if (nonbracket == null) nonbracket = new StringBuilder();
                    nonbracket.append(s.substring(start, left));

                    if (bracket == null) bracket = new StringBuilder();
                    bracket.append(s.substring(left, right + 1));

                    start = right + 1;
                    continue;
                }
            }
            break;
        }

        if (start == 0) {
            // special case if string contains no brackets
            return s;
        } else {
            if (start < len) {
                if (nonbracket == null) nonbracket = new StringBuilder();
                nonbracket.append(s.substring(start));
            }

            return (nonbracket == null ? "" : nonbracket.toString()) +
                   (bracket == null ? "" : bracket.toString());
        }
    }

    /**
     * Transform each component of a HierName according to the given function.
     **/
    private static HierName
    transformHierName(final HierName h, final UnaryFunction<String,String> f) {
        if (h == null) {
            return h;
        } else {
            final HierName parent = transformHierName(h.getParent(), f);
            final String suffix = f.execute(h.getSuffixString());
            if (parent == h.getParent() && suffix == h.getSuffixString()) {
                return h;
            } else {
                try {
                    return HierName.makeHierName(parent, suffix);
                } catch (InvalidHierNameException e) {
                    throw new AssertionError("Cannot construct HierName");
                }
            }
        }
    }
    
    /**
     * Given an instance, return a CAST name
     **/
    public static HierName
    getCastNodeName(final HierName inst) {
        return transformHierName(
            inst,
            new UnaryFunction<String,String>() {
                public String execute(String x) {
                    return StringUtil.replaceSubstring(moveBrackets(x),
                                                       "][", ",");
                }
            });
    }

    /**
     * Given a STATICIZER instance, return a pair consisting of the CAST name
     * of the small inverter node, and its real "analog" name.
     **/
    private static HierName SMALL_INVERTER_NODE = HierName.makeHierName("1");
    public static Pair<HierName,HierName>
    getSmallInverterNode(final HierName inst) {
        return new Pair<HierName,HierName>(
            HierName.append(
                transformHierName(
                    inst,
                    new UnaryFunction<String,String>() {
                        public String execute(String x) {
                            return StringUtil.replaceSubstring(moveBrackets(x),
                                                               "][", ",");
                        }
                    }),
                SMALL_INVERTER_NODE),
            HierName.append(inst, SMALL_INVERTER_NODE));
    }

    public static Map<HierName,HierName>
    getOutputNodes(final CellInterface cell,
                   final CastFileParser cfp,
                   final Cadencize cad,
                   final AliasedSet nodes,
                   final Map<HierName,HierName> result) {
        if (!cell.containsNetlist()) return null;

        for (Iterator i = nodes.getCanonicalKeys(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            result.put(name, name);
        }

        final NetlistBlock nb =
            (NetlistBlock) cell.getBlockInterface()
                               .iterator(BlockInterface.NETLIST)
                               .next();

        nb.getCDLTemplate().execute(
            new CDLFactoryAdaptor() {
                public void makeCall(HierName name, String subName,
                                     HierName[] args, Map parameters,
                                     com.avlsi.cast.impl.Environment env) {
                    if (subName.startsWith("gate.STATICIZER")) {
                        final Pair<HierName,HierName> invNode =
                            getSmallInverterNode(name);
                        if (result.containsKey(invNode.getFirst()))
                            throw new AssertionError();
                        result.put(invNode.getFirst(), invNode.getSecond());
                    }
                }
            });

        /*
        final ArrayList problems = new ArrayList();
        final NetGraph ng = new NetGraph(nodes,
                cell.getLocalExclusiveNodeSets(),
                problems,
                HierName.makeHierName("Vdd"),
                HierName.makeHierName("GND"),
                Collections.emptySet());
        try {
            ng.addCellInterface(cell, new NetGraph[0], cfp, cad);
        } catch (com.avlsi.prs.UnimplementableProductionRuleException e) {
            throw new RuntimeException("Can't happen");
        }
        ng.prepareForLvs();
        assert problems.isEmpty();
        for (Iterator i = ng.getOutputNodes().iterator();
             i.hasNext(); ) {
            final NetGraph.NetNode n = (NetGraph.NetNode) i.next();
            result.add(n.name);
        }
        */

        return result;
    }

    /**
     * For a given node name, returns the instance path to the cell where the
     * node is local, the cell type, and the node's canonical name where it is
     * local.  If <code>top</code> is true, then even if <code>name</code> is
     * already local, still recurse.
     **/
    public static Triplet<HierName,CadenceInfo,HierName>
    localize(final HierName name, final CadenceInfo ci, final boolean top) {
        HierName path = null;
        CadenceInfo type = ci;

        final AliasedSet localNodes = ci.getLocalNodes();
        HierName canon = (HierName) localNodes.getCanonicalKey(name);
        boolean inSubcell = false;
        if (canon == null || top) {
            for (HierName h = name; h != null; h = h.tail()) {
                path = HierName.append(path, h.head());
                type = ci.getSubcell(path);
                if (type != null) {
                    final Triplet<HierName,CadenceInfo,HierName> sublocal =
                        localize(h.tail(), type, false);
                    if (sublocal != null) {
                        type = sublocal.getSecond();
                        canon = sublocal.getThird();
                        if (sublocal.getFirst() == null) {
                            // it's possible this can now be a local aliases
                            if (!top) {
                                final HierName candidate = (HierName)
                                    localNodes.getCanonicalKey(
                                            HierName.append(path,canon));
                                if (candidate != null) {
                                    canon = candidate;
                                    path = null;
                                }
                            }
                        } else {
                            path = HierName.append(path, sublocal.getFirst());
                        }
                    }
                    inSubcell = true;
                    break;
                }
            }
        }

        final Triplet<HierName,CadenceInfo,HierName> result;
        if (canon == null) {
            if (inSubcell) {
                result =
                    new Triplet<HierName,CadenceInfo,HierName>(path, type, name.tail());
            } else {
            result = null;
            }
        } else {
            if (top && !inSubcell) {
                // this is an implied port node; treat as if it belongs to the
                // top-level cell
                final Pair<HierName,CadenceInfo> p =
                    ci.getSubcellPairIterator().next();
                path = p.getFirst();
                type = p.getSecond();
            }
            result =
                new Triplet<HierName,CadenceInfo,HierName>(path, type, canon);
        }

        return result;
    }

    /**
     * Returns <code>name</code> decomposed into two parts: name of an instance
     * in <code>cell</code>, the remaining part of <code>name</code> with the
     * instance name removed.  If <code>name</code> does not start with an
     * instance name, return <code>Pair(null, name)</code>.
     **/
    public static Pair<HierName,HierName>
    getFirstInstance(final CellInterface cell, final HierName name,
                     final boolean excludeInlined) {
        final Iterator<Pair<HierName,HierName>> i =
            HierName.getSplitsIterator(name);
        final Pair<HierName,HierName> notfound = i.next();
        while (i.hasNext()) {
            final Pair<HierName,HierName> p = i.next();
            if (cell.getSubcell(p.getFirst()) != null &&
                (!excludeInlined || !cell.isInlinedSubcell(p.getFirst()))) {
                return p;
            }
        }
        return notfound;
    }

    /**
     * Returns a map from canonical port name to port direction.  If aliases of
     * a port include both input and output ports, then inout will be returned.
     **/
    public static Map<HierName,Integer>
    getCanonicalDir(final Map<HierName,Integer> portDirs,
                    final CellInterface cell,
                    final AliasedSet nodes) {
        for (Map.Entry<String,Integer> portDir :
                CellUtils.markPorts(cell).entrySet()) {
            final HierName canon = (HierName)
                nodes.getCanonicalKey(toHier(portDir.getKey()));
            final Integer prev = portDirs.get(canon);
            final Integer curr = portDir.getValue();
            if (prev == null) {
                portDirs.put(canon, curr);
            } else {
                final boolean in  = prev == PortDefinition.IN    ||
                                    prev == PortDefinition.INOUT ||
                                    curr == PortDefinition.IN    ||
                                    curr == PortDefinition.INOUT;
                final boolean out = prev == PortDefinition.OUT   ||
                                    prev == PortDefinition.INOUT ||
                                    curr == PortDefinition.OUT   ||
                                    curr == PortDefinition.INOUT;
                if (in && out) {
                    portDirs.put(canon, PortDefinition.INOUT);
                } else if (in) {
                    portDirs.put(canon, PortDefinition.IN);
                } else if (out) {
                    portDirs.put(canon, PortDefinition.OUT);
                } else {
                    portDirs.put(canon, PortDefinition.NONE);
                }
            }
        }
        return portDirs;
    }

    /**
     * Returns the FQCN of the refinement parent of all asynchronous channels.
     **/
    public static String getAsynchronousChannelParent() {
        return "standard.channel.asynchronous_channel";
    }

    /**
     * Returns true if the given cell is an asynchronous channel.
     **/
    public static boolean isAsyncChannel(CellInterface cell) {
        final String ancestor = getAsynchronousChannelParent();
        while (cell != null) {
            if (cell.getFullyQualifiedType().equals(ancestor)) {
                return true;
            }
            cell = cell.getDirectRefinementParent();
        }
        return false;
    }

    public static BigInteger getNumValues(final CellInterface chan) {
        final BigInteger numValues = (BigInteger)
            DirectiveUtils.getTopLevelDirective(chan,
                                                DirectiveConstants.NUM_VALUES);
        return numValues == null ? BigInteger.ONE : numValues;
    }
}
