/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.tools.jauto;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.text.Format;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.SortedMap;
import java.util.TreeSet;

import com.avlsi.cast.*;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.DirectiveInterfaceFactory;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.directive.impl.CastEmitter;
import com.avlsi.cast2.directive.impl.DirectiveDifference;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.cast2.directive.impl.DirectiveImpl;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.cast2.util.DirectiveActionFilter;
import com.avlsi.cast2.util.DirectiveFilter;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.DirectiveWalker;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.CastDesign;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.MergeDirective;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.ports.ArrayType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.common.HierName;
import com.avlsi.io.IndentWriter;
import com.avlsi.io.NullWriter;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.impl.CDLEmitter;
import com.avlsi.netlist.impl.DeviceProcessor;
import com.avlsi.netlist.impl.FoldTransistor;
import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.BinaryFunction;

/**
 * Class to output subtypes.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/

public final class SubtypeOutput {
    private static final Writer NOWRITE = NullWriter.getInstance();
    private SubtypeOutput() {
        throw new AssertionError();
    }

    /**
     * Class used to hold the result of executing a Policy.
     **/
    public static final class Decision {
        /**
         * Module name of the cell subtyped to, e.g., lib.buffer.half.BUF_1of4
         **/
        public final String module;

        /**
         * What the cell is subtyped to, e.g., 0, 1, 2.
         **/
        public final String subtype;

        /**
         * The cell the subtype is refining.
         **/
        public final String from;

        /**
         * Directives to output.  A mapping from <code>BlockInterface</code>
         * constants to <code>DirectiveInterfaceFactory</code>s.
         **/
        public final Map directives;

        /**
         * Extra directives to output.  A mapping from
         * <code>BlockInterface</code> constants to
         * <code>DirectiveInterfaceFactory</code>s.
         **/
        public final Map extraDirs;

        /**
         * Writer to output the subtype to.  <code>null</code> if no output
         * should be done.
         **/
        public final Writer writer;

        /**
         * Informational comment at the beginning of the file.
         **/
        public final String header;

        /**
         * Attribute cells to inherit.
         **/
        public final String[] attributes;

        /**
         * Cell whose directives will be merged against.
         **/
        public final CellInterface dirCell;

        /**
         * Cell whose content will be used.  For a leaf cell, this is the
         * netlist block; for a midlevel cell, this is the subcells contained
         * in the cell.
         **/
        public final CellInterface contentCell;

        public Decision(final String module, final String subtype,
                        final Map directives, final Writer writer,
                        final String header, final String[] attributes) {
            this(module, subtype, module, directives, writer, header,
                 attributes);
        }

        public Decision(final String module, final String subtype,
                        final String from, final Map directives,
                        final Writer writer, final String header,
                        final String[] attributes) {
            this(module, subtype, from, directives, writer, header,
                 attributes, null);
        }

        public Decision(final String module, final String subtype,
                        final String from, final Map directives,
                        final Writer writer, final String header,
                        final String[] attributes,
                        final CellInterface dirCell) {
            this(module, subtype, from, directives, writer, header, attributes,
                 dirCell, null);
        }

        public Decision(final String module, final String subtype,
                        final String from, final Map directives,
                        final Writer writer, final String header,
                        final String[] attributes, final CellInterface dirCell,
                        final CellInterface contentCell) {
            this(module, subtype, from, directives, Collections.EMPTY_MAP,
                 writer, header, attributes, dirCell, contentCell);
        }

        public Decision(final String module, final String subtype,
                        final String from, final Map directives,
                        final Map extraDirs,
                        final Writer writer, final String header,
                        final String[] attributes, final CellInterface dirCell,
                        final CellInterface contentCell) {
            this.module = module;
            this.subtype = subtype;
            this.from = from;
            this.directives = directives;
            this.extraDirs = extraDirs;
            this.writer = writer;
            this.header = header;
            this.attributes = attributes;
            this.dirCell = dirCell;
            this.contentCell = contentCell;
        }
    }

    /**
     * Class that encapsulate all decision making on how to subtype cells, what
     * files to write to, and what directives to output.
     **/
    public interface Policy {
        /**
         * Returns a Decision after applying this Policy to subtype the given
         * cell.
         **/
        Decision getDecision(final CellInterface cell) throws IOException;

        /**
         * Returns what the given instance of a subcell in a parent cell should
         * be subtyped to.  parent may be null to indicate the top level cell.
         * If a subtype line is
         *     lib.buffer.half.BUF_1of4 :&gt; lib.buffer.half.BUF_1of4.0
         * then The first element is the Pair is lib.buffer.half.BUF_1of4, and
         * the second is lib.buffer.half.BUF_1of4.0.
         */
        Pair getSubtype(final CellInterface parent, final CellInterface subcell,
                        final HierName instance);
    }

    /**
     * Return the next available subtype given a directory and constraints on
     * subtype ranges.  Subtypes are assumed to have filenames of the form
     * X.cast, where X, the subtype, is a non-negative integer.  If L is the
     * largest subtype in a directory, then the next available subtype N is
     * max(L + 1, minSubtype).  If N &gt; maxSubtype, than <code>null</code> is
     * returned.
     **/
    private static String findNextSubtype(final File dir,
                                          final int minSubtype,
                                          final int maxSubtype) {
        final int[] result = new int[1];
        result[0] = -1;
        // XXX: Should cache directory information
        dir.list(new FilenameFilter() {
            public boolean accept(final File from, final String name) {
                if (name.endsWith(".cast")) {
                    String index = name.substring(0, name.length() - 5);
                    try {
                        result[0] = Math.max(result[0], Integer.parseInt(index));
                    } catch (NumberFormatException e) { }
                }
                return false;
            }
        });
        result[0] = Math.max(result[0] + 1, minSubtype);
        if (result[0] > maxSubtype) return null;
        else return Integer.toString(result[0]);
    }

    /**
     * Return the next available subtype given a directory.  This is equivalent
     * to findNextSubtype(dir, 0, Integer.MAX_VALUE).
     **/
    private static String findNextSubtype(final File dir) {
        return findNextSubtype(dir, 0, Integer.MAX_VALUE);
    }

    /**
     * Return the next open subtype given an array of directories, and
     * constraints on subtype ranges.  Subtypes are assumed to have filenames
     * of the form X.cast, where X, the subtype, is a non-negative integer.
     * The smallest available subtype not used in the range specified is
     * returned.  If no such subtype exists, than <code>null</code> is
     * returned.
     **/
    private static String findOpenSubtype(final File[] dir,
                                          final int minSubtype,
                                          final int maxSubtype) {
        final TreeSet s = new TreeSet();
        // XXX: Should cache directory information
        for (int i = 0; i < dir.length; ++i) {
            dir[i].list(new FilenameFilter() {
                public boolean accept(final File from, final String name) {
                    if (name.endsWith(".cast")) {
                        String index = name.substring(0, name.length() - 5);
                        try {
                            s.add(new Integer(index));
                        } catch (NumberFormatException e) { }
                    }
                    return false;
                }
            });
        }
        for (int i = minSubtype; i <= maxSubtype; ++i) {
            if (!s.contains(new Integer(i))) {
                return Integer.toString(i);
            }
            // don't wrap due to overflow and loop infinitely if maxSubtype is
            // Integer.MAX_VALUE
            if (i == maxSubtype) break;
        }
        return null;
    }

    private static String findOpenSubtype(final File dir,
                                          final int minSubtype,
                                          final int maxSubtype) {
        return findOpenSubtype(new File[] { dir }, minSubtype, maxSubtype);
    }

    /**
     * Opens a file given a directory and the file name, creating the directory
     * if necessary.
     **/
    private static Writer openFile(final File dir, final String name) throws IOException {
        if (!dir.isDirectory() && !dir.mkdirs()) {
            throw new IOException("Unable to create directory: " + dir);
        }
        final File castFile = new File(dir, name);
        System.out.println("Subtype output to: " + castFile.getCanonicalPath());
        return new BufferedWriter(new FileWriter(castFile));
    }

    /**
     * If the given cell is not fixed size, opens a file given a directory and
     * the file name, creating the directory if necessary.  Otherwise, return
     * an instance of <code>NullWriter</code>.
     **/
    private static Writer openFile(final File dir, final String name,
                                   final CellInterface ci) throws IOException {
        return isFixed(ci) ? NOWRITE : openFile(dir, name);
    }

    /**
     * Returns a String representing the directory of a cell, assuming "." is
     * the directory seperator.
     **/
    private static String cellPath(final String name) {
        return CellUtils.hashMetaParameters(name).replace('.', File.separatorChar);
    }

    /**
     * Returns a File representing the directory of a cell, assuming "." is the
     * directory seperator.
     **/
    private static File cellPath(final File path, final String name) {
        return new File(path, CellUtils.hashMetaParameters(name).replace('.', File.separatorChar));
    }

    private static File[] cellPath(final File[] path, final String name) {
        final File[] result = new File[path.length];
        for (int i = 0; i < path.length; ++i) {
            result[i] = cellPath(path[i], name);
        }
        return result;
    }

    /**
     * Returns the base type of a subtype.
     **/
    private static String getBaseType(final String subtype) {
        final int index = subtype.lastIndexOf('.');
        return subtype.substring(0, index);
    }

    /**
     * Returns the name of the direct refinement parent.
     **/
    private static String getParentName(final CellInterface cell) {
        return cell.getDirectRefinementParent().getFullyQualifiedType();
    }

    /**
     * Return the type of instance named <code>inst</code> in the direct
     * refinement parent of <code>cell</code>.
     **/
    private static String getParentName(final CellInterface cell,
                                        final HierName inst) {
        final CellInterface parent = cell.getDirectRefinementParent();
        final CellInterface subcell = parent.getSubcell(inst);
        return subcell.getFullyQualifiedType();
    }

    /**
     * Returns a header, with parameters substituted in.  Allowed parameters
     * follow:
     * <ol>
     * <li> {0} - p2n mode
     * </ol>
     **/
    private static String getHeader(final Format form, final String mode) {
        final Object[] args = { mode };
        return form.format(args);
    }

    /**
     * Return the attribute cells inherited by the given cell.
     **/
    private static String[] getAttributes(final CellInterface cell) {
        return cell.getInheritedCells()
                   .map(c -> c.getFullyQualifiedType())
                   .toArray(String[]::new);
    }

    private static void emitDirective(final BlockIterator bi,
                                      final DirectiveEmitter emitter,
                                      final DirectiveActionFilter f,
                                      final IndentWriter iw) throws IOException
    {
        boolean wroteHeader = false;
        while (bi.hasNext()) {
            final BlockInterface block = bi.next();
            final BlockIterator bdir = block.iterator(BlockInterface.DIRECTIVE);

            if (bdir.hasNext()) {
                final List dirs = emitDirective(block, null, f, emitter);

                // Do not write any headers, unless there are actual
                // directives to output
                if (dirs.size() > 0) {
                    if (!wroteHeader) {
                        iw.write(block.getType() + " {\n");
                        iw.nextLevel();
                        wroteHeader = true;
                    }
                    emitDirective(dirs.iterator(), iw);
                }
            }
        }
        if (wroteHeader) {
            iw.prevLevel();
            iw.write("}\n");
        }
    }

    /**
     * Emit the directives contained in the give block.  Directives contained in
     * <code>source</code> is first merged with the directives block in
     * <code>block</code>, and the result is then emitted.
     *
     * @param block Emit the directives of this block
     * @param emitter The <code>DirectiveEmitter</code> to convert directive
     * values to strings.
     * @param source What directives to merge in.
     * @param extra Additional directives to emit.
     * @param iw Where to write the directives.
     * @param header <code>true</code> if the block level name and braces
     * should be written (e.g., <code>pre { }</code>).
     **/
    private static void emitDirective(final BlockInterface block,
                                      final DirectiveEmitter emitter,
                                      final DirectiveInterfaceFactory source,
                                      final DirectiveInterfaceFactory extra,
                                      final IndentWriter iw,
                                      final boolean header)
        throws IOException {
        DirectiveBlock realDir = DirectiveUtils.getDirectiveBlock(block);
        if (realDir == null) {
            if (source == null) return;
            realDir = new DirectiveBlock(source.getDirectiveInterface());
        } else if (source != null) {
            realDir =
                new MergeDirective(realDir, source.getDirectiveInterface());
        }
        final String type = block.getType();
        final DirectiveFilter.ByDirective filter = FILTER_MAP.get(type);
        final List dirs =
            emitDirective(block, realDir, filter, emitter);
        final List extraDirs;
        if (extra == null) {
            extraDirs = Collections.EMPTY_LIST;
        } else {
            extraDirs = emitDirective(
                    block, new DirectiveBlock(extra.getDirectiveInterface()),
                    filter.getReverse(), emitter);
        }

        if (dirs.size() > 0 && header) {
            iw.write(type + " {\n");
            iw.nextLevel();
        }

        emitDirective(new FlatteningIterator(dirs.iterator(),
                                             extraDirs.iterator()), iw);

        if (dirs.size() > 0 && header) {
            iw.prevLevel();
            iw.write("}\n");
        }
    }

    private static void emitDirective(final BlockInterface block,
                                      final DirectiveEmitter emitter,
                                      final DirectiveInterfaceFactory source,
                                      final IndentWriter iw,
                                      final boolean header)
        throws IOException {
        emitDirective(block, emitter, source, null, iw, header);
    }

    private final static DirectiveFilter.ByDirective TOP_LEVEL_FILTER =
        new DirectiveFilter.ByDirective(
            true,
            new HashSet(
                Arrays.asList(new String[] {
                    DirectiveConstants.FIXED_SIZE,
                    DirectiveConstants.TAU,
                    DirectiveConstants.DENSITY_FACTOR,
                    DirectiveConstants.HEIGHT,
                    DirectiveConstants.WIDTH,
                    DirectiveConstants.AUTO_LAYOUT,
                    DirectiveConstants.TRANSISTOR_TYPE,
                    DirectiveConstants.STATICIZER_RATIO,
                    DirectiveConstants.STATICIZER_TYPE
                })));

    private final static DirectiveFilter.ByDirective SUBCELL_FILTER =
        new DirectiveFilter.ByDirective(
            true,
            new HashSet(
                Arrays.asList(new String[] {
                    DirectiveConstants.INLINE_LAYOUT,
                    DirectiveConstants.CAP,
                    DirectiveConstants.ESTIMATED_DELAY
                })));

    private final static DirectiveFilter.ByDirective PRS_FILTER =
        new DirectiveFilter.ByDirective(
            true,
            new HashSet(
                Arrays.asList(new String[] {
                    DirectiveConstants.CAP,
                    DirectiveConstants.ESTIMATED_DELAY,
                    DirectiveConstants.TRANSISTOR_TYPE,
                    DirectiveConstants.STATICIZER_RATIO,
                    DirectiveConstants.STATICIZER_TYPE,
                    DirectiveConstants.SYMMETRIZE,
                    DirectiveConstants.SHARED
                })));

    private final static Map<String,DirectiveFilter.ByDirective> FILTER_MAP =
        new HashMap<String,DirectiveFilter.ByDirective>();
    static {
        FILTER_MAP.put(BlockInterface.CELL, TOP_LEVEL_FILTER);
        FILTER_MAP.put(BlockInterface.SUBCELL, SUBCELL_FILTER);
        FILTER_MAP.put(BlockInterface.PRS, PRS_FILTER);
    }

    /**
     * Emit directives in all blocks of <code>cell</code>, except block types
     * which appear in <code>exclude</code>.
     *
     * @param cell Emit the directives of all (except types contained in
     * <code>exclude</code> blocks this cell.
     * @param emitter The <code>DirectiveEmitter</code> to convert directive
     * values to strings.
     * @param exclude Set of block types to ignore.
     * @param iw Where to write the directives.
     **/
    private static void emitDirective(final CellInterface cell,
                                      final DirectiveEmitter emitter,
                                      final Set exclude,
                                      final IndentWriter iw) throws IOException
    {
        final BlockInterface cellBlock = cell.getBlockInterface();
        /*
        for (Iterator i = cellBlock.getAttachedTypes(); i.hasNext(); ) {
            final String type = (String) i.next();
            if (exclude.contains(type)) continue;
            emitDirective(cellBlock.iterator(type), emitter, iw);
        }
        */
    }

    /**
     * Emit a list of directives as strings to an <code>IndentWriter</code>.
     * If the iterator is empty, do not output anything, otherwise, add the
     * property directive block structure.
     *
     * @param directiveStrings Iterator of strings to output.
     * @param iw <code>IndentWriter</code> to write to.
     * @return <code>true</code> if anything is written, <code>false</code>
     * otherwise.
     **/
    private static boolean emitDirective(final Iterator directiveStrings,
                                         final IndentWriter iw)
        throws IOException
    {
        if (directiveStrings.hasNext()) {
            iw.write("directives {\n");
            iw.nextLevel();
            while (directiveStrings.hasNext()) {
                iw.write((String) directiveStrings.next());
                iw.write("\n");
            }
            iw.prevLevel();
            iw.write("}\n");
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns the value of the fixed_size top level directive.
     **/
    private static boolean isFixed(final CellInterface cell) {
        return CellUtils.isFixed(cell);
    }

    /**
     * Emit directives contained in <code>dirBlock</code> as
     * <code>String</code>s, and return them in a list.
     *
     * @param block The block the directives are contained in.
     * @param dirBlock The <code>DirectiveBlock</code> containing the
     * directives.  If <code>null</code>, then the <code>DirectiveBlock</code>
     * is assumed to be the one attached to <code>block</code>
     * @param emitter The <code>DirectiveEmitter</code> to convert directive
     * values to strings.
     *
     * @return list of string representations of the directives.
     **/
    private static List emitDirective(final BlockInterface block,
                                      final DirectiveBlock dirBlock,
                                      final DirectiveActionFilter f,
                                      final DirectiveEmitter emitter) {
        final List result = new ArrayList();
        final DirectiveActionInterface act =
            new DirectiveActionInterface() {
                public void doUnParameterizedDirective(BlockInterface block,
                                                       DirectiveBlock db,
                                                       String directive,
                                                       Object value,
                                                       String valueType)
                throws IOException {
                    result.add(directive + " = " + emitter.emit(block.getType(), valueType, value) + ";");
                }
                public void doParameterizedDirectiveValue(BlockInterface block,
                                                          DirectiveBlock db,
                                                          String directive,
                                                          Object parameter,
                                                          Object value,
                                                          String parameterType,
                                                          String valueType)
                throws IOException {
                    result.add(directive + "(" + emitter.emit(block.getType(), parameterType, parameter) + ") = " + emitter.emit(block.getType(), valueType, value) + ";");
                }
                public void doParameterizedDirectiveType(BlockInterface block,
                                                         DirectiveBlock db,
                                                         String directive,
                                                         String parameterType,
                                                         String valueType)
                throws IOException { }
                public void doBlockInterface(BlockInterface block)
                throws IOException { }
            };

        try {
            final DirectiveActionInterface fullAct =
                f == null ? act : f.filter(act);
            final DirectiveWalker walker = new DirectiveWalker(fullAct);

            if (dirBlock == null) walker.walk(block);
            else walker.walk(block, dirBlock);
        } catch (UnknownDirectiveException e) {
            throw new RuntimeException(e);
        } catch (IOException e) {
            throw new AssertionError("Should never happen.");
        }
        return result;
    }

    private static DirectiveInterfaceFactory getExtraDirs(
            final BlockInterface child, final BlockInterface parent) {
        final DirectiveDifference dirs =
            new DirectiveDifference(child.getType());
        final DirectiveWalker walker = new DirectiveWalker(dirs);
        try {
            walker.walk(child);
            dirs.setMode(false);
            walker.walk(parent);
        } catch (UnknownDirectiveException e) {
        } catch (IOException e) {
        }
        return dirs;
    }

    private static Map getExtraDirs(final CellInterface child) {
        final Map result = new HashMap();
        final CellInterface parent = child.getDirectRefinementParent();
        result.put(BlockInterface.CELL,
                   getExtraDirs(child.getBlockInterface(),
                                parent.getBlockInterface()));
        result.put(BlockInterface.SUBCELL,
                   getExtraDirs(child.getBlockInterface()
                                     .iterator(BlockInterface.SUBCELL)
                                     .next(),
                                parent.getBlockInterface()
                                      .iterator(BlockInterface.SUBCELL)
                                      .next()));
        return result;
    }

    abstract static class SimplePolicy implements Policy {
        protected final Map subtypeMap;
        public SimplePolicy() {
            subtypeMap = new LinkedHashMap();
        }

        public abstract Decision getDecision(final CellInterface cell)
            throws IOException;

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String oldType = subcell.getFullyQualifiedType();
            return new Pair(oldType, subtypeMap.get(oldType));
        }
    }

    /**
     * Implements the --subtype mode policy: (1) Create new subtypes for all
     * cells, unless (2) subtype is specified, in which case use the specified
     * subtype for all cells, overwrite if necessary.
     **/
    public static class Subtype implements Policy {
        private final File path;
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final Map seen;
        private final Set warn;
        private String defaultAttribute, current;
        private final BinaryFunction writeSubtype;
        private final CastDesign design;
        private final Map missingAlias;
        
        private String getLayout(final CellInterface cell) {
            return (String) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.LAYOUT_ATTRIBUTES);
        }

        private String getExtra(final CellInterface cell) {
            return (String) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.EXTRA_LAYOUT_ATTRIBUTES);
        }

        /**
         * Apply the name_mapping directive.  Returns the module name, which is
         * also the refinement parent.
         **/
        private String resolveMapping(final CellInterface cell) {
            final String map = (String) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.NAME_MAPPING);
            final String parentMap = (String) DirectiveUtils.getTopLevelDirective(cell.getDirectRefinementParent(), DirectiveConstants.NAME_MAPPING);
            final String result;
            if (map == null || map.equals(parentMap)) {
                result = cell.getFullyQualifiedType();
            } else {
                result =
                    map.indexOf('.') == -1 ? cell.getModuleName() + "." + map
                                           : map;
                boolean isAlias = false;
                try {
                    final CellInterface alias =
                        design.getCastFileParser().getFullyQualifiedCell(result);
                    if (!alias.isAlias()) {
                        missingAlias.put(result, null);
                    }
                } catch (Exception e) {
                    missingAlias.put(result, e);
                }
            }
            return result;
        }

        private String futureLayout(final String current,
                                    final CellInterface cell) {
            final String s = getLayout(cell);
            if (s == null) return current;
            else return s;
        }

        static Map getDirectives(final CellInterface cell) {
            final DirectiveSource top =
                new DirectiveSource(BlockInterface.CELL);
            if (CellUtils.isLeaf(cell)) {
                top.definition(DirectiveConstants.FIXED_SIZE,
                               CellUtils.isFixedSize(cell) ?  Boolean.TRUE :
                                                              Boolean.FALSE);
            } else {
                top.definition(DirectiveConstants.FIXED_SIZE, Boolean.FALSE);
            }

            return Collections.singletonMap(BlockInterface.CELL, top);
        }

        public Subtype(final int minSubtype, final String path,
                       final Format header, final String layoutAttribute,
                       final BinaryFunction writeSubtype,
                       final CastDesign design,
                       final Map<String,Exception> missingAlias) {
            this(path, header, minSubtype, Integer.MAX_VALUE, layoutAttribute,
                 writeSubtype, design, missingAlias);
        }

        public Subtype(final String path, final Format header,
                       final int minSubtype, final int maxSubtype,
                       final String layoutAttribute,
                       final BinaryFunction writeSubtype,
                       final CastDesign design,
                       final Map<String,Exception> missingAlias) {
            this.path = new File(path);
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.seen = new HashMap();
            this.warn = new HashSet();
            this.current = null;
            this.defaultAttribute = layoutAttribute;
            this.writeSubtype = writeSubtype;
            this.design = design;
            this.missingAlias = missingAlias;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            if (!warn.add(type)) {
                System.err.println("Warning: creating subtype for cell " + type + " due to incompatible layout_attribute.");
            }
            final String base = resolveMapping(cell);
            final File dir = cellPath(path, base);
            final String subtyped =
                findNextSubtype(dir, minSubtype, maxSubtype);
            final Writer writer = openFile(dir, subtyped + ".cast");

            final Map directives = getDirectives(cell);

            if (current == null) {
                current = getLayout(cell);
                if (current == null) current = defaultAttribute;
            }

            final String extra = getExtra(cell);
            final List attributes = new ArrayList(2);
            if (current != null) attributes.add(current);
            if (extra != null) attributes.add(extra);

            return new Decision(base, subtyped, directives, writer,
                                getHeader(header, "subtype"),
                                (String[]) attributes.toArray(new String[0]));
        }
        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            final String layout = futureLayout(current, subcell);
            final Pair key = new Pair(resolveMapping(subcell), layout);
            if (!seen.containsKey(key)) {
                final String oldcurr;
                if (layout == null || layout.equals(current)) {
                    oldcurr = null;
                } else {
                    oldcurr = current;
                    current = layout;
                }

                final Object o = (String) writeSubtype.execute(this, design.getCell(type));
                if (o instanceof IOException) {
                    System.err.println("Cannot create new subtype for " + type + ": " + ((IOException) o).getMessage() + "!");
                    return null;
                }
                seen.put(key, (String) o);

                if (oldcurr != null) {
                    current = oldcurr;
                }
            }
            return new Pair(type, (String) seen.get(key));
        }
    }

    public static class Compact implements Policy {
        private final File[] path; // path[0] is the subtype output path
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final Map unsplittable;
        private final MultiMap spec;
        private final Set skip;
        private final int lo, hi;

        public Compact(final String[] path, final Format header,
                       final int minSubtype, final int maxSubtype,
                       final MultiMap spec, final Set skip,
                       final int rangeLo, final int rangeHi) {
            this.path = new File[path.length];
            for (int i = 0; i < path.length; ++i) {
                this.path[i] = new File(path[i]);
            }
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.unsplittable = new LinkedHashMap();
            this.spec = spec;
            this.skip = skip;
            this.lo = rangeLo;
            this.hi = rangeHi;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final File[] dir = cellPath(path, module);

            Integer subtype;
            try {
                subtype = new Integer(cell.getType());
            } catch (NumberFormatException e) {
                subtype = null;
            }

            final String subtyped;
            if (subtype != null && lo <= subtype.intValue() &&
                subtype.intValue() <= hi && !skip.contains(type)) {
                subtyped = findOpenSubtype(dir, minSubtype, maxSubtype);
            } else {
                subtyped = cell.getType();
            }

            if (subtyped == null) {
                throw new RuntimeException("No available subtype in range " + minSubtype + ".." + maxSubtype + " for cell " + type);
            }
            final Writer writer = openFile(dir[0], subtyped + ".cast");

            unsplittable.put(type, module + "." + subtyped);
            spec.put(type, subtyped);
            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, getExtraDirs(cell),
                                writer, getHeader(header, "compact"),
                                getAttributes(cell), null, null);
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            if (unsplittable.containsKey(type)) {
                return new Pair(getParentName(parent, instance),
                                (String) unsplittable.get(type));
            } else {
                try {
                    final String subtype;
                    subtype = writeSubtype(this, subcell);
                    return new Pair(getParentName(parent, instance), subtype);
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                    return null;
                }
            }
        }
    }

    /**
     * Implements the copy policy.  Output all cells with given subtype.
     **/
    public static class Copy extends SimplePolicy {
        private final String subtype; // 0
        private final File path;
        private final Format header;
        private final MultiMap spec;

        public Copy(final String path, final Format header,
                    final String subtype, final MultiMap spec) {
            this.path = new File(path);
            this.header = header;
            this.subtype = subtype;
            this.spec = spec;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final File dir = cellPath(path, module);
            final String subtyped =  subtype;
            final Writer writer = openFile(dir, subtyped + ".cast");
            spec.put(type, subtyped); // keep a split spec to copy views

            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "copy"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            try {
                final String subtype = writeSubtype(this, subcell);
                return new Pair(getParentName(parent, instance), subtype);
            } catch (IOException e) {
                System.err.println("Cannot create new subtype for " + type + "!");
                return null;
            }
        }
    }

    /**
     * The policy to transform directives.  Currently, it only works with
     * top-level directives.
     **/
    public static class DirectiveTransform implements Policy {
        private final boolean recursive;
        private final File path;
        private final Format header;
        private final Map dirs;
        private final Set seen;

        public DirectiveTransform(final String path, final Format header,
                                  final boolean recursive, final Map dirs) {
            this.path = new File(path);
            this.header = header;
            this.recursive = recursive;
            this.dirs = dirs;
            this.seen = new HashSet();
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String module = cell.getModuleName();
            final String subtyped =  cell.getType();;
            final File dir = cellPath(path, module);
            final Writer writer;
            if (seen.add(cell.getFullyQualifiedType())) {
                writer = openFile(dir, subtyped + ".cast");
            } else {
                writer = null;
            }

            return new Decision(module, subtyped, getParentName(cell),
                                dirs, writer, getHeader(header, "transform"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            if (recursive) {
                try {
                    writeSubtype(this, subcell);
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                }
            }
            return new Pair(getParentName(parent, instance), type);
        }
    }

    private static class CopySpec implements SubtypeSplit.Spec {
        private final String from, to;

        public CopySpec(final String from, final String to) {
            assert from != null && to != null;
            this.from = from;
            this.to = to;
        }

        public String getSpec(final CDLNameInterface renamer) {
            try {
                return "COPY: " + renamer.renameCell(from) + " " + renamer.renameCell(to);
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename " + from + " or " + to, e);
            }
        }

        public boolean equals(final Object o) {
            if (o instanceof CopySpec) {
                final CopySpec spec = (CopySpec) o;
                return from.equals(spec.from) && to.equals(spec.to);
            } else {
                return false;
            }
        }

        public int hashCode() {
            return from.hashCode() + to.hashCode();
        }
    }

    /**
     * Describes a change of instance <code>inst</code> in cell
     * <code>cell</code> from type <code>from</code> to type <code>to</code>.
     **/
    private static class ChangeSpec implements SubtypeSplit.Spec {
        private final String cell, inst, from, to;

        public ChangeSpec(final String cell, final String inst,
                          final String from, final String to) {
            this.cell = cell;
            this.inst = inst;
            this.from = from;
            this.to = to;
        }

        public String getSpec(final CDLNameInterface renamer) {
            try {
                return "CHANGE: " +
                       renamer.renameCell(cell) + " " +
                       renamer.renameSubCellInstance(inst) + " " +
                       renamer.renameCell(from) + " " +
                       renamer.renameCell(to);
            } catch (CDLRenameException e) {
                throw new RuntimeException(
                        "Cannot rename " + cell + ", " + inst + ", " +
                        from + ", or " + to, e);
            }
        }

        public boolean equals(final Object o) {
            if (o instanceof ChangeSpec) {
                final ChangeSpec spec = (ChangeSpec) o;
                return cell.equals(spec.cell) &&
                       inst.equals(spec.inst) &&
                       from.equals(spec.from) &&
                       to.equals(spec.to);
            } else {
                return false;
            }
        }

        public int hashCode() {
            return cell.hashCode() + inst.hashCode() + from.hashCode() +
                   to.hashCode();
        }
    }

    /**
     * Implements the forced reuse policy.
     * XXX: possible bad interaction with meta parameter compression in
     * hashMetaParameters()
     **/
    public static class Reuse implements Policy {
        /**
         * A map: module:String -&gt; subtype:CellInterface
         **/
        private final Map reuse;
        private final Collection spec;
        private final File path;
        private final Format header;
        private final Set seen;
        private Policy current;
        private CellInterface repl;

        private static boolean hasMetaParameters(final CellInterface ci) {
            final String module = ci.getModuleName();
            final int lparen = module.lastIndexOf('(');
            final int rparen = module.lastIndexOf(')');
            return lparen > 0 && lparen < rparen;
        }

        private class Transparent implements Policy {
            public Decision getDecision(final CellInterface cell)
                throws IOException {
                final String type = cell.getFullyQualifiedType();
                final String module = cell.getModuleName();
                final String subtype = cell.getType();
                final Writer writer;
                if (seen.add(type)) {
                    final File dir = cellPath(path, module);
                    writer = openFile(dir, subtype + ".cast", cell);
                } else {
                    writer = null;
                }

                return new Decision(module, subtype, getParentName(cell),
                                    Collections.EMPTY_MAP, writer,
                                    getHeader(header, "reuse"),
                                    getAttributes(cell));
            }
            public Pair getSubtype(final CellInterface parent,
                                   final CellInterface subcell,
                                   final HierName instance) {
                final String type = subcell.getFullyQualifiedType();
                try {
                    spec.add(new CopySpec(type, type));
                    writeSubtype(this, subcell);
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " +
                                       type + "!");
                    return null;
                }
                return new Pair(getParentName(parent, instance), type);
            }
        }

        private class Metaparameter implements Policy {
            private CellInterface target;
            public Metaparameter(final CellInterface target) {
                this.target = target;
            }
            public Decision getDecision(final CellInterface cell)
                throws IOException {
                final String type = cell.getFullyQualifiedType();
                final String module = cell.getModuleName();
                final String subtype = cell.getType();
                final Writer writer;
                if (seen.add(type)) {
                    final File dir = cellPath(path, module);
                    writer = openFile(dir, subtype + ".cast");
                } else {
                    writer = null;
                }

                return new Decision(module, subtype, getParentName(cell),
                                    Collections.EMPTY_MAP, writer,
                                    getHeader(header, "reuse"),
                                    getAttributes(cell));
            }
            public Pair getSubtype(final CellInterface parent,
                                   final CellInterface subcell1,
                                   final HierName instance) {
                final String type1 = subcell1.getFullyQualifiedType();
                final CellInterface subcell2 = target.getSubcell(instance);
                final String type2 = subcell2.getFullyQualifiedType();
                final String actual;
                String type = null;
                try {
                    if (hasMetaParameters(subcell1) && !type1.equals(type2)) {
                        assert hasMetaParameters(subcell2);
                        type = type1;
                        spec.add(new CopySpec(type2, type1));
                        final CellInterface old = target;
                        target = subcell2;
                        actual = writeSubtype(this, subcell1);
                        target = old;
                    } else {
                        type = type2;
                        actual = writeSubtype(new Transparent(), subcell2);
                    }
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                    return null;
                }
                return new Pair(getParentName(parent, instance), actual);
            }
        }

        public Reuse(final String path, final Format header, final Map reuse,
                     final Collection spec) {
            assert(reuse != null && spec != null);
            this.path = new File(path);
            this.header = header;
            this.reuse = reuse;
            this.spec = spec;
            this.seen = new HashSet();
            this.current = null;
            this.repl = null;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final CellInterface content;

            if (reuse.containsKey(module)) { // pedantic forced reuse
                repl = (CellInterface) reuse.get(module);
                current = new Transparent();
                spec.add(new CopySpec(repl.getFullyQualifiedType(), type));
                content = repl;
            } else {
                final String base = CellUtils.getBaseType(module);
                repl = cell;
                if (reuse.containsKey(base)) {
                    // Metaparameterized forced reuse
                    final CellInterface meta = (CellInterface) reuse.get(base);
                    spec.add(new CopySpec(meta.getFullyQualifiedType(), type));
                    current = new Metaparameter(meta);
                } else { // Not subject to forced reuse
                    current = null;
                }
                content = null;
            }

            final Writer writer;
            if (seen.add(repl.getModuleName() + "." + repl.getType())) {
                final File dir = cellPath(path, repl.getModuleName());
                writer = openFile(dir, repl.getType() + ".cast", repl);
            } else {
                writer = null;
            }

            return new Decision(repl.getModuleName(), repl.getType(),
                                getParentName(repl),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "reuse"),
                                getAttributes(repl), repl, content);
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            if (current == null) {
                final String type;
                final Policy old = current;
                final CellInterface lper = repl;
                try {
                    type = writeSubtype(this, subcell);
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + subcell.getFullyQualifiedType() + "!");
                    return null;
                }
                current = old;
                repl = lper;
                return new Pair(getParentName(parent, instance), type);
            } else if (repl == parent) {
                return current.getSubtype(parent, subcell, instance);
            } else {
                return current.getSubtype(parent, repl.getSubcell(instance),
                                          instance);
            }
        }
    }

    /**
     * A helper class for SplitInstance.
     **/
    public static class SplitHelper extends SimplePolicy {
        private final File path;
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final MultiMap spec;

        public SplitHelper(final String path, final Format header,
                           final int minSubtype, final int maxSubtype,
                           final MultiMap spec) {
            this.path = new File(path);
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.spec = spec;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final File dir = cellPath(path, module);
            final String subtyped = findNextSubtype(dir, minSubtype, maxSubtype);
            final Writer writer = openFile(dir, subtyped + ".cast");
            spec.put(type, subtyped);

            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "split-by-instance"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String oldType = subcell.getFullyQualifiedType();
            if (!subtypeMap.containsKey(oldType)) {
                try {
                    final String newType = writeSubtype(this, subcell);
                    subtypeMap.put(oldType, newType);
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + oldType + "!");
                    return null;
                }
            }
            return new Pair(getParentName(parent, instance), subtypeMap.get(oldType));
        }
    }

    /**
     * Implements the splitting all policy: Create new subtypes for each
     * instance of a cell that is splittable (determined via a directive).
     * Chose subtype number within the limits given, and if the limits are too
     * small, emit a warning.
     **/
    public static class SplitAll implements Policy {
        private final File path;
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final Set splittable;
        private final boolean maximal;
        private final Stack stack;
        private final MultiMap spec;
        private CellInterface parent;
        private final Set seen;

        private boolean isSplittable(final CellInterface cell) {
            return ((maximal || CellUtils.isSplittable(cell)) && !isFixed(cell))
                   || splittable.contains(cell.getFullyQualifiedType());
        }

        private String getSubtype(final CellInterface parent,
                                  final CellInterface current, final File dir) {
            final boolean fixed;
            if (parent == null) { // top level cell
                fixed = isFixed(current);
            } else { // not top level cell
                fixed = isFixed(parent) || isFixed(current)
                                        || !isSplittable(current);
            }

            if (fixed) {
                return current.getType();
            } else {
                final String type = current.getFullyQualifiedType();
                final String subtype =
                    findNextSubtype(dir, minSubtype, maxSubtype);
                if (subtype == null) {
                    throw new RuntimeException(
                            "No available subtype in range " + minSubtype +
                            ".." + maxSubtype + " for cell " + type);
                }
                spec.put(type, subtype);
                return subtype;
            }
        }

        public SplitAll(final String path, final Format header,
                        final int minSubtype, final int maxSubtype,
                        final Set splittable, final boolean maximal,
                        final boolean splitArray, final MultiMap spec) {
            this.path = new File(path);
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.splittable = splittable;
            this.maximal = maximal;
            if (splitArray) {
                this.stack = null;
            } else {
                this.stack = new Stack();
                this.stack.push(new LinkedHashMap());
            }
            this.spec = spec;
            this.parent = null;
            this.seen = new HashSet();
        }

        public Decision getDecision(final CellInterface cell)
            throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final File dir = cellPath(path, module);
            final String subtyped = getSubtype(parent, cell, dir);

            final Writer writer = seen.add(module + "." + subtyped) ?
                openFile(dir, subtyped + ".cast", cell) : null;
            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "split"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            try {
                final CellInterface oldParent = this.parent;
                this.parent = parent;
                final String subtype;
                if (stack == null) {
                    subtype = writeSubtype(this, subcell);
                } else {
                    final Map done = (Map) stack.peek();
                    final String arrayBase =
                        instance.getArrayBase().getAsString('.');
                    final String fullBase = getArrayBase(arrayBase);
                    final Pair key = new Pair(fullBase, type);
                    if (done.containsKey(key)) {
                        subtype = (String) done.get(key);
                    } else {
                        stack.push(new LinkedHashMap());
                        subtype = writeSubtype(this, subcell);
                        stack.pop();
                        done.put(key, subtype);
                    }
                }
                this.parent = oldParent;
                return new Pair(getParentName(parent, instance), subtype);
            } catch (IOException e) {
                System.err.println("Cannot create new subtype for " + type + "!");
                return null;
            }
        }
    }

    /**
     * Implements splitting by type policy: Create new subtypes for each
     * instance of a cell that is specified as splittable.  Any children of
     * such a cell is treated as if it were handled by the splitting all
     * policy.
     **/
    public static class SplitType implements Policy {
        private final File path;
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final Set splittable;
        private final Policy splitAll;
        private final MultiMap spec;

        public SplitType(final String path, final Format header,
                         final int minSubtype, final int maxSubtype,
                         final Set splittable, final MultiMap spec) {
            this.path = new File(path);
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.splittable = splittable;
            this.spec = spec;
            this.splitAll = new SplitAll(path, header, minSubtype, maxSubtype, splittable, false, false, spec);
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final String subtyped = cell.getType();
            final File dir = cellPath(path, module);

            final Writer writer;
            if (CellUtils.isLeaf(cell)) {
                writer = null;
            } else {
                writer = openFile(dir, subtyped + ".cast");
                spec.put(type, subtyped);
            }

            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "split-by-type"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            if (splittable.contains(type)) {
                try {
                    return new Pair(getParentName(parent, instance),
                                    writeSubtype(splitAll, subcell));
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                    return null;
                }
            } else {
                try {
                    return new Pair(getParentName(parent, instance),
                                    writeSubtype(this, subcell));
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                    return null;
                }
            }
        }
    }

    /**
     * Implements splitting by instance policy: Create new subtypes for the
     * specified instances in the top-level cell.  For each new subtype, a new
     * subtype tree will be created rooted there, i.e., --mode=subtype, not
     * taking into account the splittable directive.
     **/
    public static class SplitInstance implements Policy {
        private final File path;
        private final Format header;
        private final int minSubtype, maxSubtype;
        private final Set instances;
        private final Policy subtypePolicy;
        private final MultiMap spec;

        public SplitInstance(final String path, final Format header,
                             final int minSubtype, final int maxSubtype,
                             final Set instances, final MultiMap spec) {
            this.path = new File(path);
            this.header = header;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.instances = instances;
            this.spec = spec;
            this.subtypePolicy =
                new SplitHelper(path, header, minSubtype, maxSubtype, spec);
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final String subtyped = cell.getType();
            final File dir = cellPath(path, module);

            final Writer writer;
            if (CellUtils.isLeaf(cell)) {
                writer = null;
            } else {
                writer = openFile(dir, subtyped + ".cast");
                spec.put(type, subtyped);
            }

            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, writer,
                                getHeader(header, "split-by-instance"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            final String inst = instance.getAsString('.');
            if (instances.contains(inst)) {
                try {
                    return new Pair(getParentName(parent, instance),
                                    writeSubtype(subtypePolicy, subcell));
                } catch (IOException e) {
                    System.err.println("Cannot create new subtype for " + type + "!");
                    return null;
                }
            } else {
                return new Pair(getParentName(parent, instance), type);
            }
        }
    }

    /**
     * Split by hierarchical instance name.
     **/
    static class SplitHierarchy implements Policy {
        private static class HierarchyException extends RuntimeException {
            public HierarchyException(String s) {
                super(s);
            }
        }

        /**
         * Replacement represents <code>changes</code> in the types of the
         * subcells to the <code>prototype</code> cell.
         **/
        private class Replacement implements Policy {
            private final CellInterface prototype;
            private final Map<HierName,Replacement> changes;
            private String subtype;

            public Replacement(final CellInterface prototype) {
                this.prototype = prototype;
                this.changes = new HashMap<HierName,Replacement>();
                this.subtype = null;
            }

            public Replacement getSubcell(final HierName inst) {
                return changes.get(inst);
            }

            public Replacement putSubcell(final HierName inst,
                                          final Replacement replace) {
                return changes.put(inst, replace);
            }

            public void setSubtype(final String subtype) {
                this.subtype = subtype;
            }

            public Decision getDecision(final CellInterface cell)
                throws IOException {
                final String module;
                final String next;
                final Writer writer;
                final Map extraDirs;

                if (changes.isEmpty()) {
                    final int dot = subtype.lastIndexOf('.');
                    assert dot > 0;
                    module = subtype.substring(0, dot);
                    next = subtype.substring(dot + 1);
                    writer = null;
                    extraDirs = Collections.EMPTY_MAP;
                } else {
                    module = cell.getModuleName();
                    final File dir = cellPath(path, module);
                    next = findNextSubtype(dir, minSubtype, maxSubtype);
                    writer = openFile(dir, next + ".cast");
                    setSubtype(module + "." + next);

                    extraDirs = getExtraDirs(cell);
                }

                return new Decision(module, next, getParentName(cell),
                                    Collections.EMPTY_MAP, extraDirs, writer,
                                    getHeader(header, "split-by-hierarchy"),
                                    getAttributes(cell), null, null);
            }

            public Pair getSubtype(final CellInterface parent,
                                   final CellInterface subcell,
                                   final HierName instance) {
                final Replacement replace = getSubcell(instance);
                final String newType;
                if (replace == null) {
                    newType = subcell.getFullyQualifiedType();
                } else {
                    try {
                        newType = getReplacementType(replace);
                    } catch (IOException e) {
                        System.err.println("Cannot create a new subtype for: " +
                                           subcell.getFullyQualifiedType());
                        return null;
                    }
                }
                return new Pair(getParentName(parent, instance), newType);
            }

            private void updateSpec(Replacement lastRouted, HierName path,
                                    final Collection<SubtypeSplit.Spec> spec) {
                if (CellUtils.isRouted(prototype)) {
                    if (!changes.isEmpty()) {
                        if (subtype == null) {
                            try {
                                subtype = getReplacementType(this);
                            } catch (IOException e) {
                                throw new RuntimeException(
                                    "Cannot create a new subtype for: " +
                                    prototype.getFullyQualifiedType(), e);
                            }
                        }
                        spec.add(new CopySpec(prototype.getFullyQualifiedType(),
                                              subtype));
                    }
                    lastRouted = this;
                    path = null;
                }
                for (Map.Entry<HierName,Replacement> change :
                        changes.entrySet()) {
                    final HierName full =
                        HierName.append(path, change.getKey());
                    final Replacement subcell = change.getValue();
                    if (CellUtils.isRouted(subcell.prototype)) {
                        if (subcell.subtype == null) {
                            try {
                                getReplacementType(subcell);
                            } catch (IOException e) {
                                throw new RuntimeException(
                                    "Cannot create a new subtype for: " +
                                    subcell.prototype.getFullyQualifiedType(),
                                    e);
                            }
                        }
                        spec.add(new ChangeSpec(
                                    lastRouted.subtype,
                                    full.getAsString('.'),
                                    subcell.prototype.getFullyQualifiedType(),
                                    subcell.subtype));
                    }
                    if (!subcell.changes.isEmpty()) {
                        subcell.updateSpec(lastRouted, full, spec);
                    }
                }
            }

            public void updateSpec(final Collection<SubtypeSplit.Spec> spec) {
                // always assume the top-level cell is routed
                updateSpec(this, null, spec);
            }

            public boolean equals(final Object o) {
                if (o instanceof Replacement) {
                    return equals((Replacement) o);
                } else {
                    return false;
                }
            }

            public boolean equals(final Replacement o) {
                if (subtype != null && o.subtype != null) {
                    return subtype.equals(o.subtype);
                } else {
                    if (prototype == o.prototype) {
                        return changes.equals(o.changes);
                    } else {
                        return false;
                    }
                }
            }

            public int hashCode() {
                return prototype.getFullyQualifiedType().hashCode() +
                       changes.hashCode();
            }
        }

        private final File path;
        private final Format header;
        private final CellInterface cell;
        private final Replacement replacement;
        private final Collection<Pair<HierName,String>> instances;
        private final int minSubtype, maxSubtype;
        private final Map<Replacement,Replacement> written;
        private final Collection<SubtypeSplit.Spec> spec;

        public SplitHierarchy(
                final String path, final Format header,
                final CellInterface cell,
                final Collection<Pair<HierName,String>> instances,
                final int minSubtype, final int maxSubtype,
                final Collection<SubtypeSplit.Spec> spec,
                final Collection<String> errors) {
            this.path = new File(path);
            this.cell = cell;
            this.header = header;
            this.replacement = new Replacement(cell);
            this.instances = instances;
            this.minSubtype = minSubtype;
            this.maxSubtype = maxSubtype;
            this.written = new HashMap<Replacement,Replacement>();
            this.spec = spec;
            for (Pair<HierName,String> inst : instances) {
                try {
                    traverse(cell, replacement, inst.getFirst(),
                             inst.getSecond());
                } catch (HierarchyException e) {
                    errors.add(inst.getFirst() + " " + e.getMessage());
                }
            }
        }

        private Replacement traverse(final CellInterface cell,
                                     Replacement replace,
                                     final HierName inst,
                                     final String sub) {
            if (replace == null) replace = new Replacement(cell);
            if (inst == null) {
                replace.setSubtype(sub);
                return replace;
            }

            boolean found = false;
            for (Pair<HierName,HierName> split :
                    new IterableIterator<Pair<HierName,HierName>>(
                        HierName.getSplitsIterator(inst))) {
                final HierName prefix = split.getFirst();
                final HierName suffix = split.getSecond();
                final CellInterface subcell = cell.getSubcell(prefix);
                if (subcell != null && !cell.isInlinedSubcell(prefix)) {
                    replace.putSubcell(
                        prefix,
                        traverse(subcell, replace.getSubcell(prefix),
                                 suffix, sub));
                    found = true;
                    break;
                }
            }

            if (!found) {
                throw new HierarchyException(
                    cell.getFullyQualifiedType() + " " + inst);
            }

            return replace;
        }

        private String getReplacementType(final Replacement replace)
            throws IOException {
            final Replacement canon = written.get(replace);
            if (canon == null) {
                writeSubtype(replace, replace.prototype);
                written.put(replace, replace);
            } else {
                replace.setSubtype(canon.subtype);
            }
            return replace.subtype;
        }

        private Decision getDummyDecision(final CellInterface cell,
                                          final Replacement r)
            throws IOException {
            final String subtype = getReplacementType(r);
            final int dot = subtype.lastIndexOf('.');
            final String module = subtype.substring(0, dot);
            final String type = subtype.substring(dot + 1);
            return new Decision(module, type, getParentName(cell),
                                Collections.EMPTY_MAP, null, "",
                                getAttributes(cell));
        }

        public Decision getDecision(final CellInterface cell)
            throws IOException {
            writeSubtype(replacement, cell);
            replacement.updateSpec(spec);
            return getDummyDecision(cell, replacement);
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            throw new RuntimeException("Shouldn't be called");
        }
    }

    /**
     * Implements the --size mode policy: Use the specified subtype as
     * indicated by the cell name and overwrite.  If trial mode is false, then
     * set fixed_size = true, otherwise, persist the fixed_size directive.
     **/
    public static class Size implements Policy {
        private final File path;
        private final boolean trial;
        private final Format header;
        private final float tau;
        private final Map capMap;
        private final boolean updateDirective;

        public Size(final String path, final boolean trial, final Format header,
                    final float tau, final Map capMap,
                    final boolean updateDirective) {
            this.path = new File(path);
            this.trial = trial;
            this.header = header;
            this.tau = tau;
            this.capMap = capMap;
            this.updateDirective = updateDirective;
        }

        private void dumpCap(final DirectiveSource src, final String type) {
            final List l = (List) capMap.get(type);
            if (l != null) {
                for (Iterator i = l.iterator(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final Triplet t = (Triplet) p.getSecond();
                    if (t.getFirst() != null) {
                        src.definition(DirectiveConstants.CAP,
                                       DirectiveConstants.NODE_TYPE,
                                       p.getFirst(), t.getFirst());
                    }
                    if (t.getSecond() != null) {
                        src.definition(DirectiveConstants.ESTIMATED_DELAY,
                                       DirectiveConstants.HALFOP_TYPE,
                                       new Pair(p.getFirst(), Boolean.TRUE),
                                       t.getSecond());
                    }
                    if (t.getThird() != null) {
                        src.definition(DirectiveConstants.ESTIMATED_DELAY,
                                       DirectiveConstants.HALFOP_TYPE,
                                       new Pair(p.getFirst(), Boolean.FALSE),
                                       t.getThird());
                    }
                }
            }
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            // When sizing, the cell's name will contain the subtype part.
            final String subtyped = cell.getType();

            final Writer writer;
            final Map directives;

            // If a leaf cell is fixed size, there is no point overwriting it
            // with the exact same information.
            if (updateDirective || !isFixed(cell)) {
                directives = new HashMap();
                final File dir = cellPath(path, module);
                writer = openFile(dir, subtyped + ".cast");

                final DirectiveSource top =
                    new DirectiveSource(BlockInterface.CELL);
                directives.put(BlockInterface.CELL, top);
                if (!trial) {
                    top.definition(DirectiveConstants.FIXED_SIZE, Boolean.TRUE);
                }
                if (!isFixed(cell) && !Float.isNaN(tau)) {
                    top.definition(DirectiveConstants.TAU, new Float(tau));
                }
                final String capType =
                    CellUtils.isLeaf(cell) ? BlockInterface.PRS :
                                             BlockInterface.SUBCELL;
                final DirectiveSource cap = new DirectiveSource(capType);
                dumpCap(cap, type);
                directives.put(capType, cap);
            } else {
                directives = Collections.EMPTY_MAP;
                writer = null;
            }

            return new Decision(module, subtyped, getParentName(cell),
                                directives, writer, getHeader(header, "size"),
                                getAttributes(cell));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            return new Pair(getParentName(parent, instance), type);
        }
    }

    /**
     * Implements subtype merge policy.
     **/
    public static class Merge implements Policy {
        private final File path;
        private final Format header;
        private final SortedMap typeMap;
        private final Set seen;
        private final boolean writeLeaf;
        private final Collection<SubtypeSplit.Spec> spec;

        public Merge(final String path, final Format header,
                     final SortedMap typeMap) {
            this(path, header, typeMap, false,
                 new HashSet<SubtypeSplit.Spec>());
        }

        /**
         * Construct the Merge policy.
         * @param path Subtype path.
         * @param header A header that is put at the top of subtypes written.
         * @param typeMap A map that tells what subtypes should become.  It is
         * really a map from cell name -&gt; (subtype -&gt; subtype).
         * @param writeLeaf Should leaves cells, which are never changed by a
         * merge, be written out also?
         **/
        public Merge(final String path, final Format header,
                     final SortedMap typeMap, final boolean writeLeaf,
                     final Collection<SubtypeSplit.Spec> spec) {
            this.path = new File(path);
            this.header = header;
            this.typeMap = typeMap;
            this.seen = new LinkedHashSet();
            this.writeLeaf = writeLeaf;
            this.spec = spec;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String type = cell.getFullyQualifiedType();
            final String module = cell.getModuleName();
            final String subtyped = cell.getType();
            final File dir = cellPath(path, module);
            final Writer writer =
                (!writeLeaf && CellUtils.isLeaf(cell)) ? null : openFile(dir, subtyped + ".cast", cell);

            seen.add(type);

            return new Decision(module, subtyped, getParentName(cell),
                                Collections.EMPTY_MAP, getExtraDirs(cell),
                                writer, getHeader(header, "merge"),
                                getAttributes(cell), null, null);
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            final String type = subcell.getFullyQualifiedType();
            final String module = subcell.getModuleName();
            final String subtype = subcell.getType();
            final SortedMap subtypeMap = (SortedMap) typeMap.get(module);
            final Pair p1 = (Pair) subtypeMap.get(subtype);
            final Pair p2 = (Pair) subtypeMap.get(p1.getSecond());
            final CellInterface cell = (CellInterface) p2.getFirst();
            final String newType = cell.getFullyQualifiedType();
            if (!seen.contains(newType)) {
                try {
                    writeSubtype(this, cell);
                } catch (IOException e) {
                    System.err.println("Cannot overwrite subtype " + newType + "!");
                    return null;
                }
            }
            if (!newType.equals(subcell.getFullyQualifiedType())) {
                spec.add(new ChangeSpec(parent.getFullyQualifiedType(),
                                        instance.getAsString('.'),
                                        subcell.getFullyQualifiedType(),
                                        newType));
            }
            return new Pair(getParentName(parent, instance), newType);
        }
    }

    /**
     * Implements the update policy.  One shot usage.  Should only be applied
     * to leaf cells.
     **/
    public static class Update implements Policy {
        private final File path;
        private final Format header;
        private final Pair to;

        public Update(final String path, final Format header, final Pair to) {
            this.path = new File(path);
            this.header = header;
            this.to = to;
        }

        public Decision getDecision(final CellInterface cell) throws IOException {
            final String module = cell.getFullyQualifiedType();
            final String subtype = (String) to.getSecond();
            final CellInterface updatee = (CellInterface) to.getFirst();
            final Writer writer;
            if (CellUtils.isLeaf(cell)) {
                final File dir = cellPath(path, module);
                writer = openFile(dir, subtype + ".cast");
            } else {
                writer = null;
            }

            final Map directives = Subtype.getDirectives(cell);

            return new Decision(module, subtype, directives, writer,
                                getHeader(header, "update"),
                                getAttributes(updatee));
        }

        public Pair getSubtype(final CellInterface parent,
                               final CellInterface subcell,
                               final HierName instance) {
            throw new RuntimeException("Should not call getSubtype()!");
        }
    }

    /**
     * A specialized CDLEmitter that instead of printing out .SUBCKT and .ENDS
     * lines, prints out the netlist block header and footer.
     **/
    private static final class SubtypeEmitter extends CDLEmitter {
        private final IndentWriter iw;
        public SubtypeEmitter(final IndentWriter iw, final String lengthUnit) {
            super(iw, lengthUnit, 6);
            this.iw = iw;
        }
        public void subcktBegin(final AbstractNetlist netlist) {
            print("netlist {");
            println();
            iw.nextLevel();
        }
        public void subcktEnd(final AbstractNetlist netlist) {
            iw.prevLevel();
            print("}");
            println();
        }
    }

    /**
     * Returns the string representation of an non-arrayed type.
     **/
    private static String nonArrayTypeString(final PortTypeInterface type) {
        if (type instanceof ChannelType) {
            final ChannelType channel = (ChannelType) type;
            final int width = channel.getWidth();
            if (channel.isArrayed()) {
                return channel.getTypeName() + "[" + width + "]";
            } else {
                assert width == 1;
                return channel.getTypeName();
            }
        } else if (type instanceof NodeType) {
            final NodeType node = (NodeType) type;
            final int width = node.getWidth();
            if (node.isArrayed()) {
                return "node[" + width + "]";
            } else {
                assert width == 1;
                return "node";
            }
        } else if (type instanceof StructureType) {
            final String tag = ((StructureType) type).getTag();
            if (tag == null) return type.toString();
            else return tag;
        } else {
            Debug.assertTrue(false, "Unknown non-arrayed type " + type.getClass() + "!");
            return null;
        }
    }

    /**
     * Returns the string representation of an ArrayType.  The Pair contains
     * (range, string representation of the arrayed type).
     **/
    private static Pair arrayTypeString(final ArrayType type) {
        final int min = type.getMinIndex();
        final int max = type.getMaxIndex();
        final PortTypeInterface arrayedType = type.getArrayedType();
        final String expr = min + ".." + max;
        if (arrayedType instanceof ArrayType) {
            final Pair sub = arrayTypeString((ArrayType) arrayedType);
            return new Pair(expr + "," + sub.getFirst(),
                            sub.getSecond());
        } else {
            return new Pair(expr, nonArrayTypeString(arrayedType));
        }
    }

    /**
     * Returns the string representation for a port in a cell.  The result is
     * suitable for reemitting CAST when showDir is true.
     **/
    public static String portString(PortDefinition port, boolean showDir) {
        final PortTypeInterface type = port.getType();
        final StringBuffer result = new StringBuffer();
        String dir = "";
        if (showDir) {
            switch (port.getDirection()) {
              case PortDefinition.IN:
                dir = "-";
                break;
              case PortDefinition.OUT:
                dir = "+";
                break;
              case PortDefinition.INOUT:
                dir = "-+";
                break;
            }
        }
        if (type instanceof ArrayType) {
            final Pair info = arrayTypeString((ArrayType) type);
            result.append((String) info.getSecond());
            result.append(" ");
            result.append(dir);
            result.append(port.getName());
            result.append("[");
            result.append((String) info.getFirst());
            result.append("]");
        } else {
            result.append(nonArrayTypeString(type));
            result.append(" ");
            result.append(dir);
            result.append(port.getName());
        }
        return result.toString();
    }

    private static String portString(PortDefinition port) {
        return portString(port, true);
    }

    /**
     * Returns a string representation of the header of a refinement of a cell
     * in the CAST language.
     * @param ci Cell whose port to copy.
     * @param fromName Name of the cell refined from.
     * @param toName Name of the cell refined to.
     * @param attributes Attribute cells to inherit from
     * @param explicitPortList If <code>true</code>, explicitly emit the port
     * list of the cell; otherwise, omit the port list.
     **/
    private static String castHeader(final CellInterface ci,
                                     final String fromName,
                                     final String toName,
                                     final String[] attributes,
                                     final boolean explicitPortList) {
        final StringBuffer buf = new StringBuffer();
        buf.append("define ");
        buf.append(toName);
        buf.append("()");

        if (explicitPortList) {
            buf.append("(");
            boolean first = true;
            for (Iterator ports = ci.getPortDefinitions(); ports.hasNext(); ) {
                final PortDefinition port = (PortDefinition) ports.next();
                if (!ci.isImpliedPort(port.getName())) {
                    if (first) first = false;
                    else buf.append("; ");
                    buf.append(portString(port));
                }
            }
            buf.append(")");
        }

        if (attributes != null) {
            for (int i = 0; i < attributes.length; ++i) {
                buf.append(" <+ " + attributes[i]);
            }
        }
        buf.append(" <: ");
        buf.append(fromName);
        buf.append(" {");
        return buf.toString();
    }

    /**
     * Write out a header appropriate for subtypes, omitting the port list.
     **/
    public static void writeHeader(final Writer w,
                                   final String module,
                                   final String from,
                                   final String subtype,
                                   final CellInterface ci,
                                   final String[] attributes,
                                   final String header)
        throws IOException {
        writeHeader(w, module, from, subtype, ci, attributes, header, false);
    }

    /**
     * Write out a header appropriate for subtypes.
     **/
    public static void writeHeader(final Writer w,
                                   final String module,
                                   final String from,
                                   final String subtype,
                                   final CellInterface ci,
                                   final String[] attributes,
                                   final String header,
                                   final boolean explicitPortList)
        throws IOException {
        w.write("/* Copyright " + (new GregorianCalendar()).get(Calendar.YEAR) +  " Intel Corporation.  All rights reserved.\n");
        // Prevent Perforce from doing substitution on the string literal
        w.write(" * $" + "Id$\n");
        w.write(" * $" + "DateTime$\n");
        w.write(" * $" + "Author$\n");
        w.write(" */\n");
        w.write("/* Automatically generated.  Modify at your own risk. */\n");
        if (header != null) {
            w.write("/*\n"); w.write(header); w.write("*/\n");
        }

        w.write("module " + CellUtils.hashMetaParameters(module) + ";\n");
        w.write(castHeader(ci, from, "\"" + subtype + "\"", attributes,
                           explicitPortList));
        w.write('\n');
    }

    /**
     * Write out a CDL template inside a netlist block.
     **/
    private static String writeTemplate(final Policy policy,
                                        final CellInterface cell) throws IOException
    {
        final Decision decision = policy.getDecision(cell);
        if (decision.writer != null) {
            final IndentWriter iw = new IndentWriter(decision.writer);
            final CellInterface contentCell =
                decision.contentCell == null ? cell : decision.contentCell;
            writeHeader(iw, decision.module, decision.from, decision.subtype,
                        contentCell, decision.attributes, decision.header);
            iw.nextLevel();

            iw.write("netlist {\n");
            iw.nextLevel();

            final CDLFactoryEmitter emitter = new CDLFactoryEmitter(iw);
            final BlockInterface cellBlock = contentCell.getBlockInterface();
            final NetlistBlock block = (NetlistBlock) cellBlock.iterator(BlockInterface.NETLIST).next();
            final Template templ = block.getCDLTemplate();
            if (templ == null) throw new RuntimeException("Netlist block does not exist for cell " + contentCell.getFullyQualifiedType());
            templ.execute(emitter);

            iw.prevLevel();
            iw.write("}\n");

            final CellInterface dirCell =
                decision.dirCell == null ? contentCell : decision.dirCell;
            final BlockInterface dirCellBlock = dirCell.getBlockInterface();
            emitDirective(dirCellBlock.iterator(BlockInterface.PRS).next(),
                          CastEmitter.getInstance(),
                          (DirectiveSource)
                            decision.directives.get(BlockInterface.PRS),
                          iw,
                          true);

            emitDirective(dirCellBlock, CastEmitter.getInstance(),
                          (DirectiveSource)
                            decision.directives.get(BlockInterface.CELL),
                          iw,
                          false);

            iw.prevLevel();
            iw.write("}\n");

            iw.close();
        }
        return decision.module + "." + decision.subtype;
    }

    /**
     * Write out a Netgraph inside a netlist block.
     **/
    private static String writeNetgraph(final Policy policy,
                                        final AbstractNetlist netlist,
                                        final String lengthUnit,
                                        final CellType cell) throws IOException
    {
        final CellInterface ci = cell.cast_cell;
        final Decision decision = policy.getDecision(ci);
        if (decision.writer != null) {
            final IndentWriter iw = new IndentWriter(decision.writer);
            final CellInterface contentCell =
                decision.contentCell == null ? ci : decision.contentCell;
            writeHeader(iw, decision.module, decision.from, decision.subtype,
                        contentCell, decision.attributes, decision.header);
            iw.nextLevel();

            if (!cell.transistors.isEmpty()) {
                final CDLEmitter emitter = new SubtypeEmitter(iw, lengthUnit);
                emitter.format(netlist, emitter, false);

                final CellInterface dirCell =
                    decision.dirCell == null ? contentCell : decision.dirCell;
                final BlockInterface cellBlock = dirCell.getBlockInterface();
                emitDirective(cellBlock.iterator(BlockInterface.PRS).next(),
                              CastEmitter.getInstance(),
                              (DirectiveSource)
                                decision.directives.get(BlockInterface.PRS),
                              iw,
                              true);

                emitDirective(cellBlock, CastEmitter.getInstance(),
                              (DirectiveSource)
                                decision.directives.get(BlockInterface.CELL),
                              iw,
                              false);
            }

            iw.prevLevel();
            iw.write("}\n");

            iw.close();
        }
        return decision.module + "." + decision.subtype;
    }

    /**
     * Write out an empty cell body for when the cell acts like an internal
     * environment
     **/
    private static String writeEmptyBody(final Policy policy,
                                         final CellInterface ci)
        throws IOException {
        final Decision decision = policy.getDecision(ci);
        if (decision.writer != null) {
            final IndentWriter iw = new IndentWriter(decision.writer);
            final CellInterface contentCell =
                decision.contentCell == null ? ci : decision.contentCell;
            writeHeader(iw, decision.module, decision.from, decision.subtype,
                        contentCell, decision.attributes, decision.header);
            iw.write("}\n");
            iw.close();
        }
        return decision.module + "." + decision.subtype;
    }

    /**
     * Given a HierName of the form x[1,2].y[3,4] returns x[.y[.
     **/
    private static String getArrayBase(final String s) {
        final int len = s.length();
        final StringBuffer buf = new StringBuffer(len);
        boolean bracket = false;
        for (int i = 0; i < len; ++i) {
            final char c = s.charAt(i);
            if (!bracket) buf.append(c);
            if (c == '[') bracket = true;
            else if (c == ']') bracket = false;
        }
        return buf.toString();
    }

    /**
     * Write out a midlevel cell inside a subtypes block.
     **/
    private static String writeMidlevel(final Policy policy,
                                        final CellInterface cell) throws IOException
    {
        final Decision decision = policy.getDecision(cell);
        if (decision.writer != null) {
            final IndentWriter iw = new IndentWriter(decision.writer);
            final CellInterface contentCell =
                decision.contentCell == null ? cell : decision.contentCell;
            writeHeader(iw, decision.module, decision.from, decision.subtype,
                        contentCell, decision.attributes, decision.header);
            iw.nextLevel();

            iw.write("subtypes {\n");
            iw.nextLevel();
            for (Iterator i = contentCell.getLocalSubcellPairs();
                 i.hasNext(); ) {
                final Pair pair = (Pair) i.next();
                final CellInterface subcell = (CellInterface) pair.getSecond();
                if (!CellUtils.hasRouted(subcell) &&
                    CellUtils.isWiring(subcell)) continue;
                final HierName instanceName = (HierName) pair.getFirst();

                final Pair types =
                    policy.getSubtype(contentCell, subcell, instanceName);

                final String oldType = (String) types.getFirst();
                final String newType = (String) types.getSecond();
                Debug.assertTrue(newType != null, "No subtype for type " + subcell.getFullyQualifiedType() + " in " + contentCell.getFullyQualifiedType() + "!");
                iw.write(oldType + " :>\n");
                iw.nextLevel();
                iw.write(CellUtils.hashMetaParameters(newType) + " " + instanceName.getCadenceString() + ";\n");
                iw.prevLevel();
            }
            final CellInterface dirCell =
                decision.dirCell == null ? contentCell : decision.dirCell;
            final BlockInterface cellBlock = dirCell.getBlockInterface();
            emitDirective(cellBlock.iterator(BlockInterface.SUBCELL).next(),
                          CastEmitter.getInstance(),
                          (DirectiveInterfaceFactory)
                            decision.directives.get(BlockInterface.SUBCELL),
                          (DirectiveInterfaceFactory)
                            decision.extraDirs.get(BlockInterface.SUBCELL),
                          iw,
                          false);
            iw.prevLevel();
            iw.write("}\n");

            emitDirective(cellBlock, CastEmitter.getInstance(),
                          (DirectiveInterfaceFactory)
                            decision.directives.get(BlockInterface.CELL),
                          (DirectiveInterfaceFactory)
                            decision.extraDirs.get(BlockInterface.CELL),
                          iw,
                          false);

            iw.prevLevel();
            iw.write("}\n");

            iw.close();
        }
        return decision.module + "." + decision.subtype;
    }

    /**
     * Write subtypes.
     * @param policy Subtype policy to consult.
     * @param cell Cell to write out.
     * @param subtypeProc Transformation to be done on a Netgraph before
     * outputting it.
     * @param cutoff Largest transistor size not folded.
     * @param cutoffmin Smallest transistor size after folding.
     **/
    public static String writeSubtype(final Policy policy,
                                      final CellType cell,
                                      final DeviceProcessor subtypeProc,
                                      final String lengthUnit,
                                      final double cutoff,
                                      final double cutoffmin,
                                      final Map stackMap,
                                      final HierName GND,
                                      final HierName Vdd,
                                      final float minWidth) throws IOException {
        final String subtype;
        final CellInterface ci = cell.cast_cell;
        final boolean midRouted =
            CellUtils.hasRouted(ci) && ci.containsCompleteSubcells();
        if ((CellUtils.isLeaf(ci) && !midRouted) || cell.isInternalEnv()) {
            if (!cell.isInternalEnv() && CellUtils.isFixedSize(ci)) {
                subtype = writeTemplate(policy, ci);
            } else {
                final AbstractNetlist netlist = new FoldTransistor(new NetlistAdapter(cell, false, stackMap, GND, Vdd, minWidth), cutoff, cutoffmin);
                subtypeProc.setNetlist(netlist);
                subtype = writeNetgraph(policy, subtypeProc, lengthUnit, cell);
            }
        } else {
            subtype = writeMidlevel(policy, ci);
        }
        return subtype;
    }

    public static String writeSubtype(final Policy policy,
                                      final CellInterface cell) throws IOException {
        final String subtype;
        final boolean midRouted =
            CellUtils.hasRouted(cell) && cell.containsCompleteSubcells();
        if (CellUtils.isInternalEnv(cell)) {
            subtype = writeEmptyBody(policy, cell);
        } else if (CellUtils.isLeaf(cell) && !midRouted) {
            subtype = writeTemplate(policy, cell);
        } else {
            subtype = writeMidlevel(policy, cell);
        }
        return subtype;
    }
}
