package com.avlsi.tools.synthesis;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.LinkedList;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.Rename;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.liberty.parser.*;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionVisitorInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.UnionCommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;

public class GenerateProteusLib {
    /** Wraps CDLNameInterface to discard checked exceptions */
    private class WrappedNameInterface {
        public final CDLNameInterface ni;
        public WrappedNameInterface(CDLNameInterface ni) {
            this.ni = ni;
        }
        public String renameCell(final String old) {
            try { return ni.renameCell(old); }
            catch (CDLRenameException e) { throw new RuntimeException(e); }
        }
        public String renameNode(final String old) {
            try { return ni.renameNode(old); }
            catch (CDLRenameException e) { throw new RuntimeException(e); }
        }
        public String renameSubCellInstance(final String old) {
            try { return ni.renameSubCellInstance(old); }
            catch (CDLRenameException e) { throw new RuntimeException(e); }
        }
    }

    private final CastFileParser cfp;
    private final LibertyParser parser;
    private final Cadencize cad;

    /** From input lib to CAST */
    private final WrappedNameInterface libToCast;

    /** From CAST to output lib */
    private final WrappedNameInterface castToLib;

    /** List of pin name groups to rename */
    private static final Set<String> PIN_GROUPS = new HashSet<>(Arrays.asList(
        "library.cell.pin",
        "library.cell.pg_pin"
    ));

    /** List of pin name attributes to rename */
    private static final Set<String> PIN_ATTRS = new HashSet<>(Arrays.asList(
        "library.cell.pin.timing.related_pin"
    ));

    public GenerateProteusLib(final CastFileParser cfp,
                              final LibertyParser parser,
                              final CDLNameInterface libToCast,
                              final CDLNameInterface castToLib) {
        this.cfp = cfp;
        this.parser = parser;
        this.cad = new Cadencize(true, Cadencize.NETLIST_PRIORITY);
        this.libToCast = new WrappedNameInterface(libToCast);
        this.castToLib = new WrappedNameInterface(castToLib);
    }

    /** Returns the top-level "library" group */
    private LibertyGroup getLibraryGroup() {
        return parser.getGroups().next();
    }

    /** Adds a new define, if it doesn't exist already */
    private void addDefineIfNeeded(final LibertyGroup group,
                                   final String defineName,
                                   final String allowedGroup,
                                   final int valueType) {
        if (CollectionUtils.stream(group.getDefines())
                           .noneMatch(
                                   d -> defineName.equals(d.getName()) &&
                                        allowedGroup.equals(
                                            d.getAllowedGroupName()) &&
                                        valueType == d.getValueType())) {
            group.createDefine(defineName, allowedGroup, valueType);
        }
    }

    /** Sets the value of a simple attribute */
    private void setSimpleAttribute(final LibertyGroup group,
                                    final String name,
                                    final Object val) {
        LibertyAttr attr = group.findAttrByName(name);
        if (attr == null) {
            attr = group.createAttr(name, LibertyParser.SI2DR_SIMPLE);
        }
        LibertySimpleAttr simpleAttr = attr.getSimpleAttr();
        simpleAttr.setValue(val);
    }

    /** Renames names of cell groups using renamers */
    public void renameCells() {
        final LibertyGroup libraryGroup = getLibraryGroup();
        CollectionUtils.stream(libraryGroup.getGroups())
                       .filter(g -> g.getGroupType().equals("cell"))
                       .forEach(g -> {
                               String name = g.getNames().next();
                               g.deleteName(name);
                               g.addName(
                                   castToLib.renameCell(
                                       libToCast.renameCell(name)));
                       });
    }

    /** Returns the types of the group as a dot delimited string */
    private String getGroupPath(List<LibertyGroup> path) {
        return path.stream()
                   .map(LibertyGroup::getGroupType)
                   .collect(Collectors.joining("."));
    }

    /** Recursively executes the consumer on all groups */
    private void recurseGroup(final LibertyGroup group,
                              final LinkedList<LibertyGroup> path,
                              final Consumer<LinkedList<LibertyGroup>> consumer) {
        path.addLast(group);
        consumer.accept(path);
        CollectionUtils.stream(group.getGroups())
                       .forEachOrdered(g -> { recurseGroup(g, path, consumer); } );
        path.removeLast();
    }

    /** Renames names of pins, both in group names, and in simple attributes */
    public void renamePins() {
        recurseGroup(getLibraryGroup(), new LinkedList<LibertyGroup>(),
            p -> {
                final LibertyGroup g = p.getLast();
                final String gpath = getGroupPath(p);

                // rename groups
                if (PIN_GROUPS.contains(gpath)) {
                    String name = g.getNames().next();
                    g.deleteName(name);
                    g.addName(
                        castToLib.renameNode(
                            libToCast.renameNode(name)));
                }

                // rename simple attributes
                CollectionUtils.stream(g.getAttrs())
                               .filter(a -> PIN_ATTRS.contains(gpath + "." +
                                                               a.getName()))
                               .map(LibertyAttr::getSimpleAttr)
                               .forEach(sa -> {
                                   sa.setStringValue(
                                       castToLib.renameNode(
                                           libToCast.renameNode(
                                               sa.getStringValue())));
                               });
            });
    }

    private String getFunction(final BooleanExpressionInterface expr,
                               final AliasedSet aliases) {
        final StringBuilder sb = new StringBuilder();
        expr.visitWith(
            new BooleanExpressionVisitorInterface() {
                private void junction(final boolean sense,
                                      final Collection terms,
                                      final char op) {
                    if (!sense) sb.append('!');
                    sb.append('(');
                    boolean first = true;
                    for (BooleanExpressionInterface term :
                            (Collection<BooleanExpressionInterface>) terms) {
                        if (first) first = false;
                        else sb.append(op);
                        term.visitWith(this);
                    }
                    sb.append(')');
                }
                public void visit(AndBooleanExpressionInterface and) {
                    junction(and.getSense(), and.getConjuncts(), '*');
                }
                public void visit(OrBooleanExpressionInterface or) {
                    junction(or.getSense(), or.getDisjuncts(), '+');
                }
                public void visit(HierNameAtomicBooleanExpression atom) {
                    sb.append('(');
                    if (!atom.getSense()) {
                        sb.append('!');
                    }
                    sb.append(castToLib.renameNode(
                                ((HierName) aliases.getCanonicalKey(
                                    atom.getName())).getAsString('.')));
                    sb.append(')');
                }
            });
        return sb.toString();
    }

    private HierName canonize(final AliasedSet aliases, final String name) {
        try {
            return (HierName) aliases.getCanonicalKey(
                        HierName.makeHierName(name, '.'));
        } catch (InvalidHierNameException e) {
            throw new RuntimeException(e);
        }
    }

    /** Annotates proteus directives */
    public void annotateDirective() {
        final LibertyGroup libraryGroup = getLibraryGroup();
        addDefineIfNeeded(libraryGroup, "proteus_cell_type", "cell",
                          LibertyParser.SI2DR_STRING);

        final Map<String,LibertyGroup> cells =
            CollectionUtils.stream(libraryGroup.getGroups())
                           .filter(g -> g.getGroupType().equals("cell"))
                           .collect(Collectors.toMap(
                                       g -> libToCast.renameCell(
                                           g.getNames().next()),
                                       Function.identity()));
        for (Map.Entry<String,LibertyGroup> entry : cells.entrySet()) {
            final String cellName = entry.getKey();
            final LibertyGroup cellGroup = entry.getValue();
            try {
                final CellInterface cell = cfp.getFullyQualifiedCell(cellName);
                final AliasedSet aliases = cad.convert(cell).getLocalNodes();
                String cellType = (String)
                    DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.PROTEUS_CELL_TYPE);
                if (cellType == null) {
                    cellType = "seq_logic";
                }
                setSimpleAttribute(cellGroup, "proteus_cell_type", cellType);

                final Map<HierName,BooleanExpressionInterface> funcs =
                    (Map<HierName,BooleanExpressionInterface>)
                    DirectiveUtils.canonizeKey(aliases,
                        DirectiveUtils.getTopLevelDirective(cell,
                                DirectiveConstants.PROTEUS_LIBERTY_FUNCTION,
                                DirectiveConstants.NODE_TYPE));
                CollectionUtils.stream(cellGroup.getGroups())
                               .filter(g -> g.getGroupType().equals("pin"))
                               .forEach(g -> {
                                    HierName canon =
                                        canonize(aliases, g.getNames().next());
                                    BooleanExpressionInterface func =
                                        funcs.get(canon);
                                    if (func != null) {
                                        setSimpleAttribute(g, "function",
                                            getFunction(func, aliases));
                                    }
                               });
            } catch (CastSemanticException e) {
                System.err.println("Can't parse CAST for " + cellName);
            }
        }
    }

    /** Writes a liberty file */
    public void write(final String filename) {
        LibertyGroup first = parser.getGroups().next();
        parser.writeLibertyFile(filename, first);
    }

    public static void main(final String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs);
        final CommandLineArgs theArgs =
            new CachingCommandLineArgs(argsWithConfigs);

        final String cellName = theArgs.getArgValue("cell", null);
        final String castPath = theArgs.getArgValue("cast-path", ".");
        final StandardParsingOption spo = new StandardParsingOption(theArgs);
        final CastFileParser cfp = new CastFileParser(
                new FileSearchPath(castPath), "2", spo);
        final CellInterface cell = cfp.getFullyQualifiedCellPretty(cellName, 2);

        final String inputLib = theArgs.getArgValue("input-lib", null);
        final String outputLib = theArgs.getArgValue("output-lib", "/dev/stdout");
        final LibertyParser parser = new LibertyParser(inputLib);

        final String inputName = theArgs.getArgValue("input-names", "gds2");
        final String outputName = theArgs.getArgValue("output-names", "mw");

        final GenerateProteusLib gen = new GenerateProteusLib(cfp, parser,
                Rename.getInterface(inputName, "cast"),
                Rename.getInterface("cast", outputName));
        gen.annotateDirective();
        gen.renameCells();
        gen.renamePins();
        gen.write(outputLib);
    }
}
