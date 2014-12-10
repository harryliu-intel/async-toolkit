package com.avlsi.tools.cast2def;

import java.io.IOException;
import java.io.FileWriter;
import java.io.Writer;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;

import com.avlsi.file.cdl.util.rename.*;
import com.avlsi.file.common.HierName;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.Value;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cell.CellConstants;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.debug.Debug;

public class Cast2Def {
    static abstract class Formatter {
        protected final Writer w;
        protected final CDLNameInterface renamer;
        protected final CellInterface cell;
        protected final float wireLambda;
        private final Cadencize cad;
        private final Float widthDefault;
        private final Float spaceDefault;
        private final static float EPSILON = 0.00001f;
        public final static int WIDTH_DEFAULT_FACTOR = 7; // W=7*wireLambda
        public final static int SPACE_DEFAULT_FACTOR = 6; // S=6*wireLambda

        public Formatter(final Writer w, final CellInterface cell,
                         final Cadencize cad,
                         final float wireLambda, final Float widthDefault,
                         final Float spaceDefault) {
            this.w = w;
            this.cell = cell;
            this.cad = cad;
            this.renamer = new CadenceNameInterface();
            this.wireLambda = wireLambda;
            this.widthDefault = widthDefault;
            this.spaceDefault = spaceDefault;
        }

        protected String renameCell(final String cellName) {
            try {
                return renamer.renameCell(cellName);
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename cell " + cellName, e);
            }
        }

        protected String renameNet(final HierName net) {
            try {
                return renamer.renameNode(net.getAsString('.'));
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename node " + net, e);
            }
        }

        private int discretize(final Float val) {
            return (int) Math.ceil(val.floatValue() / wireLambda - EPSILON);
        }

        private String getRule(Float width, Float space, Boolean reset) {
            if (width == null) width = widthDefault;
            if (space == null) space = spaceDefault;
            if (reset == null) reset = Boolean.FALSE;

            int w = discretize(width);
            int s = discretize(space);

            if (w < WIDTH_DEFAULT_FACTOR) w = WIDTH_DEFAULT_FACTOR;
            if (s < SPACE_DEFAULT_FACTOR) s = SPACE_DEFAULT_FACTOR;

            if (reset.booleanValue()) {
                // return "RESET_WIRE"; // use special nondefault W and S
            }

            return w + "W" + s + "S";
        }

        private void putDirective(final CellInterface cell,
                                  final HierName prefix,
                                  final String dir,
                                  final AliasedMap ports,
                                  final Map result) {
            final Map dirs = DirectiveUtils.getSubcellDirective(
                    cell, dir, DirectiveConstants.NODE_TYPE);
            for (final Iterator i = dirs.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName local = (HierName) entry.getKey();
                if (prefix == null || !ports.contains(local)) {
                    result.put(HierName.append(prefix, local),
                               entry.getValue());
                }
            }
        }

        private void getRoutedDirective(final CellInterface cell,
                                        final HierName prefix,
                                        final Map wireWidth,
                                        final Map wireSpace,
                                        final Map resetNet,
                                        final Set nodes,
                                        final Set propagated) {
            if (propagated.add(cell)) {
                DirectiveUtils.propagateWireDirective(cell, cad);
            }

            final AliasedSet locals = cad.convert(cell).getLocalNodes();
            final AliasedMap ports = cad.convert(cell).getPortNodes();
            for (Iterator i = locals.getCanonicalKeys(); i.hasNext(); ) {
                final HierName local = (HierName) i.next();
                if (prefix == null || !ports.contains(local)) {
                    nodes.add(HierName.append(prefix, local));
                }
            }

            putDirective(cell, prefix, DirectiveConstants.WIREWIDTH, ports,
                         wireWidth);
            putDirective(cell, prefix, DirectiveConstants.WIRESPACE, ports,
                         wireSpace);
            putDirective(cell, prefix, DirectiveConstants.RESET_NET, ports,
                         resetNet);

            final Map m = DirectiveUtils.getSubcellDirective(cell,
                    DirectiveConstants.ROUTED,
                    DirectiveConstants.INSTANCE_TYPE);

            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                final Boolean inlineSubcell = (Boolean) m.get(subcellName);
                final boolean unrouted =
                    !CellUtils.isRouted(subcell) ||
                    (inlineSubcell != null && !inlineSubcell.booleanValue());

                if (unrouted && !subcell.isChannel() && !subcell.isNode() &&
                    !CellUtils.isLeaf(subcell)) {
                    getRoutedDirective(subcell,
                                       HierName.append(prefix, subcellName),
                                       wireWidth,
                                       wireSpace,
                                       resetNet,
                                       nodes,
                                       propagated);
                }
            }
        }

        protected Map findProperty() throws IOException {
            final Set propagated = new HashSet();
            final Set nodes = new HashSet();

            final Map wireWidth = new HashMap();
            final Map wireSpace = new HashMap();
            final Map resetNet = new HashMap();
            getRoutedDirective(cell, null, wireWidth, wireSpace, resetNet,
                               nodes, new HashSet());

            final Map result = new TreeMap();
            for (Iterator i = nodes.iterator(); i.hasNext(); ) {
                final HierName key = (HierName) i.next();
                final Float width = (Float) wireWidth.get(key);
                final Float space = (Float) wireSpace.get(key);
                final Boolean reset = (Boolean) resetNet.get(key);
                final String rule = getRule(width, space, reset);
                if (rule != null) result.put(key, rule);
            }

            return result;
        }

        protected void header() throws IOException { }
        protected void footer() throws IOException { }
        protected void nets() throws IOException { }

        public void output() throws IOException {
            header();
            nets();
            footer();
        }
    }

    static class DefFormatter extends Formatter {
        private final String version;
        private final String dividerChar;
        private final String busBitChars;
        private final String ruleDefault;
        private final UnaryPredicate filter;
        public DefFormatter(final String version, final String dividerChar,
                            final String busBitChars, final Writer w,
                            final CellInterface cell, final Cadencize cad,
                            final float wireLambda, final Float widthDefault,
                            final Float spaceDefault, final String ruleDefault,
                            final UnaryPredicate filter) {
            super(w, cell, cad, wireLambda, widthDefault, spaceDefault);
            this.version = version;
            this.dividerChar = dividerChar;
            this.busBitChars = busBitChars;
            this.ruleDefault = ruleDefault;
            this.filter = filter;
        }

        public DefFormatter(final Writer w, final CellInterface cell,
                            final Cadencize cad,
                            final float wireLambda,
                            final Float widthDefault, final Float spaceDefault,
                            final String ruleDefault,
                            final UnaryPredicate filter) {
            this("5.5", "|", "<>", w, cell, cad, wireLambda,
                 widthDefault, spaceDefault, ruleDefault, filter);
        }

        private static final MessageFormat HEADER = new MessageFormat(
            "VERSION {0} ;\n" +
            "NAMESCASESENSITIVE ON ;\n" +
            "DIVIDERCHAR \"{1}\" ;\n" +
            "BUSBITCHARS \"{2}\" ;\n" +
            "DESIGN {3} ;\n");

        protected void header() throws IOException {
            w.write(
                HEADER.format(
                    new Object[] {
                        version,
                        dividerChar,
                        busBitChars,
                        renameCell(cell.getFullyQualifiedType())}));
            w.write("# wire lambda = " + wireLambda + " default rule = " +
                    (ruleDefault == null ? "none" : ruleDefault) + "\n");
        }

        protected void footer() throws IOException {
            w.write("END DESIGN\n");
        }

        private void netRule(final HierName node, final String rule)
            throws IOException {
            w.write("  - " + renameNet(node) + " + NONDEFAULTRULE " + rule +
                    " ;\n");
        }

        protected void nets() throws IOException {
            final Map result = findProperty();
            int count = 0;
            for (Iterator i =
                    new FilteringIterator(result.entrySet().iterator(),
                                          filter); i.hasNext();
                 i.next(), ++count);
            w.write("NETS " + count + " ;\n");

            for (Iterator i =
                    new FilteringIterator(result.entrySet().iterator(),
                                          filter); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName node = (HierName) entry.getKey();
                final String rule = (String) entry.getValue();
                netRule(node, rule);
            }

            w.write("END NETS\n");
        }
    }

    static class SkillFormatter extends Formatter {
        private final String ruleDefault;
        private final UnaryPredicate filter;
        public SkillFormatter(final Writer w, final CellInterface cell,
                              final Cadencize cad,
                              final float wireLambda,
                              final Float widthDefault,
                              final Float spaceDefault,
                              final String ruleDefault,
                              final UnaryPredicate filter) {
            super(w, cell, cad, wireLambda, widthDefault, spaceDefault);
            this.ruleDefault = ruleDefault;
            this.filter = filter;
        }

        protected void header() throws IOException {
            w.write("; wire lambda = " + wireLambda + " default rule = " +
                    (ruleDefault == null ? "none" : ruleDefault) + "\n");
            w.write("NondefaultRoutingDirective=nil\n");
        }

        protected void nets() throws IOException {
            final Map result = findProperty();
            final MultiMap inverse = new MultiMap();

            for (Iterator i =
                    new FilteringIterator(result.entrySet().iterator(),
                                          filter); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName node = (HierName) entry.getKey();
                final String rule = (String) entry.getValue();
                inverse.put(rule, node);
            }

            for (Iterator i = inverse.keySet().iterator(); i.hasNext(); ) {
                final String rule = (String) i.next();
                w.write("NondefaultRoutingDirective=cons(");
                w.write(" list( \"" + rule + "\"");

                final Collection nets = (Collection) inverse.get(rule);
                for (Iterator j = nets.iterator(); j.hasNext(); ) {
                    w.write(" \"" + renameNet((HierName) j.next()) + "\"");
                }

                w.write(")\n");
                w.write("   NondefaultRoutingDirective)\n");
            }
        }
    }

    static class TxtFormatter extends Formatter {
        private final UnaryPredicate filter;
        public TxtFormatter(final Writer w, final CellInterface cell,
                            final Cadencize cad,
                            final float wireLambda,
                            final Float widthDefault, final Float spaceDefault,
                            final UnaryPredicate filter) {
            super(w, cell, cad, wireLambda, widthDefault, spaceDefault);
            this.filter = filter;
        }

        protected void nets() throws IOException {
            final Map result = findProperty();

            for (Iterator i =
                    new FilteringIterator(result.entrySet().iterator(),
                                          filter); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName node = (HierName) entry.getKey();
                final String rule = (String) entry.getValue();
                w.write(renameNet(node) + " " + rule + "\n");
            }
        }
    }

    private static void usage( String m ) {
        System.err.println(
"Usage: cast2def --cast-path=<CAST path>\n" +
"  --cell=<cell>\n" +
"  --outfile=<file> (output file)\n" +
"  [--format=[def|skill|txt] (output format of the wiring rules)]\n" +
"  [--cadence-name (cell name is a Cadence name)]\n" +
"  [--ignore-ports (do not generate output for ports)]\n" +
"  [--default-width=width (default wire width; 2*lambda)]\n" +
"  [--default-space=space (default wire space; 2*lambda)]\n" +
"  [--default-rule=rule (default rule name, not output; 7W6S)]\n" +
"  [--lambda=lambda (wire lambda)");
    if (m != null && m.length() > 0)
        System.err.println( m );
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        if (theArgs.argExists("version")) {
            System.out.println(com.avlsi.util.debug.VersionInfo.getVersionString
(Cast2Def.class));
        }

        final String castPath = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        final String outfile = theArgs.getArgValue("outfile", null);
        final String format = theArgs.getArgValue("format", "def");

        Float defaultWidth;
        Float defaultSpace;
        final Float lambda;
        try {
            defaultWidth =
                CommandLineArgsUtil.getFloatArgValue(theArgs, "default-width",
                                                     null);
            defaultSpace =
                CommandLineArgsUtil.getFloatArgValue(theArgs, "default-space",
                                                     null);
            lambda =
                CommandLineArgsUtil.getFloatArgValue(theArgs, "lambda", null);
        } catch (CommandLineArgFormatException e) {
            System.err.println("Invalid " + e.getArgName() + " argument.");
            System.exit(2);
            return;
        }

        if (cellName == null || outfile == null) {
            usage( "\n--cell and/or --outfile not specified" );
            System.exit(1);
        }
        if (!format.equals("skill") &&
             !format.equals("def") &&
             !format.equals("txt")) {
            usage( "\n--format must be one of skill, def, or txt" );
            System.exit(1);
        }

        final boolean cadenceName = theArgs.argExists("cadence-name");
        final String realCell = cadenceName ?
            new CadenceReverseNameInterface().renameCell(cellName)
          : cellName;

        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castPath), castVersion);
        final Writer w = new FileWriter(outfile);
        final CellInterface ci = castParser.getFullyQualifiedCell(realCell);

        final CellConstants consts = new CellConstants(castParser);
        final float wlambda;
        if (lambda == null) {
            final Environment topConsts =
                consts.getTopLevelConstants(ci.getFullyQualifiedType());
            final Value wireLambda =
                topConsts.lookup(Symbol.create("WIRE_LAMBDA"));
            if (!(wireLambda instanceof FloatValue)) {
                System.err.println("Wire lambda not specified on the " +
                                   "command line and undefined in CAST.");
                System.exit(2);
            }
            wlambda = FloatValue.valueOf(wireLambda).toFloat();
        } else {
            wlambda = lambda.floatValue();
        }

        if (defaultWidth == null)
            defaultWidth = new Float(wlambda * Formatter.WIDTH_DEFAULT_FACTOR);
        if (defaultSpace == null)
            defaultSpace = new Float(wlambda * Formatter.SPACE_DEFAULT_FACTOR);

        final Cadencize cad = new Cadencize(false);

        final UnaryPredicate portFilter;
        if (theArgs.argExists("ignore-ports")) {
            final AliasedMap ports = cad.convert(ci).getPortNodes();
            portFilter = new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    final Map.Entry entry = (Map.Entry) o;
                    return !ports.contains(entry.getKey());
                }
            };
        } else {
            portFilter = new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return true;
                }
            };
        }

        final String defaultRule = theArgs.getArgValue("default-rule", "7W6S");

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }

        final UnaryPredicate ruleFilter = new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                final Map.Entry entry = (Map.Entry) o;
                return !ObjectUtils.equals(defaultRule, entry.getValue());
            }
        };

        final AliasedSet nodes = cad.convert(ci).getLocalNodes();
        final UnaryPredicate groundFilter = new UnaryPredicate() {
            final HierName ground = HierName.makeHierName("GND");
            public boolean evaluate(final Object o) {
                final Map.Entry entry = (Map.Entry) o;
                return !nodes.areEquivalent(ground, (HierName) entry.getKey());
            }
        };

        final UnaryPredicate filter = new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                return portFilter.evaluate(o) && groundFilter.evaluate(o) &&
                       ruleFilter.evaluate(o);
            }
        };

        final Formatter formatter;

        if (format.equals("skill")) {
            formatter = new SkillFormatter(w, ci, cad, wlambda,
                                           defaultWidth, defaultSpace,
                                           defaultRule, filter);
        } else if (format.equals("def")) {
            formatter = new DefFormatter(w, ci, cad, wlambda,
                                         defaultWidth, defaultSpace,
                                         defaultRule, filter);
        } else {
            formatter = new TxtFormatter(w, ci, cad, wlambda,
                                         defaultWidth, defaultSpace, filter);
        }

        formatter.output();
        w.close();
    }
}
