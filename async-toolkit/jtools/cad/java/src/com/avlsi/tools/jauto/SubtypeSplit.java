package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.text.DateFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.layout.LayerCallback;
import com.avlsi.io.SearchPath;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

public final class SubtypeSplit {

    private SubtypeSplit() {
        throw new AssertionError();
    }

    public interface Spec {
        String getSpec(final CDLNameInterface renamer);
    }

    private static class SplitSpec implements Spec {
        private final Collection targets;
        private final String from;

        public SplitSpec(final String from, final Collection targets) {
            this.from = from;
            this.targets = targets;
        }

        public String getSpec(final CDLNameInterface renamer) {
            final StringBuffer buf = new StringBuffer();
            final int index = from.lastIndexOf('.');
            final String base = from.substring(0, index);
            final String subtyped = from.substring(index + 1);
            buf.append("SPLIT: ");
            try {
                buf.append(renamer.renameCell(base));
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename: " + base, e);
            }
            buf.append(' ');
            buf.append(subtyped); 
            for (Iterator j = targets.iterator(); j.hasNext(); ) {
                buf.append(' ');
                buf.append((String) j.next());
            }
            return buf.toString();
        }
    }

    private static Collection convertSpec(final MultiMap mm) {
        final Collection result = new ArrayList();
        for (Iterator i = mm.keySet().iterator(); i.hasNext(); ) {
            final String type = (String) i.next();
            result.add(new SplitSpec(type, mm.get(type)));
        }
        return result;
    }

    private static void usage() {
        System.err.println(
"java com.avlsi.tools.jauto.SubtypeSplit\n" +
"    [standard CAST options] function [function options]\n\n" +
"Standard CAST options are:\n" +
"    [ --cast-path=<path> ] (defaults to .)\n" +
"    [ --cast-version=[ 1 | 2 ] ] (defaults to 2)\n" +
"    --cell=<mod.ule.cell.subtype> (top level cell)\n" +
"Functions are:\n" +
"    --all (split splittable instances)\n" +
"    --instance instance1 instance2 ... (split specified instances)\n" +
"    --type type1 type2 ... (split specified types)\n" +
"    --force-reuse (force reuse of specified subtypes)\n" +
"    --copy (copy to min-subtype)\n" +
"    --compact (compact to smallest available subtype)\n" +
"    --directive (set non-parameterized directives)\n" +
"    --hierarchy (split specified hierarchical instances)\n" +
"Function options are:\n" +
"    for all functions:\n" +
"        [ --subtype-path=<path> ] (defaults to .)\n" +
"    for --all, --copy, --compact, --type, --instance, --force-reuse,\n" +
"        --hierarchy:\n" +
"        [ --output=<spec file> ] (write split spec)\n" +
"    for --all, --copy, --compact, --type, --instance, --hierarchy:\n" +
"        [ --min-subtype=<number> ] (defaults to 0)\n" +
"        [ --max-subtype=<number> ] (defaults to 2^31-1)\n" +
"    for --all\n" +
"        [ --maximal ] (split all instances)\n" +
"        [ --split-array ] (break apart arrays)\n" +
"    for --compact\n" +
"        [ --compact-skip=<file> ] (lists cells to not compact)\n" +
"        [ --compact-range=<subtype:subtype> ] (compact only specified range)\n" +
"        [ --compact-path=<path> ] (colon seperated list of existing spec)\n" +
"    for --force-reuse\n" +
"        [ --reuse-spec=<file> ] (reuse without regard to metaparameters)\n" +
"        [ --pedantic-reuse-spec=<file> ] (reuse considering metaparameters)\n" +
"    for --directive\n" +
"        [ --recursive ] (recursively set directive)\n" +
"        <block>:<directive>=<value> ...\n" +
"    for --hierarchy\n" +
"        --replace-spec=<file> (each line: new type, instance name)\n"
);
        System.exit(1);
    }

    private static int getArgValue(final String number, final int fallback) {
        if (number == null) {
            return fallback;
        } else {
            try {
                return Integer.parseInt(number);
            } catch (NumberFormatException e) {
                return fallback;
            }
        }
    }

    private static Format getHeader(final String cellName) {
        final DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM);
        final DateFormat timeFormat = DateFormat.getTimeInstance(DateFormat.LONG);
        final Date now = new Date();
        final StringBuffer header = new StringBuffer();
        header.append(cellName);
        header.append(":");
        header.append(dateFormat.format(now));
        header.append(":");
        header.append(timeFormat.format(now));
        header.append(":");
        header.append(System.getProperty("user.name"));
        header.append("\n");
        return new MessageFormat(header.toString());
    }

    private static boolean reusable(final CellInterface ci) {
        return !ci.isNode() && !ci.isChannel();
    }

    private static Pattern DIRECTIVE_SPEC =
        Pattern.compile("(\\w+):([^=]+)=(.*)");
    private static void setDirective(final Map dirs, final String str) {
        final Matcher m = DIRECTIVE_SPEC.matcher(str);
        if (m.matches()) {
            final String block = m.group(1);
            final String key = m.group(2);
            final String val = m.group(3);

            final Pair info = DirectiveTable.lookupDirective(block, key);
            if (info == null) {
                System.err.println("No " + key + " directives in block " + block);
                return;
            }

            if (!dirs.containsKey(block)) {
                dirs.put(block, new DirectiveSource(block));
            }
            final DirectiveSource src = (DirectiveSource) dirs.get(block);
            final String valType = (String) info.getFirst();
            final Object value = DirectiveUtils.parseDirective(valType, val);
            if (value == null) {
                System.err.println("Cannot set directive " + key + " = " + val);
            } else {
                src.definition(key, value);
            }
        } else {
            System.err.println("Invalid directive specification: " + str);
        }
    }

    private static void processReuseSpec(final String file,
                                         final boolean pedantic,
                                         final Map reuseMap,
                                         final CastFileParser cfp)
        throws IOException {
        final BufferedReader br = new BufferedReader(new FileReader(file));
        int num = 1;
        String line;
        while ((line = br.readLine()) != null) {
            final String s[] = StringUtil.tokenize(line);
            try {
                if (s.length == 1) {
                    final CellInterface ci = cfp.getFullyQualifiedCell(s[0]);
                    if (!pedantic)
                        reuseMap.put(CellUtils.getBaseType(ci.getModuleName()),
                                     ci);
                    reuseMap.put(ci.getModuleName(), ci);
                } else if (s.length == 3 && s[1].equals(":>")) {
                    final CellInterface ci1 = cfp.getFullyQualifiedCell(s[0]);
                    final CellInterface ci2 = cfp.getFullyQualifiedCell(s[2]);
                    final String fqcn = ci1.getFullyQualifiedType();
                    if (!pedantic)
                        reuseMap.put(CellUtils.getBaseType(fqcn), ci2);
                    reuseMap.put(fqcn, ci2);
                } else {
                    System.err.println("WARNING: Invalid reuse specification format in " + file + ":" + num);
                }
            } catch (CastSemanticException e) {
                System.err.println("WARNING: Invalid cell in reuse specification in " + file + ":" + num);
                ExceptionPrettyPrinter.printException(e);
            }
            ++num;
        }
        br.close();
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        final PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String subtypePath = theArgs.getArgValue("subtype-path", ".");
        final String cellName = theArgs.getArgValue("cell", null);
        final boolean splitAll = theArgs.argExists("all");
        final boolean copyOnly = theArgs.argExists("copy");
        final boolean compact = theArgs.argExists("compact");
        final boolean splitType = theArgs.argExists("type");
        final boolean splitInstance = theArgs.argExists("instance");
        final boolean splitHierarchy = theArgs.argExists("hierarchy");
        final boolean reuse = theArgs.argExists("force-reuse");
        final boolean directive = theArgs.argExists("directive");
        final String output = theArgs.getArgValue("output", null);
        final int minSubtype =
            getArgValue(theArgs.getArgValue("min-subtype", null), 0);
        final int maxSubtype =
            getArgValue(theArgs.getArgValue("max-subtype", null),
                        Integer.MAX_VALUE - 1); // workaround bug 5695
        if (cellName == null) {
            System.err.println("ERROR: You must specify a cellname.");
            usage();
        } else if (subtypePath == null) {
            System.err.println("ERROR: You must specify a subtype output path.");
            usage();
        }

        // Warn if zero or multiple functions are specified
        int count = 0;
        if (splitAll) ++count;
        if (splitInstance) ++count;
        if (splitType) ++count;
        if (splitHierarchy) ++count;
        if (reuse) ++count;
        if (copyOnly) ++count;
        if (compact) ++count;
        if (directive) ++count;
        if (count != 1) {
            System.err.println("ERROR: You must specify exactly 1 function.");
            usage();
        }

        final CastFileParser cfp =
            new CastFileParser(new FileSearchPath(castRoot), castVersion,
                               new StandardParsingOption(theArgs));

        Collection<Spec> spec = null;
        final MultiMap mmspec = new MultiMap();
        SubtypeOutput.Policy policy;
        Collection<Pair<HierName,String>> hierarchyInstances = null;
        if (copyOnly) {
            // kind of goofy
            policy = new SubtypeOutput.Copy(subtypePath,
                                            getHeader(cellName),
                                            "" + minSubtype,
                                            mmspec);
        } else if (compact) {
            final String skip = theArgs.getArgValue("compact-skip", null);
            final Set skipSet;
            if (skip == null) {
                skipSet = Collections.EMPTY_SET;
            } else {
                skipSet = new HashSet();
                try {
                    final BufferedReader r =
                        new BufferedReader(new FileReader(skip));
                    String line;
                    while ((line = r.readLine()) != null) {
                        skipSet.add(line);
                    }
                } catch (IOException e) {
                    throw new RuntimeException("Cannot read compact skip file: " + skip, e);
                }
            }
            final String range = theArgs.getArgValue("compact-range", null);
            final int rangeLo, rangeHi;
            if (range == null) {
                rangeLo = 0;
                rangeHi = Integer.MAX_VALUE;
            } else {
                String[] r = StringUtil.split(range, ':');
                try {
                    rangeLo = Integer.parseInt(r[0]);
                    rangeHi = Integer.parseInt(r[1]);
                } catch (NumberFormatException e) {
                    throw new RuntimeException("Invalid compacting range: " + range, e);
                }
            }
            final String[] paths;
            final String compactPath = theArgs.getArgValue("compact-path", null);
            if (compactPath == null) {
                paths = new String[1];
            } else {
                final String s[] = StringUtil.split(compactPath, ':');
                paths = new String[s.length + 1];
                System.arraycopy(s, 0, paths, 1, s.length);
            }
            paths[0] = subtypePath;
            policy = new SubtypeOutput.Compact(paths, getHeader(cellName),
                                               minSubtype, maxSubtype, mmspec,
                                               skipSet, rangeLo, rangeHi);
        } else if (reuse) {
            final String normal = theArgs.getArgValue("reuse-spec", null);
            final String pedantic = theArgs.getArgValue("pedantic-reuse-spec", null);
            if (normal == null && pedantic == null) {
                throw new RuntimeException("Reuse specification not specified");
            }

            final Map reuseMap = new HashMap();

            if (normal != null) processReuseSpec(normal, false, reuseMap, cfp);

            if (pedantic != null)
                processReuseSpec(pedantic, true, reuseMap, cfp);

            spec = new HashSet<Spec>();
            policy = new SubtypeOutput.Reuse(subtypePath, getHeader(cellName),
                                             reuseMap, spec);
        } else if (splitAll) {
            final boolean maximal = theArgs.argExists("maximal");
            final boolean splitArray = theArgs.argExists("split-array");
            policy = new SubtypeOutput.SplitAll(subtypePath,
                                                getHeader(cellName),
                                                minSubtype, maxSubtype,
                                                Collections.EMPTY_SET, maximal,
                                                splitArray, mmspec);
        } else if (splitType) {
            final Set splittable = new HashSet();
            for (StringContainerIterator types =
                    theArgs.nonParsedArgumentsIterator(); types.hasNext(); ) {
                splittable.add(types.next());
            }
            policy = new SubtypeOutput.SplitType(subtypePath,
                                                 getHeader(cellName),
                                                 minSubtype, maxSubtype,
                                                 splittable, mmspec);
        } else if (splitInstance) {
            final Set instances = new HashSet();
            for (StringContainerIterator instance =
                    theArgs.nonParsedArgumentsIterator(); instance.hasNext(); ) {
                instances.add(instance.next());
            }
            policy = new SubtypeOutput.SplitInstance(subtypePath,
                                                     getHeader(cellName),
                                                     minSubtype, maxSubtype,
                                                     instances, mmspec);
        } else if (splitHierarchy) {
            hierarchyInstances = new ArrayList<Pair<HierName,String>>();
            final String replaceSpec =
                theArgs.getArgValue("replace-spec", null);
            if (replaceSpec == null) {
                System.err.println("ERROR: replace specification missing");
                usage();
            }
            BufferedReader br = null;
            try {
                br = new BufferedReader(new FileReader(replaceSpec));
                String line;
                while ((line = br.readLine()) != null) {
                    if (line.startsWith("#")) continue;
                    final String[] words = StringUtil.split(line.trim(), ' ');
                    if (words.length != 2) {
                        System.err.println("WARNING: Invalid replace specification format: " + line);
                        continue;
                    }
                    final HierName hName;
                    try {
                        hName = HierName.makeHierName(words[1].trim(), '.');
                    } catch (InvalidHierNameException e) {
                        throw new RuntimeException("Can't make HierName: " +
                                                   line, e);
                    }
                    hierarchyInstances.add(
                            new Pair<HierName,String>(hName, words[0]));
                }
            } finally {
                if (br != null) br.close();
            }
            spec = new HashSet<Spec>();
            policy = null;
        } else if (directive) {
            final boolean recursive = theArgs.argExists("recursive");
            final Map dirs = new HashMap();
            for (StringContainerIterator types =
                    theArgs.nonParsedArgumentsIterator(); types.hasNext(); ) {
                final String dirspec = types.next();
                setDirective(dirs, dirspec);
            }
            policy = new SubtypeOutput.DirectiveTransform(subtypePath,
                                                          getHeader(cellName),
                                                          recursive, dirs);
        } else {
            policy = null;
            System.err.println("ERROR: You must specify one of --all, --type or --instance.");
            usage();
        }

        if (!pedanticArgs.pedanticOK(false, true)) {
            System.err.println(pedanticArgs.pedanticString());
            usage();
        }

        final CellInterface cell = cfp.getFullyQualifiedCell(cellName);

        if (!CellUtils.isSubtype(cell)) {
            System.err.println("ERROR: Cell " + cellName + " does not appear to be a subtype.");
            usage();
        }

        if (splitHierarchy) {
            final Collection<String> errors = new ArrayList<String>();
            policy =
                new SubtypeOutput.SplitHierarchy(
                        subtypePath, getHeader(cellName), cell,
                        hierarchyInstances, minSubtype, maxSubtype, spec,
                        errors);
            if (!errors.isEmpty()) {
                System.err.println("ERROR: Some instances not found:");
                for (String error : errors) {
                    System.err.println(error);
                }
                System.exit(1);
            }
        }

        final Writer specWriter =
            output == null ? null : new FileWriter(output);
        SubtypeOutput.writeSubtype(policy, cell);
        if (specWriter != null) {
            if (spec == null) {
                spec = convertSpec(mmspec);
            }
            final CDLNameInterface renamer = new CadenceNameInterface();
            for (Spec s : spec) {
                specWriter.write(s.getSpec(renamer));
                specWriter.write('\n');
            }
            specWriter.flush();
            specWriter.close();
        }
    }
}
