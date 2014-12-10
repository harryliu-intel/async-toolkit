package com.avlsi.tools.jauto;

import java.io.IOException;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.io.FileSearchPath;
import com.avlsi.file.common.HierName;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

public class GeneratePLTSubtypes {

    private static Boolean verbose;

    public static String cell2dir(String cellName) {
        return cellName.replace('.', File.separatorChar);
    }

    private static void writeCell(final CellInterface ci,
                                 final String cellName,
                                 final Map instances,
                                 final Collection<BufferedReader> brs,
                                 final File outputDir,
                                 final boolean writeSubtypes,
                                 final boolean ifexists) throws IOException {
        final int lastDot = cellName.lastIndexOf('.');
        assert lastDot > 0;
        final String module = cellName.substring(0, lastDot);
        final String cell = cellName.substring(lastDot + 1);
        final File d = new File(outputDir, cell2dir(module));
        if (!d.isDirectory() && !d.mkdirs()) {
            throw new RuntimeException("Cannot create directory: " + d);
        }
        final File f = new File(d, cell + ".cast");
        if (f.exists() && ifexists) {
            if (verbose)
                System.out.println("Exists " + f);
            return;
        }
        final PrintWriter w = new PrintWriter(new FileWriter(f));
        SubtypeOutput.writeHeader(w, module, ci.getFullyQualifiedType(),
                                  cell, ci, null, null);
        if (CellUtils.isLeaf(ci)) {
            w.println("  prs {");
        } else {
            w.println("  subtypes {");
            if (writeSubtypes) {
                for (Iterator i = instances.entrySet().iterator(); i.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final HierName inst = (HierName) entry.getKey();
                    final Pair p = (Pair) entry.getValue();
                    w.println("    " + p.getFirst() + " :>");
                    w.println("      " + p.getSecond() + " " + inst + ";");
                }
            }
        }

        if (!brs.isEmpty()) {
            w.println("    directives {");
        }
        for (BufferedReader br : brs) {
            String line;
            while ((line = br.readLine()) != null) w.println(line);
        }
        if (!brs.isEmpty()) {
            w.println("    }");
        }

        w.println("  }");
        w.println("}");
        w.close();
        if (verbose)
            System.out.println("Wrote " + f);
    }

    private static BufferedReader openReader(final File f) {
        try {
            return new BufferedReader(new FileReader(f));
        } catch (FileNotFoundException e) {
            return null;
        }
    }

    private static class QuickExitException extends RuntimeException { }

    public static BufferedReader getDirective(final File measureDir,
                                              final String moduleName,
                                              final String type,
                                              final String[] dirFiles) {
        final BufferedReader[] result = new BufferedReader[1];
        final File dir = new File(measureDir, cell2dir(moduleName));

        // If directives exist for the subtype, use it
        for (int i = 0; i < dirFiles.length; ++i) {
            final File real;
            if (dirFiles[i].startsWith("/")) {
                real = new File(dirFiles[i]);
            }
            else {
                real = new File(new File(dir, type), dirFiles[i]);
            }
            result[0] = openReader(real);
            if (result[0] != null) {
                // System.out.println("Directives from "+real);
                return result[0];
            }
        }

        return result[0];
    }

    public static BufferedReader tryDirective(final File measureDir,
                                              final String moduleName,
                                              final String type,
                                              final String[] dirFiles) {
        final BufferedReader[] result = new BufferedReader[1];
        final File dir = new File(measureDir, cell2dir(moduleName));

        // Search for directives from FQCN+- directories, choose one arbitrarily
        try {
            dir.listFiles(new FilenameFilter() {
                public boolean accept(final File d, final String name) {
                    try {
                        // Prepend with "x." to avoid quoting the name, which
                        // may be numeric
                        final Triplet t =
                            PartialExtract.parseCellPlusMinus("x." + name);
                        if (t.getFirst().equals("x." + type)) {
                            for (int i = 0; i < dirFiles.length; ++i) {
                                final File x = new File(new File(d, name),
                                                        dirFiles[i]);
                                if ((result[0] = openReader(x)) != null) {
                                    System.out.println("Using " + x + " for " +
                                                       moduleName + "." + type);
                                    // Don't examine any more files
                                    throw new QuickExitException();
                                }
                            }
                        }
                    } catch (Exception e) { }
                    return false;
                }
            });
        } catch (QuickExitException e) { }

        return result[0];
    }

    private static Set<String> warned = new HashSet<String>();

    public static BufferedReader getDirective(final File[] measureDirs,
                                              final String moduleName,
                                              final String type,
                                              final String[] dirFiles) {
        for (int i = 0; i < measureDirs.length; ++i) {
            final BufferedReader br =
                getDirective(measureDirs[i], moduleName, type, dirFiles);
            if (br != null) {
                return br;
            }
        }

        for (int i = 0; i < measureDirs.length; ++i) {
            final BufferedReader br =
                tryDirective(measureDirs[i], moduleName, type, dirFiles);
            if (br != null) {
                return br;
            }
        }

        final String fqcn = moduleName + "." + type;
        if (warned.add(fqcn)) {
            System.err.println("Warning: directives file not found for " +
                               fqcn);
        }
        return null;
    }

    public static String getReplacement(final CellInterface ci,
                                        final CellInterface cip,
                                        final File[] measureDirs,
                                        final String[][] dirFiles,
                                        final String prefix,
                                        final File outputDir,
                                        final Map cache,
                                        final int topOnly,
                                        final boolean top) throws IOException {
        final String type = ci.getFullyQualifiedType();
        if (cache.containsKey(type)) return (String) cache.get(type);

        final Collection<BufferedReader> brs = new ArrayList<BufferedReader>();
        for (String[] dirFile : dirFiles) {
            final BufferedReader br =
                getDirective(measureDirs, ci.getModuleName(), ci.getType(),
                             dirFile);
            if (br != null) brs.add(br);
        }
        final Map changed = new HashMap();
//        if(top || topOnly == 0 || br == null) {
            for (Iterator i = cip.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subci = (CellInterface) p.getSecond();
                if (CellUtils.isWiring(subci)) continue;
                final String subtype = subci.getFullyQualifiedType();
                final String newType =
                    getReplacement(subci, subci, measureDirs, dirFiles, prefix, outputDir,
                                   cache, topOnly, false);
                if (newType != null) {
                    if (!subtype.equals(newType)) {
                        final HierName inst = (HierName) p.getFirst();
                        changed.put(inst, new Pair(subtype, newType));
                    }
                }
            }
//        }

        final String newName;
        if (changed.isEmpty() && brs.isEmpty()) {
            newName = topOnly == 0 ? type : null;
        } else {
            newName = prefix + "." + type;
            // top cell, always write
            // topOnly == 0 do all
            // no directive found, always write
            final boolean writeifexists = (top || (topOnly == 0) || brs.isEmpty());
            final boolean writeSubtypes = (prefix.equals("plt") || topOnly == 0);
            if (top || writeSubtypes)
                writeCell(cip, newName, changed, brs, outputDir, writeSubtypes, ! writeifexists);
        }
        for (BufferedReader br : brs) {
            br.close();
        }
        cache.put(type, newName);
        return newName;
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        final String parentName = theArgs.getArgValue("parent", cellName);

        final String output = theArgs.getArgValue("output-dir", null);
        final String measure = theArgs.getArgValue("measure-dir", null);
        final String prefix = theArgs.getArgValue("prefix", "measured");
        final String dirFile = theArgs.getArgValue("dir-file", "directives");
        final int topOnly =Integer.parseInt(theArgs.getArgValue("top-only", "0"));
        verbose = theArgs.argExists("verbose");
        if (cellName == null || output == null || measure == null) {
            System.err.println(
"Usage: generate_plt_subtypes\n" +
"     --cast-path=<cast-path> (defaults to .)\n" +
"     --cast-version=<version> (defaults to 2)\n" +
"     --cell=<cell> (fully qualified cell name)\n" +
"     --output-dir=<path> (where to write new subtypes)\n" +
"     --measure-dir=<path>:... (paths to search for --dir-files)\n" +
"     [ --parent=<fqcn> (instead of cell if different) ]\n" +
"     [ --prefix=<prefix> (prefix for cell name, defaults to measured) ]\n" +
"     [ --top-only=[0|1] (just do top cell and as needed if 1) ]\n" +
"     [ --dir-file=<file>:...|...|... (files to search in --measure-dirs,\n" +
"                                      defaults to directives) ]\n" +
"     [ --verbose extra info]\n");
            System.exit(1);
        }
        final String[] dirGroups = StringUtil.split(dirFile, '|');
        final String[][] dirFiles = new String[dirGroups.length][];
        for (int i = 0; i < dirGroups.length; ++i) {
            dirFiles[i] = StringUtil.split(dirGroups[i], ':');
        }
        final File outputDir = new File(output);
        final String[] measures = StringUtil.split(measure, ':');
        final File[] measureDirs = new File[measures.length];
        for (int i = 0; i < measures.length; ++i) {
            measureDirs[i] = new File(measures[i]);
        }
        final CellInterface cip =
            CellUtils.loadCell(castRoot, parentName, castVersion);
        final CellInterface ci =
            CellUtils.loadCell(castRoot, cellName, castVersion);
        final String newName =
            getReplacement(ci, cip, measureDirs, dirFiles, prefix, outputDir,
                           new HashMap(), topOnly, true);
        System.out.println(ci.getFullyQualifiedType() + " -> " + newName);
    }
}
