package com.avlsi.csp.coverage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.StringContainerIterator;

public class CoverageTool {
    private static void help(String message, int status) {
        System.err.println(message);
        System.err.println("Usage: csp_coverage [--missing|--merge] [zip files]");
        System.exit(status);
    }

    private static boolean load(Monitor m) {
        try {
            m.load();
            return true;
        } catch (IOException e) {
            System.err.println(
                "Error: can't load " + m.getPath() + ": " + e.getMessage());
        } catch (ParseException e) {
            System.err.println(
                "Error: can't parse " + m.getPath() + ": " + e.getMessage());
        }
        return false;
    }

    public static void main(String[] args) {
        // get command line args
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs);
        final CommandLineArgs cachedArgs =
            new CachingCommandLineArgs(argsWithConfigs);
        final CommandLineArgs theArgs = cachedArgs;

        final boolean missing = theArgs.argExists("missing");
        final boolean merge = theArgs.argExists("merge");

        if (missing && merge) {
            help("Both --missing and --merge specified", 1);
        }

        if (!missing && !merge) {
            help("Neither --missing and --merge specified", 1);
        }

        final ArrayList<String> zips = new ArrayList<>();
        for (StringContainerIterator i = theArgs.nonParsedArgumentsIterator();
             i.hasNext(); ) {
            zips.add(i.next());
        }

        if (missing) {
            for (String zip : zips) {
                Monitor m = new Monitor();
                m.setPath(zip);
                if (!load(m)) {
                    continue;
                }
                m.listMisses(System.out);
            }
        } else {
            Monitor merged = Monitor.merge(
                zips.stream()
                    .map(s -> { Monitor m = new Monitor();
                                m.setPath(s);
                                if (!load(m)) m.clearAll();
                                return m; }),
                (m, t) -> {
                    System.err.println("Error: " + t + " from " + m.getPath() +
                            " has a different modified date");
                    return true;
                }
            );
            merged.setPath("/dev/stdout");
            try {
                merged.save();
            } catch (IOException e) {
                System.err.println(
                    "Error: can't save merged output: " + e.getMessage());
            }
        }
    }
}
