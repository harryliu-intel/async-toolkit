/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

import java.io.ByteArrayOutputStream;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import java.util.Collections;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.io.CopyingOutputStream;
import com.avlsi.csp.csp2tt.Csp2TT;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.LinkedHashSet;
import com.avlsi.io.OutputPumper;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.debug.VersionInfo;

public class PReSto {
    private static void usage(int exitStatus) {
        System.err.println("Usage:");
	System.err.println("presto --cast-path=/your/cast/path --cell=some.cell.NAME [--go=channel,...]");
	System.exit(exitStatus);
    }

    private Process proc;
    private OutputPumper pump;
    private ByteArrayOutputStream output;

    private InputStream csp2tt(String cast_path, String cell,
                               String package_root) throws IOException {
        Runtime r = Runtime.getRuntime();
        long totalMemory = r.totalMemory();
        long maxMemory = r.maxMemory();
        String java_home = System.getProperty("java.home");
        String class_path = System.getProperty("java.class.path");
        char sep = File.separatorChar;
        LinkedList cmd = new LinkedList();
        cmd.add(java_home + sep + "bin" + sep + "java");
        cmd.add("-cp");
        cmd.add(class_path);
        cmd.add("-Xms" + totalMemory);
        cmd.add("-Xmx" + maxMemory);
        cmd.add(Csp2TT.class.getName());
        if (package_root != null)
            cmd.add("--package-root=" + package_root);
        cmd.add("--cast-path=" + cast_path);
        cmd.add("--cell=" + cell);
        proc = r.exec((String[])cmd.toArray(new String[0]));
        proc.getOutputStream().close();
        output = new ByteArrayOutputStream();
        OutputStream[] streams = new OutputStream[] { System.err, output };
        pump = new OutputPumper(proc.getErrorStream(),
                                new CopyingOutputStream(streams));
        pump.start();
        return proc.getInputStream();
    }

    private static void addWarnings(WarningAccumulator wacc, String s) {
        String[] strs = StringUtil.split(s, '\n');
        final String warn = "warning:";
        for (int i = 0; i < strs.length; i++) {
            try {
                String ss = strs[i].substring(0, warn.length());
                if (ss.equalsIgnoreCase(warn))
                    wacc.add(strs[i]);
            } catch (IndexOutOfBoundsException e) {
                // do nothing, but continue with the loop
            }
        }
    }

    public static void main(String[] args) throws IOException,
                                                  InterruptedException {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 
	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);
	final CommandLineArgs theArgs = cachedArgs;
	
        String version = VersionInfo.getVersionString(PReSto.class);

        if (theArgs.argExists("version")) {
            System.out.println(version);
        }

	final String castPath = theArgs.getArgValue("cast-path", ".");
        final String castCellName = theArgs.getArgValue("cell", null);
        final String packageRoot = theArgs.getArgValue("package-root", null);
        final String goArg = theArgs.getArgValue("go", null);
        boolean goAll = (goArg == null && theArgs.argExists("go"));

        if (castCellName == null ||
            theArgs.nonParsedArgumentsIterator().hasNext())
            usage(1);

        PReSto pr = new PReSto();
        InputStream in = pr.csp2tt(castPath, castCellName, packageRoot);

        LinkedHashSet go = new LinkedHashSet();
        if (goArg != null) {
            String[] goSplit = StringUtil.split(goArg, ',');
            for (int i = 0; i < goSplit.length; i++)
                go.add(goSplit[i]);
        }

        TruthTable[] tt = null;
        try {
            tt = LogicFileReader.readLogicFile(new InputStreamReader(in));
        } catch (LogicFileReader.TruthTableFormatException e) {
            int exitCode = pr.proc.waitFor();
            pr.pump.join();
            if (exitCode == 0 || pr.output.size() == 0) {
                /* If process exited normally or didn't write to stderr,
                 * print an error message.  Otherwise, assume it printed
                 * a meaningful error message and we don't need to. */
                System.err.println("Internal PReSto error: bad truth table: " +
                                   e.getMessage());
                if (exitCode == 0)
                    exitCode = 100;
            }
            System.exit(exitCode);
        }

        int exitCode = pr.proc.waitFor();
        pr.pump.join();
        if (exitCode != 0) {
            if (pr.output.size() == 0) {
                System.err.println("Internal PReSto error: truth table " +
                                   "generation failed");
            }
            System.exit(exitCode);
        }


        if (goAll)
            for (int i = 0; i < tt.length; i++)
                go.add(tt[i].getOutput().getNameWithIndices());

        pr.pump.join();
        String tterr = pr.output.toString();
        WarningAccumulator wacc = new WarningAccumulator();
        addWarnings(wacc, tterr);

        PrintWriter w = new PrintWriter(new OutputStreamWriter(System.out));
        Presto2.presto(w, tt, go, version, wacc);
        w.flush();
    }
}
