/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.io.*;
import java.util.*;

import com.avlsi.util.container.MultiSet;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;

import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;

public class GenerateLogicLibrary {

    /** usage banner **/
    static public void usage( String m ) {
        System.err.println("Usage: GenerateLogicLibrary\n" +
            " --inputs=[1,2,3,4,5,6]  Set number of inputs to gates.\n" +
            " --strengths=N           Set number of strengths of each gate.\n" +
            " --task=[domino|qdi|mld|bd] Select task.\n" +
            " --format=[cast|verilog|image|lib|lib_vc]  Select output format.\n" +
            " --cast-path=path      Cast path.\n" +
            " [--header]  Generate lib header.\n" +
            " [--footer]  Generate lib footer.\n" +
            " [--cast-root=path]      Cast root path to generate cast files.\n" +
            " [--timing-dir=path]     Directory containing timing files.\n" +
            " [--fulcrum-pdk-root=path] PDK root is required for generating verilog.\n" +
            " [--module=name]         Cast module name for special cells.\n" +
            " [--cells=C1:C2:...]     Generate list of special cells.\n" +
            " [--gates=G1:G2:...]     Generate gates with given Karnaugh map.\n" +
            " [--tau=Float]           Specifies the forward latency for generating an image Library\n"+
            " [--tables=table_file]   Generate gates from a table file.\n");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    /** Produce a Proteus gate library. **/
    static public void main(String[] args) throws Exception {
        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs);
        CommandLineArgs cachedArgs =
            new CachingCommandLineArgs(argsWithConfigs);
        PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs(cachedArgs);
        CommandLineArgs theArgs = pedanticArgs;

        // get arguments
        int Inputs = Integer.parseInt(theArgs.getArgValue("inputs", "1"));
        int Strengths = Integer.parseInt(theArgs.getArgValue("strengths","1"));
        String  Task = theArgs.getArgValue("task", null);
        String  castPath = theArgs.getArgValue("cast-path", null);
        String  castRoot = theArgs.getArgValue("cast-root", null);
        String  timingDir = theArgs.getArgValue("timing-dir",".");
        String  cellList = theArgs.getArgValue("cells", null);
        String  tablesFile = theArgs.getArgValue("tables", null);
        String  gateList = theArgs.getArgValue("gates",null);
        String  format = theArgs.getArgValue("format",null);
        String  module = theArgs.getArgValue("module",null);
        String  pdkRoot = theArgs.getArgValue("fulcrum-pdk-root", null );
        String  castVersion = theArgs.getArgValue("cast-version", "2");
        String tau = theArgs.getArgValue("tau",null);
        boolean noheader = !theArgs.argExists("header");
        boolean nofooter = !theArgs.argExists("footer");

        // check obvious errors
        if (!pedanticArgs.pedanticOK(false, true)) {
            usage(pedanticArgs.pedanticString());
        }
        if ((Inputs > 4) &&
            (tablesFile == null) &&
            (gateList == null) ||
            (Inputs < 1) ||
            (Inputs > 6) ||
            (Task==null) ||
            (format==null) ||
            !Task.equals("domino") &&
            !Task.equals("qdi") &&
            !Task.equals("mld") &&
            !Task.equals("bd") ||
            !format.equals("cast") &&
            !format.equals("verilog") &&
            !format.equals("lib") &&
            !format.equals("lib_vc") &&
            !format.equals("image") ||
            (castPath==null)) {
            usage( null );
        }
        if (module==null) module = "synthesis." + Task + ".logic" + Inputs;

        // create a castParser
        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser
            (new FileSearchPath(castPath), castVersion,
             new StandardParsingOption(theArgs));

        // emit special gates
        if (cellList != null) {
            StringTokenizer t = new StringTokenizer(cellList,":");
            while (t.hasMoreTokens()) {
                String cellName = t.nextToken();
                if (format.equals("verilog"))
                    new GenerateVerilog(Strengths, cellName, castPath,
                                        pdkRoot, module, Task);
                else if (format.equals("lib") ||
                         format.equals("lib_vc") ||
                         format.equals("image")){
                        if(tau ==null)
                            new GenerateLib(Strengths, cellName, Task,
                                        noheader, nofooter,
                                        timingDir, module,
                                        format.equals("image"),
                                        format.equals("lib_vc"),
                                        false,
                                        castParser, null);
                        else
                            new GenerateLib(Strengths, cellName, Task,
                                        noheader, nofooter,
                                        timingDir, module,
                                        format.equals("image"),
                                        format.equals("lib_vc"),
                                        false,
                                        castParser, null,tau);


                }
            }
            System.exit(0);
        }
        
        // initialize static variables of the Karnaugh utility module
        new Karnaugh(Inputs);

        // generate a list of logic gates
        MultiSet gates = new MultiSet();
        if (gateList!=null) {
            // comma separated list of Karnaugh maps as hex strings
            StringTokenizer t = new StringTokenizer(gateList,":");
            while (t.hasMoreTokens()) {
                long T = Karnaugh.parseGateString(t.nextToken());
                if (!Karnaugh.degenerateGate(T))
                    gates.addIfUnique(new Long(Karnaugh.canonicalGate(T)));
            }
        }
        if (tablesFile!=null) {
            // add gates specified with --tables to set of gates
            try {
                BufferedReader br = new BufferedReader(new FileReader(tablesFile));
                String line;
                while ((line = br.readLine()) != null) {
                    if (line.startsWith("#")) continue;
                    String t = (new StringTokenizer(line)).nextToken();
                    long T = Karnaugh.getKarnaughFromTable(t);
                    if (!Karnaugh.degenerateGate(T))
                        gates.addIfUnique(new Long(Karnaugh.canonicalGate(T)));
                }
                br.close();
            } catch (IOException e) {
                System.err.println("ERROR: can't read tables file " + tablesFile);
            }
        }
        if (gateList==null && tablesFile==null) {
            // enumerate all canonical logic gates
            for (long T = 0; T <= Karnaugh.StateMask; T++) {
                if (Karnaugh.degenerateGate(T)) continue;
                if (T != Karnaugh.canonicalGate(T)) continue;
                gates.addIfUnique(new Long(T));
            }
        }

        // generate cast, verilog, or liberty for the logic gates
        if (format.equals("cast"))
            new GenerateCast(Inputs, gates, castRoot, Task);
        else if (format.equals("verilog"))
            new GenerateVerilog(Inputs, Strengths, gates, castPath, pdkRoot, Task);
        else if (format.equals("lib") ||
                 format.equals("lib_vc") ||
                 format.equals("image")){
            if(tau == null)
                new GenerateLib(Inputs, Strengths, gates, Task,
                                noheader, nofooter,
                                timingDir, module,
                                format.equals("image"),
                                format.equals("lib_vc"),
                                false,
                                castParser);
            else
                new GenerateLib(Inputs, Strengths, gates, Task,
                                noheader, nofooter,
                                timingDir, module,
                                format.equals("image"),
                                format.equals("lib_vc"),
                                false,
                                castParser,tau);


        }
    }
}
