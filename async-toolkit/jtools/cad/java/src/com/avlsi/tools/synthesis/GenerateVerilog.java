/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.io.*;
import java.util.*;

import java.io.OutputStreamWriter;
import com.avlsi.util.container.MultiSet;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.tools.prs2verilog.Prs2Verilog;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryImpl;
import com.avlsi.tools.prs2verilog.verilog.SimpleRenamingVerilogFactory;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;

/** for generating verilog ".v" files **/
public class GenerateVerilog {

    /** number of inputs **/
    final int Inputs;

    /** cast path **/
    final String CastPath;

    /** pdk path **/
    final String PdkRoot;

    /** current task **/
    String Task;

    /** verilog generation tool command line interface **/
    public GenerateVerilog(int Inputs, int Strengths,
                           MultiSet gates, String path, String pdk, String Task) {
        this.Inputs = Inputs;
        this.CastPath = path;
        this.PdkRoot = pdk;
        this.Task = Task;
        String module = "synthesis." + Task + ".logic" + Inputs;
        Iterator iter = gates.iterator();
        while (iter.hasNext()) {
            long T = ((Long) iter.next()).longValue();
            String Kstr = Karnaugh.getGateString(T);
            for (int i = 0; i < Strengths; i++) {
                String S = Strengths>1 ? "." + i : "";
                if (Task.equals("domino")) {
                    String name = "DOMINO" + Inputs + "_" + Kstr + S;
                    generateVerilog(module,name);
                    if (false /*Inputs<=3*/) generateVerilog(module,"C_" + name);
                }
                else if (Task.equals("mld")) {
                    String name = "LOGIC" + Inputs + "_" + Kstr + S;
                    generateVerilog(module,name);
                    generateVerilog(module,"V_" + name);
                }
                else if (Task.equals("qdi")) {
                    String name = "LOGIC" + Inputs + "_" + Kstr + S;
                    generateVerilog(module,name);
                    if (false /*Inputs<=3*/) generateVerilog(module,"C_" + name);
                }
                else if (Task.equals("bd")) {
                    // combinational logic, include inverted versions too
                    MultiSet equiv = Karnaugh.getInvertedEquivalentGates(T);
                    for (Iterator k = equiv.iterator(); k.hasNext(); ) {
                        long K = ((Long) k.next()).longValue();
                        Kstr = Karnaugh.getGateString(K);
                        generateVerilog(module,"GATE" + Inputs + "_" + Kstr + S);
                    }
                }
            }
        }
    }

    /** generate verilog file for special gate **/
    public GenerateVerilog(int Strengths, String gateName, String path, String pdk,
                           String module, String Task) {
        this.Inputs = 0;
        this.CastPath = path;
        this.PdkRoot = pdk;
        this.Task = Task;
        if (module==null) module = "synthesis." + Task + ".special";
        for (int i=0; i < Strengths; i++) {
            String S = Strengths>1 ? "." + i : "";
            generateVerilog(module, gateName + S);
        }
    }
    
    /** generate verilog files. **/
    private void generateVerilog(String module, String gateName) {
        final String fqcn = module + "." + gateName;
        System.out.println(fqcn);
        if (PdkRoot==null) {
            System.err.println("ERROR: need --fulcrum-pdk-root for --format=verilog");
            return;
        }
        try {
            final VerilogFactoryInterface factory =
                new SimpleRenamingVerilogFactory(new CadenceNameInterface());
            final Prs2Verilog.VisitorFactory verilogVisitorFactory =
                new Prs2Verilog.MultipleFileVisitor(
                        "./verilog/" + Task + "/", // dir for generated files
                        null,             // path to manually translated files
                        new ArrayList(),  // list of files written
                        true,             // use relative path in file list
                        true              // surround modules with ifdef
                );
            
            CastFileParser cfp = new CastFileParser(new FileSearchPath(CastPath), "2");
            CellInterface ci = cfp.getFullyQualifiedCell(module, gateName);

            final String[] args = new String[] {
                "--skip-power-rail",
                "--make-gnd-0",
                "--make-vdd-1"
            };
            final CommandLineArgs PRS2VerilogArgs = new CommandLineArgsDefImpl(args);

            Prs2Verilog.writeVerilog(ci, verilogVisitorFactory,
                                     cfp, PRS2VerilogArgs, factory, "netgraph", false );
        } catch (IOException e) {
            System.err.println("ERROR: can't write Verilog file: " +
                               e.getMessage());
        } catch (CastSemanticException e) {
            System.err.println("ERROR: couldn't parse cast for " + fqcn);
        } catch (CastSyntaxException e) {
            System.err.println("ERROR: couldn't parse cast for " + fqcn);
        }
    }
}
