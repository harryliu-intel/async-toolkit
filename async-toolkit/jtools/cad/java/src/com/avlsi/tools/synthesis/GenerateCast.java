/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.io.*;
import java.util.*;

import com.avlsi.util.container.MultiSet;
import java.io.OutputStreamWriter;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.text.NumberFormatter;

/** for generating cast ".cast" files **/
public class GenerateCast {

    /** number of inputs **/
    final int Inputs;

    /** which type of library to generate **/
    final String Task;
    
    /** verilog generation tool command line interface **/
    public GenerateCast(int nInputs,MultiSet gates, String castRoot, String Task) {
        this.Inputs    = nInputs;
        this.Task      = Task;

        BufferedWriter w;
        final File out = new File(castRoot + "/" + Task + "/logic" + Inputs + ".cast");
        try {
            w = new BufferedWriter(new FileWriter(out));

            // module header
            w.write("module synthesis." + Task + ".logic" + Inputs + ";\n");
            if (!Task.equals("domino")) {
                w.write("import synthesis." + Task + ".logic.*;\n");
            }
            else {
                w.write("import synthesis.domino.logic.*;\n");
            }
            if (Task.equals("mld") || Task.equals("qdi")) {
                w.write("import synthesis.domino.logic" + Inputs + ".*;\n");
            }
            w.newLine();
            
            // emit gates
            Iterator iter = gates.iterator();
            while (iter.hasNext()) 
                generateCast(((Long) iter.next()).longValue(), w);
            w.close();

        } catch (IOException e) {
            System.err.println("ERROR: can't write cast file " + out);
        }
    }

    /** generate Cast files. **/
    private void generateCast(long T, BufferedWriter w) throws IOException {
        String Kstr = Karnaugh.getGateString(T);
        if (Task.equals("domino")) {

            // go-signal domino logic
            w.write("define DOMINO" + Inputs + "_" + Kstr + 
                    " <: DOMINO(" + Inputs + ") {\n");
            w.write("  prs {\n" +
                    "    go & (" + Karnaugh.getFuncString(T,false,false,false) + 
                    ") -> _X.0-\n" +
                    "    go & (" + Karnaugh.getFuncString(T,false,false,true) +  
                    ") -> _X.1-\n" +
                    "  }\n");
            w.write("}\n\n");

            // domino logic with integrated CTREE2
            if (false /*Inputs<=3*/) {
                w.write("define C_DOMINO" + Inputs + "_" + Kstr + 
                        " <: C_DOMINO(" + Inputs + ") {\n");
                w.write("  prs {\n" +
                        "    en & Xe & (" + Karnaugh.getFuncString(T,false,false,false) + 
                        ") -> _X.0-\n" +
                        "    en & Xe & (" + Karnaugh.getFuncString(T,false,false,true) +  
                        ") -> _X.1-\n" +
                        "  }\n");
                w.write("}\n\n");
            }
        }
        else if (Task.equals("qdi")) {
            
            // with completion
            w.write("define LOGIC" + Inputs + "_" + Kstr + 
                    " <: LOGIC(" + Inputs + ") {\n");
            w.write("  prs {\n" +
                    "    DOMINO" + Inputs + "_" + Kstr + " domino(a,_X,en);\n" +
                    "  }\n");
            w.write("}\n\n");

            // with completion and integrated CTREE2
            if (false /*Inputs<=3*/) {
                w.write("define C_LOGIC" + Inputs + "_" + Kstr + 
                        " <: C_LOGIC(" + Inputs + ") {\n");
                w.write("  prs {\n" +
                        "    C_DOMINO" + Inputs + "_" + Kstr + " domino(a,_X,en,X.e);\n" +
                        "  }\n");
                w.write("}\n\n");
            }
        }
        else if (Task.equals("mld")) {
            
            // without completion
            w.write("define LOGIC" + Inputs + "_" + Kstr + 
                    " <: LOGIC(" + Inputs + ") {\n");
            w.write("  prs {\n" +
                    "    DOMINO" + Inputs + "_" + Kstr + " domino(A,_X,en);\n" +
                    "  }\n");
            w.write("}\n\n");
            
            // with completion
            w.write("define V_LOGIC" + Inputs + "_" + Kstr + 
                    " <: V_LOGIC(" + Inputs + ") {\n");
            w.write("  prs {\n" +
                    "    DOMINO" + Inputs + "_" + Kstr + " domino(A,_X,eval);\n" +
                    "  }\n"); 
            w.write("}\n\n");
        }
        else if (Task.equals("bd")) {

            // combinational logic, include inverted versions too
            MultiSet gates = Karnaugh.getInvertedEquivalentGates(T);
            for (Iterator i = gates.iterator(); i.hasNext(); ) {
                long K = ((Long) i.next()).longValue();
                Kstr = Karnaugh.getGateString(K);
                w.write("define GATE" + Inputs + "_" + Kstr + 
                        " <: GATE(" + Inputs + ") {\n");
                w.write("  prs {\n" +
                        "    isochronic unstab " + 
                        Karnaugh.getFuncString(K,false,true,true) + " -> X+\n" +
                        "    isochronic unstab " + 
                        Karnaugh.getFuncString(K,false,true,false) + " -> X-\n" +
                        "  }\n");
                w.write("}\n\n");
            }
        }
    }
}
