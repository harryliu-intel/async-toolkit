/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.PrintWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import antlr.collections.AST;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLParser;
import com.avlsi.file.cdl.parser.CDLWalker;
import com.avlsi.file.common.HierName;

/**
 * An CDLFactoryInterface that can be used to rename nodes and names of circuit
 * components to another format
 **/
public class RenumberFactory implements CDLFactoryInterface {
    private static int MAX_LENGTH = 256;

    private interface TranslateInterface {
        /** Translates the name of a component */
        String translateName(String name, int tries);
        /** Translates the name of a node */
        String translateNode(String name, int tries);
        /** Translates the name of a subcircuit */
        String translateCircuit(String name, int tries);
    }

    private static class NullTranslation implements TranslateInterface {
        public String translateName(String name, int tries) {
            return name;
        }

        public String translateNode(String name, int tries) {
            return name;
        }

        public String translateCircuit(String name, int tries) {
            return name;
        }
    }

    private static class SpiceTranslation implements TranslateInterface {
        public String translateName(String name, int tries) {
            return translate(name, tries);
        }

        public String translateNode(String name, int tries) {
            return translate(name, tries);
        }

        public String translateCircuit(String name, int tries) {
            return translate(name, tries);
        }
        
        private String translate(String s, int tries) {
            String ss = translate(s);
            if (tries > 0) {
                return ss + tries;
            } else {
                return ss;
            }
        }

        private String translate(String s) {
            StringBuffer sb = new StringBuffer();
            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                    continue;
                }
                switch (c) {
                  case '.': sb.append("_D_"); break;
                  case ',': sb.append("_C_"); break;
                  case '[': sb.append("_l_"); break;
                  case ']': sb.append("_r_"); break;
                  case '(': sb.append("_L_"); break;
                  case ')': sb.append("_R_"); break;
                  case '-': sb.append("_M_"); break;
                  case '_': sb.append("_U_"); break;
                  default: sb.append("_"); sb.append((int) c); sb.append("_");
                }
            }
            return sb.toString();
        }
    }

    private Map nameMap, nodeMap, circuitMap;
    private PrintWriter w;
    private TranslateInterface trans;

    private static void usage() {
        System.err.println("Usage: java com.avlsi.file.cdl.parser.RenumberFactory [ null | spice ] < old_cdlfile > new_cdlfile");
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 1) {
            usage();
            System.exit(1);
        }
        final CDLLexer lexer = new CDLLexer(new InputStreamReader(System.in), false);
        final CDLParser parser = new CDLParser(lexer);
        parser.setASTNodeClass(CDLParser.ASTWithToken.class.getName());
        parser.goal();
        final AST ast = parser.getAST();
        final CDLWalker walker = new CDLWalker();
        final Writer w = new OutputStreamWriter(System.out);
        final TranslateInterface ti;
        if (args[0].equals("spice")) ti = new SpiceTranslation();
        else ti = new NullTranslation();
        final RenumberFactory factory = new RenumberFactory(w, ti);
        walker.goal(ast, new LocalEnvironment(), factory);
        w.flush();
    }

    public RenumberFactory(Writer w, TranslateInterface trans) {
        this.w = new PrintWriter(w);
        this.nameMap = new HashMap();
        this.nodeMap = new HashMap();
        this.circuitMap = new HashMap();
        this.trans = trans;
    }

    private void checkSize(String old, String s) {
        if (s.length() > MAX_LENGTH) {
            System.err.println("New name too long: \"" + old + "\" -> \"" + s + "\"");
        }
    }

    private String newName(String old) {
        int tries = 0;
        String s = trans.translateName(old, tries++);
        while (nameMap.containsValue(s)) {
            s = trans.translateName(old, tries++);
        }
        checkSize(old, s);
        return s;
    }

    private String newNode(String old) {
        int tries = 0;
        String s = trans.translateNode(old, tries++);
        while (nodeMap.containsValue(s)) {
            s = trans.translateNode(old, tries++);
        }
        checkSize(old, s);
        return s;
    }

    private String newCircuit(String old) {
        int tries = 0;
        String s = trans.translateCircuit(old, tries++);
        while (circuitMap.containsValue(s)) {
            s = trans.translateCircuit(old, tries++);
        }
        checkSize(old, s);
        return s;
    }

    private String lookupName(String name) {
        if (!nameMap.containsKey(name)) nameMap.put(name, newName(name));
        return (String) nameMap.get(name);
    }

    private String lookupNode(String name) {
        if (!nodeMap.containsKey(name)) nodeMap.put(name, newNode(name));
        return (String) nodeMap.get(name);
    }

    private String lookupCircuit(String name) {
        if (!circuitMap.containsKey(name)) circuitMap.put(name, newCircuit(name));
        return (String) circuitMap.get(name);
    }

    private String lookupName(HierName name) {
        return lookupName(name.getCadenceString());
    }

    private String lookupNode(HierName name) {
        return lookupNode(name.getCadenceString());
    }

    private void outputParameters(Map parameters, Environment env) {
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String key = (String) entry.getKey();
            final String val = ((CDLLexer.InfoToken) entry.getValue()).getText(env);
            w.print(" " + key + "=" + val);
        }
    }

    public void makeResistor(HierName name,
                             HierName n1,
                             HierName n2,
                             CDLLexer.InfoToken val,
                             Map parameters,
                             Environment env) {
        w.print("R" + lookupName(name) + " " + lookupNode(n1) + " " + lookupNode(n2) + " " + val.getText(env));
        outputParameters(parameters, env);
        w.println();
    }

    public void makeCapacitor(HierName name,
                              HierName npos,
                              HierName nneg,
                              CDLLexer.InfoToken val,
                              Map parameters,
                              Environment env) {
        w.print("C" + lookupName(name) + " " + lookupNode(npos) + " " + lookupNode(nneg) + " " + val.getText(env));
        outputParameters(parameters, env);
        w.println();
    }

    public void makeTransistor(HierName name,
                               String type,
                               HierName ns,
                               HierName nd,
                               HierName ng,
                               HierName nb,
                               CDLLexer.InfoToken width,
                               CDLLexer.InfoToken length,
                               Map parameters,
                               Environment env) {
        w.print("M" + lookupName(name) + " " + lookupNode(nd) + " " + lookupNode(ng) + " " + lookupNode(ns) + " " + lookupNode(nb) + " " + type + " W=" + width.getText(env) + " L=" + length.getText(env));
        outputParameters(parameters, env);
        w.println();
    }

    public void makeDiode(HierName name,
                          String type,
                          HierName npos,
                          HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters,
                          Environment env) {
        w.print("D" + lookupName(name) + " " + lookupNode(npos) + " " + lookupNode(nneg) + " " + val.getText(env));
        outputParameters(parameters, env);
        w.println();
    }

    public void makeInductor(HierName name,
                             HierName npos,
                             HierName nneg,
                             CDLLexer.InfoToken val,
                             Map parameters,
                             Environment env) {
        w.print("L" + lookupName(name) + " " + lookupNode(npos) + " " + lookupNode(nneg) + " " + val.getText(env));
        outputParameters(parameters, env);
        w.println();
    }

    public void makeBipolar(HierName name,
                            String type,
                            HierName nc,
                            HierName nb,
                            HierName ne,
                            CDLLexer.InfoToken val,
                            Map parameters,
                            Environment env) {
        w.print("Q" + lookupName(name) + " " + lookupNode(nc) + " " + lookupNode(nb) + " " + lookupNode(ne) + " " + type + " area=" + val.getText(env));
        outputParameters(parameters, env);
        w.println();
    }


    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        w.print("X" + lookupName(name));
        for (int i = 0; i < args.length; i++) {
            w.print(" " + lookupNode(args[i]));
        }
        w.print(" / " + lookupCircuit(subName));
        outputParameters(parameters, env);
        w.println();
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        w.print(".SUBCKT " + lookupCircuit(subName));
        for (int i = 0; i < in.length; i++) {
            w.print(" " + lookupNode(in[i]));
        }
        for (int i = 0; i < out.length; i++) {
            w.print(" " + lookupNode(out[i]));
        }
        outputParameters(parameters, env);
        w.println();
    }

    /**
     * Called by the parser after processing a subcircuit.
     * @param subName Name of the subcircuit
     **/
    public void endSubcircuit(String subName, Environment env) {
        w.println(".ENDS");
    }
}
