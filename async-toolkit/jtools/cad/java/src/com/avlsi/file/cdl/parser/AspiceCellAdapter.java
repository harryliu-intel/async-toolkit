/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import antlr.collections.AST;
import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Diode;
import com.avlsi.file.aspice.Resistor;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.cdl.CDLFileFormatException;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLParser;
import com.avlsi.file.cdl.parser.CDLWalker;
import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.StringUtil;

public class AspiceCellAdapter extends CDLInterfaceSimplifier {
    private final Map aspiceCellRepos = new HashMap();
    private final Map portLists = new HashMap();
    private final char subcellConnectionSeparatorChar;
    private boolean assura_rcx_extract = false;
    private AspiceFile aspiceCell = null;

    public AspiceCellAdapter() {
        this('/');
    }

    public AspiceCellAdapter(final char subcellConnectionSeparatorChar) {
        this.subcellConnectionSeparatorChar = subcellConnectionSeparatorChar;
    }

    public void setAssuraRCXParsing() {
        assura_rcx_extract = true;
    }

    private HierName parseNodeName(HierName s) {
        return parseNodeName(s.getCadenceString());
    }

    /* Copied from com.avlsi.file.common.cdl.CDLParser */
    private boolean isInteger(final String s) {
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                return false;
            }
        }

        return true;
    }

    /* Copied from com.avlsi.file.common.cdl.CDLParser */
    private HierName parseNodeName(String s) {
        // convert arrays back
        s = StringUtil.replaceSubstring(s, "][", ",");

        // Convert 'Xblah/' to 'blah.' if this is an Assura RCX extract
        if (assura_rcx_extract) {
            StringTokenizer st = new StringTokenizer(s,"/");
            StringBuffer sb = new StringBuffer();
            while (st.hasMoreTokens()) {
                String x = st.nextToken();
                if (st.hasMoreTokens()) sb.append(x.substring(1)+".");
                else sb.append(x);
            }
            s = sb.toString();
        }
        try {
            if (s.startsWith("av")) {
                // spice uses av[A-Z][0-9]+(_[0-9]+)?
                // XXX: what if there is a real node called this?
                if (s.length() > 3 &&
                    s.charAt(2) >= 'A' && s.charAt(2) <= 'Z') {
                    final String t = s.substring(3);
                    if (isInteger(t))
                        s += '#';
                    else {
                        final int underIdx = t.indexOf('_');
                        if (underIdx != -1 &&
                            isInteger(t.substring(0, underIdx)) &&
                            isInteger(t.substring(underIdx + 1))) {
                            s += '#';
                        }
                    }
                }
            } else {
                // cdl uses integers for anonymous nodes: [0-9]+

                // add a hash on to the end if the name is an integer
                if (isInteger(s))
                    s += '#';
            }
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionFailure(e);
        }
    }
        
    public HierName [] getPorts(final String cellType) {
        return (HierName []) portLists.get(cellType);
    }

    public AspiceFile getCell(final String cellType) {
        return (AspiceFile) aspiceCellRepos.get(cellType);
    }

    public Iterator getCellTypes() {
        return aspiceCellRepos.keySet().iterator();
    }

    public void parseFile(final String fileName) throws CDLFileFormatException, IOException {
        parseStream(new FileInputStream(fileName));
    }

    public void parseStream(final InputStream in) throws CDLFileFormatException {
        final CDLLexer lexer = new CDLLexer(new InputStreamReader(in), false);
        final CDLParser parser = new CDLParser(lexer);
        parser.setASTNodeClass(CDLParser.ASTWithToken.class.getName());
        try {
            parser.goal();
            final AST ast = parser.getAST();
            final CDLWalker walker = new CDLWalker();
            walker.goal(ast, new LocalEnvironment(), this);
        } catch (RecognitionException e) {
            throw new CDLFileFormatException("Error parsing CDL file: " +
                                             e.getMessage(), e);
        } catch (TokenStreamException e) {
            throw new CDLFileFormatException("Error parsing CDL file: " +
                                             e.getMessage(), e);
        }
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) {
        aspiceCell.addResistor(new Resistor(parseNodeName(n1), parseNodeName(n2), val));
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) {
        aspiceCell.addCapacitor(new Capacitor(parseNodeName(npos), parseNodeName(nneg), val));
    }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) {
        aspiceCell.addTransistor(new Transistor(type, parseNodeName(ns), parseNodeName(nd), parseNodeName(ng), parseNodeName(nb), w, l));
    }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) {
        aspiceCell.addDiode(new Diode(type, parseNodeName(npos), parseNodeName(nneg), w, l, a, p));
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) {
        throw new RuntimeException("AspiceCell does not support inductors.");
    }

    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) {
        throw new RuntimeException("AspiceCell does not support bipolar devices.");
    }

    public void makeCall(HierName name, String subName, HierName[] args, Map parameters) {
        HierName[] ports = (HierName []) portLists.get(subName);
        if (ports == null) {
            throw new RuntimeException("Subcircuit " + subName + " undefined!");
        }
        if (ports.length != args.length) {
            throw new RuntimeException("Subcircuit " + subName + " defined with " + ports.length + " ports, but used with " + args.length + " ports!");
        }
        aspiceCell.addSubcell(name.getAsString('.'), getCell(subName));
        for (int i = 0; i < ports.length; i++) {
            HierName hn = null;
            try {
                hn = HierName.makeHierName(name.getAsString('.') +
                                           subcellConnectionSeparatorChar +
                                           ports[i].getAsString('.'), '.');
            } catch (InvalidHierNameException e) {
                throw new AssertionFailure(e);
            }
            aspiceCell.connectNames(args[i], hn);
        }
    }

    public void beginSubcircuit(String subName, String[] in, String[] out) {
        if (portLists.containsKey(subName)) {
            throw new RuntimeException("Subcircuit " + subName + " redefined!");
        }
        int index = 0;
        HierName[] all = new HierName[in.length + out.length];
        aspiceCell = new AspiceFile(subName, aspiceCellRepos);
        for (int i = 0; i < in.length; i++, index++) {
            all[index] = parseNodeName(in[i]);
            aspiceCell.addPort(all[index]);
        }
        for (int i = 0; i < out.length; i++, index++) {
            all[index] = parseNodeName(out[i]);
            aspiceCell.addPort(all[index]);
        }
        portLists.put(subName, all);
    }

    public void endSubcircuit(String subName) {
        aspiceCellRepos.put(subName, aspiceCell);
    }
}
