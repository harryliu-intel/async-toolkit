/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.PrintWriter;
import java.io.Writer;

import java.util.Iterator;
import java.util.Map;

import antlr.Token;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;

public class CDLFactoryEmitter implements CDLFactoryInterface, CDLSimpleInterface {
    private final PrintWriter w;

    /**
     * The maximum line size.
     **/
    private final int maxLineSize;

    /**
     * Length of the current line is.
     **/
    private int linesize;

    /**
     * Filter out 0 width or length transistors, and 0 farad
     * capacitors?  Never skip 0 ohm resistors, since they are wires
     * and must be emitted!
     **/
    private final boolean filter0;

    /**
     * Surround expressions with single quotes?
     **/
    private final boolean quoteExpression;

    private final boolean evaluateTokens;

    /**
     * Delimiter to use between ports of an instantiation and the name of the
     * subcircuit to be instantiated.
     **/
    private final String callDelimiter;

    public CDLFactoryEmitter(final Writer w) {
        this(w, true);
    }

    public CDLFactoryEmitter(final Writer w, boolean filter0) {
        this(w, filter0, 999);
    }

    public CDLFactoryEmitter(final Writer w, boolean filter0, int maxLineSize) {
        this(w, filter0, maxLineSize, true);
    }

    public CDLFactoryEmitter(final Writer w, boolean filter0, int maxLineSize,
                             final boolean quoteExpression) {
        this(w, filter0, maxLineSize, quoteExpression, false);
    }

    public CDLFactoryEmitter(final Writer w, boolean filter0, int maxLineSize,
                             final boolean quoteExpression,
                             final boolean evaluateTokens) {
        this(w, filter0, maxLineSize, quoteExpression, evaluateTokens, "" /*"/"*/);
    }

    public CDLFactoryEmitter(final Writer w, boolean filter0, int maxLineSize,
                             final boolean quoteExpression,
                             final boolean evaluateTokens,
                             final String callDelimiter) {
        this.w = new PrintWriter(w);
        this.linesize = 0;
        this.filter0 = filter0;
        this.maxLineSize = maxLineSize;
        this.quoteExpression = quoteExpression;
        this.evaluateTokens = evaluateTokens;
        this.callDelimiter = callDelimiter;
    }


    private String stringNode(HierName node) {
        return node.getCadenceString();
    }

    private String stringToken(final Token token) {
        if (token instanceof CDLLexer.MathExprToken) {
            return "'" + token.getText() + "'";
        } else {
            return token.getText();
        }
    }

    /* Print the string as is */
    protected void print(String s) {
        linesize += s.length();
        w.print(s);
    }

    /* Create whitespace, then print the string */
    protected void printws(String s) {
        int l = s.length();
        if (linesize + l + 1 > maxLineSize) {
            println();
            w.print("+");
        } else {
            w.print(" ");
        }
        w.print(s);
        linesize += 1 + l;
    }

    /* Go to a newline */
    protected void println() {
        linesize = 0;
        w.println();
    }

    private boolean skip(CDLLexer.InfoToken token, Environment env) {
        if (filter0) {
            Double d = token.getValue(env);
            if (d != null && d.doubleValue() == 0) return true;
        }
        return false;
    }

    private boolean skip(double val) {
        return filter0 && val == 0;
    }

    private void keypair(Map parameters, Environment env) {
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Token val = (Token) entry.getValue();
            printws(key + "=" + getTokenVal(val, env));
        }
    }

    private String getTokenVal(Token token, Environment env) {
        if(evaluateTokens && token instanceof CDLLexer.InfoToken) {
            Double eval = ((CDLLexer.InfoToken)token).getValue(env);
            if(eval == null )
                return stringToken(token);
            else 
                return "" + eval.floatValue();
        }
        else
            return stringToken(token);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        if (skip(w, env) || skip(l, env)) return;
        print("M" + name.getCadenceString());
        printws(stringNode(nd));
        printws(stringNode(ng));
        printws(stringNode(ns));
        printws(stringNode(nb));
        printws(type);
        printws("w=" + getTokenVal(w, env));
        printws("l=" + getTokenVal(l, env));
        keypair(parameters, env);
        println();
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val, Map parameters, Environment env) {
        if (skip(val, env)) return;
        print("D" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(type);
        printws(getTokenVal(val, env));
        keypair(parameters, env);
        println();
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        // don't skip 0 ohm resistors!
        print("R" + name.getCadenceString());
        printws(stringNode(n1));
        printws(stringNode(n2));
        printws(getTokenVal(val, env));
        keypair(parameters, env);
        println();
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        if (skip(val, env)) return;
        print("C" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(getTokenVal(val, env));
        keypair(parameters, env);
        println();
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        if (skip(val, env)) return;
        print("L" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(getTokenVal(val, env));
        keypair(parameters, env);
        println();
    }

    public void makeBipolar(HierName name, String type,
                            HierName nc, HierName nb, HierName ne,
                            CDLLexer.InfoToken val, Map parameters,
                            Environment env) {
        if (skip(val, env)) return;
        print("Q" + name.getCadenceString());
        printws(stringNode(nc));
        printws(stringNode(nb));
        printws(stringNode(ne));
        printws(type);
        printws("area=" + getTokenVal(val, env));
        keypair(parameters, env);
        println();
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        print("X" + name.getCadenceString());

        for (int i = 0; i < args.length; i++) {
            printws(stringNode(args[i]));
        }

        if (!callDelimiter.equals("")) printws(callDelimiter);
        printws(subName);
        keypair(parameters, env);
        println();
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        print(".SUBCKT");
        printws(subName);

        for (int i = 0; i < in.length; i++) {
            printws(in[i]);
        }

        if (out.length > 0) {
            // printws("/");
            for (int i = 0; i < out.length; i++) {
                printws(out[i]);
            }
        }

        keypair(parameters, env);
        println();
    }

    public void endSubcircuit(String subName, Environment env) {
        print(".ENDS");
        println();
    }

    // Implementatin of CDLSimpleInterface
    private void keypairSimple(Map parameters) {
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Double val = (Double) entry.getValue();
            printws(key + "=" + val);
        }
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) {
        // don't skip 0 ohm resistors!
        print("R" + name.getCadenceString());
        printws(stringNode(n1));
        printws(stringNode(n2));
        printws(Double.toString(val));
        println();
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) {
        if (skip(val)) return;
        print("C" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(Double.toString(val));
        println();

    }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) {
        if (skip(w) || skip(l)) return;
        print("M" + name.getCadenceString());
        printws(stringNode(nd));
        printws(stringNode(ng));
        printws(stringNode(ns));
        printws(stringNode(nb));
        printws(type == DeviceTypes.N_TYPE ? "n" : "p");
        printws("w=" + w);
        printws("l=" + l);
        println();
    }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) {
        if (skip(a)) return;
        print("D" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(type == DeviceTypes.N_TYPE ? "dw" : "dp");
        printws(Double.toString(a));
        println();
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) {
        if (skip(val)) return;
        print("L" + name.getCadenceString());
        printws(stringNode(npos));
        printws(stringNode(nneg));
        printws(Double.toString(val));
        println();
    }

    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) {
        if (skip(a)) return;
        print("Q" + name.getCadenceString());
        printws(stringNode(nc));
        printws(stringNode(nb));
        printws(stringNode(ne));
        printws(type == DeviceTypes.N_TYPE ? "npn" : "pnp");
        printws("area=" + Double.toString(a));
        println();
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters) {
        print("X" + name.getCadenceString());

        for (int i = 0; i < args.length; i++) {
            printws(stringNode(args[i]));
        }

        if (!callDelimiter.equals("")) printws(callDelimiter);
        printws(subName);
        keypairSimple(parameters);
        println();
    }

    public void beginSubcircuit(String subName, String[] in, String[] out) {
        throw new RuntimeException();
    }

    public void endSubcircuit(String subName) {
        throw new RuntimeException();
    }
}
