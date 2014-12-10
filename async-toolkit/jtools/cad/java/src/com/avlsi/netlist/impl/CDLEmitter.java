/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.io.PrintWriter;
import java.io.Writer;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.NetlistFormatter;
import com.avlsi.util.text.NumberFormatter;

public class CDLEmitter extends NetlistFormatter implements Visitor {
    private static final Map suffixMap = new HashMap();
    private static final Set needSuffix = new HashSet();
    private final PrintWriter w;
    private final double deviceScale;
    private final String suffix;
    private final int precision;
    private final int maxLineSize;
    private int linesize;

    static {
        suffixMap.put(new Character('m'), new Double(1e3));
        suffixMap.put(new Character('u'), new Double(1e6));
        suffixMap.put(new Character('n'), new Double(1e9));
        suffixMap.put(new Character('p'), new Double(1e12));
        suffixMap.put(new Character('f'), new Double(1e15));

        needSuffix.add("nw"); /* n-stack width for gates */
        needSuffix.add("pw"); /* p-stack width for gates */
        needSuffix.add("nl"); /* n-stack length for staticizers */
        needSuffix.add("pl"); /* p-stack length for staticizers */
    }

    public CDLEmitter( final Writer w ) {
        this( w, "" );
    }

    public CDLEmitter(final Writer w, final String suffix) {
        this(w, suffix, -1);
    }
    public CDLEmitter(final Writer w, final String suffix, int precision) {
        this(w, suffix, precision, 999);
    }
    public CDLEmitter(final Writer w, final String suffix, int precision,
                      int maxLineSize) {
        this.w = new PrintWriter(w);
        double scale = 1;
        if (!suffix.equals("")) {
            /* CDL interprets the first letter in the suffix as the scale
             * factor */
            final Double x = (Double) suffixMap.get(new Character(Character.toLowerCase(suffix.charAt(0))));
            if (x != null) {
                scale = x.doubleValue();
            }
        }
        this.deviceScale = scale;
        this.suffix = suffix;
        this.precision = precision;
        this.linesize = 0;
        this.maxLineSize = maxLineSize;
    }

    private String stringNode(AbstractNode node) {
        return node.getCanonicalName().getCadenceString();
    }

    private String d2s(double num) {
        if (precision > 0) {
            return NumberFormatter.format(num, precision);
        } else {
            return Double.toString(num);
        }
    }

    private String lengthSuffix(double length) {
        return d2s(length * deviceScale) + suffix;
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

    private void keypair(Map parameters, Set needSuffix) {
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Double val = (Double) entry.getValue();
            final String sval;

            boolean found = false;
            for (Iterator j = needSuffix.iterator(); j.hasNext(); ) {
                final String prefix = (String) j.next();
                if (key.toLowerCase().startsWith(prefix)) {
                    found = true;
                    break;
                }
            }
            if (found) {
                sval = lengthSuffix(val.doubleValue());
            } else {
                sval = d2s(val.doubleValue());
            }
            printws(key + "=" + sval);
        }
    }

    private void keypair(Map parameters) {
        keypair(parameters, Collections.EMPTY_SET);
    }

    public void genericTransistor(final HierName name,
                                  final AbstractNode drain,
                                  final AbstractNode gate,
                                  final AbstractNode source,
                                  final AbstractNode bulk,
                                  final double length,
                                  final double width,
                                  final String type,
                                  final Map parameters) {
        
        print("M" + name.getCadenceString());
        printws(stringNode(drain));
        printws(stringNode(gate));
        printws(stringNode(source));
        printws(stringNode(bulk));
        printws(type);
        printws("W=" + lengthSuffix(width));
        printws("L=" + lengthSuffix(length));
        keypair(parameters);
        println();
    }

    public void genericDiode(final HierName name,
                             final AbstractNode positive,
                             final AbstractNode negative,
                             final double length,
                             final double width,
                             final double perimeter,
                             final double area,
                             final String type,
                             final Map parameters) { }

    public void genericResistor(final HierName name,
                                final AbstractNode node1,
                                final AbstractNode node2,
                                final double conductance,
                                final Map parameters) {
        print("R" + name.getCadenceString());
        printws(stringNode(node1));
        printws(stringNode(node2));
        printws(d2s(1 / conductance));
        keypair(parameters);
        println();
    }

    public void genericCapacitor(final HierName name,
                                 final AbstractNode positive,
                                 final AbstractNode negative,
                                 final double capacitance,
                                 final Map parameters) {
        print("C" + name.getCadenceString());
        printws(stringNode(positive));
        printws(stringNode(negative));
        printws(d2s(capacitance));
        keypair(parameters);
        println();
    }

    public void genericInductor(final HierName name,
                                final AbstractNode positive,
                                final AbstractNode negative,
                                final double inductance,
                                final Map parameters) { }

    public void subcircuitCall(final HierName name,
                               final AbstractNetlist circuit,
                               final AbstractNodeIterator nodes,
                               final Map parameters) {
        print("X" + name.getCadenceString());

        while (nodes.hasNext()) {
            printws(stringNode(nodes.next()));
        }

        printws("/");
        printws(circuit.getName().getCadenceString());
        keypair(parameters, needSuffix);
        println();
    }

    public void subcktBegin(final AbstractNetlist netlist) {
        print(".SUBCKT");
        printws(netlist.getName().getCadenceString());

        AbstractNodeIterator i;
        i = netlist.getOutputNodes();
        while (i.hasNext()) {
            printws(stringNode(i.next()));
        }

        i = netlist.getInputNodes();
        if (i.hasNext()) {
            printws("/");
            while (i.hasNext()) {
                printws(stringNode(i.next()));
            }
        }

        println();
    }

    public void subcktEnd(final AbstractNetlist netlist) {
        print(".ENDS");
        println();
    }
}
