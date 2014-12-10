/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/file/cdl/parser/CDLFactoryEmitter
.java#4 $
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util;

import java.io.IOException;
import java.io.Writer;

public class CDLWriter {
    public class Exception extends RuntimeException {
        public Exception(String message, Throwable cause) {
            super(message, cause);
        }
        public Exception(String message) {
            super(message);
        }
    }

    protected Writer w;
    protected int linesize, maxLineSize;

    public CDLWriter(final Writer w, final int maxLineSize) {
        this.w = w;
        this.linesize = 0;
        this.maxLineSize = maxLineSize;
    }

    private void write(final String s) {
        try {
            w.write(s);
        } catch (IOException e) {
            throw new Exception("I/O problems while writing", e);
        }
    }

    /* Print the string as is */
    protected void print(final String s) {
        linesize += s.length();
        write(s);
    }

    /* Create whitespace, then print the string */
    protected void printws(final String s) {
        int l = s.length();
        if (linesize + l + 1 > maxLineSize) {
            println();
            write("+");
        } else {
            write(" ");
        }
        write(s);
        linesize += 1 + l;
    }

    protected void printws(final String[] s) {
        for (int i = 0; i < s.length; ++i) printws(s[i]);
    }

    /* Go to a newline */
    protected void println() {
        linesize = 0;
        write("\n");
    }

    public void comment(String comment) {
        print("*" + comment);
        println();
    }

    public void subckt(String name, String[] in, String[] out,
                       String[] parameters) {
        print(".SUBCKT");
        printws(name);

        printws(in);
        printws(out);
        printws(parameters);
        println();
    }

    public void ends(String name) {
        print(".ENDS");
        println();
    }

    public void C(String name, String n1, String n2, String mname,
                  String capacitance, String[] parameters) {
        print("C" + name);
        printws(n1);
        printws(n2);
        if (mname != null) printws(mname);
        if (capacitance != null) printws(capacitance);
        printws(parameters);
        println();
    }

    public void E(String name, String np, String nn, String keyword,
                  String[] parameters) {
        print("E" + name);
        printws(np);
        printws(nn);
        printws(keyword);
        printws(parameters);
        println();
    }

    public void G(String name, String np, String nn, String keyword,
                  String[] parameters) {
        print("G" + name);
        printws(np);
        printws(nn);
        printws(keyword);
        printws(parameters);
        println();
    }

    public void M(String name, String nd, String ng, String ns, String nb,
                  String mname, String length, String width,
                  String[] parameters) {
        print("M" + name);
        printws(nd);
        printws(ng);
        printws(ns);
        if (nb != null) printws(nb);
        printws(mname);
        printws("L=" + length);
        printws("W=" + width);
        printws(parameters);
        println();
    }

    public void R(String name, String n1, String n2, String mname,
                  String resistance, String[] parameters) {
        print("R" + name);
        printws(n1);
        printws(n2);
        if (mname != null) printws(mname);
        printws(resistance);
        printws(parameters);
        println();
    }

    public void X(String name, String[] args, String subname,
                  String[] parameters) {
        print("X" + name);
        printws(args);
        printws(subname);
        printws(parameters);
        println();
    }
}
