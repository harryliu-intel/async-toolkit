/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import java.io.PrintWriter;
import java.io.Writer;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;

public class VerilogEmitter implements VerilogVisitor {
    private final PrintWriter w;

    /**
     * The maximum number of characters on a line
     **/
    private final int maxLineSize;

    /**
     * The current number of characters on a line
     **/
    private int linesize;

    /**
     * If whitespace is needed before the next token is emitted
     **/
    private boolean needWhiteSpace;

    /**
     * Current non-white space token
     **/
    private String token;

    public VerilogEmitter(final Writer w) {
        this(w, 80);
    }

    public VerilogEmitter(final Writer w, int maxLineSize) {
        this.w = new PrintWriter(w);
        this.maxLineSize = maxLineSize;
        this.needWhiteSpace = false;
        this.token = "";
    }

    private void println() {
        ws();
        linesize = 0;
        w.println();
        needWhiteSpace = false;
    }
    private void print(String s) {
        // Glue together consecutive non-whitespace tokens
        token = token + s;
    }
    private void println(String s) {
        print(s);
        println();
    }
    /**
     * Write out any remaining text not already written out.
     **/
    public void flush() {
        ws();
    }

    private void ws() {
        // combine consecutive whitespace tokens into one
        if (needWhiteSpace && token.length() > 0) {
            // output the previous whitespace token
            if (linesize + token.length() + 1 > maxLineSize) {
                w.println();
                linesize = 0;
            } else {
                ++linesize;
                w.print(" ");
            }
        }
        // output the current non-whitespace token
        linesize += token.length();
        w.print(token);
        token = "";
        needWhiteSpace = true;
    }

    private void list(final VerilogObject[] objs) {
        if (objs.length == 0) return;
        for (int i = 0; i < objs.length - 1; ++i) {
            if (objs[i] != null) objs[i].accept(this);
            print(","); ws();
        }
        if (objs[objs.length - 1] != null) objs[objs.length - 1].accept(this);
    }
    public void arrayAccess(final VerilogObject array,
                            final VerilogObject index) {
        array.accept(this);
        print("[");
        index.accept(this);
        print("]");
    }
    public void binaryOp(final VerilogObject op1, final String op,
                         final VerilogObject op2) {
        print("(");
        op1.accept(this);
        print(" " + op + " ");
        op2.accept(this);
        print(")");
    }
    public void continuousAssign(final VerilogObject strength,
                                 final VerilogObject delay,
                                 final VerilogObject lhs,
                                 final VerilogObject rhs) {
        print("assign");
        if (strength != null) { ws(); strength.accept(this); }
        if (delay != null) { ws(); delay.accept(this); }
        ws(); lhs.accept(this);
        ws(); print("=");
        ws(); rhs.accept(this);
        println(";");
    }
    public void delay(final VerilogObject rise,
                      final VerilogObject fall,
                      final VerilogObject off) {
        print("#");
        if (fall != null) print("(");
        rise.accept(this);
        if (fall != null) { print(","); fall.accept(this); }
        if (off != null) { print(","); off.accept(this); }
        if (fall != null) print(")");
    }
    public void expr(final String expr) {
        print(expr);
    }
    public void ident(final String ident, final boolean escape) {
        print(VerilogUtil.escapeIfNeeded(ident));
    }
    public void hierIdent(final VerilogObject[] parts) {
        for (int i = 0; i < parts.length; ++i) {
            parts[i].accept(this);
            if (i < parts.length - 1) print(".");
        }
    }
    public void moduleInst(final VerilogObject ident,
                           final VerilogObject module,
                           final VerilogObject[] parameters,
                           final VerilogObject[] ports) {
        module.accept(this);
        if (parameters != null && parameters.length > 0) {
            ws();
            print("#(");
            list(parameters);
            print(")");
        }
        ws(); ident.accept(this);
        print("(");
        list(ports);
        println(");");
    }
    public void module(final VerilogObject ident,
                       final VerilogObject[] ports,
                       final VerilogObject[] items) {
        print("module");
        ws(); ident.accept(this);
        if (ports.length > 0) {
            print(" (");
            list(ports);
            print(")");
        }
        println(";");
        for (int i = 0; i < items.length; i++) {
            items[i].accept(this);
        }
        println("endmodule");
    }
    public void netDecl(final String type,
                        final VerilogObject ident,
                        final VerilogObject delay) {
        print(type);
        if (delay != null) { ws(); delay.accept(this); }
        ws(); ident.accept(this);
        println(";");
    }
    public void parameter(final VerilogObject type, final VerilogObject ident,
                          final VerilogObject value) {
        print("parameter");
        if (type != null) { ws(); type.accept(this); }
        ws(); ident.accept(this);
        ws(); print("=");
        ws(); value.accept(this);
        println(";");
    }
    public void parameterDecl(final VerilogObject ident, final String type) {
        print(type);
        ws(); ident.accept(this);
        println(";");
    }
    public void primitive(final String type,
                          final VerilogObject delay,
                          final VerilogObject ident,
                          final VerilogObject[] terminals) {
        print(type);
        if (delay != null) { ws(); delay.accept(this); }
        if (ident != null) { ws(); ident.accept(this); }
        ws(); print("("); list(terminals); println(");");
    }
    public void unaryOp(final String op, final VerilogObject operand) {
        print("(");
        print(op + " ");
        operand.accept(this);
        print(")");
    }
    public void concatOp(final VerilogObject[] elements) {
        print("{");
        list(elements);
        print("}");
    }
    public void namedPort(final VerilogObject portName,
                          final VerilogObject port) {
        print(".");
        portName.accept(this);
        print("(");
        if (port != null) port.accept(this);
        print(")");
    }
    public void compilationUnit(final VerilogObject[] objs) {
        for (int i = 0; i < objs.length - 1; ++i) {
            if (objs[i] != null) objs[i].accept(this);
            println();
        }
        if (objs[objs.length - 1] != null) objs[objs.length - 1].accept(this);
    }
    public void macroUse(final VerilogObject ident,
                         final VerilogObject[] args) {
        print("`");
        ident.accept(this);
        if (args != null) {
            print("(");
            list(args);
            println(")");
        }
    }
    public void lineComment(final String comment) {
        println();
        println("//" + comment);
    }
}
