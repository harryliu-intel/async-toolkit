/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;

public interface VerilogVisitor {
    void arrayAccess(final VerilogObject array,
                     final VerilogObject index);
    void binaryOp(final VerilogObject op1, final String op,
                  final VerilogObject op2);
    void continuousAssign(final VerilogObject strength,
                          final VerilogObject delay,
                          final VerilogObject lhs,
                          final VerilogObject rhs);
    void delay(final VerilogObject rise, final VerilogObject fall,
               final VerilogObject off);
    void expr(final String expr);
    void ident(final String ident, final boolean escape);
    void hierIdent(final VerilogObject[] parts);
    void moduleInst(final VerilogObject ident,
                    final VerilogObject module,
                    final VerilogObject[] parameters,
                    final VerilogObject[] ports);
    void module(final VerilogObject ident,
                final VerilogObject[] ports,
                final VerilogObject[] items);
    void netDecl(final String type,
                 final VerilogObject ident,
                 final VerilogObject delay);
    void parameter(final VerilogObject type, final VerilogObject ident,
                   final VerilogObject value);
    void parameterDecl(final VerilogObject ident, final String type);
    void primitive(final String type,
                   final VerilogObject delay,
                   final VerilogObject ident,
                   final VerilogObject[] terminals);
    void unaryOp(final String op, final VerilogObject operand);
    void concatOp(final VerilogObject[] elements);

    /**
     * A named port connection.
     * @param portName Name of the port.
     * @param port Net to connect to the port, or <code>null</code> to indicate
     * no connection.
     **/
    void namedPort(final VerilogObject portName, final VerilogObject port);
    void compilationUnit(final VerilogObject[] objects);

    /**
     * A macro usage.  For example, <code>`BLACKBOX</code>.
     * @param ident Name of the macro
     * @param args Optional arguments to the macro
     **/
    void macroUse(final VerilogObject ident, final VerilogObject[] args);

    /**
     * A single line comment.
     * @param comment Comment.
     **/
    void lineComment(final String comment);
}
