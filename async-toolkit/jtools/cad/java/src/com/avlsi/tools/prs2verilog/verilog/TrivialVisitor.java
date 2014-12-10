/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

/**
 * An implementation <code>VerilogVisitor</code> with all empty methods.
 **/
public class TrivialVisitor implements VerilogVisitor {
    public void arrayAccess(final VerilogObject array,
                            final VerilogObject index) { }
    public void binaryOp(final VerilogObject op1, final String op,
                         final VerilogObject op2) { }
    public void continuousAssign(final VerilogObject strength,
                                 final VerilogObject delay,
                                 final VerilogObject lhs,
                                 final VerilogObject rhs) { }
    public void delay(final VerilogObject rise, final VerilogObject fall,
                      final VerilogObject off) { }
    public void expr(final String expr) { }
    public void ident(final String ident, final boolean escape) { }
    public void hierIdent(final VerilogObject[] parts) { }
    public void moduleInst(final VerilogObject ident,
                           final VerilogObject module,
                           final VerilogObject[] parameters,
                           final VerilogObject[] ports) { }
    public void module(final VerilogObject ident,
                       final VerilogObject[] ports,
                       final VerilogObject[] items) { }
    public void netDecl(final String type,
                        final VerilogObject ident,
                        final VerilogObject delay) { }
    public void parameter(final VerilogObject type, final VerilogObject ident,
                          final VerilogObject value) { }
    public void parameterDecl(final VerilogObject ident, final String type) { }
    public void primitive(final String type,
                          final VerilogObject delay,
                          final VerilogObject ident,
                          final VerilogObject[] terminals) { }
    public void unaryOp(final String op, final VerilogObject operand) { }
    public void concatOp(final VerilogObject[] elements) { }
    public void namedPort(final VerilogObject portName,
                          final VerilogObject port) { }
    public void compilationUnit(final VerilogObject[] objects) { }
    public void macroUse(final VerilogObject ident,
                         final VerilogObject[] args) { }
    public void lineComment(final String comment) { }
}
