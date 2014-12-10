/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.VerilogObject;

public interface VerilogFactoryInterface {
    VerilogObject arrayAccess(final VerilogObject array,
                              final VerilogObject index);
    VerilogObject binaryOp(final VerilogObject op1,
                           final String op,
                           final VerilogObject op2);
    VerilogObject continuousAssign(final VerilogObject strength,
                                   final VerilogObject delay,
                                   final VerilogObject lhs,
                                   final VerilogObject rhs);
    VerilogObject delay(final VerilogObject all);
    VerilogObject delay(final VerilogObject rise, final VerilogObject fall);
    VerilogObject delay(final VerilogObject rise, final VerilogObject fall,
                        final VerilogObject off);
    VerilogObject expr(final String expr);
    VerilogObject ident(final String ident, final boolean escape);
    VerilogObject hierIdent(final VerilogObject[] parts);
    VerilogObject moduleInst(final VerilogObject ident,
                             final VerilogObject module,
                             final VerilogObject[] parameters,
                             final VerilogObject[] ports);
    VerilogObject module(final VerilogObject ident,
                         final VerilogObject[] ports,
                         final VerilogObject[] items);
    VerilogObject netDecl(final String type,
                          final VerilogObject ident,
                          final VerilogObject delay);
    VerilogObject parameter(final VerilogObject type,
                            final VerilogObject ident,
                            final VerilogObject value);
    VerilogObject parameterDecl(final VerilogObject ident,
                                final String type);
    VerilogObject primitive(final String type,
                            final VerilogObject delay,
                            final VerilogObject ident,
                            final VerilogObject[] terminals);
    VerilogObject unaryOp(final String op, final VerilogObject operand);
    VerilogObject concatOp(final VerilogObject[] elements);
    VerilogObject namedPort(final VerilogObject portName,
                            final VerilogObject port);
    VerilogObject compilationUnit(final VerilogObject[] objects);
    VerilogObject macroUse(final VerilogObject ident,
                           final VerilogObject[] args);
    VerilogObject lineComment(final String comment);
}
