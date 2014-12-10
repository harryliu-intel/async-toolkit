/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog.verilog;

import com.avlsi.tools.prs2verilog.verilog.CompilationUnit;
import com.avlsi.tools.prs2verilog.verilog.ContinuousAssign;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.Expr;
import com.avlsi.tools.prs2verilog.verilog.Ident;
import com.avlsi.tools.prs2verilog.verilog.MacroUse;
import com.avlsi.tools.prs2verilog.verilog.ModuleInst;
import com.avlsi.tools.prs2verilog.verilog.Module;
import com.avlsi.tools.prs2verilog.verilog.NetDecl;
import com.avlsi.tools.prs2verilog.verilog.Parameter;
import com.avlsi.tools.prs2verilog.verilog.ParameterDecl;
import com.avlsi.tools.prs2verilog.verilog.Primitive;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;

public class VerilogFactoryImpl implements VerilogFactoryInterface {
    public VerilogObject arrayAccess(final VerilogObject array,
                                     final VerilogObject index) {
        return new ArrayAccess(array, index);
    }
    public VerilogObject binaryOp(final VerilogObject op1,
                                  final String op,
                                  final VerilogObject op2) {
        return new BinaryOp(op1, op, op2);
    }
    public VerilogObject continuousAssign(final VerilogObject strength,
                                          final VerilogObject delay,
                                          final VerilogObject lhs,
                                          final VerilogObject rhs) {
        return new ContinuousAssign(strength, delay, lhs, rhs);
    }
    public VerilogObject delay(final VerilogObject all) {
        return new Delay(all);
    }
    public VerilogObject delay(final VerilogObject rise,
                               final VerilogObject fall) {
        return new Delay(rise, fall);
    }
    public VerilogObject delay(final VerilogObject rise,
                               final VerilogObject fall,
                               final VerilogObject off) {
        return new Delay(rise, fall, off);
    }
    public VerilogObject expr(final String expr) {
        return new Expr(expr);
    }
    public VerilogObject ident(final String ident, final boolean escape) {
        return new Ident(ident, escape);
    }
    public VerilogObject hierIdent(final VerilogObject[] parts) {
        return new HierIdent(parts);
    }
    public VerilogObject moduleInst(final VerilogObject ident,
                                    final VerilogObject module, 
                                    final VerilogObject[] parameters,
                                    final VerilogObject[] ports) {
        return new ModuleInst(ident, module, parameters, ports);
    }
    public VerilogObject module(final VerilogObject ident,
                                final VerilogObject[] ports,
                                final VerilogObject[] items) {
        return new Module(ident, ports, items);
    }
    public VerilogObject netDecl(final String type,
                                 final VerilogObject ident,
                                 final VerilogObject delay) {
        return new NetDecl(type, ident, delay);
    }
    public VerilogObject parameter(final VerilogObject type,
                                   final VerilogObject ident,
                                   final VerilogObject value) {
        return new Parameter(type, ident, value);
    }
    public VerilogObject parameterDecl(final VerilogObject ident,
                                       final String type) {
        return new ParameterDecl(ident, type);
    }
    public VerilogObject primitive(final String type,
                                   final VerilogObject delay,
                                   final VerilogObject ident,
                                   final VerilogObject[] terminals) {
        return new Primitive(type, delay, ident, terminals);
    }
    public VerilogObject unaryOp(final String op, final VerilogObject operand) {
        return new UnaryOp(op, operand);
    }
    public VerilogObject concatOp(final VerilogObject[] elements) {
        return new ConcatOp(elements);
    }
    public VerilogObject namedPort(final VerilogObject namedPort,
                                   final VerilogObject port) {
        return new NamedPort(namedPort, port);
    }
    public VerilogObject compilationUnit(final VerilogObject[] objects) {
        return new CompilationUnit(objects);
    }
    public VerilogObject macroUse(final VerilogObject ident,
                                  final VerilogObject[] args) {
        return new MacroUse(ident, args);
    }
    public VerilogObject lineComment(final String comment) {
        return new LineComment(comment);
    }
}
