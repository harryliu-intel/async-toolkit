/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Set;

import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionVisitorInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;

class TriConverter extends AbstractConverter {
    private final CellType cell;
    private final Map up, down;
    private final ConnectionInfo ports;
    private float tau = 1;
    public TriConverter(final CellType cell,
                        final VerilogFactoryInterface factory,
                        final ConnectionInfo ports,
                        final boolean alwaysEscape ) {
        super(factory, alwaysEscape);
        this.cell = cell;
        this.up = new HashMap();
        this.down = new HashMap();
        this.ports = ports;
    }

    private String h2s(final HierName h) {
        return h.getCadenceString();
    }
    public VerilogObject convert(final CommandLineArgs theArgs,
                                 final VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        if (unconvertible(cell)) return null;

        //final String tauStr = theArgs.getArgValue("tau", null);
        //if (tauStr != null) tau = Float.parseFloat(tauStr);

        for (Iterator i = cell.prs.getProductionRules(); i.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) i.next();
            doProductionRule(pr.canonicalizeNames(cell.namespace));
        }

        final Set updown = new HashSet();
        updown.addAll(up.keySet());
        updown.addAll(down.keySet());
        for (Iterator i = updown.iterator(); i.hasNext(); ) {
            final HierName node = (HierName) i.next();
            final VerilogObject pullup = (VerilogObject) up.get(node);
            final VerilogObject pulldown = (VerilogObject) down.get(node);
            final VerilogObject lhs = lookupNode(node, "trireg");
            //Debug.assertTrue(pullup != null, "Node " + node + " has no pull up network!");
            //Debug.assertTrue(pulldown != null, "Node " + node + " has no pull down network!");
            if (pullup != null) {
                final VerilogObject upz = fire("bufif1", pullup);
                items.add(factory.continuousAssign(null, null, lhs, upz));
            }
            if (pulldown != null) {
                final VerilogObject downz = fire("notif1", pulldown);
                items.add(factory.continuousAssign(null, null, lhs, downz));
            }
        }
        final ArrayList params = new ArrayList();
        formal(params, ports, "trireg");

        for (Iterator i = cell.getAllSubcellConnections().iterator();
             i.hasNext(); ) {
            final ConnectionInfo ci = (ConnectionInfo) i.next();
            final ArrayList args = new ArrayList();
            actual(args, ci, "trireg");
            final VerilogObject ident =
                factory.ident(ci.nameInParent.getCadenceString(), true);
            final VerilogObject[] parameters = getModuleParameter(ci);
            items.add(factory.moduleInst(ident,
                                         factory.ident(ci.child.cast_cell
                                             .getFullyQualifiedType(), true),
                                         parameters,
                                         (VerilogObject[])
                                         args.toArray(new VerilogObject[0])));
        }

        items.addAll(0, wireDecl);
        items.addAll(0, inout);
        return factory.module(moduleName == null ?
                                factory.ident(cell.cast_cell
                                       .getFullyQualifiedType(), true) :
                                moduleName,
                              (VerilogObject[])
                              params.toArray(new VerilogObject[0]),
                              (VerilogObject[])
                              items.toArray(new VerilogObject[0]));
    }

    private VerilogObject fire(final String buftype, final VerilogObject wor) {
        final VerilogObject[] args = new VerilogObject[3];
        args[0] = newNet("wire");
        args[1] = wor;
        args[2] = wor;
        items.add(factory.primitive(buftype, null, nextPrimitive(), args));
        return args[0];
    }

    private void doProductionRule(final ProductionRule pr) {
        final Map current;
        final String suffix;
        if (pr.getDirection() == ProductionRule.UP) {
            current = up;
            suffix = "+";
        } else {
            current = down;
            suffix = "-";
        }
        ExprMaker v = new ExprMaker();
        pr.getGuard().visitWith(v);

        final VerilogObject delay =
            factory.delay(getDelayWithMacro(factory.expr(Float.toString(
                            cell.getDelay().getDelay(
                                pr.getTarget(),
                                pr.getDirection() == ProductionRule.UP,
                                tau)))));

        final String target = h2s(pr.getTarget()) + suffix;
        final VerilogObject rhs = lookupNode(target, "trior");
        final VerilogObject ca =
            factory.continuousAssign(null, delay, rhs, v.result);
        items.add(ca);
        current.put(pr.getTarget(), rhs);
    }

    private class ExprMaker implements BooleanExpressionVisitorInterface {
        public VerilogObject result = null;
        private void terminals(final Collection c, final String op) {
            VerilogObject tmp = null;
            for (Iterator i = c.iterator(); i.hasNext(); ) {
                final BooleanExpressionInterface expr =
                    (BooleanExpressionInterface) i.next();
                expr.visitWith(this);
                if (tmp == null) {
                    tmp = result;
                } else {
                    tmp = factory.binaryOp(tmp, op, result);
                }
            }
            result = tmp;
        }
        private void checkNot(final boolean sense) {
            if (!sense) {
                result = factory.unaryOp("~", result);
            }
        }
        public void visit(final AndBooleanExpressionInterface andExpr) {
            final Collection conjuncts = andExpr.getConjuncts();
            terminals(conjuncts, "&");
            checkNot(andExpr.getSense());
        }
        public void visit(final HierNameAtomicBooleanExpression atomicExpr) {
            final HierName name = atomicExpr.getName();
            result = lookupNode(name, "trireg");
            checkNot(atomicExpr.getSense());
        }
        public void visit(final OrBooleanExpressionInterface orExpr) {
            final Collection disjuncts = orExpr.getDisjuncts();
            terminals(disjuncts, "|");
            checkNot(orExpr.getSense());
        }
    }
}
