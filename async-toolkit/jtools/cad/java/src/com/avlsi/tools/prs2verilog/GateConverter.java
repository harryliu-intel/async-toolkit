/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Set;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.lvs.Dnf;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionVisitorInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;

class GateConverter extends NetgraphGateConverter {
    private final MultiMap up, down;
    public GateConverter(final CellType cell,
                         final VerilogFactoryInterface factory,
                         final ConnectionInfo ports,
                         final HierName GND,
                         final boolean alwaysEscape,
                         final Prs2Verilog.VerilogChooser chooser,
                         final Cadencize cad) {
        super(cell, factory, ports, GND, alwaysEscape, chooser, cad);
        this.up = new MultiMap(new HashMap(), MultiMap.ARRAY_LIST_FACTORY);
        this.down = new MultiMap(new HashMap(), MultiMap.ARRAY_LIST_FACTORY);
    }

    protected String wireType(final CellNet net) {
        return "wire";
    }

    private boolean isCombinational(final OrBooleanExpressionInterface orUp,
                                    final OrBooleanExpressionInterface orDn) {
        final MultiSet dnfDn = Dnf.fromExpression(orDn, cell.namespace,
                                                  cell.exclusives);
        final MultiSet dualUp = Dnf.fromExpression(orUp.negated(),
                                                   cell.namespace,
                                                   cell.exclusives);
        return dnfDn.compareTo(dualUp) == 0;
    }

    public VerilogObject convert(final CommandLineArgs theArgs,
                                 final VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        if (unconvertible(cell)) return null;

        final boolean assumeCombinational =
            theArgs.argExists("assume-combinational");
        this.topEnv = topEnv;

        init(theArgs);

        //final String tauStr = theArgs.getArgValue("tau", null);
        //if (tauStr != null) tau = Float.parseFloat(tauStr);

        for (Iterator i = cell.prs.getProductionRules(); i.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) i.next();
            doProductionRule(pr.canonicalizeNames(cell.namespace));
        }

        final Set updown = new HashSet();
        updown.addAll(up.keySet());
        updown.addAll(down.keySet());

        final Set nostats =
            DirectiveUtils.getExplicitTrues(
                DirectiveUtils.canonizeKey(cell.namespace,
                    DirectiveUtils.getPrsDirective(
                            cell.cast_cell, DirectiveConstants.NO_STAT,
                            DirectiveConstants.NODE_TYPE)));

        for (Iterator i = updown.iterator(); i.hasNext(); ) {
            final HierName node = (HierName) i.next();
            Collection prsUp = up.get(node);
            if (prsUp == null) prsUp = Collections.EMPTY_LIST;
            Collection prsDn = down.get(node);
            if (prsDn == null) prsDn = Collections.EMPTY_LIST;
            final OrBooleanExpression orUp = new OrBooleanExpression(true,
                                                                     prsUp);
            final OrBooleanExpression orDn = new OrBooleanExpression(true,
                                                                     prsDn);
            if (assumeCombinational || isCombinational(orUp, orDn)) {
                final GateMaker gateUp = new GateMaker(node);
                orUp.visitWith(gateUp);
            } else {
                final GateMaker gateUp;
                if (prsUp.isEmpty()) {
                    gateUp = null;
                } else {
                    gateUp = new GateMaker();
                    orUp.visitWith(gateUp);
                }

                final GateMaker gateDn;
                if (prsDn.isEmpty()) {
                    gateDn = null;
                } else {
                    gateDn = new GateMaker();
                    orDn.visitWith(gateDn);
                }

                items.add(
                    createBlackBox(node,
                                   gateUp == null ? null : gateUp.result,
                                   gateDn == null ? null : gateDn.result,
                                   (nostats.contains(node) &&
                                    cell.getNet(node).isPortNet())
                                       ? "BLACKBOX" : "BLACKBOX_TRIREG"));
            }
        }
        final ArrayList params = new ArrayList();
        if (clk != null) addFormal(params, clk, "wire", "input");
        formal(params, ports, "wire");

        processSubcells();

        items.addAll(0, wireDecl);
        items.addAll(0, inout);
        addParameters();
        if (timescale) items.add(0, getTimeScaleMacro());

        final List topModel = new ArrayList();
        topModel.add(
            factory.module(moduleName == null ?
                factory.ident(getModuleName(cell.cast_cell), true) : moduleName,
                              (VerilogObject[])
                              params.toArray(new VerilogObject[0]),
                              (VerilogObject[])
                              items.toArray(new VerilogObject[0])));
        if (zoix && shouldSuppressFaults(cell)) {
            topModel.add(0, factory.expr("`suppress_faults\n"));
            topModel.add(factory.expr("`nosuppress_faults\n"));
        }

        return factory.compilationUnit((VerilogObject[])
                    topModel.toArray(new VerilogObject[0]));
    }

    private VerilogObject orGate(Collection wires) {
        final VerilogObject[] old = (VerilogObject[]) wires.toArray(new VerilogObject[0]);
        if (old.length == 1) return old[0];
        final VerilogObject[] terminals = new VerilogObject[old.length + 1];
        System.arraycopy(old, 0, terminals, 1, old.length);
        terminals[0] = newWire();
        items.add(factory.primitive("or", null, nextPrimitive(), terminals));
        return terminals[0];
    }

    private void doProductionRule(final ProductionRule pr) {
        final MultiMap current;
        if (pr.getDirection() == ProductionRule.UP) current = up;
        else current = down;
        final HierName target = pr.getTarget();
        current.put(target, pr.getGuard());
    }

    private class GateMaker implements BooleanExpressionVisitorInterface {
        /**
         * The desired final output node.  If not specified, a new unique node
         * will be generated.  If the expression contains only a literal, this
         * name will NOT be used.
         **/
        private HierName topNode;

        public VerilogObject result = null;

        public GateMaker() {
            this(null);
        }
        public GateMaker(final HierName topNode) {
            this.topNode = topNode;
        }
        private VerilogObject[] terminals(final Collection c,
                                          final boolean sense) {
            final ArrayList results = new ArrayList();
            final HierName desiredNode = topNode;
            topNode = null;
            for (Iterator i = c.iterator(); i.hasNext(); ) {
                final BooleanExpressionInterface expr =
                    (BooleanExpressionInterface) i.next();
                expr.visitWith(this);
                results.add(result);
            }
            topNode = desiredNode;
            final boolean newWire = desiredNode == null || !sense;
            results.add(0, newWire ? newWire() : lookupNode(desiredNode));
            return (VerilogObject[]) results.toArray(new VerilogObject[0]);
        }
        private void checkNot(final boolean sense) {
            if (!sense) {
                final VerilogObject[] args = new VerilogObject[2];
                args[0] = topNode == null ? newWire() : lookupNode(topNode);
                args[1] = result;
                items.add(
                    factory.primitive(
                        "not",
                        topNode == null ? null : getDelay(topNode),
                        nextPrimitive(), args));
                result = args[0];
            }
        }
        private BooleanExpressionInterface getFirst(final Collection c) {
            return (BooleanExpressionInterface) c.iterator().next();
        }
        public void visit(final AndBooleanExpressionInterface andExpr) {
            VerilogObject delay = null;
            final Collection conjuncts = andExpr.getConjuncts();
            if (conjuncts.size() == 1) {
                getFirst(conjuncts).visitWith(this);
            } else {
                final boolean sense = andExpr.getSense();
                final VerilogObject[] args = terminals(conjuncts, sense);
                result = args[0];
                if (topNode == null || !sense) {
                    items.add(factory.primitive("and", delay, nextPrimitive(),
                                                args));
                    checkNot(andExpr.getSense());
                } else {
                    items.add(factory.moduleInst(nextPrimitive(),
                                                 macro("AND"),
                                                 getDelayExpr(topNode),
                                                 args));
                }
            }
        }
        public void visit(final HierNameAtomicBooleanExpression atomicExpr) {
            final HierName name = atomicExpr.getName();
            if (atomicExpr.getSense()) {
                result = lookupNode(name);
            } else {
                result = lookupNot(name);
            }
            if (topNode != null) {
                final VerilogObject[] args = new VerilogObject[] {
                    lookupNode(topNode),
                    result
                };
                items.add(factory.primitive("buf", getDelay(topNode),
                                            nextPrimitive(), args));
            }
        }
        public void visit(final OrBooleanExpressionInterface orExpr) {
            VerilogObject delay = null;
            final Collection disjuncts = orExpr.getDisjuncts();
            if (disjuncts.size() == 1) {
                getFirst(disjuncts).visitWith(this);
            } else {
                final boolean sense = orExpr.getSense();
                final VerilogObject[] args = terminals(disjuncts, sense);
                result = args[0];
                if (topNode == null || !sense) {
                    items.add(factory.primitive("or", delay, nextPrimitive(),
                                                args));
                    checkNot(orExpr.getSense());
                } else {
                    items.add(factory.moduleInst(nextPrimitive(),
                                                 macro("OR"),
                                                 getDelayExpr(topNode),
                                                 args));
                }
            }
        }
    }
}
