// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */         

package com.avlsi.tools.prs2verilog;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.jauto.CastQuery;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.TrivialVisitor;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.text.PrintfFormat;

public abstract class NetgraphGateConverter extends AbstractConverter {
    public static enum PowerPortHandling {
        /** use standard CAST rules */
        CAST,
        /** don't emit power rail connections */
        SKIP,
        /** add power rails connections for verilog block instances */
        VERILOG_ADD
    }
    public static final String CLK = "PRS2VERILOG_clk";
    protected final CellType cell;
    protected final ConnectionInfo ports;
    protected final HierName GND;
    protected final Cadencize cad;
    protected float tau = 1;
    protected boolean internalWire = false;
    protected boolean topEnv = false;
    protected String clk = null;
    protected Map transportUp = null, transportDn = null;
    protected final Prs2Verilog.VerilogChooser chooser;
    protected boolean zoix = false;
    protected boolean zeroify = false;
    protected boolean oneify = false;
    protected PowerPortHandling skip = PowerPortHandling.CAST;
    protected int powerRailOption = 0;
    protected boolean outputDelay = false;
    protected boolean timescale = false;
    protected final static PrintfFormat fmt = new PrintfFormat("%.6g");
    private Set<HierName> powerNets = new HashSet<>();
    private Set<HierName> groundNets = new HashSet<>();
    private Set<HierName> childPowerNets = new HashSet<>();
    private Set<HierName> childGroundNets = new HashSet<>();
    private Set<HierName> childPorts = new HashSet<>();

    public NetgraphGateConverter(final CellType cell,
                                 final VerilogFactoryInterface factory,
                                 final ConnectionInfo ports,
                                 final HierName GND,
                                 final boolean alwaysEscape,
                                 final Prs2Verilog.VerilogChooser chooser,
                                 final Cadencize cad) {
        super(factory, alwaysEscape);
        this.cell = cell;
        this.ports = ports;
        this.GND = GND;
        this.chooser = chooser;
        this.cad = cad;
        updatePowerNets(powerNets, groundNets, cell);
    }

    private void updatePowerNets(final Set<HierName> powerNets,
                                 final Set<HierName> groundNets,
                                 final CellType cell) {
        DirectiveUtils.getNetDirectives(cell.cast_cell, null,
                cad.convert(cell.cast_cell).getLocalNodes(), null,
                DirectiveConstants.POWER_NET, 
                powerNets, powerNets, new HashSet<>());
        DirectiveUtils.getNetDirectives(cell.cast_cell, null,
                cad.convert(cell.cast_cell).getLocalNodes(), null,
                DirectiveConstants.GROUND_NET, 
                groundNets, groundNets, new HashSet<>());
    }

    protected VerilogObject newWire() {
        return newNet("wire");
    }

    protected String wireType(final HierName net) {
        return wireType(cell.getNet(net));
    }

    private boolean isRail(final Set<HierName> powerNets,
                           final Set<HierName> groundNets,
                           final HierName net) {
        return powerNets.contains(net) || groundNets.contains(net);
    }

    private boolean isRail(final Set<HierName> powerNets,
                           final Set<HierName> groundNets,
                           final String net) {
        return isRail(powerNets, groundNets, HierName.makeHierNameUnchecked(net, '.'));
    }

    protected void actual(final List params, final ConnectionInfo ci,
                          final boolean byName, final UnaryFunction uf) {
        super.actual(params, ci, byName, uf);
    }

    protected void addFormal(final List params, final String name,
                             final String type, final String dir) {
        if (skip != PowerPortHandling.SKIP || !isRail(powerNets, groundNets, name))
            super.addFormal(params, name, type, dir);
    }   

    protected void addActual(final boolean byName, final List params,
                             final HierName child, final HierName parent,
                             final String type) {
        if (skip != PowerPortHandling.SKIP || !isRail(childPowerNets, childGroundNets, child))
            super.addActual(byName, params, child, parent, type);
    }

    protected abstract String wireType(final CellNet net);

    protected VerilogObject lookupNode(final HierName node) {
        final CellNet net = cell.getNet(node);
        return lookupNode(node, wireType(net));
    }       

    protected VerilogObject lookupNot(final HierName node) {
        final CellNet net = cell.getNet(node);
        return lookupNot(node, wireType(net));
    }       

    protected String getDelayMacroName() {
        return transportUp == null ? PRS2VERILOG_TAU : PRS2VERILOG_DELAY_SCALE;
    }

    protected VerilogObject getDelayBiasParameter() {
        return factory.ident(ConverterConstants.getDelayBiasString(), false);
    }

    protected VerilogObject getExtraDelayParameter(final HierName node,
                                                   final boolean up) {
        return factory.ident(ConverterConstants.getExtraDelayString(node, up),
                             false);
    }

    protected VerilogObject getDelayWithMacro(final VerilogObject o) {
        return factory.binaryOp(getDelayBiasParameter(), "*",
                                super.getDelayWithMacro(o));
    }

    protected VerilogObject lookupNode(final String node, final String type) {
        final CellNet net = cell.getNet(node);
        final VerilogObject delayExpr;
        if (net != null && net.isInternalNet()) {
            final double up, dn;
            if (transportUp == null) {
                up = 0; dn = 0;
            } else {
                final Float pu = (Float) transportUp.get(net.canonicalName);
                up = pu == null ? 0 : pu.doubleValue();

                final Float nd = (Float) transportDn.get(net.canonicalName);
                dn = nd == null ? 0 : nd.doubleValue();
            }
            delayExpr = up == 0 && dn == 0 ? null :
                getDelay(getDelayExpr(fmt.sprintf(up), fmt.sprintf(dn)));
        } else {
            delayExpr = null;
        }
        return lookupNode(node, type, delayExpr);
    }

    private VerilogObject getDelayExpr(final VerilogObject prsDelay,
                                       final VerilogObject delayBias,
                                       final VerilogObject extraDelay) {
        if (zoix) {
            // (prsDelay * delayBias + extraDelay) * PRS2VERILOG_TAU
            return
                factory.binaryOp(
                    factory.binaryOp(
                        factory.binaryOp(prsDelay, "*", delayBias),
                        "+",
                        extraDelay),
                    "*",
                    macro(getDelayMacroName()));
        } else {
            return factory.macroUse(
                    factory.ident(ConverterConstants.getDelayMacroString(),
                                  false),
                    new VerilogObject[] { prsDelay, delayBias, extraDelay });
        }
    }

    protected VerilogObject[] getDelayExpr(final HierName node) {
        if (!outputDelay && (clk != null || transportUp != null)) return null;
        final float up = cell.getDelay().getDelay(node, true, tau);
        final VerilogObject upObj =
            cell.getDelay().isAbsolute(node, true) ?
                factory.expr(String.format("%.3fps", up * 100.0 / tau))
              : getDelayExpr(factory.expr(fmt.sprintf(up)),
                             getDelayBiasParameter(),
                             getExtraDelayParameter(node, true));
                
        final float dn = cell.getDelay().getDelay(node, false, tau);
        final VerilogObject dnObj =
            cell.getDelay().isAbsolute(node, false) ?
                factory.expr(String.format("%.3fps", dn * 100.0 / tau))
              : getDelayExpr(factory.expr(fmt.sprintf(dn)),
                             getDelayBiasParameter(),
                             getExtraDelayParameter(node, false));

        return new VerilogObject[] { upObj, dnObj };
    }

    protected VerilogObject getDelay(final HierName node) {
        final VerilogObject[] delays = getDelayExpr(node);
        if (delays == null) return null;
        else return getDelay(delays);
    }

    private class VddGndBlockValue implements AbstractConverter.BlockValue {
        private final AbstractConverter.BlockValue bv;
        public VddGndBlockValue(final AbstractConverter.BlockValue bv) {
            this.bv = bv;
        }
        public VerilogObject node(final HierName instance,
                                  final HierName name) {
            return constify(bv.node(instance, name));
        }
        public VerilogObject array(final VerilogObject[] objs) {
            return bv.array(objs);
        }
    }

    protected Pair verilogInstance(final VerilogBlock.Instance inst,
                                   final HierName instance,
                                   final AliasedSet ns,
                                   final VerilogFactoryInterface factory,
                                   final AbstractConverter.BlockValue bv) {
        final Pair result =
            super.verilogInstance(inst, instance, ns, factory,
                                  new VddGndBlockValue(bv));

        List ports = null;
        if (skip == PowerPortHandling.VERILOG_ADD && inst.getPortsByOrder() == null) {
            final Set<String> left =
                Stream.concat(childPowerNets.stream(),
                              childGroundNets.stream())
                      .map(HierName::toString)
                      .collect(Collectors.toCollection(HashSet::new));
            final Iterator name = inst.getPortsByName();
            while (name.hasNext()) {
                final Map.Entry entry = (Map.Entry) name.next();
                final String key = (String) entry.getKey();
                left.remove(key);
            }
            for (String port : left) {
                if (ports == null) ports = new ArrayList();
                final HierName hport = HierName.makeHierNameUnchecked(port, '.');
                if (childPorts.contains(hport)) {
                    final VerilogObject formal = factory.ident(port, false);
                    try {
                        final List<VerilogObject> actual =
                            verilogValue(instance, ns, new NodeValue(hport), bv);
                        ports.add(factory.namedPort(formal, actual.get(0)));
                    } catch (Exception e) {}
                }
            }
        }

        if (clk != null) {
            if (ports == null) ports = new ArrayList();
            final VerilogObject clock = lookupNode(clk, "wire");
            ports.add(inst.getPortsByOrder() == null ?
                    factory.namedPort(factory.ident(clk, false), clock) : clock);
        }

        if (ports == null) {
            return result;
        } else {
            ports.addAll((List) result.getSecond());
            return new Pair(result.getFirst(), ports);
        }
    }

    protected void init(final CommandLineArgs theArgs) {
        internalWire = theArgs.argExists("internal-net-wire");

        final String estimatedTauStr =
            theArgs.getArgValue("estimated-tau", "100e-12");
        final float estimatedTau = Float.parseFloat(estimatedTauStr);

        if (theArgs.argExists("estimated-delay")) {
            final CastQuery.LocalNodes ln =
                new CastQuery.LocalNodes(null, cad, false, false, false, false,
                                         false, false, new HashMap(),
                                         estimatedTau);
            transportUp = ln.getDelay(cell.cast_cell, true);
            transportDn = ln.getDelay(cell.cast_cell, false);
        }
        if (theArgs.argExists("clk")) {
            clk = theArgs.getArgValue("clk", CLK);
        }
        zoix = theArgs.argExists("zoix");
        if (theArgs.argExists("skip-power-rail")) {
            skip = PowerPortHandling.valueOf(theArgs.getArgValue("skip-power-rail", "SKIP"));
        }
        outputDelay = theArgs.argExists("output-delay");
        zeroify = theArgs.argExists("make-gnd-0");
        oneify = theArgs.argExists("make-vdd-1");
        timescale = theArgs.argExists("timescale");
    }

    /**
     * Create module instantiations for subcells.  If a verilog block exist for
     * the subcell, and is to be used, then use its content instead of
     * instantiating the subcell.
     **/
    protected void processSubcells(boolean byName) {
        for (Iterator i = cell.getAllSubcellConnections().iterator();
             i.hasNext(); ) {
            final ConnectionInfo ci = (ConnectionInfo) i.next();
            childPowerNets.clear();
            childGroundNets.clear();
            updatePowerNets(childPowerNets, childGroundNets, ci.child);
            childPorts.clear();
            childPorts.addAll(ports(ci));
            final List block = verilogBlock(
                ci.nameInParent,
                cell.namespace,
                ci.child.cast_cell,
                chooser,
                new DefaultBlockValue(this, cell.namespace,
                    new UnaryFunction() {
                      public Object execute(final Object o) {
                          return wireType((HierName) o);
                      }
                    }));
            if (block != null) {
                items.addAll(block);
                continue;
            }
            final ArrayList args = new ArrayList();
            if (clk != null) addActual(args, clk, "wire");
            actual(args, ci, byName, new UnaryFunction() {
                      public Object execute(final Object o) {
                          return wireType((CellNet) o);
                      }
                   });
            final VerilogObject ident = factory.ident(ci.nameInParent.getCadenceString(), mAlwaysEscape);
            final VerilogObject[] parameters = getModuleParameter(ci);
            items.add(factory.moduleInst(ident,
                                         factory.ident(getModuleName(ci.child.cast_cell), mAlwaysEscape),
                                         parameters,
                                         constify(toArray(args))));
        }
    }

    /**
     * Instantiate a BLACKBOX.  Automatically insert a clock signal if
     * required.
     *
     * @param out name to connect the output of the blackbox
     * @param up pull up signal
     * @param dn pull down signal
     * @param box type of black box; this should usually be a macro for
     * flexibility at run time.
     *
     * @return an instantiation of the blackbox
     **/
    VerilogObject createBlackBox(final HierName out,
                                 final VerilogObject up,
                                 final VerilogObject dn,
                                 final String box) {
        final VerilogObject[] args = new VerilogObject[clk != null ? 4 : 3];
        args[0] = lookupNode(out);
        args[1] = up == null ? constify(lookupNode(GND, "supply0")) : up;
        args[2] = dn == null ? constify(lookupNode(GND, "supply0")) : dn;
        if (clk != null) args[3] = lookupNode(clk, "wire");
        return factory.moduleInst(factory.ident("network_" + out,
                                                mAlwaysEscape),
                                  macro(box + (clk == null ? "" : "_CLK")),
                                  getDelayExpr(out),
                                  constify(args));
    }

    protected void addParameters() {
        final Set seen = new HashSet();
        for (Iterator i = cell.cast_cell.getProductionRuleSet()
                                        .getProductionRules(); i.hasNext(); ) {
            final ProductionRule prs = (ProductionRule) i.next();
            final HierName target = prs.getTarget();
            if (seen.add(target)) {
                items.add(0, factory.parameter(null,
                                getExtraDelayParameter(target, true),
                                factory.expr("0.0")));
                items.add(0, factory.parameter(null,
                                getExtraDelayParameter(target, false),
                                factory.expr("0.0")));
            }
        }
        items.add(0, factory.parameter(null, getDelayBiasParameter(),
                                       factory.expr("1.0")));
    }

    protected String getIdent(final VerilogObject o) {
        final String[] result = new String[] { null };
        final boolean[] found = new boolean[] { false };
        o.accept(new TrivialVisitor() {
            public void ident(final String ident, final boolean escape) {
                if (found[0]) { result[0] = null; }
                else {
                    found[0] = true;
                    result[0] = ident;
                }
            }
        });
        return result[0];
    }

    protected VerilogObject constify(final VerilogObject o) {
        final VerilogObject result;
        if (zeroify && "GND".equals(getIdent(o))) {
            result = factory.expr("1'b0");
        } else if (oneify && "Vdd".equals(getIdent(o))) {
            result = factory.expr("1'b1");
        } else {
            result = o;
        }
        return result;
    }

    protected VerilogObject[] constify(final VerilogObject[] o) {
        if (!zeroify && !oneify) return o;
        final VerilogObject[] result = new VerilogObject[o.length];
        for (int i = 0; i < o.length; ++i) {
            result[i] = constify(o[i]);
        }
        return result;
    }

    protected String forceOutputX() {
        final Set<String> outPorts = new TreeSet<>();
        final Set<String> pwrPorts = new TreeSet<>();
        final Set<String> gndPorts = new TreeSet<>();
        formal(ports, "wire", new FormalProcessor() {
            public void addFormal(String port, String type, String dir) {
                final HierName hport = HierName.makeHierNameUnchecked(port, '.');
                if (powerNets.contains(hport)) {
                    pwrPorts.add(port);
                } else if (groundNets.contains(hport)) {
                    gndPorts.add(port);
                } else if (dir.equals("output")) {
                    outPorts.add(port);
                }
            }
        });
        if (outPorts.isEmpty() || (pwrPorts.isEmpty() && gndPorts.isEmpty())) {
            return null;
        }

        final String always =
            Stream.concat(pwrPorts.stream(), gndPorts.stream())
                  .map(x -> VerilogUtil.escapeIfNeeded(x))
                  .collect(Collectors.joining(" or ", "always @(", ")"));
        final String cond =
            Stream.concat(
                pwrPorts.stream().map(x -> VerilogUtil.escapeIfNeeded(x) + " === 1'b1"),
                gndPorts.stream().map(x -> VerilogUtil.escapeIfNeeded(x) + " === 1'b0"))
                  .collect(Collectors.joining(" && ", "if (", ")"));
        final String force =
            outPorts.stream()
                    .map(x -> "force " + VerilogUtil.escapeIfNeeded(x) + " = 1'bx;")
                    .collect(Collectors.joining("\n"));
        final String release =
            outPorts.stream()
                    .map(x -> "release " + VerilogUtil.escapeIfNeeded(x) + ";")
                    .collect(Collectors.joining("\n"));
        return always + " " + cond + " begin\n" + release + "\nend else begin\n" + force + "\nend\n";
    }

    public abstract VerilogObject convert(final CommandLineArgs theArgs,
                                          final VerilogObject moduleName,
                                          final boolean toplevel,
                                          final boolean topEnv);
}
