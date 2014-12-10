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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import org.antlr.stringtemplate.StringTemplate;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cadencize.Cadencize;
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
import com.avlsi.util.container.DefaultedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;

/** Convert to a Verilog netlist, i.e., Verilog with only macro instantiations
  * and macro definitions without the body. */
class NetlistConverter extends AbstractConverter {
    private static final String CLK = "CLK";
    private final CellType cell;
    private final Map up, down;
    private final ConnectionInfo ports;
    private boolean skip = false;
    private boolean keepCsp = false;
    private boolean skipRouted = false;
    private String routedClk = CLK;
    private String clk = null;
    private final HierName Vdd, GND;
    private final static String DEFAULT_POWER_GRID =
        "$lib$.wires.$type$_$subtype$_POWER_GRID_TIEOFF";
    private final static Pattern COMPONENTS =
        Pattern.compile("(.*)\\.([^.]+)\\.([^.]+)");
    private final Prs2Verilog.VerilogChooser chooser;

    public NetlistConverter(final CellType cell,
                            final VerilogFactoryInterface factory,
                            final ConnectionInfo ports,
                            final HierName Vdd,
                            final HierName GND,
                            final boolean alwyasEscape,
                            final Prs2Verilog.VerilogChooser chooser) {
        super(factory, alwyasEscape);
        this.cell = cell;
        this.up = new HashMap();
        this.down = new HashMap();
        this.ports = ports;
        this.Vdd = Vdd;
        this.GND = GND;
        this.chooser = chooser;
    }

    private String h2s(final HierName h) {
        return h.getCadenceString();
    }
    protected void addFormal(final List params, final String name,
                             final String type, final String dir) {
        if (!skip || (!name.equals("Vdd") && !name.equals("GND")))
            super.addFormal(params, name, type, dir);
    }   
    protected void addActual(final boolean byName, final List params,
                             final HierName child, final HierName parent,
                             final String type) {
        if (!skip || (!child.isVdd() && !child.isGND()))
            super.addActual(byName, params, child, parent, type);
    }
    private void processSubcells(boolean byName) {
        final List block = verilogBlock(
            null,
            cell.namespace,
            cell.cast_cell,
            chooser,
            new DefaultBlockValue(this, cell.namespace,
                new UnaryFunction() {
                  public Object execute(final Object o) {
                      return "wire";
                  }
                }));
        if (block != null) {
            items.addAll(block);
            return;
        }

        for (Iterator i = cell.getAllSubcellConnections().iterator();
             i.hasNext(); ) {
            final ConnectionInfo ci = (ConnectionInfo) i.next();
            final ArrayList args = new ArrayList();

            // temporarily change the setting of skip to always skip power
            // rails on hard macro routed cells
            final boolean oldSkip = skip;
            if (skipRouted && CellUtils.isRouted(ci.child.cast_cell)) {
                skip = true;
            }
            actual(args, ci, "wire", byName);
            if (skipRouted && CellUtils.isRouted(ci.child.cast_cell)) {
                skip = oldSkip;
            }

            if (clk != null) {
                addActual(byName,
                          args,
                          HierName.makeHierName(
                              CellUtils.isRouted(ci.child.cast_cell) ? routedClk
                                                                     : clk),
                          HierName.makeHierName(clk),
                          "wire");
            }
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
    }

    private String getPowerGridName(final String cellName, final String templ) {
        final StringTemplate st = new StringTemplate(templ);
        final Matcher m = COMPONENTS.matcher(cellName);
        if (m.matches()) {
            st.setAttribute("lib", m.group(1));
            st.setAttribute("type", m.group(2));
            st.setAttribute("subtype", m.group(3));
            return st.toString();
        } else {
            throw new RuntimeException("Cannot parse cell name " + cellName +
                                       " into components");
        }
    }

    private Map getMapping(final CellType cell,
                           final Prs2Verilog.VerilogChooser chooser) {
        try {
            final GeneratePortMapping mapper =
                new GeneratePortMapping(factory, cell.cast_cell, chooser,
                                        false, new Cadencize(false));
            final VerilogObject block =
                mapper.convert(null, null, false, false);
            if (block == null) {
                return null;
            } else {
                final Map map = new LinkedHashMap();
                GeneratePortMapping.generateMapping(map, Collections.emptyMap(),
                                                    mapper.getBounds(),
                                                    block);
                return map;
            }
        } catch (Exception e) {
            return null;
        }
    }

    public VerilogObject convert(final CommandLineArgs theArgs,
                                 VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        final String renameBlock = theArgs.getArgValue("name-map-block", null);
        final Prs2Verilog.VerilogChooser renameChooser =
            renameBlock == null ? null
                                : new Prs2Verilog.SimpleChooser(renameBlock);

        keepCsp = theArgs.argExists("keep-csp");
        // do not generate anything for routed cells, unless it's the toplevel
        skipRouted = theArgs.argExists("skip-routed");
        // named of the clock pin on routed cells
        routedClk = theArgs.getArgValue("routed-clk", CLK);

        if (unconvertible(cell) &&
            !(keepCsp && cell.cast_cell.hasRunnableCsp())) return null;

        if (!toplevel && skipRouted && CellUtils.isRouted(cell.cast_cell))
            return null;

        skip = theArgs.argExists("skip-power-rail");
        final boolean routed = theArgs.argExists("routed");
        if (theArgs.argExists("clk")) {
            clk = theArgs.getArgValue("clk", CLK);
        }

        final ArrayList params = new ArrayList();
        final Map<String,String> verilogMap =
            renameChooser == null ? null
                                  : (Map<String,String>) getMapping(cell, renameChooser);
        if (verilogMap == null) {
            formal(params, ports, "wire");
            if (clk != null) addFormal(params, clk, "wire", "input");
        } else {
            final Collection<String> portOrder = new LinkedHashSet<String>();

            final Map<String,Integer> max =
                new DefaultedMap<String,Integer>(
                        Integer.MIN_VALUE,
                        new HashMap<String,Integer>());

            final Map<String,Integer> min =
                new DefaultedMap<String,Integer>(
                        Integer.MAX_VALUE,
                        new HashMap<String,Integer>());

            final Map<HierName,Integer> portDir = 
                CellUtils.getCanonicalDir(new HashMap<HierName,Integer>(),
                                          cell.cast_cell, cell.namespace);

            final Map<String,Integer> baseDir =
                new HashMap<String,Integer>();

            final Pattern pat = Pattern.compile("^(.*)\\[(\\d+)\\]$");
            for (Map.Entry<String,String> e : verilogMap.entrySet()) {
                final Matcher m = pat.matcher(e.getKey());
                final CellNet net = cell.getNet(toHier(e.getValue()));
                final HierName canon = net.canonicalName;
                final String base;
                if (m.matches()) {
                    base = m.group(1);
                    final int idx = Integer.parseInt(m.group(2));
                    max.put(base, Math.max(max.get(base), idx));
                    min.put(base, Math.min(min.get(base), idx));
                    portOrder.add(base);
                    nodes.put(
                            h2s(canon),
                            factory.arrayAccess(
                                factory.ident(base, mAlwaysEscape),
                                factory.expr(m.group(2))));
                } else {
                    base = e.getKey();
                    nodes.put(
                            h2s(canon),
                            factory.ident(e.getKey(), mAlwaysEscape));
                }
                portOrder.add(base);

                final Integer newDir = portDir.get(canon);
                final Integer oldDir = baseDir.get(base);
                if (oldDir == null) {
                    baseDir.put(base, newDir);
                } else {
                    if (!oldDir.equals(newDir)) {
                        baseDir.put(base, PortDefinition.INOUT);
                    }
                }
            }

            for (String port : portOrder) {
                final int dir = baseDir.get(port);
                final String inout =
                    dir == PortDefinition.IN ? "input" :
                        (dir == PortDefinition.OUT ? "output" : "inout");
                if (max.containsKey(port)) {
                    final int hi = max.get(port);
                    final int lo = min.get(port);
                    final String range = " [" + hi + ":" + lo + "]";
                    addFormal(params, port, "wire" + range, inout + range);
                } else {
                    addFormal(params, port, "wire", inout);
                }
            }

            final VerilogBlock.NamedBlock block =
                chooseVerilog(cell.cast_cell, renameChooser);
            String name = (String) DirectiveUtils.gruntDirective(block,
                    DirectiveConstants.NAME_MAPPING);
            if (name == null) {
                final VerilogBlock.Instance first =
                    (VerilogBlock.Instance) block.getInstances().next();
                name = first.getModule();
            }
            moduleName = factory.ident(name, mAlwaysEscape);
        }

        if (toplevel || !routed || !CellUtils.isRouted(cell.cast_cell)) {
            // should the module instantiation done by position or by name
            processSubcells(theArgs.argExists("by-name"));
        }

        if (toplevel) {
            final ArrayList args = new ArrayList();
            // temporarily turn off skip
            final boolean oldSkip = skip;
            skip = false;
            addActual(true, args, GND, GND, "wire");
            addActual(true, args, Vdd, Vdd, "wire");

            final VerilogObject ident = factory.ident("tiehilo", true);
            final String tieoff = getPowerGridName(cell.typeName,
                theArgs.getArgValue("power-grid-template", DEFAULT_POWER_GRID));
            if (!tieoff.equals("")) {
                items.add(
                    factory.moduleInst(
                        ident,
                        factory.ident(tieoff, true),
                        null,
                        (VerilogObject[])
                        args.toArray(new VerilogObject[0])));
            }
            skip = oldSkip;
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
}
