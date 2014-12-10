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
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.util.DirectiveOverride;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.cdl.CDLParser;
import com.avlsi.file.cdl.CDLFileFormatException;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.prs2verilog.verilog.Delay;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.jauto.CastQuery;
import com.avlsi.tools.jauto.GlobalNet;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanExpressionVisitorInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.functions.UnaryFunction;

final class NetgraphConverter extends NetgraphGateConverter {
    private final NetGraph graph;
    private CDLParser cdlParser;
    private final HierName VDD;
    private boolean minimizeTriRegs = false;
    private static DirectiveInterface emptyDirective = null;
    private boolean macroModel = false;
    private boolean noResetFault = false;
    private boolean extraPortInfo = false;
    private VerilogObject parameterPrefix = null;
    private int faultParamNum = 0;

    private String getPortString(final VerilogObject[] ports) {
        return getPortString(ports, Collections.EMPTY_SET,
                             Collections.EMPTY_SET);
    }

    private String getPortString(final VerilogObject[] ports,
                                 final Set pullUpOnly,
                                 final Set pullDnOnly) {
        if (!extraPortInfo) return "";
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < ports.length; ++i) {
            final String port = getIdent(ports[i]);
            buf.append("/");
            if (port == null) {
                buf.append("null");
            } else {
                buf.append(port);
            }
            if (pullUpOnly.contains(ports[i])) buf.append("-");
            if (pullDnOnly.contains(ports[i])) buf.append("+");
        }
        return buf.toString();
    }

    protected VerilogObject nextPrimitive(VerilogObject[] o) {
        return factory.ident("PRS2VERILOG_primitive_" + (primitiveCount++) +
                             getPortString(o), false);
    }

    private VerilogObject addPrefixIfNeeded(final VerilogObject ident) {
        if (parameterPrefix == null) {
            return ident;
        } else {
            return factory.hierIdent(new VerilogObject[] { parameterPrefix,
                                                           ident });
        }
    }

    protected VerilogObject getDelayBiasParameter() {
        return addPrefixIfNeeded(super.getDelayBiasParameter());
    }

    protected VerilogObject getExtraDelayParameter(final HierName node,
                                                   final boolean up) {
        return addPrefixIfNeeded(super.getExtraDelayParameter(node, up));
    }

    private DirectiveInterface getEmptyDirective() {
        if (emptyDirective == null) {
            emptyDirective =
                new DirectiveSource(BlockInterface.PRS).getDirectiveInterface();
        }
        return emptyDirective;
    }

    // Copied from tools/lvs/PrsToNet.java.  Should refactor.
    private NetGraph getCdlNetGraph(String cdlCellName) {
        AspiceFile aspiceCell = cdlParser.getCell(cdlCellName);

        if (aspiceCell == null) {
            System.err.println("Cell " + cdlCellName + 
                               " not found in library!");
            System.exit(3);
        }
        
        NetGraph netgraph =
            new NetGraph(null,null,null, VDD, GND, Collections.EMPTY_SET);
        netgraph.addAspice(aspiceCell);
        return netgraph;
    }

    public NetgraphConverter(final CellType cell,
                             final VerilogFactoryInterface factory,
                             final ConnectionInfo ports,
                             final HierName VDD,
                             final HierName GND,
                             final boolean alwaysEscape,
                             final Prs2Verilog.VerilogChooser chooser,
                             final Cadencize cad,
                             final boolean minimizeTriRegs) {
        super(cell, factory, ports, GND, alwaysEscape, chooser, cad);

        this.minimizeTriRegs = minimizeTriRegs;

        if (this.minimizeTriRegs) {
             // in this case, netgraphs should already be created 
             assert cell.transistors != null : 
                "Netgraph not set-up as expected."; 
             this.graph = cell.transistors;
        } else {
             this.graph = new NetGraph(cell.namespace,
                                  cell.exclusives,
                                  new ArrayList(), VDD, GND,
                                  Collections.EMPTY_SET);
        }
        this.cdlParser = null;
        this.VDD = VDD;
    }

    protected void addFormal(final List params, final String name,
                             String type, final String dir) {
        super.addFormal(params, name, type, dir);
    }

    protected String wireType(final CellNet net) {
        return "wire";
    }

    private VerilogObject getNextFaultParameter() {
        faultParamNum++;
        return factory.ident("no_fault" + faultParamNum, false);
    }

    private boolean isReset(final String port) {
        return port.equals("_RESET") || port.equals("Reset") ||
               port.equals("_Reset") || port.equals("reset") ||
               port.equals("_reset");
    }

    private String undot(final String ident) {
        return noResetFault ? ident.replaceAll("\\.", "_") : ident;
    }

    private void moduleInst(final VerilogObject ident,
                            final VerilogObject module,
                            final VerilogObject[] parameters,
                            final VerilogObject[] ports) {
        items.add(factory.moduleInst(ident, module, parameters,
                                     constify(ports)));
        if (noResetFault) {
            final String inst = getIdent(ident);
            if (inst != null) {
                boolean hasReset = false;
                String faultExpr = inst + "(";;
                for (int i = 0; i < ports.length; ++i) {
                    final String port = getIdent(ports[i]);
                    if (port != null && isReset(port)) {
                        hasReset = true;
                        faultExpr = faultExpr + "B";
                    } else {
                        faultExpr = faultExpr + "-";
                    }
                    if (i < ports.length - 1)
                        faultExpr = faultExpr + ",";
                }
                if (hasReset) {
                    faultExpr = faultExpr + ")";
                    items.add(
                        factory.parameter(
                            null,
                            getNextFaultParameter(),
                            factory.expr("\"" + faultExpr + "\"")));
                }
            }
        }
    }

    class GateInstance {
        private final String gate;
        private final TreeMap map;
        private final NetGraph.NetNode node;
        public GateInstance(NetGraph.GateInstance gi, NetGraph.NetNode node) {
            this.gate = gi.getType();
            this.map = new TreeMap(gi.getUnmodifiableMap());
            this.node = node;
        }
        public void verilog() {
            final ArrayList args = new ArrayList();
            for (final Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName from = (HierName) entry.getKey();
                final HierName to = (HierName) entry.getValue();
                if (!from.isVdd() && !from.isGND()) {
                    if (node.name.compareTo(to) == 0) { 
                        // Put output first to correspond to Verilog primitives
                        args.add(0, lookupNode(to));
                    } else {
                        args.add(lookupNode(to));
                    }
                }
            }
            moduleInst(factory.ident(undot("gate_" + node.name +
                                           NetgraphConverter.this.getPortString(toArray(args))),
                                     mAlwaysEscape),
                       macro(gate),
                       getDelayExpr(node.name),
                       (VerilogObject[]) args.toArray(new VerilogObject[0]));
        }
    }

    public VerilogObject doPath(Collection gates, int nao) {
        final ArrayList args = new ArrayList(gates);
        if (args.size() == 1) {
            return (VerilogObject) args.get(0);
        } else {
            String andType = "AND";
            if (args.size() <= nao) andType += args.size();
            final VerilogObject and = macro(andType);
            args.add(0, newWire());
            moduleInst(nextPrimitive(toArray(args)), and, null, toArray(args));
            return (VerilogObject) args.get(0);
        }
    }

    public VerilogObject doPaths(Collection paths, int nao) {
        final ArrayList args = new ArrayList();
        for (final Iterator p = paths.iterator(); p.hasNext(); ) {
            final Collection gates = (Collection) p.next();
            args.add(doPath(gates, nao));
        }
        if (args.size() != 1) {
            String orType = "OR";
            if (args.size() <= nao) orType += args.size();
            final VerilogObject or = macro(orType);
            args.add(0, newWire());
            moduleInst(nextPrimitive(toArray(args)), or, null,
                       toArray(args));
        }
        return (VerilogObject) args.get(0);
    }

    /**
     * Return gates on the specified <code>path</code> as VerilogObjects,
     * taking into account whether the gates need to be inverted or not
     **/
    private Pair getGateNodes(final NetGraph.NetPath path) {
        final ArrayList edges = path.getEdges();
        final ArrayList args = new ArrayList();
        final ArrayList nonInverting = new ArrayList();
        for (final Iterator e = edges.iterator(); e.hasNext(); ) {
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) e.next();
            if (path.getDir() == 1) {
                args.add(lookupNot(edge.gate.name));
            } else {
                args.add(lookupNode(edge.gate.name));
            }
            nonInverting.add(lookupNode(edge.gate.name));
        }
        return new Pair(args, nonInverting);
    }

    private VerilogObject createMacroModel(final VerilogObject moduleName,
                                           final Collection params,
                                           final List items,
                                           final List decls) {
        final List inout = new ArrayList();
        final List wireDecl = new ArrayList();

        // declare ports and directionality; the first element in ports is the
        // output, all others are inputs; all ports are declared as wires
        boolean first = true;
        for (Iterator i = params.iterator(); i.hasNext(); first = false) {
            final VerilogObject port = (VerilogObject) i.next();
            inout.add(factory.parameterDecl(port, first ? "output" : "input"));
            wireDecl.add(factory.netDecl("wire", port, null));
        }

        inout.addAll(wireDecl);
        inout.addAll(decls);
        inout.addAll(items);

        return factory.module(moduleName,
                              (VerilogObject[])
                              params.toArray(new VerilogObject[0]),
                              (VerilogObject[])
                              inout.toArray(new VerilogObject[0]));
    }

    public VerilogObject convert(final CommandLineArgs theArgs,
                                 final VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        if (unconvertible(cell)) return null;

        init(theArgs);

        final String naoArg = theArgs.getArgValue("number-gate", null);
        final int nao;
        if (naoArg == null) {
            nao = 0;
        } else {
            nao = Integer.parseInt(naoArg);
        }

        this.topEnv = topEnv;
        this.macroModel = theArgs.argExists("macro-model");
        this.noResetFault = theArgs.argExists("no-reset-fault");
        this.extraPortInfo = theArgs.argExists("extra-port-info");
        if (theArgs.argExists("trireg-output")) {
            System.err.println("Warning: trireg-output is obselete, ignoring.");
        }

        // if haven't already done so, create NetGraphs 
        if (!minimizeTriRegs) {

            final String libraryFile = theArgs.getArgValue("library", null);
            final ArrayList gates = new ArrayList();
            if (libraryFile != null) {
                try {
                    cdlParser = new CDLParser();
                    cdlParser.parseFile(libraryFile);
                } catch (Exception e) {
                    System.err.println("Invalid library file " + libraryFile +
                                       ": " + e);
                    System.exit(1);
                } 
                for (final Iterator i = cdlParser.getCellTypes();
                     i.hasNext(); ) {
                    final String gn = (String) i.next();
                    final NetGraph ng = getCdlNetGraph(gn);
                    ng.gateType = gn;
                    ng.prepareForLvs();
                    gates.add(ng);
                }
            }

            // Disable all symmetrization directives (because they do not
            // affect logic) by substituting the directive block associated
            // with the prs block.
            final BlockInterface block = cell.cast_cell.getBlockInterface();
            final BlockIterator dirIter =
                block.iterator(BlockInterface.PRS).next()
                     .iterator(BlockInterface.DIRECTIVE);
            final DirectiveBlock original =
                dirIter.hasNext() ? (DirectiveBlock) dirIter.next() : null;
            if (original != null) {
                dirIter.set(
                    new DirectiveBlock(
                        new DirectiveOverride(
                            original, getEmptyDirective(),
                            DirectiveConstants.SYMMETRIZE)));
            }

            try {
                graph.addCellInterfacePrs(cell.cast_cell, (NetGraph[])
                                      gates.toArray(new NetGraph[0]),
                                      null, null);
            } catch (UnimplementableProductionRuleException e) {
                System.err.println("UnimplementableProdunctionRule found: " +
                               e.getMessage());
                System.err.println("Falling back to GateConverter");
                final AbstractConverter conv =
                    new GateConverter(cell, factory, ports, GND, 
                                      mAlwaysEscape, chooser, cad);
                return conv.convert(theArgs, moduleName, toplevel, topEnv);
            } finally {
                // Restore symmetrization directives
                if (original != null) dirIter.set(original);
            }

            graph.prepareForLvs();
        }

        final VerilogObject realModuleName =
            moduleName == null ?
                factory.ident(cell.cast_cell.getFullyQualifiedType(), 
                              mAlwaysEscape) :
                moduleName;

        // if the macro-model option is given, each blackbox has its own
        // module; we collect all such modules here
        final List macroModels =
            macroModel ? new ArrayList() : Collections.EMPTY_LIST;

        for (final Iterator i = graph.getNodes().iterator(); i.hasNext(); ) {
            final NetGraph.NetNode node = (NetGraph.NetNode) i.next();
            if (!node.isOutput() || !node.isNamed()) continue;
            final boolean combinational = node.isCombinational();
            if ((node.getGate() != null) && combinational) {
                final GateInstance g = new GateInstance(node.getGate(), node);
                g.verilog();
            } else if (combinational) {
                final Collection paths = node.getPaths();
                final ArrayList gndPaths = new ArrayList();
                for (final Iterator p = paths.iterator(); p.hasNext(); ) {
                    final NetGraph.NetPath path = (NetGraph.NetPath) p.next();
                    if (path.getDir() == -1)
                        gndPaths.add(getGateNodes(path).getFirst());
                }
                final VerilogObject[] args = new VerilogObject[2];
                args[0] = lookupNode(node.name);
                args[1] = doPaths(gndPaths, nao);
                items.add(factory.primitive("not", getDelay(node.name), 
                                             nextPrimitive(args),
                                             constify(args)));
            } else {
                final Collection paths = node.getPaths();
                final ArrayList vddPaths = new ArrayList();
                final ArrayList gndPaths = new ArrayList();
                final Set inputs = new HashSet();

                // process all names so that every node will have been seen
                // already before processing the blackboxes
                for (final Iterator p = paths.iterator(); p.hasNext(); ) {
                    final NetGraph.NetPath path = (NetGraph.NetPath) p.next();
                    final ArrayList edges = path.getEdges();
                    for (final Iterator e = edges.iterator(); e.hasNext(); ) {
                        final NetGraph.NetEdge edge =
                            (NetGraph.NetEdge) e.next();
                        lookupNode(edge.gate.name);
                    }
                    lookupNode(path.getStartNode().name);
                    lookupNode(path.getEndNode().name);
                }

                // doPaths and createBlackBox will, as a side effect, add
                // Verilog constructs to the items and wireDecl members;
                // because we want them to go into a seperate module, we
                // temporarily swap in different members.
                final Map macroModelNot, originalNot;
                final List originalWireDecl, macroModelWireDecl;
                final List originalItems, macroModelItems;
                final Set pullUpInputs, pullDnInputs;
                if (macroModel) {
                    macroModelNot = new HashMap();
                    originalNot = not;
                    not = macroModelNot;

                    macroModelWireDecl = new ArrayList();
                    originalWireDecl = wireDecl;
                    wireDecl = macroModelWireDecl;

                    macroModelItems = new ArrayList();
                    originalItems = items;
                    items = macroModelItems;
                    parameterPrefix = moduleName;

                    pullUpInputs = new HashSet();
                    pullDnInputs = new HashSet();
                } else {
                    macroModelNot = null; originalNot = null;
                    macroModelWireDecl = null; originalWireDecl = null;
                    macroModelItems = null; originalItems = null;
                    pullUpInputs = Collections.EMPTY_SET;
                    pullDnInputs = Collections.EMPTY_SET;
                }
                for (final Iterator p = paths.iterator(); p.hasNext(); ) {
                    final NetGraph.NetPath path = (NetGraph.NetPath) p.next();
                    final Pair pair = getGateNodes(path);
                    final Collection gates = (Collection) pair.getFirst();
                    final Collection nonInverting =
                        (Collection) pair.getSecond();
                    if (path.getDir() == 1) {
                        vddPaths.add(gates);
                        if (pullUpInputs != Collections.EMPTY_SET)
                            pullUpInputs.addAll(nonInverting);
                    } else if (path.getDir() == -1) {
                        gndPaths.add(gates);
                        if (pullDnInputs != Collections.EMPTY_SET)
                            pullDnInputs.addAll(nonInverting);
                    } else {
                        assert false : "Sneak path found: " + path;
                    }
                    inputs.addAll(nonInverting);
                }

                items.add(
                    createBlackBox(
                        node.name,
                        vddPaths.size() > 0 ? doPaths(vddPaths, nao) : null,
                        gndPaths.size() > 0 ? doPaths(gndPaths, nao) : null,
                        (graph.nostaticizers.contains(node.getName()) &&
                         node.isPort()) ?
                            "BLACKBOX" : "BLACKBOX_TRIREG"));

                // restore the original members
                if (macroModel) {
                    not = originalNot;
                    wireDecl = originalWireDecl;
                    items = originalItems;
                    parameterPrefix = null;
                }

                if (macroModel) {
                    final VerilogObject macroModelName =
                        factory.ident(cell.cast_cell.getFullyQualifiedType() +
                                      "$$" + node.name, mAlwaysEscape);
                    final Collection args = new ArrayList();
                    inputs.add(lookupNode(GND, "wire"));
                    args.add(lookupNode(node.name));
                    args.addAll(inputs);

                    final Set pullUpOnly = new HashSet(pullUpInputs);
                    pullUpOnly.removeAll(pullDnInputs);
                    final Set pullDnOnly = new HashSet(pullDnInputs);
                    pullDnOnly.removeAll(pullUpInputs);

                    String macroModelInst =
                        undot("network_" + node.name +
                              getPortString(toArray(args), pullUpOnly,
                                            pullDnOnly));
                    moduleInst(factory.ident(macroModelInst, mAlwaysEscape),
                               macroModelName,
                               null,
                               toArray(args));
                    macroModels.add(createMacroModel(macroModelName, args,
                                                     macroModelItems,
                                                     macroModelWireDecl));
                }
            }
        }

        final ArrayList params = new ArrayList();
        if (clk != null) addFormal(params, clk, "wire", "input");
        formal(params, ports, "wire");

        processSubcells();

        items.addAll(0, wireDecl);
        items.addAll(0, inout);

        addParameters();
        final List topModel = new ArrayList();
        topModel.add(
            factory.module(realModuleName,
                           (VerilogObject[])
                           params.toArray(new VerilogObject[0]),
                           (VerilogObject[])
                           items.toArray(new VerilogObject[0])));
        if (zoix && shouldSuppressFaults(cell)) {
            topModel.add(0, factory.expr("`suppress_faults\n"));
            topModel.add(factory.expr("`nosuppress_faults\n"));
        }
        if (macroModels.isEmpty()) {
            return factory.compilationUnit((VerilogObject[])
                        topModel.toArray(new VerilogObject[0]));
        } else {
            macroModels.add(0, factory.expr("`endif\n"));
            macroModels.add(0, factory.expr("`enable_portfaults\n"));
            macroModels.add(0, factory.expr("`suppress_faults\n"));
            macroModels.add(0, factory.expr("`ifdef PRS2VERILOG_MACRO_MODEL\n"));
            macroModels.add(factory.expr("`ifdef PRS2VERILOG_MACRO_MODEL\n"));
            macroModels.add(factory.expr("`disable_portfaults\n"));
            macroModels.add(factory.expr("`nosuppress_faults\n"));
            macroModels.add(factory.expr("`endif\n"));
            macroModels.addAll(topModel);
            return factory.compilationUnit((VerilogObject[])
                        macroModels.toArray(new VerilogObject[0]));
        }
    }

    /**
     * Determines if the specified CellNet which is supposed to be intenral
     * is part of a shared bus, i.e. if it has multiple drivers.  
     * Equivalent to
     * <code>((GlobalNet) cellNet.getGlobalNets.get(0)).isShared()</code>,
     * but asserts that there is exactly one global net for the cell net.
     *
     * @param cellNet
     *        The cell net to check.
     **/
    private static boolean isShared(final /*@ non_null @*/ CellNet cellNet) {
        final List/*<GlobalNet>*/ cellNetGlobalNets =
            cellNet.getGlobalNets();

        assert cellNetGlobalNets.size() == 1 :
            "Found " + cellNetGlobalNets.size() +
            " instead of 1 global net in supposedly internal net" +
            cellNetGlobalNets + "cell net: " + cellNet; 

        return ((GlobalNet) cellNetGlobalNets.get(0)).isShared();
    }

    /**
     * Determines if the specified NetNode is part of a shared bus, 
     * i.e. if it has multiple drivers.  
     *
     * @param node 
     *        The NetGraph.NetNode node to check.
     **/
    private boolean isShared(NetGraph.NetNode node) {
        final CellNet cellNet = cell.getNet(node.getName());
        final List/*<GlobalNet>*/ cellNetGlobalNets = cellNet.getGlobalNets(); 

        // If any global net is shared return true
        for (int i = 0; i < cellNetGlobalNets.size(); i++) {
            if (((GlobalNet) cellNetGlobalNets.get(i)).isShared())
                return(true);
        }
          
        return false; 
    }
}
