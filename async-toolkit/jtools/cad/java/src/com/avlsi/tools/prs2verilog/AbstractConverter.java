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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.function.Function;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.ArrayValue;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.InstanceValue;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.CellType;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.fast.ports.ArrayType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.prs2verilog.ConverterInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;

public abstract class AbstractConverter implements ConverterInterface {
    protected static final String libPrefix = "lib.verilog";
    protected final VerilogFactoryInterface factory;
    protected Map nodes, not;
    protected List wireDecl, items, inout;
    protected int wireId;
    protected final boolean mAlwaysEscape;
    protected int primitiveCount = 0;

    /**
     * A map from cell (<code>CellInterface</code>) to files
     * (<code>String[]</code>) that is depended on by the verilog block choosen
     * for that cell.
     **/
    protected final Map<CellInterface, String[]> depends;

    protected final static String PRS2VERILOG_TAU = "PRS2VERILOG_TAU";
    protected final static String PRS2VERILOG_DELAY_SCALE = "PRS2VERILOG_DELAY_SCALE";

    public static VerilogObject[] toArray(final Collection l) {
        return (VerilogObject[]) l.toArray(new VerilogObject[0]);
    }

    protected AbstractConverter(final VerilogFactoryInterface factory,
                                boolean alwaysEscape ) {
        this.factory = factory;
        this.nodes = new HashMap();
        this.not = new HashMap();
        this.wireDecl = new ArrayList();
        this.items = new ArrayList();
        this.inout = new ArrayList();
        this.wireId = 0;
        mAlwaysEscape = alwaysEscape;
        this.depends = new HashMap<CellInterface, String[]>();
    }
    public VerilogObject newNet(final String type) {
        final String name = "PRS2VERILOG_internal_" + wireId++;
        final VerilogObject ident = factory.ident(name, mAlwaysEscape);
        wireDecl.add(factory.netDecl(type, ident, null));
        return ident;
    }
    private VerilogObject find(final DirectiveBlock db,
                               final String key,
                               final String type,
                               final String param) {
        Object o = null;
        try {
            o = db.lookup(key, type, param);
        } catch (UnknownDirectiveException e) {
            Debug.assertTrue(false, "Unknown directive: " + key);
        }
        Debug.assertTrue(o != null, "Directive " + key + "(" + param + ":" + type + ") is null.");
        String s = o.toString();
        if (o instanceof String) s = "\"" + s + "\"";
        return factory.expr(s);
    }
    private VerilogObject[] source(final String inst, final DirectiveBlock db) {
        ArrayList param = new ArrayList(4);
        param.add(find(db, "infile", "string", inst));
        param.add(find(db, "memsize", "string", inst));
        param.add(find(db, "data_set", "string", inst));
        param.add(find(db, "data_reset", "string", inst));
        return toArray(param);
    }
    private VerilogObject[] sink(final String inst, final DirectiveBlock db) {
        ArrayList param = new ArrayList(4);
        param.add(find(db, "outfile", "string", inst));
        param.add(find(db, "enable_set", "string", inst));
        param.add(find(db, "enable_reset", "string", inst));
        return toArray(param);
    }
    protected VerilogObject[] getModuleParameter(final ConnectionInfo ci) {
        final String type = ci.child.cast_cell.getFullyQualifiedType();
        if (type.indexOf(libPrefix) == -1) return null;
        final String inst = ci.nameInParent.getCadenceString();
        final CellInterface c = ci.parent.cast_cell;
        final BlockIterator bi =
            c.getBlockInterface().iterator(BlockInterface.DIRECTIVE);
        if (!bi.hasNext()) return null;
        final DirectiveBlock db = (DirectiveBlock) bi.next();
        if (type.indexOf("SOURCE") != -1) return source(inst, db);
        else if (type.indexOf("SINK") != -1) return sink(inst, db);
        else return null;
    }
    protected VerilogObject lookupNode(final HierName node, final String type) {
        return lookupNode(node.getCadenceString(), type);
    }
    protected VerilogObject lookupNode(final String node, final String type) {
        return lookupNode(node, type, null);
    }
    protected VerilogObject lookupNode(final String node, final String type,
                                       final VerilogObject delay) {
/*
        if (excludeNodes.containsKey(node)) {
            return factory.ident((String) excludeNodes.get(node), true);
        }
*/
        if (!nodes.containsKey(node)) {
            final VerilogObject ident = factory.ident(node, mAlwaysEscape);
            wireDecl.add(factory.netDecl(type, ident, delay));
            nodes.put(node, ident);
        }
        return (VerilogObject) nodes.get(node);
    }
    protected VerilogObject lookupNot(final HierName node, final String type) {
        return lookupNot(node.getCadenceString(), type);
    }
    protected VerilogObject lookupNot(final String node, final String type) {
        if (!not.containsKey(node)) {
            final VerilogObject wire = lookupNode(node, type);
            final VerilogObject[] args = new VerilogObject[2];
            args[0] = newNet("wire");
            args[1] = wire;
            items.add(factory.primitive("not", null, nextPrimitive(), args));
            not.put(node, args[0]);
        }
        return (VerilogObject) not.get(node);
    }
    private Collection ports(final ConnectionInfo ci) {
        final Map map = new TreeMap();
        for (ConnectionInfo.NameIterator it = ci.childIterator();
             it.hasNext(); ) {
            HierName port = (HierName) it.next();
            CellNet net = ci.child.getNet(port);
            //if (excludeNodes.containsKey(net.canonicalName)) continue;
            map.put(net.canonicalName, port);
        }
        return map.values();
    }
    protected void addFormal(final List params, final String name,
                             final String type, final String dir) {
        params.add(lookupNode(name, type));
        final VerilogObject ident = factory.ident(name, mAlwaysEscape);
        inout.add(factory.parameterDecl(ident, dir));
    }
    protected void addFormal(final List params, final String name,
                             final String type) {
        addFormal(params, name, type, "inout");
    }
    private Integer accumPortDir(final Integer oldDir, final Integer newDir) {
        if (oldDir == null || oldDir == newDir) return newDir;
        else if (oldDir == PortDefinition.IN && newDir == PortDefinition.OUT ||
                 oldDir == PortDefinition.OUT && newDir == PortDefinition.IN)
            return PortDefinition.IN;
        else if (oldDir == PortDefinition.NONE) return newDir;
        else if (newDir == PortDefinition.NONE) return oldDir;
        else return PortDefinition.INOUT;
    }
    protected void formal(final List params, final ConnectionInfo ci,
                          final String type) {
        final Map<String,Integer> ports =
            CellUtils.markPorts(ci.child.cast_cell);
        final Map<HierName,Integer> canonPorts =
            new HashMap<HierName,Integer>();
        for (Map.Entry<String,Integer> entry : ports.entrySet()) {
            final HierName hn;
            try {
                hn = HierName.makeHierName((String) entry.getKey(), '.');
            } catch (InvalidHierNameException e) {
                throw new AssertionError("Cannot construct HierName: " +
                                         entry.getKey());
            }

            final CellNet pn = ci.child.getNet(hn);
            if (pn != null) {
                // This could happen if the net is unused.
                canonPorts.put(
                    pn.canonicalName,
                    accumPortDir(canonPorts.get(pn.canonicalName),
                                 entry.getValue()));
            }
        }
        final Collection set = ports(ci);
        for (Iterator i = set.iterator(); i.hasNext(); ) {
            HierName h = ci.child.getNet((HierName) i.next()).canonicalName;
            final Integer dir = canonPorts.get(h);
            assert dir != null : "Cannot get direction for " + h + " in cell " + ci.child.typeName + " canonPorts = " + canonPorts;
            final String dirString;
            switch (dir.intValue()) {
              case PortDefinition.IN: dirString = "input"; break;
              case PortDefinition.OUT: dirString = "output"; break;
              case PortDefinition.INOUT: dirString = "inout"; break;
              default:
                   throw new AssertionError("Unknown port direction: " + dir);
            }
            addFormal(params, h.getCadenceString(), type, dirString);
        }
    }
    protected void addActual(final List params, final String name,
                             final String type) {
        params.add(lookupNode(name, type));
    }
    protected void addActual(final List params, final String portName,
                             final String name, final String type) {
        final VerilogObject port = factory.ident(portName, true);
        params.add(factory.namedPort(port, lookupNode(name, type)));
    }
    protected void addActual(final boolean byName, final List params,
                             final HierName child, final HierName parent,
                             final String type) {
        final String sparent = parent.getCadenceString();
        if (byName) {
            addActual(params, child.getCadenceString(), sparent, type);
        } else {
            addActual(params, sparent, type);
        }
    }
    protected void actual(final List params, final ConnectionInfo ci,
                          final String type) {
        actual(params, ci, type, false);
    }
    protected void actual(final List params, final ConnectionInfo ci,
                          final String type, final boolean byName) {
        actual(params, ci, byName, new UnaryFunction() {
                  public Object execute(final Object o) { return type; }
               });
    }
    protected void actual(final List params, final ConnectionInfo ci,
                          final UnaryFunction uf) {
        actual(params, ci, false, uf);
    }
    protected void actual(final List params, final ConnectionInfo ci,
                          final boolean byName, final UnaryFunction uf) {
        final Collection set = ports(ci);
        for (Iterator i = set.iterator(); i.hasNext(); ) {
            final HierName port = (HierName) i.next();
            final CellNet net = ci.parent.getNet(ci.getParentName(port));
            final String type = (String) uf.execute(net);
            final CellNet childNet = ci.child.getNet(port);
            addActual(byName, params, childNet.canonicalName,
                      net.canonicalName, type);
        }
    }
    protected boolean unconvertible(final CellType cell) {
        return !cell.cast_cell.containsVerilog() &&
               (cell.isWiringCell ||
               cell.prs == null ||
               cell.cast_cell.getFullyQualifiedType().startsWith(libPrefix));
    }

    public interface BlockValue {
        VerilogObject node(final HierName instance, final HierName name);
        VerilogObject array(final VerilogObject[] objs);
    }

    protected static class DefaultBlockValue implements BlockValue {
        private final AbstractConverter converter;
        private final AliasedSet ns;
        private final UnaryFunction uf;
        public DefaultBlockValue(final AbstractConverter converter,
                                 final AliasedSet ns, final UnaryFunction uf) {
            this.converter = converter;
            this.ns = ns;
            this.uf = uf;
        }
        public VerilogObject node(final HierName instance,
                                  final HierName name) {
            final HierName parent =
                (HierName) ns.getCanonicalKey(HierName.append(instance, name));
            assert parent != null : "Cannot get net name in parent for " + name;
            final String type = (String) uf.execute(parent);
            return converter.lookupNode(parent, type);
        }
        public VerilogObject array(final VerilogObject[] objs) {
            return converter.factory.concatOp(objs);
        }
    }

    private class NonNodeException extends RuntimeException {
        private final Value v;
        public NonNodeException(final Value v) {
            this.v = v;
        }
        public Value getValue() {
            return v;
        }
    }

    private class NonDefchanException extends RuntimeException {
        private final CellInterface cell;
        public NonDefchanException(final CellInterface cell) {
            this.cell = cell;
        }
        public CellInterface getCell() {
            return cell;
        }
    }

    static HierName toHier(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionError("Cannot construct HierName: " + s);
        }
    }

    protected List<VerilogObject> verilogValue(final HierName instance,
                                               final AliasedSet ns,
                                               final Value v,
                                               final BlockValue value) {
        if (v == null) {
            // no connection
            return null;
        } else if (v instanceof ArrayValue) {
            final ArrayValue av = (ArrayValue) v;
            final HierName avi = av.getInstanceName();
            final HierName ai;
            if (avi == null || avi.isLocal()) {
                ai = instance;
            } else {
                ai = HierName.append(instance, avi.getParent());
            }
            final List<VerilogObject> objs = new ArrayList<VerilogObject>();
            for (Iterator i = av.getIterator(); i.hasNext(); ) {
                objs.addAll(0, verilogValue(ai, ns, (Value) i.next(), value));
            }
            return objs;
        } else if (v instanceof NodeValue) {
            final HierName name = ((NodeValue) v).getInstanceName();
            return Collections.singletonList(value.node(instance, name));
        } else if (v instanceof InstanceValue) {
            final InstanceValue iv = (InstanceValue) v;
            final CellInterface cell = iv.getCell();
            final List<VerilogObject> objs = new ArrayList<VerilogObject>();
            if (cell.isChannel()) {
                (new CellUtils.MarkPort() {
                    protected void mark(final NodeType nodeType,
                                        final String name,
                                        final int direction) {
                        objs.add(value.node(instance,
                                    HierName.append(iv.getInstanceName(),
                                                    toHier(name))));
                    }
                    protected void mark(final NodeType nodeType,
                                        final String name,
                                        final int direction,
                                        final int width) {
                        for (int i = width - 1; i >= 0; --i) {
                            mark(nodeType, name + i + "]", direction);
                        }
                    }
                    protected void mark(final ChannelType channelType,
                                        final String name,
                                        final int direction, final int width) {
                        for (int i = width - 1; i >= 0; --i) {
                            mark(channelType, name + i + "]", direction);
                        }
                    }
                    protected void mark(final ArrayType arrayType,
                                        final String name,
                                        final int direction) {
                        final int min = arrayType.getMinIndex();
                        final int max = arrayType.getMaxIndex();

                        final PortTypeInterface arrayedType =
                            arrayType.getArrayedType();
                        for (int i = max; i >= min; --i) {
                            mark(arrayedType, name + i, direction, true);
                        }
                    }
                }).mark(cell);
                return objs;
            } else {
                throw new NonDefchanException(cell);
            }
        } else {
            throw new NonNodeException(v);
        }
    }

    // Return an identifier object that's not subject to renaming.
    private VerilogObject dontRename(final String name, final boolean escape) {
        // XXX: use of hierIdent is a temporary workaround
        return factory.hierIdent(
                new VerilogObject[] { factory.ident(name, escape) });
    }

    private VerilogObject concat(final List<VerilogObject> objs,
                                 final BlockValue bv) {
        if (objs == null) {
            return null;
        } else if (objs.size() == 1) {
            return objs.get(0);
        } else {
            return bv.array(toArray(objs));
        }
    }

    protected Pair verilogInstance(final VerilogBlock.Instance inst,
                                   final HierName instance,
                                   final AliasedSet ns,
                                   final VerilogFactoryInterface factory,
                                   final BlockValue bv) {
        final Iterator order = inst.getPortsByOrder();
        final List params = new ArrayList();
        for (Iterator i = inst.getParameters(); i.hasNext(); ) {
            final Value v = (Value) i.next();
            final String s;
            try {
                if (v instanceof IntValue)
                    s = ((IntValue) v).getValue().toString();
                else
                    s = Double.toString(((FloatValue) v).getValue());
            } catch (InvalidOperationException e) {
                throw new AssertionError("Cannot get int or double value of: " + v);
            }
            params.add(factory.expr(s));
        }
        final List ports = new ArrayList();
        if (order == null) {
            final Iterator name = inst.getPortsByName();
            assert name != null;
            while (name.hasNext()) {
                final Map.Entry entry = (Map.Entry) name.next();
                final String key = (String) entry.getKey();
                final Value val = (Value) entry.getValue();
                final VerilogObject portName = dontRename(key, false);
                final VerilogObject port;
                try {
                    port = concat(verilogValue(instance, ns, val, bv), bv);
                } catch (NonNodeException e) {
                    throw new RuntimeException(
                        "Encountered unknown value while process connection " +
                        "in module " + inst.getModule() + " for port " + key +
                        ": " + e.getValue());
                } catch (NonDefchanException e) {
                    throw new RuntimeException(
                        "Encountered non-channel instance while process " +
                        "connection in module " + inst.getModule() +
                        " for port " + key + ": " +
                        e.getCell().getFullyQualifiedType());
                }
                ports.add(factory.namedPort(portName, port));
            }
        } else {
            int k = 1;
            while (order.hasNext()) {
                final Value v = (Value) order.next();
                final VerilogObject port;
                try {
                    port = concat(verilogValue(instance, ns, v, bv), bv);
                } catch (NonNodeException e) {
                    throw new RuntimeException(
                        "Encountered unknown value while process connection " +
                        "in module " + inst.getModule() +
                        " for port in position " + k + ": " + e.getValue());
                } catch (NonDefchanException e) {
                    throw new RuntimeException(
                        "Encountered non-channel instance while process " +
                        "connection in module " + inst.getModule() +
                        " for port in position " + k + ": " +
                        e.getCell().getFullyQualifiedType());
                }
                ports.add(port);
                ++k;
            }
        }
        return new Pair(params, ports);
    }

    static VerilogBlock.NamedBlock
    chooseVerilog(final CellInterface cell,
                  final Prs2Verilog.VerilogChooser chooser) {
        final BlockIterator bi =
            cell.getBlockInterface().iterator(BlockInterface.VERILOG);
        if (!bi.hasNext()) return null;
        final VerilogBlock vb = (VerilogBlock) bi.next();
        return chooser.choose(cell, vb);
    }

    protected List verilogBlock(final HierName instance, final AliasedSet ns,
                                final CellInterface cell,
                                final Prs2Verilog.VerilogChooser chooser,
                                final BlockValue bv) {
        final int[] id = new int[1];
        final String prefix = instance == null ? "inst" : (instance.toString() + "$");
        return verilogBlock(instance, ns, cell, chooser, bv,
                            vinst -> prefix + id[0]++);
    }

    protected List verilogBlock(final HierName instance, final AliasedSet ns,
                                final CellInterface cell,
                                final Prs2Verilog.VerilogChooser chooser,
                                final BlockValue bv,
                                final Function<VerilogBlock.Instance,String> nameFunc) {
        final VerilogBlock.NamedBlock nb = chooseVerilog(cell, chooser);
        if (nb == null) return null;
        final List insts = new ArrayList();
        int id = 0;
        final List files = new ArrayList();
        final boolean addFiles = !depends.containsKey(cell);
        if (addFiles)
            for (Iterator j = nb.getFiles(); j.hasNext(); )
                files.add(j.next());
        for (Iterator i = nb.getInstances(); i.hasNext(); ++id) {
            final VerilogBlock.Instance inst = (VerilogBlock.Instance) i.next();
            final Pair p = verilogInstance(inst, instance, ns, factory, bv);
            final List params = (List) p.getFirst();
            final List ports = (List) p.getSecond();
            final String name = nameFunc.apply(inst);
            final VerilogObject instName = dontRename(name, mAlwaysEscape);
            final VerilogObject moduleName =
                dontRename(inst.getModule(), mAlwaysEscape);
            insts.add(factory.moduleInst(instName, moduleName, toArray(params),
                                         toArray(ports)));
            if (addFiles)
                for (Iterator j = inst.getFiles(); j.hasNext(); )
                    files.add(j.next());
        }
        if (addFiles)
            depends.put(cell, (String[]) files.toArray(new String[0]));
        return insts;
    }

    protected String getDelayMacroName() {
        return PRS2VERILOG_TAU;
    }

    protected VerilogObject getDelayWithMacro(final VerilogObject o) {
        return factory.binaryOp(macro(getDelayMacroName()), "*", o);
    }

    protected VerilogObject[] getDelayExpr(final CellType cell,
                                           final HierName node,
                                           float tau) {
        final float up = cell.getDelay().getDelay(node, true, tau);
        final float dn = cell.getDelay().getDelay(node, false, tau);
        return getDelayExpr(Float.toString(up), Float.toString(dn));
    }

    protected VerilogObject[] getDelayExpr(final String up, final String dn) {
        return new VerilogObject[] {
            getDelayWithMacro(factory.expr(up)),
            getDelayWithMacro(factory.expr(dn))
        };
    }

    protected VerilogObject getDelay(final VerilogObject[] d) {
        assert d.length >= 1 && d.length <= 3;
        return d.length == 1 ? factory.delay(d[0]) :
               d.length == 2 ? factory.delay(d[0], d[1])
                             : factory.delay(d[0], d[1], d[2]);
    }

    public Map<CellInterface, String[]> getDependencies() {
        return depends;
    }

    protected String getModuleName(final CellInterface cell) {
        return CellUtils.hashMetaParameters(cell.getFullyQualifiedType());
    }

    protected VerilogObject macro(final String s) {
        return factory.macroUse(factory.ident(s, mAlwaysEscape), null);
    }

    protected VerilogObject nextPrimitive() {
        return factory.ident("PRS2VERILOG_primitive_" + primitiveCount++, 
                             false);
    }

    protected boolean shouldSuppressFaults(final CellType cell) {
        return ((Boolean) DirectiveUtils.getTopLevelDirective(
                    cell.cast_cell,
                    DirectiveConstants.SUPPRESS_FAULTS)).booleanValue();
    }

    protected VerilogObject getTimeScaleMacro() {
        return factory.expr(
                "`" + ConverterConstants.getTimeScaleMacroString() + "\n");
    }

    public abstract VerilogObject convert(final CommandLineArgs theArgs,
                                          final VerilogObject moduleName,
                                          final boolean toplevel,
                                          final boolean topEnv);
}
