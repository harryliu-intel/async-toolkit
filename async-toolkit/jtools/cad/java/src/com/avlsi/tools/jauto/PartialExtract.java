/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.jauto;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.collections.AST;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast2.impl.CastTwoLexer;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.prs.ProductionRule;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;

public final class PartialExtract {
    public interface Info {
        int INCLUDE = 0, EXCLUDE = 1;
        int getAction(final HierName instance);
    }

    private final Info info;
    private final String top;
    private final Map cells;
    private final Map results;

    public PartialExtract(final Map cells,
                          final String top,
                          final Info info) {
        this.cells = cells;
        this.top = top;
        this.info = info;
        this.results = new HashMap();
    }
    
    private static class CellMaker implements Template.Visitor {
        private final CDLFactoryInterface target;

        /**
         * A mapping from instance name (HierName) to instance type (String).
         **/
        private final Map instances;

        public CellMaker(final CDLFactoryInterface target, final Map instances)
        {
            this.target = target;
            this.instances = instances;
        }

        public void resistorStatement(final HierName name,
                                      final HierName n1,
                                      final HierName n2,
                                      final CDLLexer.InfoToken val,
                                      final Map parameters,
                                      final Environment env) {
            target.makeResistor(name, n1, n2, val, parameters, env);
        }

        public void capacitorStatement(final HierName name,
                                       final HierName npos,
                                       final HierName nneg,
                                       final CDLLexer.InfoToken val,
                                       final Map parameters,
                                       final Environment env) {
            target.makeCapacitor(name, npos, nneg, val, parameters, env);
        }

        public void transistorStatement(final HierName name,
                                        final String type,
                                        final HierName ns,
                                        final HierName nd,
                                        final HierName ng,
                                        final HierName nb,
                                        final CDLLexer.InfoToken w,
                                        final CDLLexer.InfoToken l,
                                        final Map parameters,
                                        final Environment env) {
            target.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters,
                                  env);
        }

        public void inductorStatement(final HierName name,
                                      final HierName npos,
                                      final HierName nneg,
                                      final CDLLexer.InfoToken val,
                                      final Map parameters,
                                      final Environment env) {
            target.makeInductor(name, npos, nneg, val, parameters, env);
        }

        public void diodeStatement(final HierName name,
                                   final String type,
                                   final HierName npos,
                                   final HierName nneg,
                                   final CDLLexer.InfoToken val,
                                   final Map parameters,
                                   final Environment env) {
            target.makeDiode(name, type, npos, nneg, val, parameters, env);
        }

        public void bipolarStatement(final HierName name,
                                     final String type,
                                     final HierName nc,
                                     final HierName nb,
                                     final HierName ne,
                                     final CDLLexer.InfoToken val,
                                     final Map parameters,
                                     final Environment env) {
            target.makeBipolar(name, type, nc, nb, ne, val, parameters, env);
        }

        public void callStatement(final HierName name,
                                  final String subName,
                                  final HierName[] args,
                                  final Map parameters,
                                  final Environment env) {
            final String realSub = (String) instances.get(name);
            if (realSub != null) {
                target.makeCall(name, realSub, args, parameters, env);
            }
        }
    }

    private Template makeCell(final Template templ, final Map instances) {
        final Map m = new HashMap();
        final String name = "DUMMY";
        final Template newTempl = new Template(m);
        final Pair args = templ.getArguments();
        final Environment nullEnv = NullEnvironment.getInstance();
        newTempl.beginSubcircuit(name,
                                 (String[]) args.getFirst(),
                                 (String[]) args.getSecond(),
                                 templ.getParameters(),
                                 nullEnv);
        final CellMaker maker = new CellMaker(newTempl, instances);
        for (Template.StatementIterator i = templ.getStatements();
             i.hasNext(); ) {
            i.next(maker);
        }
        newTempl.endSubcircuit(name, nullEnv);
        return (Template) m.get(name);
    }

    public interface CellResultHandler {
        void setCellName( final String cellName );
        void handleSubtype( final String subTypeName,
                                   final Map instances );
    }

    public void traverseResult( final CellResultHandler handler ) {
        final Iterator cellEntryIter = results.entrySet().iterator();
        while ( cellEntryIter.hasNext() ) {
            final Map.Entry cellEntry = ( Map.Entry ) cellEntryIter.next();
            final String cellName = ( String ) cellEntry.getKey();
            final Map cellMap = ( Map ) cellEntry.getValue();
            
            final Iterator cellSubtypeEntryIter = cellMap.entrySet().iterator();
            
            handler.setCellName( cellName );

            while ( cellSubtypeEntryIter.hasNext() ) {
                final Map.Entry subTypeEntry = ( Map.Entry ) cellSubtypeEntryIter.next();
                final Map instanceMap = ( Map ) subTypeEntry.getKey();
                final Pair subTypeInfo = ( Pair ) subTypeEntry.getValue();
                final String subTypeName = ( String ) subTypeInfo.getFirst();

                handler.handleSubtype( subTypeName,
                                       instanceMap );
            }
        }
    }

    /**
     * Given the name of a cell, and a map of instance names
     * (<code>HierName</code>) to cell types (<code>String</code>) for
     * instances that exist in the cell, return the name of the cell that
     * implements that specification.  The order of pairs inside the list
     * should be consistent, for a given cell.
     **/
    private String getCell(final String name, final Map instances) {
        if (!results.containsKey(name)) results.put(name, new HashMap());

        final Map m = (Map) results.get(name);
        Pair result = (Pair) m.get(instances);
        if (result == null) {
            final Template t = (Template) cells.get(name);
            assert t != null : "No template found for cell " + name;

            result = new Pair(name + "." + m.size(), makeCell(t, instances));
            m.put(instances, result);
        }
        return (String) result.getFirst();
    }

    private class Traverser implements Template.Visitor {
        private final HierName prefix;
        private final Map instances;

        public Traverser(final HierName prefix, final Map instances) {
            this.prefix = prefix;
            this.instances = instances;
        }

        public void resistorStatement(final HierName name,
                                      final HierName n1,
                                      final HierName n2,
                                      final CDLLexer.InfoToken val,
                                      final Map parameters,
                                      final Environment env) { }

        public void capacitorStatement(final HierName name,
                                       final HierName npos,
                                       final HierName nneg,
                                       final CDLLexer.InfoToken val,
                                       final Map parameters,
                                       final Environment env) { }

        public void transistorStatement(final HierName name,
                                        final String type,
                                        final HierName ns,
                                        final HierName nd,
                                        final HierName ng,
                                        final HierName nb,
                                        final CDLLexer.InfoToken w,
                                        final CDLLexer.InfoToken l,
                                        final Map parameters,
                                        final Environment env) { }

        public void inductorStatement(final HierName name,
                                      final HierName npos,
                                      final HierName nneg,
                                      final CDLLexer.InfoToken val,
                                      final Map parameters,
                                      final Environment env) { }

        public void diodeStatement(final HierName name,
                                   final String type,
                                   final HierName npos,
                                   final HierName nneg,
                                   final CDLLexer.InfoToken val,
                                   final Map parameters,
                                   final Environment env) { }

        public void bipolarStatement(final HierName name,
                                     final String type,
                                     final HierName nc,
                                     final HierName nb,
                                     final HierName ne,
                                     final CDLLexer.InfoToken val,
                                     final Map parameters,
                                     final Environment env) { }

        public void callStatement(final HierName name,
                                  final String subName,
                                  final HierName[] args,
                                  final Map parameters,
                                  final Environment env) {
            final HierName fullName = HierName.append(prefix, name);
            final int subBehav = info.getAction(fullName);
            if (subBehav == Info.INCLUDE) {
                final Template templ = (Template) cells.get(subName);
                if (templ == null) {
                    throw new RuntimeException("Cannot find cell " + subName);
                }
                final Map m = process(templ, fullName);
                final String s = PartialExtract.this.getCell(subName, m);
                instances.put(name, s);
            }
        }
    }

    private Map process(final Template templ, final HierName prefix) {
        final Map instances = new HashMap();
        final Traverser trav = new Traverser(prefix, instances);
        for (Template.StatementIterator i = templ.getStatements();
             i.hasNext(); ) {
            i.next(trav);
        }
        return instances;
    }

    private String process() {
        final Template t = (Template) cells.get(top);
        //final Map instances = process(t, HierName.makeHierName("X"));
        final Map instances = process(t, null);
        return getCell(top, instances);
    }

    public void execute(final CDLFactoryInterface target) {
        final String newTop = process();
        final Environment env = new LocalEnvironment();
        for (Iterator i = cells.keySet().iterator(); i.hasNext(); ) {
            final String sub = (String) i.next();
            final Map m = (Map) results.get(sub);
            if (m != null) {
                for (Iterator j = m.entrySet().iterator(); j.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) j.next();
                    final Pair p = (Pair) entry.getValue();
                    final String cellName = (String) p.getFirst();
                    final Template templ = (Template) p.getSecond();
                    templ.execute(target, env, cellName);
                }
            }
        }
    }

    /**
     * Return a data structure representing a FQCN+- (fully qualified cell name
     * plus/minus instances, i.e., lib.math.add.ADD_32.0-kpg3+data3[0].ctrl).
     * The result is a <code>Triplet</code> of top cell name, a environment
     * name which might be <code>null</code>, and a <code>Collection</code>
     * containing <code>Pair</code>s of instance name as <code>HierName</code>
     * and if it is a plus as <code>Boolean</code> (so the above example would
     * be mapped to: (lib.math.add.ADD_32.0 [(kpg3 false) (data3[0].ctrl
     * true)]).
     **/
    public static Triplet parseCellPlusMinus(final String fqcn) {
        final String err =
            "Cannot parse fully qualified cell name plus minus: " + fqcn;
        final CastTwoParser castParser =
            CastTwoParser.getParser(fqcn, 0, 0, "<no name>");
        try {
            castParser.partialExtraction();
        } catch (RecognitionException e) {
            throw new RuntimeException(err, e);
        } catch (TokenStreamException e) {
            throw new RuntimeException(err, e);
        }

        final AST ast = castParser.getAST();

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();
        try {
            return treeParser.partialExtract(ast);
        } catch (RecognitionException e) {
            throw new RuntimeException(err, e);
        }
    }

    public interface LeafCallback {
        Object leaf(CellInterface cell, HierName inst, HierName canon);
        Object midlevel(Object o, CellInterface cell, HierName inst,
                        HierName canon);
    }

    private static Pair getInstancePort(final HierName name,
                                        final CellInterface cell) {
        HierName prefix = name;
        HierName suffix = null;
        while (prefix != null && cell.getSubcell(prefix) == null) {
            final HierName child =
                HierName.makeHierName(prefix.getSuffixString());
            suffix = suffix == null ? child : HierName.append(child, suffix);
            prefix = prefix.getParent();
        }
        if (prefix == null) return null;
        else return new Pair(prefix, suffix);
    }

    public static Set getLeafDrivers(final CellInterface cell,
                                     final HierName canon,
                                     final Cadencize cad,
                                     final Map cache,
                                     final LeafCallback cb) {
        final Pair key = new Pair(cell, canon);
        final Set cached = (Set) cache.get(key);
        if (cached != null) return cached;

        final Set result = new HashSet();
        final CadenceInfo ci = cad.convert(cell);
        final AliasedSet local = ci.getLocalNodes();
        final Set seen = new HashSet();
        for (Iterator i = local.getAliases(canon); i.hasNext(); ) {
            final HierName alias = (HierName) i.next();
            final Pair instancePort = getInstancePort(alias, cell);
            assert instancePort != null : alias + " cannot be found in " +
                                          cell.getFullyQualifiedType();
            final HierName inst = (HierName) instancePort.getFirst();
            final HierName port = (HierName) instancePort.getSecond();

            final CellInterface subcell = cell.getSubcell(inst);
            if (subcell.isNode() || subcell.isChannel()) continue;

            final CadenceInfo subci = cad.convert(subcell);
            final AliasedMap ports = subci.getPortNodes();
            if (!((Boolean) ports.getValue(port)).booleanValue()) continue;

            final HierName subcanon = (HierName)
                subci.getLocalNodes().getCanonicalKey(port);

            if (!seen.add(new Pair(inst, subcanon))) continue;

            if (CellUtils.isLeaf(subcell)) {
                final Object o = cb.leaf(subcell, inst, subcanon);
                if (o != null) result.add(o);
            } else {
                final Set subresult =
                    getLeafDrivers(subcell, subcanon, cad, cache, cb);
                for (Iterator k = subresult.iterator(); k.hasNext(); ) {
                    final Object o =
                        cb.midlevel(k.next(), subcell, inst, subcanon);
                    if (o != null) result.add(o);
                }
            }
        }

        final Set readOnly = Collections.unmodifiableSet(result);
        cache.put(key, readOnly);
        return readOnly;
    }

    public static class CellPlusMinusKeyword extends CellPlusMinus {
        private static Collection<HierName> powerRails;
        private Triplet spec;
        public CellPlusMinusKeyword(final String spec, final int def) {
            this(parseCellPlusMinus(spec), def);
        }
        public CellPlusMinusKeyword(final Triplet spec, final int def) {
            super((String) spec.getFirst(), new HashMap(), new HashSet(), def,
                  null, (String) spec.getSecond());
            this.spec = spec;
        }
        public CellPlusMinusKeyword(final String spec, final int def,
                                    final CastFileParser cfp,
                                    final Cadencize cad)
            throws CastSemanticException {
            this(spec, def);
            initialize(cfp, cad);
        }
        public void initialize(final CastFileParser cfp, final Cadencize cad)
            throws CastSemanticException {
            initialize(cfp.getFullyQualifiedCell(getTop()), cad);
        }
        private Collection<HierName> getPowerRails() {
            if (powerRails == null) {
                powerRails = Arrays.asList(HierName.makeHierName("GND"),
                                           HierName.makeHierName("Vdd"));
            }
            return powerRails;
        }
        public void initialize(final CellInterface cell, final Cadencize cad) {
            final Collection c = (Collection) spec.getThird();

            String keyword = null;
            if (c.size() == 1) {
                final Pair p = (Pair) c.iterator().next();
                if (!((Boolean) p.getSecond()).booleanValue()) {
                    keyword = (String) p.getFirst();
                }
            }

            if (keyword != null && (keyword.equals("+localnodes") ||
                                    keyword.equals("+portnodes") ||
                                    keyword.equals("+localportnodes"))) {
                this.def = Info.EXCLUDE;
                final boolean all = keyword.equals("+localportnodes");
                final boolean port = keyword.equals("+portnodes");
                final Map cache = new HashMap();
                final CadenceInfo ci = cad.convert(cell);
                final AliasedSet localNodes = ci.getLocalNodes();
                final AliasedMap portNodes = ci.getPortNodes();
                
                // determine the canonical name of power rails; they must be
                // ports
                final Set<HierName> canonRails = new HashSet<HierName>();
                for (HierName rail : getPowerRails()) {
                    final HierName canon =
                        (HierName) localNodes.getCanonicalKey(rail);
                    if (canon != null && portNodes.contains(canon))
                        canonRails.add(canon);
                }

                for (Iterator i = localNodes.getCanonicalKeys();
                     i.hasNext(); ) {
                    final HierName canon = (HierName) i.next();
                    if (canonRails.contains(canon) ||
                        (!all && portNodes.contains(canon) != port))
                        continue;
                    final Set drivers = getLeafDrivers(cell, canon, cad, cache,
                        new LeafCallback() {
                            public Object leaf(CellInterface cell,
                                               HierName inst, HierName canon) {
                                return inst;
                            }
                            public Object midlevel(Object o, CellInterface cell,
                                                   HierName inst,
                                                   HierName canon) {
                                final HierName h = (HierName) o;
                                return HierName.append(inst, h);
                            }
                        });
                    for (Iterator j = drivers.iterator(); j.hasNext(); ) {
                        final HierName inst = (HierName) j.next();
                        addInstance(inst, Boolean.TRUE);
                        setDescedents(inst);
                    }
                }
                setRest(keyword);
            } else {
                setRest(processSpec(c));
            }
            spec = null;
        }
        public int getAction(HierName instance) {
            assert spec == null : "Need to call initialize first";
            return super.getAction(instance);
        }
        public boolean isEmpty() {
            assert spec == null : "Need to call initialize first";
            return super.isEmpty();
        }
    }

    public static class CellPlusMinus implements Info {
        private final String top;
        private final Map instances;
        private final Set hasDescedents;
        protected int def;
        private String rest;
        private final String env;
        public CellPlusMinus(final String spec, final int def) {
            this(parseCellPlusMinus(spec), def);
        }
        public CellPlusMinus(final Triplet spec, final int def) {
            this((String) spec.getFirst(), new HashMap(), new HashSet(), def,
                 null, (String) spec.getSecond());
            setRest(processSpec((Collection) spec.getThird()));
        }
        protected CellPlusMinus(final String top, final Map instances,
                                final Set hasDescedents, final int def,
                                final String rest, final String env) {
            this.top = top;
            this.instances = instances;
            this.hasDescedents = hasDescedents;
            this.def = def;
            this.rest = rest;
            this.env = env;
        }
        protected String processSpec(Collection c) {
            final StringBuffer sb = new StringBuffer();
            for (Iterator i = c.iterator(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final String sinst = (String) p.getFirst();
                final HierName inst;
                try {
                    inst = HierName.makeHierName(sinst, '.');
                } catch (InvalidHierNameException e) {
                    throw new RuntimeException("Cannot create HierName for: " + sinst);
                }
                final Boolean plus = (Boolean) p.getSecond();
                addInstance(inst, plus);
                if (plus.booleanValue()) {
                    sb.append('+');
                    setDescedents(inst);
                } else {
                    sb.append('-');
                }
                sb.append(sinst);
            }
            return sb.toString();
        }
        protected void addInstance(final HierName inst, final Boolean plus) {
            instances.put(inst, plus);
        }
        protected void setDescedents(HierName instance) {
            while (instance != null) {
                hasDescedents.add(instance);
                instance = instance.getParent();
            }
        }
        public int getAction(HierName instance) {
            if (hasDescedents.contains(instance)) return Info.INCLUDE;
            while (instance != null) {
                final Boolean plus = (Boolean) instances.get(instance);
                if (plus != null)
                    return plus.booleanValue() ? Info.INCLUDE : Info.EXCLUDE;
                instance = instance.getParent();
            }
            return def;
        }
        public String getTop() {
            return top;
        }
        public String getEnvironment() {
            return env;
        }
        protected void setRest(final String rest) {
            this.rest = rest;
        }
        public String getRest() {
            return rest;
        }
        public boolean isEmpty() {
            return instances.isEmpty();
        }
    }

    public static void main(String[] args) {
        final Triplet p = parseCellPlusMinus(args[0]);
        System.out.println("p = " + p);
    }
}
