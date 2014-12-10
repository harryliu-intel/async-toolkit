package com.avlsi.file.cdl.parser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.functions.UnaryPredicate;

public class CDLAliases extends CDLFactoryAdaptor {
    private static String h2s(final HierName n) {
        return n.getAsString('.');
    }

    private static HierName s2h(final String n) {
        try {
            return HierName.makeHierName(n, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException(e);
        }
    }

    private static final HierName DEMOTER_HIERNAME =
        HierName.makeHierName("$\000\000\000\000\000");

    private static HierName demote(final HierName h) {
        return HierName.append(DEMOTER_HIERNAME, h);
    }

    private static HierName undemote(final HierName h) {
        HierName result = null;
        HierName left = h;
        while (left != null) {
            final HierName parent = left.getParent();
            final HierName current =
                HierName.makeHierName(left.getSuffixString());
            if (!DEMOTER_HIERNAME.equals(current)) {
                result = result == null ? current
                                        : HierName.append(current, result);
            }
            left = parent;
        }
        return result;
    }

    private static class MultiMap {
        private final Map map;
        public MultiMap() {
            map = new HashMap();
        }
        public void put(final Object key, final Object val) {
            Collection coll = (Collection) map.get(key);
            if (coll == null) {
                coll = new ArrayList();
                map.put(key, coll);
            }
            coll.add(val);
        }
        public void put(final Object key) {
            if (!map.containsKey(key)) {
                map.put(key, new ArrayList());
            }
        }
        public Collection get(final Object key) {
            return (Collection) map.get(key);
        }
        public Set keySet() {
            return map.keySet();
        }
    }

    private class Cell {
        private class Instance {
            final String subName;
            final String instance;
            public Instance(final String subName, final String instance) {
                this.subName = subName;
                this.instance = instance;
            }
            public String getType() {
                return subName;
            }
            public String getInstance() {
                return instance;
            }
        }

        private class Port {
            final String subName;
            final String instance;
            final int position;
            public Port(final String subName, final String instance,
                        final int position) {
                this.subName = subName;
                this.instance = instance;
                this.position = position;
            }
            public String getType() {
                return subName; 
            }
            public String getInstance() {
                return instance;
            }
            public int getPosition() {
                return position;
            }
        }

        private final String name;
        private final String[] ports;
        private final Set portSet;
        private final Collection instances;
        private final MultiMap aliases;

        public Cell(final String name, final String[] in, final String[] out) {
            this.name = name;
            this.ports = new String[in.length + out.length];
            System.arraycopy(in, 0, ports, 0, in.length);
            System.arraycopy(out, 0, ports, in.length, out.length);
            this.portSet = (Set) CollectionUtils.addAll(new HashSet(), ports);
            this.instances = new ArrayList();
            this.aliases = new MultiMap();
        }

        public void addInstance(final String subName, final HierName inst) {
            instances.add(new Instance(subName, h2s(inst)));
        }

        public String getName() {
            return name;
        }

        public String getPort(int position) {
            return ports[position];
        }

        public boolean isPort(final String name) {
            return portSet.contains(name);
        }

        public void addAlias(final HierName port, final String subName,
                             final HierName inst, final int position) {
            aliases.put(h2s(port), new Port(subName, h2s(inst), position));
        }

        public void addAlias(final HierName name) {
            aliases.put(h2s(name));
        }

        public Collection getAliases(final String name) {
            final Collection results = new HashSet();
            final Collection names = aliases.get(name);
            if (names == null) return Collections.EMPTY_LIST;
            results.add(name);
            for (Iterator i = names.iterator(); i.hasNext(); ) {
                final Port port = (Port) i.next();
                final Cell subcell = (Cell) cells.get(port.getType());
                if (subcell == null) {
                    throw new IllegalStateException(
                        "Subcircuit " + port.getType() + " undefined.");
                }
                final String subport = subcell.getPort(port.getPosition());
                results.add(append(port.getInstance(), subport));
                if (!routed.evaluate(subcell.getName())) {
                    for (Iterator j = subcell.getAliases(subport).iterator();
                         j.hasNext(); ) {
                        results.add(append(port.getInstance(), (String) j.next()));
                    }
                }
            }
            return results;
        }

        private void printList(final Iterator iter, final String seperator) {
            while (iter.hasNext()) {
                System.out.print(undemote((HierName) iter.next()));
                if (iter.hasNext()) System.out.print(seperator);
            }
        }

        public void printAlias(final String prefix, final String name,
                               final Collection names) {
            final Set sorted = new TreeSet();
            for (Iterator i = names.iterator(); i.hasNext(); ) {
                sorted.add(s2h(append(prefix, (String) i.next())));
            }
            sorted.add(s2h(append(prefix, name)));
            printList(sorted.iterator(), "=");
        }

        public void printAliases(final String prefix, final boolean internal) {
            for (Iterator i = aliases.keySet().iterator(); i.hasNext(); ) {
                final String port = (String) i.next();
                if (internal && isPort(port)) continue;
                printAlias(prefix, port, getAliases(port));
                System.out.println();
            }
            for (Iterator i = instances.iterator(); i.hasNext(); ) {
                final Instance inst = (Instance) i.next();
                final Cell subcell = (Cell) cells.get(inst.getType());
                if (subcell == null) {
                    throw new IllegalStateException(
                        "Subcircuit " + inst.getType() + " undefined.");
                }
                if (!routed.evaluate(subcell.getName())) {
                    subcell.printAliases(append(prefix, inst.getInstance()),
                                         true);
                }
            }
        }
    }

    private String append(final String a, final String b) {
        return a == null ? b : a + hierarchyDelimiter + b;
    }

    private final String hierarchyDelimiter;
    private final Map/*<String,Cell>*/ cells;
    private Cell currentCell;

    /**
     * A predicate that evaluates a cell name, and returns <code>true</code> if
     * the cell is routed, and <code>false</code> otherwise.
     **/
    private final UnaryPredicate routed;

    /**
     * A predicate that evaluates a cell name, and returns <code>true</code> if
     * the cell is a gate, and <code>false</code> otherwise.
     **/
    private final UnaryPredicate gate;

    public CDLAliases(final UnaryPredicate routed, final UnaryPredicate gate) {
        this.hierarchyDelimiter = ".";
        this.cells = new TreeMap();
        this.currentCell = null;
        this.routed = routed;
        this.gate = gate;
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        currentCell.addAlias(n1);
        currentCell.addAlias(n2);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        currentCell.addAlias(npos);
        currentCell.addAlias(nneg);
    }

    public void makeDiode(HierName name, String type, HierName npos,
                          HierName nneg, CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        currentCell.addAlias(npos);
        currentCell.addAlias(nneg);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { 
        currentCell.addAlias(npos);
        currentCell.addAlias(nneg);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        currentCell.addAlias(ns);
        currentCell.addAlias(nd);
        currentCell.addAlias(ng);
        currentCell.addAlias(nb);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        final HierName instName;
        if (gate.evaluate(subName)) {
            instName = demote(name);
        } else {
            instName = name;
            currentCell.addInstance(subName, name);
        }
        for (int i = 0; i < args.length; ++i) {
            currentCell.addAlias(args[i], subName, instName, i);
        }
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        if (currentCell != null) {
            throw new IllegalStateException(
                "Found nested subcircuit definition of " + subName +
                " while processing " + currentCell.getName());
        }

        if (cells.containsKey(subName)) {
            throw new IllegalStateException(
                "Found redefinition of subcircuit " + subName);
        }

        currentCell = new Cell(subName, in, out);
        cells.put(subName, currentCell);
    }
            
    public void endSubcircuit(String subName, Environment env) {
        if (currentCell == null) {
            throw new IllegalStateException("Found unmatched .ENDS");
        }

        currentCell = null;
    }

    public void printAliases(final String type) {
        final Cell cell = type == null ? null : (Cell) cells.get(type);
        if (cell == null) {
            for (Iterator i = cells.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                System.out.println("cell = " + entry.getKey());
                ((Cell) entry.getValue()).printAliases(null, false);
            }
        } else {
            System.out.println(cell.getName());
            cell.printAliases(null, false);
        }
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        final String grayboxList = theArgs.getArgValue("graybox-list", null);
        final String cell = theArgs.getArgValue("cell", null);
        final String gatesRegex = theArgs.getArgValue("gates-regex", null);
        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            System.err.print( pedanticArgs.pedanticString() );
            System.exit(1);
        }
        final Pattern gatesPattern =
            gatesRegex == null ? null : Pattern.compile(gatesRegex);
        final UnaryPredicate gatePredicate = new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                return gatesPattern != null &&
                       gatesPattern.matcher((String) o).matches();
            }
        };

        final UnaryPredicate grayboxPredicate;
        if (grayboxList == null) {
            grayboxPredicate = new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return false;
                }
            };
        } else {
            final BufferedReader br =
                new BufferedReader(new FileReader(grayboxList));
            final Set grayboxSet = new HashSet();
            String line;
            while ((line = br.readLine()) != null) {
                grayboxSet.add(line.trim());
            }
            br.close();
            grayboxPredicate = new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return grayboxSet.contains(o);
                }
            };
        }
        final CDLAliases ca = new CDLAliases(grayboxPredicate, gatePredicate);
        // input CDL is expected to be using Cadence names
        ReadCDLIntoFactory.readCDL(
            new InputStreamReader(System.in),
            new CDLRenameFactory(ca,
                new TrivialCDLNameInterfaceFactory(
                    new CadenceReverseNameInterface())));
        ca.printAliases(cell);
    }
}
