package com.avlsi.tools.prs2verilog;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.fast.ports.*;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryPredicate;

public class GenerateWrapper {
    private static Pair verilogDecl(final List parts) {
        final List C = new ArrayList();
        final List A = new ArrayList();
        int N = 0;
        int elements = 1;
        int arrayed = 0;
        while (!parts.isEmpty()) {
            final Pair CA = getComponent(parts);
            C.add(CA.getFirst());
            final List Ai = (List) CA.getSecond();
            A.add(Ai);
            if (!Ai.isEmpty()) ++arrayed;
            for (Iterator i = Ai.iterator(); i.hasNext(); ) {
                elements *= ((Integer) i.next()).intValue();
            }
            ++N;
        }

        final StringBuffer name = new StringBuffer();
        for (int i = 0; i < N; ++i) {
            final String Ci = (String) C.get(i);
            final List Ai = (List) A.get(i);
            name.append(Ci);
            if (i < N - 1 && Ai.isEmpty()) {
                name.append('_');
            } else if (i == N - 1 && (Ai.isEmpty() ||
                                      (arrayed == 1 && Ai.size() == 1))) {
                // emit nothing
            } else {
                name.append('_');
                for (Iterator j = Ai.iterator(); j.hasNext(); ) {
                    final Integer Mj = (Integer) j.next();
                    name.append(Mj.intValue());
                    if (j.hasNext()) name.append('x');
                }
            }
            if (i < N - 1) name.append('_');
        }

        return new Pair(arrayed > 0 ? "[" + (elements - 1) + ":0] " : "",
                        name.toString());
    }

    private static HashMap declCache = new HashMap();

    private static Pair getDecl(final List original) {
        Pair result = (Pair) declCache.get(original);
        if (result == null) {
            result = verilogDecl(new ArrayList(original));
            declCache.put(original, result);
        }
        return result;
    }

    private static int accessArray(final List indices, final List parts) {
        int n = 1;
        int result = 0;
        for (ListIterator i = indices.listIterator(indices.size()),
                          j = parts.listIterator(parts.size());
             i.hasPrevious(); ) {
            final int index = ((Integer) i.previous()).intValue();
            Object size;
            while ((size = j.previous()) instanceof String);
            result += index * n;
            n *= ((Integer) size).intValue();
        }
        return result;
    }

    private static class PortInfo {
        private final List parts;
        private final List indices;
        private final int direction;
        public PortInfo(final List parts, final List indices,
                        final int direction) {
            this.parts = parts;
            this.indices = indices;
            this.direction = direction;
        }
        public String getDirection() {
            if (direction > 0) return "output";
            else if (direction == 0) return "inout";
            else return "input";
        }
        public String getName() {
            return (String) getDecl(parts).getSecond();
        }
        public String getSize() {
            return (String) getDecl(parts).getFirst();
        }
        public String getIndex() {
            return indices.isEmpty() ? ""
                                     : "[" + accessArray(indices, parts) + "]";
        }
    }

    private static class Munger extends CellUtils.MarkPort {
        private static String[] SKIP_LIST = new String[] { "GND", "Vdd" };
        private final Map portMap;
        private final boolean skipPowerRail;
        private List parts;
        private List indices;

        public Munger(final Map portMap, final boolean skipPowerRail) {
            this.portMap = portMap;
            this.skipPowerRail = skipPowerRail;
            this.parts = new ArrayList();
            this.indices = new ArrayList();
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction) {
            super.mark(channelType, name, direction);
        }
        protected void mark(final ChannelType channelType, final String name,
                            final int direction, final int width) {
            parts.add(new Integer(width));
            final List oldIndices = indices;
            for (int i = 0; i < width; ++i) {
                indices = new ArrayList(oldIndices);
                indices.add(new Integer(i));
                mark(channelType, name + i + "]", direction);
            }
            indices = oldIndices;
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction) {
            portMap.put(name, new PortInfo(parts, indices, direction));
        }
        protected void mark(final NodeType nodeType, final String name,
                            final int direction, final int width) {
            parts.add(new Integer(width));
            final List oldIndices = indices;
            for (int i = 0; i < width; ++i) {
                indices = new ArrayList(oldIndices);
                indices.add(new Integer(i));
                mark(nodeType, name + i + "]", direction);
            }
            indices = oldIndices;
        }
        protected void mark(final ArrayType arrayType, final String name,
                            final int direction) {
            final int min = arrayType.getMinIndex();
            if (min != 0) {
                throw new RuntimeException("Array " + name + " does not start from 0");
            }
            final int max = arrayType.getMaxIndex();
            parts.add(new Integer(max + 1));
            final List oldParts = parts;
            final PortTypeInterface arrayedType = arrayType.getArrayedType();
            final List oldIndices = indices;
            for (int i = min; i <= max; ++i) {
                parts = new ArrayList(oldParts);
                indices = new ArrayList(oldIndices);
                indices.add(new Integer(i));
                mark(arrayedType, name + i, direction, true);
            }
            parts = oldParts;
            indices = oldIndices;
        }
        protected void mark(final StructureType structureType,
                            final String name, final int direction) {
            super.mark(structureType, name, direction);
        }
        protected void mark(final PortDefinition p, final String prefix,
                            int direction) {
            parts.add(p.getName());
            super.mark(p, prefix, direction);
        }
        public void mark(final Iterator portDefinitions, final String prefix,
                         final int direction) {
            final List oldParts = parts;
            while (portDefinitions.hasNext()) {
                final PortDefinition p =
                    (PortDefinition) portDefinitions.next();
                parts = new ArrayList(oldParts);
                mark(p, prefix, direction);
            }
            parts = oldParts;
        }
        public void mark(final CellInterface ci) {
            final Iterator filtered;
            if (skipPowerRail) {
                filtered = 
                    new FilteringIterator(
                        ci.getPortDefinitions(),
                        new UnaryPredicate() {
                            public boolean evaluate(final Object o) {
                                final PortDefinition def = (PortDefinition) o;
                                for (int i = 0; i < SKIP_LIST.length; ++i) {
                                    if (def.getName().equals(SKIP_LIST[i])) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        });
            } else {
                filtered = ci.getPortDefinitions();
            }
            mark(filtered);
        }
    }

    private static Pair getComponent(final List parts) {
        final String C = (String) parts.remove(0);
        final List A = new ArrayList();
        while (!parts.isEmpty() && parts.get(0) instanceof Integer) {
            A.add(parts.remove(0));
        }
        return new Pair(C, A);
    }

    private static void getWrapper(final CellInterface cell,
                                   final CDLNameInterface renamer,
                                   final boolean skipPowerRail,
                                   final PrintWriter pw) {
        final Map portMap = new LinkedHashMap();
        (new Munger(portMap, skipPowerRail)).mark(cell);

        pw.println("module " + cell.getType() + "(");

        final Set declarations = new LinkedHashSet();
        final Set seen = new HashSet();
        for (Iterator i = portMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final PortInfo pinfo = (PortInfo) entry.getValue();
            if (seen.add(pinfo.getName())) declarations.add(pinfo);
        }

        for (Iterator i = declarations.iterator(); i.hasNext(); ) {
            final PortInfo pinfo = (PortInfo) i.next();
            pw.print(pinfo.getName());
            if (i.hasNext()) pw.print(',');
            pw.println();
        }
        pw.println(");");

        for (Iterator i = declarations.iterator(); i.hasNext(); ) {
            final PortInfo pinfo = (PortInfo) i.next();
            pw.println(pinfo.getDirection() + " " + pinfo.getSize() +
                       pinfo.getName() + ";");
        }

        pw.print(VerilogUtil.escapeIfNeeded(cell.getFullyQualifiedType()));
        pw.println(" X(");

        final Cadencize cad = new Cadencize(false);
        final AliasedMap ports = cad.convert(cell).getPortNodes();
        boolean first = true;
        for (Iterator i = portMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final HierName port;
            try {
                port = HierName.makeHierName((String) entry.getKey(), '.');
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Cannot make HierName: " +
                                           entry.getKey(), e);
            }
            final PortInfo pinfo = (PortInfo) entry.getValue();
            final HierName canon = (HierName) ports.getCanonicalKey(port);
            if (!((Boolean) ports.getValue(port)).booleanValue()) continue;
            if (first) first = false;
            else pw.println(',');
            final String renamed;
            try {
                renamed = renamer.renameNode(canon.getAsString('.'));
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename: " + canon, e);
            }
            pw.print("." + VerilogUtil.escapeIfNeeded(renamed));
            pw.print("(" + pinfo.getName() + pinfo.getIndex());
            pw.print(')');
        }

        pw.println(");");

        pw.println("endmodule");
        pw.flush();
    }

    private static void usage() {
        final String className = GenerateWrapper.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className );
        System.out.println("\t--cast-path=<cast-path> (defaults to .)");
        System.out.println("\t--cast-version=<version> (defaults to 2)");
        System.out.println("\t--cell=<cell> (name of cell to process)");
        System.out.println("\t[--translate=<cadence | gds2>] (translate port names of wrapped cell)");
        System.out.println("\t[--skip-power-rail] (skip GND and Vdd ports)");
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        final String translate = theArgs.getArgValue("translate", null);
        final boolean skipPowerRail = theArgs.argExists("skip-power-rail");
        if (cellName == null) {
            usage();
        }

        final SearchPath castPath = new FileSearchPath(castRoot);
        final CastFileParser cfp = new CastFileParser(castPath, castVersion);
        final CellInterface cell = cfp.getFullyQualifiedCell(cellName);

        final CDLNameInterface renamer;
        if (translate == null) renamer = new IdentityNameInterface();
        else if (translate.equals("cadence"))
            renamer = new CadenceNameInterface();
        else if (translate.equals("gds2"))
            renamer = new GDS2NameInterface();
        else {
            renamer = null;
            System.out.println("Unknown translate method: " + translate);
            usage();
        }

        getWrapper(cell, renamer, skipPowerRail, new PrintWriter(System.out));
    }
}
