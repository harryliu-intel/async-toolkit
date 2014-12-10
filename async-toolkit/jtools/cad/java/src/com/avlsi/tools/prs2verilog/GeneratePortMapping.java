/*
 * Copyright 2002-2009 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.file.verilog.grammar.PortDeclarationTreeParser;
import com.avlsi.tools.prs2verilog.verilog.*;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.functions.UnaryFunction;

public class GeneratePortMapping extends AbstractConverter {
    private final CellInterface cell;
    private final Prs2Verilog.VerilogChooser chooser;
    private final Cadencize cad;
    private Map boundsMap;

    public GeneratePortMapping(final VerilogFactoryInterface factory,
                               final CellInterface cell,
                               final Prs2Verilog.VerilogChooser chooser,
                               boolean alwaysEscape,
                               final Cadencize cad) {
        super(factory, alwaysEscape);
        this.cell = cell;
        this.chooser = chooser;
        this.cad = cad;
        this.boundsMap = Collections.EMPTY_MAP;
    }

    public Map getBounds() {
        return Collections.unmodifiableMap(boundsMap);
    }

    public VerilogObject convert(final CommandLineArgs theArgs,
                                 final VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        final AliasedSet ns = cad.convert(cell).getLocalNodes();
        final VerilogBlock.NamedBlock nb = chooseVerilog(cell, chooser);
        boundsMap =
            DirectiveUtils.gruntDirective(nb, DirectiveConstants.BOUNDS,
                                          DirectiveConstants.STRING_TYPE);

        final List block = verilogBlock(
            null, ns, cell, chooser,
            new DefaultBlockValue(this, ns,
                new UnaryFunction() {
                    public Object execute(final Object o) {
                        return "wire";
                    }
                }));
        return block == null ? null : factory.compilationUnit(toArray(block));
    }

    private void getModulePorts(final Map modulePorts,
                                final String verilogPath) {
        final File parent = new File(verilogPath);
        for (Iterator i = depends.values().iterator(); i.hasNext(); ) {
            final String[] files = (String[]) i.next();
            for (int j = 0; j < files.length; ++j) {
                FileInputStream fin = null;
                try {
                    fin = new FileInputStream(new File(parent, files[j]));
                    PortDeclarationTreeParser.getPortDeclarations(
                            fin, modulePorts);
                } catch (Exception e) {
                } finally {
                    if (fin != null) {
                        try {
                            fin.close();
                        } catch (IOException e) { }
                    }
                }
            }
        }
    }

    private static class MappingVisitor extends TrivialVisitor {
        private final Map result;
        private final Map modulePorts;
        private final Map bounds;
        private String currentPort;
        private int currentElement;
        private Map currentPorts;
        private List currentBounds;
        private boolean namedPort, moduleName;
        private PortDeclarationTreeParser.Range currentRange;

        public MappingVisitor(final Map result, final Map modulePorts,
                              final Map bounds) {
            this.result = result;
            this.modulePorts = modulePorts;
            this.bounds = bounds;
            this.currentElement = Integer.MIN_VALUE;
            this.currentBounds = null;
        }

        public void namedPort(final VerilogObject portName,
                              final VerilogObject port) {
            if (portName != null && port != null) {
                namedPort = true;
                currentPort = null;
                portName.accept(this);
                port.accept(this);
                namedPort = false;
            }
        }

        public void concatOp(final VerilogObject[] elements) {
            final int length;
            final int delta;
            if (currentBounds != null) {
                final int lbound = ((Integer) currentBounds.get(0)).intValue();
                final int rbound = ((Integer) currentBounds.get(1)).intValue();
                currentElement  = lbound;
                length = Math.abs(currentElement - rbound) + 1;
                delta = currentElement > rbound ? -1 : 1;
                if (length != elements.length)
                    System.err.println("WARNING: Directives declares " +
                                       currentPort + currentRange +
                                       ", but it is instantiated with " +
                                       length + " elements in CAST.");
            } else if (currentRange != null) {
                currentElement = currentRange.getLeft();
                length = Math.abs(currentElement - currentRange.getRight()) + 1;
                delta = currentElement > currentRange.getRight() ? -1 : 1;
                if (length != elements.length)
                    System.err.println("WARNING: Verilog declares " +
                                       currentPort + currentRange +
                                       ", but it is instantiated with " +
                                       length + " elements in CAST.");
            } else {
                currentElement = elements.length - 1;
                length = elements.length;
                delta = -1;
            }
            for (int i = 0; i < length; ++i, currentElement += delta) {
                elements[i].accept(this);
            }
            currentElement = Integer.MIN_VALUE;
        }

        public void ident(final String ident, final boolean escape) {
            if (namedPort) {
                if (currentPort == null) {
                    currentPort = ident;
                    currentRange = currentPorts == null ? null :
                        (PortDeclarationTreeParser.Range)
                            currentPorts.get(ident);
                    currentBounds = (List) bounds.get(ident);
                } else {
                    String lhs = currentPort;
                    if (currentElement != Integer.MIN_VALUE) {
                        lhs = lhs + "[" + currentElement + "]";
                    }
                    result.put(lhs, ident);
                }
            }
            if (moduleName) {
                currentPorts = (Map) modulePorts.get(ident);
            }
        }

        public void moduleInst(final VerilogObject ident,
                               final VerilogObject module,
                               final VerilogObject[] parameters,
                               final VerilogObject[] ports) {
            moduleName = true;
            module.accept(this);
            moduleName = false;
            for (int i = 0; i < ports.length; ++i) {
                ports[i].accept(this);
            }
            currentPorts = null;
        }

        public void compilationUnit(final VerilogObject[] objects) {
            for (int i = 0; i < objects.length; ++i) {
                objects[i].accept(this);
            }
        }
    }

    private static void usage( String m ) {
        System.err.println(
            "Usage: generate_port_mapping --cast-path=<CAST path>\n" +
            "                             [--cast-version=1|2]\n" +
            "                             --cell=<cell>\n" +
            "                             --verilog-block=<name>\n" +
            "                             [--verilog-path=<path>]");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    public static Map generateMapping (
        final CellInterface cell,
        final String cellName,
        final String verilogBlock,
        final String verilogPath) throws Exception {
        return generateMapping(cell, cellName, verilogBlock, verilogPath,
                               new Cadencize(false));
    }

    public static Map generateMapping (
        final CellInterface cell,
        final String cellName,
        final String verilogBlock,
        final String verilogPath,
        final Cadencize cad) throws Exception {

        if (cellName == null || verilogBlock == null) {
            return null;
        }

        final VerilogFactoryInterface factory = new VerilogFactoryImpl();
        final GeneratePortMapping mapper =
            new GeneratePortMapping(factory, cell,
                                    new Prs2Verilog.SimpleChooser(verilogBlock),
                                    false, cad);
        final VerilogObject result =
            mapper.convert(null, null, false, false);

        if (result == null) {
            System.err.println("Verilog block " + verilogBlock +
                               " not found in " + cellName);
            return null;
        }

        
        final Map modulePorts = new HashMap();
        if (verilogPath != null)
            mapper.getModulePorts(modulePorts, verilogPath);
        final Map map = new LinkedHashMap();
        generateMapping(map, modulePorts, mapper.getBounds(), result);
        return map;
    }

    static void generateMapping(final Map map,
                                final Map modulePorts,
                                final Map boundsMap,
                                final VerilogObject block) {
        final VerilogVisitor visitor =
            new MappingVisitor(map, modulePorts, boundsMap);
        block.accept(visitor);
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

        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        final String verilogBlock = theArgs.getArgValue("verilog-block", null);
        final String verilogPath = theArgs.getArgValue("verilog-path", null);
        if (cellName == null || verilogBlock == null) {
            usage( null );
        }

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }
        final SearchPath castPath = new FileSearchPath(castRoot);
        final CastFileParser cfp = new CastFileParser(castPath, castVersion);
        final CellInterface cell = cfp.getFullyQualifiedCell(cellName);


        Map map = null;
        try {
            map = generateMapping(cell, cellName, verilogBlock, verilogPath);
        } catch (Exception e) {}

        if (map != null) {
            for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                System.out.println(entry.getKey() + " " + entry.getValue());
            }
        }
    }
}
