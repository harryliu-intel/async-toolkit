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
import java.util.HashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.UnaryFunction;

/** Convert to a Verilog netlist, and generate list of cells that need to
  * be represented in CDL. **/
class LVSConverter extends AbstractConverter {
    private final CellType cell;
    private final ConnectionInfo ports;
    private final Prs2Verilog.VerilogChooser chooser;
    private Set<String> cdlDeps;

    public LVSConverter(final CellType cell,
                        final VerilogFactoryInterface factory,
                        final ConnectionInfo ports,
                        final boolean alwaysEscape,
                        final Prs2Verilog.VerilogChooser chooser) {
        super(factory, alwaysEscape);
        this.cell = cell;
        this.ports = ports;
        this.chooser = chooser;
        this.cdlDeps = new HashSet<String>();
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
            final Iterator<String> suffix = 
                Stream.concat(Stream.of(""),
                              IntStream.rangeClosed(0, Integer.MAX_VALUE)
                                       .mapToObj(x -> Integer.toString(x)))
                      .iterator();

            final List subblock = verilogBlock(
                ci.nameInParent,
                cell.namespace,
                ci.child.cast_cell,
                chooser,
                new DefaultBlockValue(this, cell.namespace,
                    new UnaryFunction() {
                      public Object execute(final Object o) {
                          return "wire";
                      }
                    }),
                vinst -> ci.nameInParent + suffix.next());
            if (subblock != null) {
                items.addAll(subblock);
                continue;
            }

            final ArrayList args = new ArrayList();

            if (ci.child.cast_cell.containsNetlist()) {
                cdlDeps.add(ci.child.typeName);
            }

            actual(args, ci, "wire", byName);

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

    public VerilogObject convert(final CommandLineArgs theArgs,
                                 VerilogObject moduleName,
                                 final boolean toplevel,
                                 final boolean topEnv) {
        if (cell.cast_cell.containsNetlist()) {
            return null;
        }

        final boolean routed = theArgs.argExists("routed");

        final ArrayList params = new ArrayList();

        formal(params, ports, "wire");

        if (toplevel || !routed || !CellUtils.isRouted(cell.cast_cell)) {
            processSubcells(true);
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

    public Map<CellInterface,String[]> getDependencies() {
        final Map<CellInterface,String[]> result = super.getDependencies();
        if (!cdlDeps.isEmpty()) {
            final List<String> augmented = new ArrayList<String>();
            final String[] dep = result.get(cell.cast_cell);
            if (dep != null) CollectionUtils.addAll(augmented, dep);
            for (String cdlDep : cdlDeps) {
                augmented.add("cdl " + cdlDep);
            }
            result.put(cell.cast_cell, augmented.toArray(new String[0]));
        }
        return result;
    }
}
