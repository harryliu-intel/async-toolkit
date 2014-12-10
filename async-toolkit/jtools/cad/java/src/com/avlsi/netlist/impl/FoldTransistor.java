/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.VisitorImpl;

public class FoldTransistor implements AbstractNetlist {
    private final AbstractNetlist netlist;
    private final double cutoff, minsize;

    public FoldTransistor(final AbstractNetlist netlist, final double cutoff,
                          final double minsize) {
        this.netlist = netlist;
        this.cutoff = cutoff;
        this.minsize = minsize;
    }

/*
    public static void main(String file) {
        FileReader cdlFile = new FileReader(file);
        final CDLLexer lexer = new CDLLexer(cdlFile);
        final CDLParser parser = new CDLParser(lexer);
        parser.setASTNodeClass(CDLParser.ASTWithToken.class.getName());
        try {
            parser.goal();
        } catch (TokenStreamException e) {
            throw new SemanticException("Error parsing netlist block" + e);
        }
        final AST ast = parser.getAST();
        final CDLWalker walker = new CDLWalker();
        final NetlistBlockFactory factory = new NetlistBlockFactory();
        walker.goal(ast, env, factory);
    }
*/

    private class FoldVisitor extends VisitorImpl {
        private final List more;
        public FoldVisitor(final List more) {
            this.more = more;
        }
        private AbstractDevice transistor(final HierName name,
                                          final AbstractNode drain,
                                          final AbstractNode gate,
                                          final AbstractNode source,
                                          final AbstractNode bulk,
                                          final double length,
                                          final double width,
                                          final String type,
                                          final Map parameters) {
            return new AbstractDevice() {
                public void accept(final Visitor visitor) {
                    visitor.genericTransistor(name, drain, gate, source, bulk,
                                              length, width, type, parameters);
                }
            };
        }
        public void genericTransistor(final HierName name,
                                      final AbstractNode drain,
                                      final AbstractNode gate,
                                      final AbstractNode source,
                                      final AbstractNode bulk,
                                      final double length,
                                      final double width,
                                      final String type,
                                      final Map parameters) {
            if (width <= 0) {
                System.err.println("Zero width transistor: " + name + " w=" + width + " l=" + length);
                return;
            }
            String n = name.getCadenceString();
            double w = width;
            int k = 0;
            while (w > cutoff) {
                final HierName newName =
                    HierName.makeHierName(n + "[" + k + "]");
                double size = cutoff;
                if (w < cutoff + minsize) {
                    size = w / 2;
                }
                w -= size;
                more.add(transistor(newName, drain, gate, source, bulk,
                                    length, size, type, parameters));
                k++;
            }

            if (w > 0) {
                if (w < minsize && k > 0) {
                    System.err.println("Cannot meet maximum and minimum cutoff size requirements: w=" + width + " max=" + cutoff + " min=" + minsize);
                }
                final HierName newName = HierName.makeHierName(n + "[" + k + "]");
                more.add(transistor(newName, drain, gate, source, bulk,
                                    length, w, type, parameters));
            }
        }
    }

    public AbstractNodeIterator getInputNodes() {
        return netlist.getInputNodes();
    }
    public AbstractNodeIterator getOutputNodes() {
        return netlist.getOutputNodes();
    }
    public HierName getName() {
        return netlist.getName();
    }
    public AbstractNodeIterator getNodes() {
        return netlist.getNodes();
    }
    public AbstractDeviceIterator getDevices() {
        final AbstractDeviceIterator devices = netlist.getDevices();
        return new AbstractDeviceIterator() {
            ArrayList more = new ArrayList();
            AbstractDevice nextDevice = null;
            public boolean hasNext() {
                if (!more.isEmpty()) {
                    return true;
                } else if (devices.hasNext()) {
                    nextDevice = devices.next();
                    more.clear();
                    nextDevice.accept(new FoldVisitor(more));
                    if (!more.isEmpty()) nextDevice = null;
                    return true;
                } else {
                    return false;
                }
            }
            public AbstractDevice next() {
                if (!more.isEmpty()) {
                    return (AbstractDevice) more.remove(0);
                } else if (nextDevice != null) {
                    final AbstractDevice tmp = nextDevice;
                    nextDevice = null;
                    return tmp;
                } else {
                    throw new NoSuchElementException();
                }
            }
        };
    }
    public AbstractNetlistIterator getSubcircuits() {
        return netlist.getSubcircuits();
    }
}
