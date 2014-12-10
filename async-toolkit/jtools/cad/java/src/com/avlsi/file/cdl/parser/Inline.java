/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.Reader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.cdl.CDLFileFormatException;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;
import com.avlsi.util.exception.AssertionFailure;


/**
 * Inlines cells in a CDL file.
 **/
public class Inline extends CDLInterfaceSimplifier {
    private class Inliner extends CDLInterfaceSimplifier {
        /** Mapping from formal to actual parameters */
        private final Map param;
        /** What to prefix to internal nodes */
        private final HierName prefix;
        public Inliner(Map param, HierName prefix) {
            this.param = param;
            this.prefix = prefix;
        }

        /* This function depends on how HierName determines a name is
         * generated.  Currently, this is determined by if there exist a '#' in
         * the name. */
        private HierName markAsGenerated(final HierName n) {
            return n.appendString("#");
        }

        private HierName prefix(HierName name) {
            return HierName.append(prefix, name);
        }
        private HierName subst(HierName name) {
            if (param.containsKey(name)) return (HierName) param.get(name);
            else {
                if (markGenerated && name.isNumeric()) {
                    name = markAsGenerated(name);
                }
                return prefix(name);
            }
        }
        private HierName[] subst(HierName[] names) {
            HierName[] translated = new HierName[names.length];
            for (int i = 0; i < names.length; i++) {
                translated[i] = subst(names[i]);
            }
            return translated;
        }
        /* Forward calls to proxy, with appropriate name substitutions */
        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters) {
            callProxy.makeCall(prefix(name), subName, subst(args), parameters);
        }
        public void makeResistor(HierName name, HierName n1, HierName n2,
                                 double val) {
            proxy.makeResistor(prefix(name), subst(n1), subst(n2), val);
        }

        public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                                  double val) {
            proxy.makeCapacitor(prefix(name), subst(npos), subst(nneg), val);
        }

        public void makeTransistor(HierName name, int type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   double w, double l) {
            proxy.makeTransistor(prefix(name), type, subst(ns), subst(nd),
                                 subst(ng), subst(nb), w, l);
        }

        public void makeDiode(HierName name, int type, HierName npos,
                              HierName nneg, double w, double l, double a,
                              double p) {
            proxy.makeDiode(prefix(name), type, subst(npos), subst(nneg), w, l,
                            a, p);
        }
        public void makeInductor(HierName name, HierName npos, HierName nneg,
                                 double val) {
            proxy.makeInductor(prefix(name), subst(npos), subst(nneg), val);
        }
        public void makeBipolar(HierName name, int type, HierName nc,
                                HierName nb, HierName ne, double a) {
            proxy.makeBipolar(prefix(name), type, subst(nc), subst(nb),
                              subst(ne), a);
        }
        public void beginSubcircuit(String subName, String[] in, String[] out) {
            throw new AssertionFailure("Should not happen!");
        }
        public void endSubcircuit(String subName) {
            throw new AssertionFailure("Should not happen!");
        }
    }

    /**
     * An interface for getting a template from the name of the subcircuit.
     **/
    public interface Retriever {
        Template getTemplate(final String name);
    }

    /**
     * Return a template for a given name by reading the cell from CAST.
     **/
    public static class RetrieveFromCast implements Retriever {
        protected final CastFileParser cfp;
        public RetrieveFromCast(final CastFileParser cfp) {
            this.cfp = cfp;
        }
        protected CellInterface loadCell(final String subName) {
            try {
                return cfp.getFullyQualifiedCell(subName);
            } catch (Exception e) {
                throw new RuntimeException("Cannot load cell " + subName + "!");
            }
        }
        protected Template makeTemplate(final CellInterface cell) {
            final Set ports = NetlistAdapter.getParameterList(cell);
            final String[] out = new String[ports.size()];
            int j = 0;
            for (Iterator i = ports.iterator(); i.hasNext(); ++j) {
                out[j] = ((HierName) i.next()).getCadenceString();
            }
            final NetlistBlock nb =
                (NetlistBlock) cell.getBlockInterface()
                                   .iterator(BlockInterface.NETLIST)
                                   .next();
            return Template.setInOut(nb.getCDLTemplate(), new String[0], out);
        }
        public Template getTemplate(final String subName) {
            final CellInterface cell = loadCell(subName);
            return makeTemplate(cell);
        }
    }

    private final Map templates;
    private CDLSimpleInterface proxy;
    private CDLSimpleInterface callProxy;
    private final boolean markGenerated;
    private final boolean bFlatten;
    private final Retriever retriever;

    public Inline() {
        this(false);
    }

    public Inline(final boolean markGenerated) {
        this(markGenerated, null);
    }

    public Inline(final boolean markGenerated, final Retriever retriever) {
        this(markGenerated, retriever, false);
    }
    /**
     * Inline constructor.
     * @param markGenerated Use true if names that look like internally
     * generated nodes in cells to be inlined are marked as such (by appending
     * a '#') in the output.
     * @param retriever A Retriever that returns an Template when given a
     * subcircuit name.  If the function is not to be inlined, the function
     * returns null.
     **/
    public Inline(final boolean markGenerated, final Retriever retriever, boolean flatten) {
        templates = new HashMap();
        this.markGenerated = markGenerated;
        this.retriever = retriever;
        this.bFlatten = flatten;
        if(this.bFlatten)
            this.callProxy = this;
    }

    /**
     * Add specified cells found in a reader to the set of cells to be
     * flattened.
     **/
    public void addTarget(Reader r, String[] cells) {
        final Map m = Template.getTemplates(r);
        for (int i = 0; i < cells.length; i++) {
            if (m.containsKey(cells[i])) {
                templates.put(cells[i], m.get(cells[i]));
            }
        }
    }
    /**
     * Add all cells found in a reader to the set of cells to be flattened.
     **/
    public void addTarget(Reader r) {
        final Map m = Template.getTemplates(r);
        templates.putAll(m);
    }

    /**
     * Add the specified cell to be flattened.
     **/
    public void addTarget(final String cell, final Template templ) {
        templates.put(cell, templ);
    }

    /**
     * Add the specified cells to be flattened.
     **/
    public void addTargets(final Map cells) {
        templates.putAll(cells);
    }

    /**
     * Set the CDLSimpleInterface to write to.
     **/
    public void setProxy(final CDLSimpleInterface proxy) {
        this.proxy = proxy;
        if(!bFlatten)
            callProxy = proxy;
    }

    /**
     * Inline all cells specified as a target in all cells found in a reader.
     **/
    public void process(Reader r, CDLSimpleInterface proxy) throws CDLFileFormatException {
        setProxy(proxy);
        try {
            ReadCDLIntoFactory.readCDL(r, this);
        } catch (Exception e) {
            e.printStackTrace();
            throw new CDLFileFormatException(e);
        }
    }

    /**
     * Return the templates associated with a subcircuit.  If the templates
     * does not already exist in the cache of templates, try to obtain it from
     * the template retriever.
     * @param subName Name of the template to get.
     **/
    private Template getTemplate(final String subName) {
        if (!templates.containsKey(subName) && retriever != null) {
            final Template t = retriever.getTemplate(subName);
            if (t != null) templates.put(subName, t);
        }
        return (Template) templates.get(subName);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters) {
        final Template t = getTemplate(subName);
        if (t == null) {
            callProxy.makeCall(name, subName, args, parameters);
        } else {
            final Pair p = t.getArguments();
            final String[] in = (String []) p.getFirst();
            final String[] out = (String []) p.getSecond();
            if (in.length + out.length != args.length) {
                System.err.println("Mismatched parameters in call " + name + " to " + subName + " expecting " + in.length + " input parameters and " + out.length + " output parameters, found " + args.length + ".");
                return;
            }
            final Map m = new HashMap();
            int index = 0;
            try {
                for (int i = 0; i < in.length; i++, index++) {
                    m.put(HierName.makeHierName(in[i], '.'), args[index]);
                }
                for (int i = 0; i < out.length; i++, index++) {
                    m.put(HierName.makeHierName(out[i], '.'), args[index]);
                }
            } catch (InvalidHierNameException e) {
                System.err.println("InvalidHierNameException cannot happen!");
            }
            final Inliner inliner = new Inliner(m, name);
            t.execute(parameters, inliner);
        }
    }
    /* The rest just forwards the call to proxy */
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) {
        proxy.makeResistor(name, n1, n2, val);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) {
        proxy.makeCapacitor(name, npos, nneg, val);
    }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) {
        proxy.makeTransistor(name, type, ns, nd, ng, nb, w, l);
    }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) {
        proxy.makeDiode(name, type, npos, nneg, w, l, a, p);
    }
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) {
        proxy.makeInductor(name, npos, nneg, val);
    }
    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) {
        proxy.makeBipolar(name, type, nc, nb, ne, a);
    }
    public void beginSubcircuit(String subName, String[] in, String[] out) {
        proxy.beginSubcircuit(subName, in, out);
    }
    public void endSubcircuit(String subName) {
        proxy.endSubcircuit(subName);
    }
}
