/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
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

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.cdl.CDLFileFormatException;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;


/**
 * Inlines cells in a CDL file.
 **/
public class CDLInlineFactory implements CDLFactoryInterface {
    private class Inliner implements CDLFactoryInterface {
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

        public void makeResistor(HierName name, HierName n1, HierName n2,
                                 CDLLexer.InfoToken val, Map parameters,
                                 Environment env) {
            proxy.makeResistor(prefix(name), subst(n1), subst(n2), val, parameters, env);
        }

        public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                                  CDLLexer.InfoToken val, Map parameters,
                                  Environment env) {
            proxy.makeCapacitor(prefix(name), subst(npos), subst(nneg), val, parameters, env);
        }

        public void makeTransistor(HierName name, String type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                                   Map parameters, Environment env) {
            proxy.makeTransistor(prefix(name), type, subst(ns), subst(nd),
                                 subst(ng), subst(nb), w, l, parameters, env);
        }
        
        public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val,
                              Map parameters, Environment env) {
            proxy.makeDiode(prefix(name), type, subst(npos), subst(nneg), val, parameters, env);
        }

        public void makeInductor(HierName name, HierName npos, HierName nneg,
                                 CDLLexer.InfoToken val, Map parameters,
                                 Environment env) {
            proxy.makeInductor(prefix(name), subst(npos), subst(nneg), val, parameters, env);
        }

        public void makeBipolar(HierName name, String type, HierName nc,
                                HierName nb, HierName ne,
                                CDLLexer.InfoToken val,
                                Map parameters, Environment env) {
            proxy.makeBipolar(prefix(name), type, subst(nc), subst(nb),
                              subst(ne), val, parameters, env);
        }

        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            callProxy.makeCall(prefix(name), subName, subst(args), parameters, env);
        }

        public void beginSubcircuit(String subName, String[] in, String[] out,
                                    Map parameters, Environment env) {
            proxy.beginSubcircuit(subName, in, out, parameters, env);
        }
        
        public void endSubcircuit(String subName, Environment env) {
            proxy.endSubcircuit(subName, env);
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
        private final CastFileParser cfp;
        public RetrieveFromCast(final CastFileParser cfp) {
            this.cfp = cfp;
        }
        public Template getTemplate(final String subName) {
            final CellInterface cell;
            try {
                cell = cfp.getFullyQualifiedCell(subName);
            } catch (Exception e) {
                throw new RuntimeException("Cannot load gate " + subName + "!");
            }
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
            return Template.setInOut(nb.getCDLTemplate(), new String[0],
                                     out);
        }
    }

    private final Map templates;
    private CDLFactoryInterface proxy;
    private CDLFactoryInterface callProxy;
    private final boolean markGenerated;
    private final Retriever retriever;
    private boolean bFlatten;

    public CDLInlineFactory() {
        this(false);
    }

    public CDLInlineFactory(final boolean markGenerated) {
        this(markGenerated, null);
    }

    /**
     * CDLInlineFactory constructor.
     * @param markGenerated Use true if names that look like internally
     * generated nodes in cells to be inlined are marked as such (by appending
     * a '#') in the output.
     * @param retriever A Retriever that returns an Template when given a
     * subcircuit name.  If the function is not to be inlined, the function
     * returns null.
     **/
    public CDLInlineFactory(final boolean markGenerated, final Retriever retriever) {
        this(markGenerated, retriever, false);
    }

    public CDLInlineFactory(final boolean markGenerated, final Retriever retriever, boolean flatten) {
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
     * Set the CDLFactoryInterface to write to.
     **/
    public void setProxy(final CDLFactoryInterface proxy) {
        this.proxy = proxy;
        if(!bFlatten)
            callProxy = proxy;
    }

    /**
     * Inline all cells specified as a target in all cells found in a reader.
     **/
    public void process(Reader r, CDLFactoryInterface proxy) throws CDLFileFormatException {
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
                         Map parameters, Environment env) {
        final Template t = getTemplate(subName);
        if (t == null) {
            callProxy.makeCall(name, subName, args, parameters, env);
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
            t.execute(inliner, parameters, NullEnvironment.getInstance(), null);
        }
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        proxy.makeResistor(name, n1, n2, val, parameters, env);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        proxy.makeCapacitor(name, npos, nneg, val, parameters, env );
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        proxy.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters, env);
    }
        
    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        proxy.makeDiode(name, type, npos, nneg, val, parameters, env);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        proxy.makeInductor(name, npos, nneg, val, parameters, env);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        proxy.makeBipolar(name, type, nc, nb, ne, val, parameters, env);
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        proxy.beginSubcircuit(subName, in, out, parameters, env);
    }
    
    public void endSubcircuit(String subName, Environment env) {
        proxy.endSubcircuit(subName, env);
    }
  
}
