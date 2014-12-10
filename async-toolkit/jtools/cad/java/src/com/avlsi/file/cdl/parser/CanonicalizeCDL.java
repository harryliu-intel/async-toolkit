package com.avlsi.file.cdl.parser;

import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedSet;

/**
 * Canonicalizes nodes in a CDL according to an AliasedSet.  Any nodes not
 * found in the AliasedSet are left unchanged.  This class works as a filter.
 **/
public class CanonicalizeCDL implements CDLFactoryInterface {
    /** Which CDLFactoryInterface to write output to */
    private final CDLFactoryInterface out;

    /** AliasedSet containing information on which nodes are aliases */
    private final AliasedSet alias;

    public CanonicalizeCDL(final CDLFactoryInterface out,
                           final AliasedSet alias) {
        this.out = out;
        this.alias = alias;
    }

    private HierName canon(final HierName name) {
        final Object result = (HierName) alias.getCanonicalKey(name);
        return result == null ? name : (HierName) result;
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        out.makeResistor(name, canon(n1), canon(n2), val, parameters, env);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        out.makeCapacitor(name, canon(npos), canon(nneg), val, parameters, env);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        out.makeTransistor(name, type, canon(ns), canon(nd), canon(ng),
                           canon(nb), w, l, parameters, env);
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        out.makeDiode(name, type, canon(npos), canon(nneg), val, parameters,env);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        out.makeInductor(name, canon(npos), canon(nneg), val, parameters, env);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne,
                            CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        out.makeBipolar(name, type, canon(nc), canon(nb), canon(ne), val,
                        parameters,env);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        HierName[] canonArgs = new HierName[args.length];
        for (int i = 0; i < args.length; i++) {
            canonArgs[i] = canon(args[i]);
        }
        out.makeCall(name, subName, canonArgs, parameters, env);
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        this.out.beginSubcircuit(subName, in, out, parameters, env);
    }

    public void endSubcircuit(String subName, Environment env) {
        out.endSubcircuit(subName, env);
    }
}
