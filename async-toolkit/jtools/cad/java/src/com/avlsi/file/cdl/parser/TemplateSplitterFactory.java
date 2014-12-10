/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;

import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

import com.avlsi.util.container.Counter;
import com.avlsi.util.container.HashCounter;
import com.avlsi.util.container.Pair;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.common.HierName;


/**
 * Factory that splits templates of differently parameterized instantiations 
 * into new templates, one for each parameterization.  So for
 * .SUBCKT subcell
 * .PARAM foo=default
 * M0 bla  ... w=foo
 * .ENDS
 * .SUBCKT cell
 * X1 / subcell w=bar
 * X2 / subcell w=baz
 * .ENDS
 * originally we have 2 templates, one for cell and one for subcell.
 * after running through this puppy, we have the same templtes for cell,
 * but now there are 2 templates from the subcell .SUBCKT:
 * subcell__w_E_bar and subcell__w_E_baz that have the environment from
 * the parameteriztion built in.
 *   The first template will always have the original name, in case
 * something else wants to use it(AspiceCellAdaptor in CDL2Cast).
 **/

public class TemplateSplitterFactory implements CDLFactoryInterface {

    Map templateMap = new HashMap();
    Map envMap = new HashMap();

    private Template target;
    public TemplateSplitterFactory(Template template) {
        this.target = template;
    }
    
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        target.makeResistor(name, n1, n2, val, parameters, env);
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        target.makeCapacitor(name, npos, nneg, val, parameters, env);
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        target.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters, env);
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        target.makeDiode(name,type,npos,nneg,val,parameters,env);
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        target.makeInductor(name,npos,nneg,val,parameters,env);
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne,
                            CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        target.makeBipolar(name,type,nc,nb,ne,val,parameters,env);
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        // Here is a key assumption...
        // that we can get a template if we're making a call
        Template template = (Template)templateMap.get(subName);

        final int count = newCellCounter.getCount(subName);
        if(count == 0) template.removeTemplate(subName);
        final String newName = 
            getParameterizedCellName(subName,parameters,env);
        if(!target.containsTemplate(newName)) {
            template.execute(new Template(target.getTemplates()),
                             parameters,
                             (Environment)envMap.get(subName),
                             newName);
        }
        target.makeCall(name,newName,args,parameters,env);        
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        target.beginSubcircuit(subName,in,out,parameters,env);
    }

    public void endSubcircuit(String subName, Environment env) {
        target.endSubcircuit(subName,env);
        // add the default template to the map
        // and it's environment so parameterized templates
        // can use it
        envMap.put(subName, env);
        Template newTemplate = target.getTemplate(subName);
        templateMap.put(subName, newTemplate );
    }

    private final Map newCellNames = new HashMap();
    private final Counter newCellCounter = new HashCounter();

    /**
     * Get a new name for a parameterized template
     * For the first parameterization we call this with,
     * we'll just get bck the original name.
     **/
    private String getParameterizedCellName(String subName,
                                            Map parameters,
                                            Environment env) {
        if(parameters.isEmpty()) return subName;
        final int count = newCellCounter.getCount(subName);
        final String newCellName =
            (count == 0) ? subName : subName + count;
        final Pair key = new Pair(subName,parameters.toString());
        if(newCellNames.containsKey(key)) {
            return (String) newCellNames.get(key);
        }
        else {
            newCellNames.put(key,newCellName);
            newCellCounter.add(subName);
            return newCellName;
        }
    }
}
