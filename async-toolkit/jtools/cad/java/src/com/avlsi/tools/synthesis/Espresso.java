/*
 * A multi-rail logic optimizer.  Uses given seed terms in a karnaugh
 * map, grows them maximally, including optional invariant terms, then
 * eliminates redundant terms starting with the longest terms.  Used
 * for various 1ofN synthesis tasks.  Ported from auto.c.
 *
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.util.List;
import java.util.Comparator;
import com.avlsi.util.container.MultiSet;

public class Espresso {

    /** Karnaugh cube to count term coverage **/
    short [] kcube;

    /** Rules (List of Lists of String) **/
    List rules;

    /** Exclusive sets (List of Lists of Strings) **/
    List exclusives;

    /** add to coverage in  kcube, return minimum coverage count of rule **/
    private int modifyKcubeTerm(List pr, int add, int d, int pos) {
        List pex;
        int i,j,val;
        
        // leaf case
        if (d>=exclusives.size()) {
            kcube[pos]+=add;
            return kcube[pos];
        }
        
        // get property for this d
        pex=(List) exclusives.get(d);
        
        // recurse to next dimension of kcube
        for (i=0; i<pex.size(); i++)
            for (j=0; j<pr.size(); j++)
                if (((String) pex.get(i)).equals((String) pr.get(j)))
                    return modifyKcubeTerm(pr,add,d+1,
                                           pex.size()*pos+i);
        
        // recurse via all choices of current dimension
        val=modifyKcubeTerm(pr,add,d+1,pex.size()*pos+0);
        for (i=1; i<pex.size(); i++) {
            j=modifyKcubeTerm(pr,add,d+1,pex.size()*pos+i);
            if (j<val) val=j;
        }
        return val;
    }
    
    /** compare two lists by length **/
    public static class ListLengthComparator implements Comparator {
        public int compare(Object A, Object B) {
            return ((List) A).size() - ((List) B).size();
        }
    }

    /** return optimized rules **/
    public List optimizedRules() {
        return rules;
    }

    /** shorthand constructor **/
    public Espresso(List exclusives, List rules) {
        this(exclusives,rules,null);
    }

    /**
     * Espresso optimize rules for a single output.  Exclusives is a
     * List of exclusives, and should only include exclusives relevant
     * to the particular guards and direction (i.e. downward rules
     * should only consider exclhi).  Also don't include vacuous
     * exclusives with only one element. Each exclusive is a List of
     * Strings and simply gives the node names in the exclusive set.
     * Rules is a List of rules.  Each rule is a List of Strings,
     * which represent the ordered conjunctive gates in that rule.
     * Never is similar to rules, but specifies states which are ok to
     * either cover or not cover (i.e. don't cares or input
     * invariants).  Works by expanding terms in a karnaugh map by
     * deleting guards from rules, then discarding redundant rules,
     * starting with the longest rules.  Greedy algorithm, only
     * locally optimal.  Good for touchup on rules which are already
     * reasonably good.
     **/
    public Espresso(List exclusives, List rules, List never) {
        int i,j,coverage,size=1;
        List pr,pex;
        String pg;
        
        // create rules, exclusives
        this.exclusives = exclusives;
        
        // initialize kcube
        for (i=0; i<exclusives.size(); i++)
            size *= ((List) exclusives.get(i)).size();
        kcube = new short[size];
        for (i=0; i<size; i++) kcube[i]=0;
 
        // fill in kcube with seed terms
        for (i=0; i<rules.size(); i++)
            modifyKcubeTerm((List) rules.get(i),1,0,0);
        
        // augment kcube with never terms
        if (never!=null)
            for (i=0; i<never.size(); i++)
                modifyKcubeTerm((List) never.get(i),1,0,0);
        
        // grow seed terms to maximum size
        for (i=0; i<rules.size(); i++) {
            pr=(List) rules.get(i);
            for (j=pr.size()-1; j>=0; j--) {
                
                // try eliminating one literal from guard
                pg=(String) pr.get(j);
                pr.remove(j);
                coverage=modifyKcubeTerm(pr,0,0,0);
                pr.add(j,pg);
                
                // now remove it if possible
                if (coverage>0) {
                    modifyKcubeTerm(pr,-1,0,0);
                    pr.remove(j);
                    modifyKcubeTerm(pr,1,0,0);
                }
            }
        }

        // sort rules by length
        this.rules = rules = (new MultiSet(rules,new ListLengthComparator())).list();
        
        // eliminate redundant terms, from longest terms to shortest
        for (i=rules.size()-1; i>=0; i--) {
            pr=(List) rules.get(i);
            coverage=modifyKcubeTerm(pr,0,0,0);
            if (coverage>1) {
                modifyKcubeTerm(pr,-1,0,0);
                rules.remove(i);
            }
        }
    }
}
