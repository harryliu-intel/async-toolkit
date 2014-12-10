/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.lvs;

import java.util.List;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;

import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.bool.*;
import com.avlsi.prs.ProductionRule;
import com.avlsi.file.common.HierName;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;

/** Utilities for handling disjunctive normal form representation of an expression. */
public class Dnf {
    /**
     * This class should not be instantiated.
     **/
    private Dnf() { }
   
    /** Compute netUp, netDn, holdUp, holdDn DNF's from NetNode's paths. */
    public static void fromNetNode(NetGraph.NetNode node,
                                   MultiSet netUp,  MultiSet netDn,
                                   MultiSet holdUp, MultiSet holdDn) {
        Iterator t = node.paths.iterator();
        while (t.hasNext()) {
            NetGraph.NetPath path = (NetGraph.NetPath) t.next();
            MultiSet guard = path.getUniqueGates();
            int dir = path.getDir();
            if (dir==+1) {
                if (!path.feedback) netUp.addIfUnique(guard);
                else holdUp.addIfUnique(guard);
            }
            else if (dir==-1) {
                if (!path.feedback) netDn.addIfUnique(guard);
                else holdDn.addIfUnique(guard);
            }
        }
    }

    /** Make a DNF from guard and dir, return null if non-inverse-monotonic */
    public static MultiSet fromExpression(BooleanExpressionInterface guard,
                                          int dir, 
                                          final AliasedSet namespace,
                                          final ExclusiveNodeSets exclusives) {
        MultiSet dnf = new MultiSet();
        Collection disjuncts = guard.DNFForm().getDisjuncts();
        for (Iterator i = disjuncts.iterator(); i.hasNext(); ) {
            MultiSet term = new MultiSet();
            Collection conjuncts = 
                ((AndBooleanExpressionInterface) i.next()).getConjuncts();
            for (Iterator j = conjuncts.iterator(); j.hasNext(); ) {
                HierNameAtomicBooleanExpression literal =
                    (HierNameAtomicBooleanExpression) j.next();
                if ((!literal.getSense() && (dir == ProductionRule.DOWN)) ||
                    ( literal.getSense() && (dir == ProductionRule.UP))) return null;
                term.add((HierName) namespace.getCanonicalKey(literal.getName()));
            }
            dnf.add(term);
        }
        return getCanonicalForm(dnf,dir,exclusives);
    }

    /** 
     * Get a canonical representation of a DNF.  Removes redundant
     * gates in terms.  Removes terms which have other terms as a
     * subset.  Removes exluded terms.
     */
    public static MultiSet getCanonicalForm(MultiSet b, final int dir,
                                            final ExclusiveNodeSets exclusives) {
        MultiSet c = new MultiSet();
        for (Iterator i = b.iterator(); i.hasNext(); ) {
            MultiSet bterm1 = (MultiSet) i.next();
            
            // search for any subsets of bterm1
            boolean ok = true;
            for (Iterator j = b.iterator(); j.hasNext(); ) {
                MultiSet bterm2 = (MultiSet) j.next();
                if (bterm2.isSubset(bterm1) && !bterm1.isSubset(bterm2)) {
                    ok=false; break;
                }
            }
            if (!ok) continue;

            // create cterm by deleting redundant gates of bterm1
            MultiSet cterm = new MultiSet();
            cterm.addAllIfUnique(bterm1); // avoid redundant terms
            
            // add cterm if not excluded
            if (((dir == ProductionRule.UP) && 
                 (!exclusives.areExclusive(ExclusiveNodeSet.LO,cterm))) ||
                ((dir == ProductionRule.DOWN) && 
                 (!exclusives.areExclusive(ExclusiveNodeSet.HI,cterm)))) 
                c.addIfUnique(cterm);
        }
        return c;
    }

    /**
     * Removes terms in a already covered by terms in b.  Used to
     * strip out unnecessary combinational staticizer transistors.
     **/
    public static MultiSet eliminateRedundantTerms(MultiSet a, MultiSet b) {
        MultiSet c = new MultiSet();
        for (Iterator i = a.iterator(); i.hasNext(); ) {
            MultiSet aterm = (MultiSet) i.next();

            // search for any subsets of aterm in b
            boolean ok = true;
            for (Iterator j = b.iterator(); j.hasNext(); ) {
                MultiSet bterm = (MultiSet) j.next();
                if (bterm.isSubset(aterm)) {
                    ok=false; break;
                }
            }
            if (!ok) continue;

            // create cterm and add it to c
            MultiSet cterm = new MultiSet();
            cterm.addAll(aterm);
            c.add(cterm);
        }
        return c;
    }
    
    /**
     * Make a DNF from guard.  This differs from
     * fromExpression(BooleanExpressionInterface,  int, AliasedSet,
     * ExclusiveNodeSets) in that it does not care about inverse monoticity.
     */
    public static MultiSet fromExpression(
            final BooleanExpressionInterface guard, final AliasedSet namespace,
            final ExclusiveNodeSets exclusives) {
        MultiSet dnf = new MultiSet();
        Collection disjuncts = guard.DNFForm().getDisjuncts();
        for (Iterator i = disjuncts.iterator(); i.hasNext(); ) {
            MultiSet term = new MultiSet();
            Collection conjuncts = 
                ((AndBooleanExpressionInterface) i.next()).getConjuncts();
            for (Iterator j = conjuncts.iterator(); j.hasNext(); ) {
                HierNameAtomicBooleanExpression literal =
                    (HierNameAtomicBooleanExpression) j.next();
                HierName canon =
                    (HierName) namespace.getCanonicalKey(literal.getName());
                term.add(new HierNameAtomicBooleanExpression(
                            literal.getSense(), canon));
            }
            dnf.add(term);
        }
        return getCanonicalForm(dnf,exclusives);
    }

    /**
     * Get a canonical representation of a DNF.  This is the same as
     * getCanonicalForm(MultiSet, int, ExclusiveNodeSet), except when removing
     * excluded terms, it doesn't assume inverse monoticity.
     **/
    public static MultiSet getCanonicalForm(
            final MultiSet b, final ExclusiveNodeSets exclusives) {
        MultiSet c = new MultiSet();
        for (Iterator i = b.iterator(); i.hasNext(); ) {
            MultiSet bterm1 = (MultiSet) i.next();
            
            // search for any subsets of bterm1
            boolean ok = true;
            for (Iterator j = b.iterator(); j.hasNext(); ) {
                MultiSet bterm2 = (MultiSet) j.next();
                if (bterm2.isSubset(bterm1) && !bterm1.isSubset(bterm2)) {
                    ok=false; break;
                }
            }
            if (!ok) continue;

            // create cterm by deleting redundant gates of bterm1
            MultiSet cterm = new MultiSet();
            cterm.addAllIfUnique(bterm1); // avoid redundant terms
            
            // partition literals in the cterm
            Set<HierName> loTerms = new HashSet<HierName>();
            Set<HierName> hiTerms = new HashSet<HierName>();
            for (Iterator j = cterm.iterator(); j.hasNext(); ) {
                HierNameAtomicBooleanExpression literal =
                    (HierNameAtomicBooleanExpression) j.next();
                if (literal.getSense()) hiTerms.add(literal.getName());
                else loTerms.add(literal.getName());
            }

            // add cterm if not excluded and no literal appears with both
            // non-negated and negated senses
            if (!exclusives.areExclusive(ExclusiveNodeSet.LO,loTerms) &&
                !exclusives.areExclusive(ExclusiveNodeSet.HI,hiTerms) &&
                Collections.disjoint(loTerms,hiTerms)) {
                c.addIfUnique(cterm);
            }
        }
        return c;
    }

    /** Create a DNF with one literal. */
    public static MultiSet createSingleGateDnf(HierName a) {
        MultiSet c = new MultiSet();
        MultiSet term = new MultiSet();
        term.add(a);
        c.add(term);
        return c;
    }

    /** Add a conjunctive literal to a DNF. */
    public static MultiSet addConjunctGate(MultiSet a, HierName b) {
        MultiSet c = new MultiSet();
        Iterator t = a.iterator();
        while (t.hasNext()) {
            MultiSet term = new MultiSet((MultiSet) t.next());
            term.add(b);
            c.addIfUnique(term);
        }
        return c;
    }

    /** 
     * Return the dual of a DNF.
     * May still have superset or exclusive terms,
     * use getCanonicalForm to strip these out.
     */
    public static MultiSet dual(MultiSet a) {
        // count depth and number of conjunctions
        int d = a.size(), n = 1;
        for (int i=0; i<d; i++) n *= ((MultiSet) a.get(i)).size();

        // generate dual dnf like doing a base conversion
        MultiSet b = new MultiSet();
        for (int i=0; i<n; i++) {
            MultiSet bterm = new MultiSet();
            int x = i;
            for (int j=0; j<d; j++) {
                MultiSet aterm = (MultiSet) a.get(j);
                bterm.addIfUnique(aterm.get(x%aterm.size()));
                x /= aterm.size();
            }
            b.addIfUnique(bterm);
        }
        return b;
    }

    /** Return a BooleanExpressionInterface from a Dnf. */
    public static BooleanExpressionInterface
        getBooleanExpression(MultiSet a, boolean nonNegatedLiterals, Map inverter_map) {
        ArrayList terms = new ArrayList();
        for (Iterator i = a.iterator(); i.hasNext(); ) {
            MultiSet aterm = (MultiSet) i.next();
            ArrayList literals = new ArrayList();
            for (Iterator j = aterm.iterator(); j.hasNext(); ) {
                HierName name = (HierName) j.next();
                boolean sense = nonNegatedLiterals;
                while (inverter_map!=null && inverter_map.get(name)!=null) {
                    // Optionally substitute a guard node that is
                    // driven by an inverter chain with the root of
                    // the chain, in the appropriate sense.  This
                    // avoids interference when simulating some
                    // standard cell with pass-gates.
                    name = (HierName) inverter_map.get(name);
                    sense = !sense;
                }
                HierNameAtomicBooleanExpression literal =
                    new HierNameAtomicBooleanExpression(sense,name);
                literals.add(literal);
            }
            AndBooleanExpression term = new
                AndBooleanExpression(true,literals);
            terms.add(term);
        }
        OrBooleanExpression guard = new
            OrBooleanExpression(true,terms);
        return guard;
    }
}
