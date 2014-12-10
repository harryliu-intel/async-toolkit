/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class Organizer implements WholeOperatorSink, NodeCanonicalizer {
    /**
     * This is for everything except aliases.  Maps a NodeExpression
     * (which has been unparameterized and canonicalized) to a
     * WholeOperator.  This makes it possible to look
     * up any existing WholeOperator which is computing
     * the same result, and alias them togther.
     */
    LinkedHashMap unique = new LinkedHashMap();

    /**
     * This is for aliases.  Maps a NodeName (which has been
     * unparameterized, and which is the destination node
     * of the WholeOperator) to a WholeOperator, which must have an
     * AliasNodeExpression as its expression.  This makes it possible
     * to look up what a node has been aliased to.
     */
    LinkedHashMap aliases = new LinkedHashMap();

    /**
     * This is strictly for error checking.  We want to make sure that the
     * same node is never defined more than once.
     */
    HashSet whereNoOneHasGoneBefore = new HashSet();

    public void hereYaGo(WholeOperator wo) {
        NodeName boldly = wo.dest.unparameterize(wo.index);
        assert (!whereNoOneHasGoneBefore.contains(boldly)) :
            boldly + " redefined";
        whereNoOneHasGoneBefore.add(boldly);
        if (!(wo.expr instanceof AliasNodeExpression)) {
            NodeExpression key = wo.expr.unparameterize(wo.index)
                                        .canonicalize(this);
            if (unique.containsKey(key)) {
                WholeOperator existing = (WholeOperator) unique.get(key);
                AliasNodeExpression ane =
                    new AliasNodeExpression(existing.dest,
                                            existing.index - wo.index);
                Section sect = (wo.preferredDirection == Direction.UP ?
                                wo.upSection : wo.downSection);
                wo = new WholeOperator(wo.dest.unparameterize(wo.index),
                                       ane, wo.index,
                                       wo.preferredDirection, sect, sect);
            } else {
                unique.put(key, wo);
            }
        }
        
        if (wo.expr instanceof AliasNodeExpression) {
            aliases.put(wo.dest.unparameterize(wo.index), wo);
        }
    }

    public NodeName canonicalize(NodeName n) {
        assert !n.isParameterized();
        
        while (aliases.containsKey(n)) {
            WholeOperator wop = (WholeOperator) aliases.get(n);
            AliasNodeExpression ane =
                (AliasNodeExpression) wop.expr.unparameterize(wop.index);
            NodeName newn = ane.getNode();
            assert (n != newn) : "stuck on " + n;
            n = newn;
            assert !n.isParameterized();
        }

        return n;
    }

    private boolean simplifyingCopy(WholeOperatorSink sink) {
        /* It is important to provide the aliases first, so that the
         * destination Organizer will be able to canonicalize on them. */
        for (Iterator it = aliases.values().iterator(); it.hasNext(); )
            sink.hereYaGo((WholeOperator) it.next());

        boolean simplified = false;

        for (Iterator it = unique.entrySet().iterator(); it.hasNext(); ) {
            Map.Entry entry = (Map.Entry) it.next();
            NodeExpression key = (NodeExpression) entry.getKey();
            WholeOperator value = (WholeOperator) entry.getValue();
            Simplification simp = key.getSimplification();
            if (simp.isSimpler()) {
                value = new WholeOperator(value.dest,
                                          value.expr.applySimplification(simp),
                                          value.index,
                                          value.preferredDirection,
                                          value.upSection, value.downSection);
                simplified = true;
            }

            sink.hereYaGo(value);
        }

        return simplified;
    }

    /**
     * Fully simplify all the rules in this Organizer and copy them to
     * another WholeOperatorSink.
     */
    public void simplify(WholeOperatorSink dest) {
        Organizer src = this;

        boolean keepGoing = true;

        while (keepGoing) {
            Organizer next = new Organizer();
            
            keepGoing = src.simplifyingCopy(next);
            if (src.unique.size() != next.unique.size() ||
                src.aliases.size() != next.aliases.size())
                keepGoing = true;

            src = next;
        }

        src.simplifyingCopy(dest);
    }
}
