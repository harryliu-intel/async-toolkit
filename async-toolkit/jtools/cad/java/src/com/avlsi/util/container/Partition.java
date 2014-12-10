/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * Class representing a partition: a set of non-intersecting sets.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public class Partition {
    private final PartitionerInterface p;
    private final Set sets;

    /**
     * Constructor.
     *
     * The iterator should not have multiple copies of the same item.
     *
     * For inconsistent equivalences, (a = b, b = c, a != c), all in each
     * set will be pairwise equivalent.  NetGraph depends on this.
     **/
    public Partition(PartitionerInterface p, Iterator i) {
        this.p = p;
        this.sets = new HashSet();
out:
        while(i.hasNext()) {
            Object o = i.next();
            for (Iterator j = sets.iterator(); j.hasNext(); ) {
                Set s2 = (Set) j.next();
                boolean ins2 = true;
                for (Iterator k = s2.iterator(); k.hasNext(); ) {
                    Object o2 = k.next();
                    if (!p.equivalent(o,o2)) {
                        ins2 = false;
                        break;
                    }
                }
                if (ins2) {
                    s2.add(o);
                    continue out;
                }
            }
            // o not in same set with any current bucket.
            Set s2 = new HashSet();
            s2.add(o);
            sets.add(s2);
        }
    }

    /*
     * FinitePartitionerInterface is for when there
     * are a finite number of sets.
    Partition(FinitePartitionerInterface p, Set s)
     */

    /*
     * Constructor.
     * Takes advantage of the imposed order to partition
     * more efficiently.
    Partition(Comparator c, Set s) {
    }
     */

    /**
     * Gives an iterator through each of the subsets.
     **/
    public Iterator iterator() {
        return sets.iterator();
    }

    /**
     * Debugging string.  What is in this partition?
     **/
    public String toString() {
        return super.toString() + ":" + sets.toString();
    }

    /**
     * This is an interface for assesing whether two objecs should belong
     * to the same set of a partition.  equivalent() should never violate
     * the constraints on an equivalence relation; a == b and b == c implies
     * a == c.  a == b and b != c implies a != c.
     **/
    public interface PartitionerInterface {
        boolean equivalent(Object o1, Object o2);
    }

    /**
     * This defines a PartitionerInterface that results in the trivial
     * "everything in one set" partition.
     **/
    public final class IdentityPartioner implements PartitionerInterface {
        public boolean equivalent(Object o1, Object o2) { return true; }
    }

    /**
     * This defines a PartitionerInterface that results in the trivial
     * "each item in its own set" partition.
     **/
    public final class SeperatePartitioner implements PartitionerInterface {
        public boolean equivalent(Object o1, Object o2) { return false; }
    }

    /**
     * This defines a PartitionerInterface that partitions according to
     * equivalences defined by a Comparator.
     **/
    public final class ComparatorPartioner implements PartitionerInterface {
        private final Comparator c;
        public ComparatorPartioner(Comparator c) {
            this.c = c;
        }
        public boolean equivalent(Object o1, Object o2) { return c.compare(o1, o2) == 0; }
    }

    /**
     * This defines a PartitionerInterface that partitions according to
     * equivalences defined by the equals() method of the objects.
     **/
    public final class EqualityPartitioner implements PartitionerInterface {
        public boolean equivalent(Object o1, Object o2) { return o1.equals(o2); }
    }
}
