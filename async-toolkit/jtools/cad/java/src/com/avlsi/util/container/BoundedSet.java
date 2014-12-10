package com.avlsi.util.container;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;

/**
 * A sorted set that keeps smallest elements up to a user specified capacity.
 **/
public class BoundedSet<E> implements Set<E> {
    /**
     * The maximum number of elements allowed.
     **/
    private final int bound;

    /**
     * The actual set.
     **/
    private final SortedSet<E> set;

    public BoundedSet(final int bound, final SortedSet<E> set) {
        if (bound < 1) throw new IllegalArgumentException("bound must be >= 1");
        this.bound = bound;
        this.set = set;
    }

    public Iterator<E> iterator() {
        return set.iterator();
    }

    public int size() {
        return set.size();
    }

    public boolean isEmpty() {
        return set.isEmpty();
    }

    public boolean contains(Object o) {
        return set.contains(o);
    }

    public Object[] toArray() {
        return set.toArray();
    }

    public <E> E[] toArray(E[] a) {
        return set.toArray(a);
    }

    public boolean add(E e) {
        if (set.size() == bound) {
            final E last = set.last();
            final Comparator<? super E> comp = set.comparator();
            // e is bigger than the tail element; don't add it
            final int c =
                comp == null ? ((Comparable<? super E>) last).compareTo(e)
                             : comp.compare(last, e);
            if (c < 0) {
                return false;
            } else {
                if (set.add(e)) {
                    set.remove(last);
                    return true;
                } else {
                    return false;
                }
            }
        } else {
            assert set.size() < bound;
            return set.add(e);
        }
    }

    public boolean remove(Object o) {
        return set.remove(o);
    }

    public boolean containsAll(Collection<?> c) {
        return set.containsAll(c);
    }

    public boolean addAll(Collection<? extends E> c) {
        boolean changed = false;
        for (E e : c) {
            if (add(e)) changed = true;
        }
        return changed;
    }

    public boolean removeAll(Collection<?> c) {
        return set.removeAll(c);
    }

    public boolean retainAll(Collection<?> c) {
        return set.retainAll(c);
    }

    public void clear() {
        set.clear();
    }

    public boolean equals(Object o) {
        if (o instanceof BoundedSet) {
            final BoundedSet other = (BoundedSet) o;
            return other.bound == this.bound && other.set.equals(this.set);
        } else {
            return false;
        }
    }

    public int hashCode() {
        return set.hashCode();
    }

    public String toString() {
        return set.toString();
    }

    // a simple unit test
    public static void main(final String[] args) {
        final int iterations = Integer.parseInt(args[0]);
        final java.util.Random rand =
            new java.util.Random(Integer.parseInt(args[1]));
        int passes = 0;
        for (int i = 0; i < iterations; ++i) {
            final int capacity = rand.nextInt(100) + 1;
            final int count = rand.nextInt(capacity * 2 + 100);
            final BoundedSet<Integer> test =
                new BoundedSet<Integer>(
                        capacity, new java.util.TreeSet<Integer>());
            final SortedSet<Integer> model = new java.util.TreeSet<Integer>();
            for (int j = 0; j < count; ++j) {
                final int element = rand.nextInt();
                test.add(element);
                model.add(element);
            }
            boolean pass = true;
            final int testSize = test.size();
            final int modelSize = model.size();
            if (pass) {
                if (modelSize <= capacity && modelSize != testSize ||
                    modelSize > capacity && testSize != capacity) {
                    pass = false;
                    System.out.println("FAIL: test size " + testSize +
                                       " model size " + modelSize +
                                       " capacity " + capacity);
                }
            }

            if (pass) {
                Iterator<Integer> testIt = test.iterator();
                Iterator<Integer> modelIt = model.iterator();
                for (int j = 0; j < testSize; ++j) {
                    if (!testIt.next().equals(modelIt.next())) {
                        pass = false;
                        System.out.println("FAIL: element mismatch at " + j);
                        break;
                    }
                }
            }
            if (pass) ++passes;
            System.out.println("Iteration " + i + " capacity=" + capacity +
                               " count=" + count + ": " +
                               (pass ? "PASS" : "FAIL"));
        }

        System.out.printf("%d out of %d tests passed\n", passes, iterations);
    }
}
