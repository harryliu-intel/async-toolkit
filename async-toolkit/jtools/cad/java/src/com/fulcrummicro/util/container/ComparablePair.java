package com.fulcrummicro.util.container;

public class ComparablePair<P extends Comparable<P>, Q extends Comparable<Q>>
             extends Pair<P, Q>
             implements Comparable<ComparablePair<P, Q>> {

    public ComparablePair(P p, Q q) {
        super(p, q);
    }

    public int compareTo(final ComparablePair<P, Q> o) {
        int c;

        c = compareTo(this.getFirst(), o.getFirst());
        if (c == 0)
            c = compareTo(this.getSecond(), o.getSecond());
        return c;
    }

    private static <T extends Comparable<T>> int compareTo(final T o1,
                                                           final T o2) {
        if (o1 == null) {
            if (o2 == null) {
                return 0;
            } else {
                return -1;
            }
        } else {
            if (o2 == null) {
                return 1;
            } else {
                return o1.compareTo(o2);
            }
        }
    }

}
