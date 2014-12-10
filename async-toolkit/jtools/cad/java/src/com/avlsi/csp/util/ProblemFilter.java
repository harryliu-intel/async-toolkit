package com.avlsi.csp.util;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.io.Printable;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.UnaryFunction;

// XXX: Eventually switch to using the standard logging interface.
public class ProblemFilter {
    public static final String ERROR = "error";
    public static final String WARNING = "warning";

    /**
     * Compares a problem by filename, line number, column number, and error
     * code.
     **/
    private static final Comparator POSITION_COMPARATOR =
        new Comparator() {
            public int compare(Object o1, Object o2) {
                final Problem p1 = (Problem) o1;
                final Problem p2 = (Problem) o2;
                final ParsePosition pp1 = p1.getParseRange().start;
                final ParsePosition pp2 = p2.getParseRange().start;
                int x;
                x = pp1.filename.compareTo(pp2.filename);
                if (x != 0) return x;
                x = pp1.compareTo(pp2);
                if (x != 0) return x;
                return p1.getCode().compareTo(p2.getCode());
            }
            public boolean equals(Object o) {
                return o == this;
            }
        };

    private final Map store;
    private final UnaryFunction classify;
    private final BinaryFunction filter;
    public ProblemFilter(final UnaryFunction classify,
                         final BinaryFunction filter) {
        this.store = new TreeMap(POSITION_COMPARATOR);
        this.classify = classify;
        this.filter = filter;
    }
    public void process(final Collection problems) {
        final Map temp = new TreeMap(POSITION_COMPARATOR);
        for (Iterator i = problems.iterator(); i.hasNext(); ) {
            final Problem prob = (Problem) i.next();
            if (!store.containsKey(prob)) {
                final String type = (String) classify.execute(prob);
                temp.put(prob, type);
            }
        }
        for (Iterator i = temp.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Problem prob = (Problem) entry.getKey();
            final String type = (String) entry.getValue();
            final Printable p = (Printable) filter.execute(type, prob);
            if (p != null) {
                p.print(type + ": ");
                prob.printMessage(p);
                p.flush();
            }
        }
        store.putAll(temp);
    }
    public boolean hasError() {
        return store.values().contains(ERROR);
    }

    public static UnaryFunction getClassifier(final Set warnings) {
        return new UnaryFunction() {
            public Object execute(final Object o) {
                final Problem p = (Problem) o;
                if (warnings.contains(p.getCode())) {
                    return ProblemFilter.WARNING;
                } else {
                    return ProblemFilter.ERROR;
                }
            }
        };
    }
}
