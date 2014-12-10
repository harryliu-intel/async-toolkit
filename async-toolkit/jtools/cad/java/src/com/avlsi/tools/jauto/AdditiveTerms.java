package com.avlsi.tools.jauto;

import java.util.Arrays;
import java.util.ArrayList;

/**
 * Represents the sum of a list of FunctionTerms.
 **/
public class AdditiveTerms extends ArrayList<FunctionTerm> {
    public AdditiveTerms(final int capacity) {
        super(capacity);
    }
    public AdditiveTerms(final FunctionTerm... terms) {
        super(Arrays.asList(terms));
    }
    public AdditiveTerms multiply(final AdditiveTerms that) {
        final AdditiveTerms result =
            new AdditiveTerms(this.size() * that.size());
        for (FunctionTerm a : this) {
            for (FunctionTerm b : that) {
                result.add(a.multiply(b));
            }
        }
        return result;
    }
    public AdditiveTerms multiply(final FunctionTerm term) {
        return multiply(new AdditiveTerms(term));
    }
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (FunctionTerm t : this) {
            if (first) {
                first = false;
            } else {
                sb.append(" + ");
            }
            sb.append(t.toString());
        }
        return sb.toString();
    }
}
