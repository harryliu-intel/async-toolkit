package com.fulcrummicro.util.properties;

import java.util.ArrayList;

import com.fulcrummicro.util.properties.MalformedSubstException;

/**
 * Evaluates a string for substitution expressions
 *
 * @author Naru Sundar
 */
public class SubstExpr {
    
    /* string being evaluated on */
    protected final String expr;

    /* bounds of match */
    protected int start = -1;
    protected int end = -1;

    public SubstExpr(String expr) {
        this.expr = expr;
    }

    public boolean canSubst() {
        /* do a loose check here, errors can be caught in the actual
         * expansion
         */
        if((expr.indexOf("$(") != -1) &&
           (expr.indexOf(")", expr.indexOf("$(")) != -1))
            return true;
        
        return false;
    }

    public String evaluateSubst() throws MalformedSubstException {
        int len = expr.length();
        if(!canSubst())
            return expr;

        for(int i = 0, depth = 0; i < len; i++) {
            /* if we find a $( increase depth */
            if((expr.charAt(i) == '$') && 
               ((i + 1) < len) && 
               (expr.charAt(i+1) == '(')) {

                /* only update start at depth 0 */
                if(depth == 0)
                    start = i;

                depth++;
            } else if((depth > 0) && // exclude cases where there are )'s not in a match expr 
                (expr.charAt(i) == ')')) {
                depth--;

                /* if we're at depth 0 now, we're done */
                if(depth == 0) {
                    end = i;
                    break;
                }
            }
        } 

        if((start != -1) && (end != -1))
            return match();
        else
            throw new MalformedSubstException("ERROR: " + expr + " has a malformed substitution");
    }

    /**
     * Returns the start index of the match.
     *
     * @return  The index of the first character matched
     */
    public int start() {
        return this.start;
    }
    
    /** 
     * Returns the offset after the last character matched.
     * @return the offset after the last character matched
     */
    public int end() {
        return this.end + 1;
    }

    public String match() {
        return expr.substring(start + 2, end);
    }

    /* test cases in main method */

    public static void main(String[] args) {
        ArrayList<String> tests = new ArrayList<String>();

        tests.add(new String("a(b)cd$(e)hi$(j)"));
        tests.add(new String("abcd"));
        tests.add(new String("$(abcd)"));
        tests.add(new String("$(abcd$(efgh))"));
        tests.add(new String("$(abcd$(efgh)hij)"));
        tests.add(new String("$(a)bcd$(e)hi$(j)"));

        for(int i = 0; tests.size() > 0; i++) {
            String test = tests.remove(0);
            SubstExpr e = new SubstExpr(test);

            if(e.canSubst()) {
                try {
                    e.evaluateSubst();
                } catch(MalformedSubstException z) {
                    System.out.println("test #" + i + ": " + test + " matches but does not evaluate! (FAIL)");
                }
                System.out.println("test #" + i + ": " + test + " matches at (" + 
                                   e.start() + ", " + e.end() + ") => " + e.match());
            } else {
                System.out.println("test #" + i + ": " + test + " does not match");
            }
        }
    }
}

