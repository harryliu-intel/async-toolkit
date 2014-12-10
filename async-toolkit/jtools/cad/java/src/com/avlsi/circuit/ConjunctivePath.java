/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.circuit;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Iterator;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.util.debug.Debug;

/**
 * Transistor conjunctive pull-up/pull-down path.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class ConjunctivePath {

    public static final int SUPPLY = -1;

    /**
     * Class to represent a single transistor term in a
     * conjunctive driver path.  Includes gate node name
     * (HierName) and the transistor type (NFET/PFET).
     * Supply end node is indicated by a type of -1.
     * Hopefully this doesn't ever conflict with 
     * DeviceTypes.N_TYPE or DeviceTypes.P_TYPE.
     **/
    public static final class Term {

        public final HierName node;
        public final int type;

        public Term(final HierName n, int t) {
            node = n; type = t;
        }

        // 
        // Object overrides:
        //
        public boolean equals(Object o) {
            return o instanceof Term && equals((Term) o);
        }

        public boolean equals(Term t) {
            return node.equals(t.node) && type == t.type;
        }

        public int hashCode() {
            return (node.hashCode()<<2) + type + 1;
        }
    }

    private final ArrayList terms;
    private boolean is_feedback;

    /** Empty constructor. **/
    public ConjunctivePath() {
        terms = new ArrayList();
    }

    /** Constructor initialized to equal the passed ConjunctivePath. **/
    public ConjunctivePath(ConjunctivePath p) {
        terms = new ArrayList(p.terms);
    }

    /** Adds a transistor term to the conjunctive path **/
    public void addTerm(final Term t) {
        terms.add(t);
    }

    /** Adds a transistor term to the path, from gate name + type **/
    public void addTerm(final HierName gate, int type) {
        terms.add(new Term(gate,type));
    }

    /** Returns the list of conjunctive transistor terms **/
    public ArrayList getTerms() { return terms; }

    public int getNumTerms() { return terms.size(); }

    public boolean drivesUp() {
        Term t = (Term) terms.get(terms.size()-1);
        return t.node.isVdd();
    }

    public boolean drivesDown() {
        Term t = (Term) terms.get(terms.size()-1);
        return t.node.isGND();
    }

    public void printToStream(final PrintStream pw) {
        pw.println(getAsString());
    }

    public String getAsString() {
        StringBuffer sb = new StringBuffer();
        Iterator it = terms.iterator();
        for (int i=terms.size()-1; i>=0; i--) {
            Term t = (Term) terms.get(i);
            if (t.type == DeviceTypes.N_TYPE)
                sb.append(t.node.getAsString('.'));
            else if (t.type == DeviceTypes.P_TYPE)
                sb.append("~" + t.node.getAsString('.'));
            else
                sb.append(t.node.getAsString('.'));
            if (i>0) sb.append(" - ");
        }
        if (is_feedback) sb.append(" [staticizer]");
        return sb.toString();
    }

    /** Sets the path's feedback state.  (Indicates staticizer.) **/
    void setFeedback() { is_feedback = true; }

    /** Clears the path's feedback state. **/
    void clearFeedback() { is_feedback = false; }

    /** Gets the path's feedback state.  (Indicates staticizer.) **/
    public boolean isFeedbackPath() { return is_feedback; }

    // 
    // Object overrides:
    //

    /** equals method override to avoid redundancies in disjunctive set **/
    public boolean equals(Object o) {
        return o instanceof ConjunctivePath && equals((ConjunctivePath) o);
    }

    public boolean equals(ConjunctivePath p) {
        if (p.terms.size() != terms.size()) return false;
        for (int i=0; i<p.terms.size(); i++) {
            Term t1 = (Term)terms.get(i);
            Term t2 = (Term)p.terms.get(i);
            if (!t1.equals(t2)) return false;
        }
        return true;
    }

    public int hashCode() {
        int h = 0;
        for (int i=0; i<terms.size(); i++)
            h = 31 * h + terms.get(i).hashCode();
        return h;
    }
}
