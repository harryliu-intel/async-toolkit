package com.avlsi.tools.slacker;

import java.util.*;
import java.io.*;
import com.avlsi.util.container.MultiSet;

public class ObjectiveTerm implements Comparable {

    /** index of variable */
    int index = -1;

    /** cost of channel */
    double cost = 0;

    /** comparion */
    public int compareTo (Object b) {
        return index - ((ObjectiveTerm) b).index;
    }

    /** constructor */
    public ObjectiveTerm(int index, double cost) {
        this.index = index;
        this.cost = cost;        
    }

    /** accumulate cost to list of objective terms */
    public static void addObjectiveTerm(int index, double cost, MultiSet terms) {
        ObjectiveTerm term = new ObjectiveTerm(index,cost);
        ObjectiveTerm find = (ObjectiveTerm) terms.find(term);
        if (find==null) terms.add(term);
        else find.cost += cost;
    }
}
