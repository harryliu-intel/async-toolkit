/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Collection;
import java.util.Iterator;

public abstract class Counter {
   
    public abstract void add(Object o, int count);
    public abstract int getCount(Object o);
    public abstract Collection elements();

    public int size() {
        return elements().size();
    }

    public void add(Object o) {
        add(o,1);
    }
     
    public void addAll(Counter counter) {
        for(Iterator i=counter.elements().iterator(); i.hasNext(); ) {
            Object o = i.next();
            int count = counter.getCount(o);
            add(o, count);
        }
    }

    public void subtract(Object o) {
        subtract(o,1);
    }
    
    public void subtract(Object o, int count) {
        add(o,-count);
    }
    
    public void subtractAll(Counter counter) {
         for(Iterator i=counter.elements().iterator(); i.hasNext(); ) {
            Object o = i.next();
            int count = counter.getCount(o);
            subtract(o, count);
        }
    }
  
    public int getTotalCount() {
        int count = 0;
        for(Iterator i=elements().iterator(); i.hasNext(); ) {
            Object o = i.next();
            count += getCount(o);
        }        
        return count;
    }

}
