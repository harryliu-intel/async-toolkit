package com.fulcrummicro.util.misc;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

public class PropValueList extends HashMap<String, String>{

    private static final long serialVersionUID = 1L;
    
    List<String> order;

    public PropValueList() {
        super();
        order = new LinkedList<String>();
    }
    
    public String put(String name, String value) {
        if (!order.contains(name)) {
            order.add(name);
        } else {
            order.remove(name);
            order.add(name);
        }
        return super.put(name, value);
    }

    public String put(String oldName, String newName, String value) {
        if (order.contains(oldName)) {
            int ind = order.indexOf(oldName);
            int cInd = order.indexOf(newName);
            if (cInd != -1) {
                if (cInd < ind) {
                    // There is a preexisting key with lower precedence than the new one.
                    // To keep precedence we remove the earlier key and insert
                    // the new key
                    order.remove(cInd);
                    order.set(ind-1, newName);                    
                    return super.put(newName, value);
                } else {
                    // There is a preexisting key with higher precedence than the new one.
                    // Remove the old key completely in this case
                    order.remove(ind);
                    return null;
                }
            } else {
                order.set(ind, newName);
            }
            super.remove(oldName);
        } else {
            order.add(newName);
        }
        return super.put(newName, value);
    }
    
    public String insert(int index, String name, String value) {
        order.add(index, name);
        return super.put(name, value);
    }
    
    public String remove(Object key) {
        order.remove(key);
        return super.remove(key);
    }
    
    public int getIndex(String name) {
        return order.indexOf(name);
    }
        
    public String[] keySetOrdered() {
        String[] keys = new String[order.size()];
        return order.toArray(keys);
    }
    
    public void clear() {
        order.clear();
        super.clear();
    }

}
