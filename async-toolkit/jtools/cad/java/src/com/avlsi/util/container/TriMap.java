/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4
/* 
 * Copyright 2000 Asynchronous Digital Design. All rights reserved.
 *
 */

package com.avlsi.util.container;

import java.util.Stack;
import java.util.ArrayList;
import java.util.Vector;
import java.util.Iterator;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.StringTokenizer;

/** A map (like a dictionary or hashtable) that uses heirarchical name keys
    stored in a TriTree to save and search for values. Faster at discovering
    non-matching keys than a hashtable, and more space efficient for names
    with repeated elements. **/
public class TriMap {
    /** Our internal storage and searching structure. **/
    TriTree root=null;
    /** The character used to split name keys into lists. **/
    String delim = ".";

    /** Storage element to put in the TriTree that behaves as its String key, 
        but stores an extra object for the map. **/
    public class TriMapNode implements Comparable {
        String key = null;
        Object val = null;
        
        public TriMapNode() {}
        public TriMapNode(String _key, Object _val) { key=_key; val=_val; }
        public void setVal(Object o) { val=o; }
        public Object getVal() { return val; }
        public int compareTo(Object o) {
            if (o instanceof TriMapNode) { return key.compareTo(((TriMapNode)o).key); }
            if (o instanceof String) { return key.compareTo((String)o); }
            throw new ClassCastException();
        }
    }
    /** Structure for storing our current place while iterating on the tree. **/
    class IteratorLog {
        TriTree cur;
        int dir = -1, max = 1;
        IteratorLog(TriTree n) { cur=n; }
    }
    /** An iterator that moves thru the tree returning only those nodes 
        that actually have values. **/
    public class TriMapIterator implements Iterator {
        Stack stack = new Stack();
        TriTree cur=null;
        boolean used = true;
        
        TriMapIterator(TriTree root) { 
            if (root!=null) { stack.push(new IteratorLog(root)); }
        }
        /** Are there more nodes? **/
        public boolean hasNext() { return advance(); }
        /** Get the next value. **/
        public Object next() { 
            advance();
            used=true; 
            return ((TriMapNode)cur.getKey()).getVal(); 
        }
        /** Allow acces to the keyset that got us here, since values in the tree may not be unique,
            only valid after a call to next(). **/
        public String getLabel() {
            if (cur==null) { return null; }
            return TriMap.getLabel(cur, delim);
        }
        /** Not implemented. **/
        public void remove() {
        // TODO implement
        }
        /** Step thru the tree until we find a node with a value or hit the end. **/
        boolean advance() {
            // if the current one isn't used, leave it
            if (!used) { return true; }
            IteratorLog il;
            // since we can't convieniently pause and resume tree traversal,
            // roll our own iterative version
            while (!stack.empty()) {
                il = (IteratorLog)stack.peek();
                // descend  while there are usable children, left first...
                if (il.dir==-1 && il.dir<=il.max) {
                    il.dir++;
                    if (il.cur.lo!=null) {
                        stack.push(new IteratorLog(il.cur.lo));
                        continue;
                    }
                } 
                // but parents are 'alphabetically' before 'equal' children
                if (il.dir==0 && cur!=il.cur && il.cur.getKey()!=null) { 
                    if (((TriMapNode)il.cur.getKey()).getVal()!=null) { 
                        cur=il.cur; 
                        used=false;
                        return true;
                    }
                }
                if (il.dir==0 && il.dir<=il.max) {
                    il.dir++;
                    if (il.cur.eq!=null) {
                        stack.push(new IteratorLog(il.cur.eq));
                        continue;
                    }
                }
                if (il.dir==1 && il.dir<=il.max) {
                    il.dir++;
                    if (il.cur.hi!=null) {
                        stack.push(new IteratorLog(il.cur.hi));
                        continue;
                    }
                }
                if (il.dir>1) {
                    // ascend when there aren't
                    stack.pop();
                    continue;
                }
            }
            return false;
        }
    }
    /** An iterator that just returns one node. **/
    public class TrivialIterator extends TriMapIterator {
        TrivialIterator(TriTree root) { 
            super(root);
            if (root!=null) { stack.pop(); }
            cur = root; used=(cur==null);
        }
        boolean advance() {
            // if the current one isn't used, leave it
            if (!used) { return true; }
            return false;
        }
    }
    /** An iterator that performs pattern matching. **/
    public class TriMatchIterator extends TriMapIterator {
        String prefix = null;
        String suffix[] = null;
        boolean exactSuf = false;

        /** Expects pre to have any delimited parts removed and root point to 
            the appropriate sub-node. Pre is a prefix to match at the current 
            level, and suff is a (possibly delimited) suffix to match. **/
        TriMatchIterator(TriTree root, String pre, String suff) {
            super(root);
            prefix=pre;
            if (suff!=null && suff.length()>0) {
                if (suff.startsWith(delim)) { // case *.blah
                    suffix = splitString(suff.substring(1));
                    exactSuf = true;
                } else { // case *blah
                    suffix = splitString(suff);
                }
            }
        }

        /** Iterate over our traversal stack and return any node names that
            are actually part of our name (from an equal parent). **/
        Vector getStackStrings() {
            int sz = stack.size();
            if (sz<1) { return null; }
            Vector v = new Vector();
            TriTree t, last;
            TriMapNode n;
            last = ((IteratorLog)stack.elementAt(0)).cur;
            for (int i=1; i<sz; i++, last=t) {
                t = ((IteratorLog)stack.elementAt(i)).cur;
                // only when we step down an equal child is the node name actually
                // part of the current heirarchy
                if (t==last.eq) {
                    v.add(((TriMapNode)last.getKey()).key);
                }
            }
            // last node is always part of heirarchy
            n = (TriMapNode)((IteratorLog)stack.elementAt(sz-1)).cur.getKey();
            v.add(n.key);
            return v;
        }

        /** Calls the superclass method until we find a matching node or run out. **/
        boolean advance() {
            if (!used) { return true; }
            while (super.advance()) {
                // keep advancing 
                used=true;
                Vector rstack = getStackStrings();
                // until stack nodes start with prefix
                if (prefix!=null && prefix.length()>0) {
                    if (rstack==null) { continue; }
                    String top = (String)rstack.elementAt(0);
                    if (top==null || !top.startsWith(prefix)) { continue; }
                }
                // and end with suffix
                // FIXME stack considers all children, but we only want eq...
                if (suffix!=null && suffix.length>0) {
                    if (rstack==null) { continue; }
                    int i=rstack.size()-1, j=suffix.length-1;
                    int minstack = suffix.length;
                    if (exactSuf && prefix!=null) { minstack++; }
                    if (rstack.size()<minstack) { continue; }
                    boolean passed = true;
                    for (; j>0; i--,j--) {
                        String s=(String)rstack.elementAt(i);
                        if (!s.equals(suffix[j])) { passed=false; break; }
                    }
                    if (!passed) { continue; }
                    String s=(String)rstack.elementAt(i);
                    if (!exactSuf) { if (!s.endsWith(suffix[j])) { continue; } }
                    else if (!s.equals(suffix[j])) { continue; }
                }
                used=false;
                return true;
            }
            return false;
        }
    }

    public TriMap() {}

    /** Returns an iterator over the whole tree **/
    public Iterator iterator() {
        return new TriMapIterator(root);
    }
    
    /** Returns an iterator of nodes that match a single "*" wildcard expression. **/
    public Iterator iterator(String s) {
        if (s==null || s.length()==0) { return new TrivialIterator(null); }
        if (s.equals("*")) { return new TriMapIterator(root); }
        int star = s.indexOf("*");
        if (s.indexOf("*", star+1)>0) { return new TrivialIterator(null); }
        if (star<0) { return new TrivialIterator(getTree(splitString(s))); }
        int len = s.length();
        TriTree t = null;
        String prefix = null;
        String remainder = null;
        int dot = s.lastIndexOf(delim, star);

        if (len>(star+1)) { remainder=s.substring(star+1); }
        if (star>0 && (dot==star-1)) { // case blah.*...
            t = getTree(splitString(s.substring(0,star)));
            if (t!=null) { t=t.eq; }
        } else { // case blah*...
            if (dot<0) { t=root; }
            else {
                t = getTree(splitString(s.substring(0,dot)));
                if (t!=null) { t=t.eq; }
            }
            if (star>0) { prefix=s.substring(dot+1,star); }
        }
        return new TriMatchIterator(t, prefix, remainder);
    }
    /** Adds all nodes that match the given (possibly sing wildcard) string. **/
    void addCompletions(TriTree t, ArrayList al, String s) {
        if (t==null) { return; }
        TriMapNode n = (TriMapNode)t.getKey(); 
        if (n.key!=null && (s==null || n.key.startsWith(s))) {
            if (t.eq!=null)  { al.add(n.key+delim); }
            else { al.add(n.key); }
        }
        addCompletions(t.lo, al, s);
        addCompletions(t.hi, al, s);
    }
    /** Gives completions (partial matches) of the input string up to the next delimiter. **/
    public String[] getCompletions(String s) {
        int dot = s.lastIndexOf(delim);
        String remainder = null;
        if (dot<0) { dot = s.length(); }
        else { remainder=s.substring(dot); }
        String split[] = splitString(s.substring(0,dot));
        ArrayList matches = new ArrayList();
        addCompletions(getTree(split), matches, remainder);
        int sz = matches.size();
        if (sz==0) { return null; }
        return (String[]) matches.toArray(new String[sz]);
    }

    /** Not implemented. **/
    public void remove(String s[]) { 
    // TODO implement 
    }
    /** Not implemented (propperly). **/
    public void remove(String s) {
        //remove(splitString(s));
        put(s, null);
    }
    /** Returns the TriTree node for a given processed key string. **/
    TriTree getTree(String s[]) {
        if (root==null) { return null; }
        return (TriTree)root.search(s);
    }
    /** Returns the TriMapNode for a given processed key string. **/
    TriMapNode getNode(String s[]) {
        TriTree t = getTree(s);
        if (t!=null) { return (TriMapNode)t.getKey(); }
        return null;
    }
    /** Returns the TriTree node for a given unprocessed key string. **/
    public TriTree getTreeNode(String s) {
        if (s==null || s.length()==0 || root==null) { return null; }
        return getTree(splitString(s));
    }
    /** Returns the TriMapNode for a given unprocessed key string. **/
    public TriMapNode getNode(String s) {
        if (s==null || s.length()==0 || root==null) { return null; }
        return getNode(splitString(s));
    }
    /** Searches the map for the given name, and if found returns its value object. **/
    public Object get(String s) {
        TriMapNode node = getNode(s);
        if (node!=null) { 
            return node.getVal(); 
        }
        return null;
    }
    /** Puts an object into the map with the given string name. */
    public void put(String s, Object o) {
        if (s==null || s.length()==0) { return; }
        Class prototype  = new TriMapNode().getClass();
        String list[] = splitString(s);
        Comparable nodes[] = new Comparable[list.length];
        int len = list.length-1;
        for (int i=0; i<len; i++) { 
            nodes[i]= new TriMapNode(list[i], null); 
            //System.out.print(delim+list[i]);
        }
        if (len>=0) { 
            nodes[len] = new TriMapNode(list[len], o); 
            //System.out.println(delim+list[len]+"  "+(String)o);
        }
        root=TriTree.insert(root, nodes, 0);
    }
    /** Reconstructs the full string key from a given node and its parents. **/
    public static String getLabel(TriTree t, String delim) {
        if (t==null) { return null; }
        String ret = ((TriMapNode)t.getKey()).key;
        for (; t.up!=null; t=t.up) {
            // only add the name if we came from the eq child
            if (t.up!=null && t.up.eq!=t) { continue; }
            String name = ((TriMapNode)t.up.getKey()).key;
            ret = name + delim + ret;
        }
        return ret;
    }
    /** Splits a string on our default delimiter. **/
    public String[] splitString(String s) {
        return splitString(s, delim);
    }
    /** Splits a string into an array of strings at each occurence of delim. **/
    public static String[] splitString(String s, String delim) {
        if (s==null || s.length()<1) { return null; }
        StringTokenizer tok = new StringTokenizer(s, delim);
        int count = tok.countTokens();
        String split[] = new String[count];
        int i=0;
        while (tok.hasMoreTokens()) {
            split[i] = tok.nextToken();
            //System.out.println("split: "+split[i]);
            i++;
        }
        return split;
    }
    /** Test method. **/
    public static void main(String[] args) {
        TriMap map = new TriMap();
        String cur;
        BufferedReader in = new BufferedReader(
                new InputStreamReader(System.in));
        try {
            while (true) {
                System.out.print("> ");
                if ((cur=in.readLine())==null) { break; }
                cur=cur.trim();
                if (cur.equals(".")) {
                  System.exit(0); 
                } else if (cur.equals("*")) {
                    Iterator it = map.iterator();
                    while (it.hasNext()) {
                        TriMapNode curVal = (TriMapNode)it.next();
                        String val = (String) curVal.getVal();
                        System.out.println("    "+val!=null?val:"null");
                    }
                    //tree.dump(delim);
                } else if (cur.startsWith("?")) {
                    StringTokenizer tok = new StringTokenizer(cur, " \t");
                    if (tok.countTokens()==2) {
                        tok.nextToken();
                        String s = (String)map.get(tok.nextToken());
                        System.out.println("Found: "+(s!=null?s:"null"));
                    } else { System.out.println("Bad arguments to get"); }
                } else if (cur.startsWith("!")) {
                    StringTokenizer tok = new StringTokenizer(cur, " \t");
                    if (tok.countTokens()==3) {
                        tok.nextToken();
                        map.put(tok.nextToken(), tok.nextToken());
                    } else { System.out.println("Bad arguments to put"); }
                }
            }
        } catch (IOException ioe) {
        }
    }
}
