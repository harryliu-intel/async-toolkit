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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.StringTokenizer;

/** A fast searchable tree structure for heirarchical names.
    The names are generally Strings but could be any Comparable.
    Nodes are divided up into less than, equal, and greater than, 
    at any given level, which removes redundant names. 
    Only equal children actually have the name of their immediate 
    parent as part of their name list. **/
public class TriTree {
    TriTree lo=null, eq=null, hi=null, up=null;
    Comparable key;
    //boolean leaf;

    public TriTree() {
        //leaf=false;
    }
    public TriTree(Comparable c) {
        key=c;
        //leaf=false;
    }
    /** Set the name of this node. **/
    public void setKey(Comparable _key) { key=_key; }
    /** Get the name of this node. **/
    public Comparable getKey() { return key; }
    /** Searches at the current level for a node with name <code>c</code> **/
    public TriTree search(Comparable c) {
        TriTree root = this;
        int dif;
        while (root!=null) {
            dif = root.key.compareTo(c);
            if (dif==0) { return root; }
            else if (dif<0) { root = root.hi; }
            else { root=root.lo; }
            //System.out.println("moved to: "+(root!=null?root.key:"null"));
        }
        return null;
    }
    /** Searches the tree heirarchy for a node matching the name list <code>c</code> **/
    public TriTree search(Comparable c[]) {
        TriTree cur=this;
        if (c==null) { return cur; }
        int i=0;
        while (cur!=null && i<c.length) {
            cur = cur.search(c[i]);
            if (cur!=null && i<c.length-1) { cur=cur.eq; }
            i++;
        }
        return cur;
    }
    /** Inserts a new name into the tree, constructing nodes as necessary. **/
    public TriTree insert(Comparable c[]) {
        return insert(this, c, 0);
    }
    /** Inserts a new name into the tree, constructing nodes as necessary
        using the tail of the comparable list starting at <code>i</code>. **/
    public static TriTree insert(TriTree t, Comparable c[], int i) {
        if (t==null) {
            // if we didn't find it, there cant be anything under it, so fully expand c[]
            t = new TriTree();
            t.setKey(c[i]);
            int j = c.length-1;
            if (j>i) {
                TriTree last = new TriTree();
                last.setKey(c[j]);
                //last.leaf=true;
                j--;
                while (j>i) {
                    TriTree cur = new TriTree();
                    cur.setKey(c[j]);
                    cur.eq=last; last.up=cur; last=cur;
                    j--;
                }
                t.eq=last; last.up=t;
            }
        } else if (i<c.length) {
            int dif = t.key.compareTo(c[i]);
            if (dif==0) { 
                //if (i==c.length-1 && t!=null) { t.leaf=true; }
                if (i==c.length-1) {
                    // force an overwrite at the end, so set.put will work
                    t.setKey(c[c.length-1]);
                } else {
                    t.eq=insert(t.eq, c, ++i);
                    t.eq.up=t;
                }
            } else if (dif>0) {
                t.lo=insert(t.lo, c, i);
                t.lo.up=t;
            } else {
                t.hi=insert(t.hi, c, i);
                t.hi.up=t;
            }
        }
        return t;
    }
//    public void dump(String pre) {
//        // this is a test method and assumes that the Comparable is a string
//        if (lo!=null) { lo.dump(pre); }
//        //if (eq==null || leaf) { System.out.println(pre+((String)key)); }
//        if (eq==null ) { System.out.println(pre+((String)key)); }
//        if (eq!=null) { eq.dump(pre+((String)key)+"."); }
//        if (hi!=null) { hi.dump(pre); }
//    }
//    public static String[] splitString(String s) {
//        StringTokenizer tok = new StringTokenizer(s, ".");
//        int count = tok.countTokens();
//        String split[] = new String[count];
//        int i=0;
//        while (tok.hasMoreTokens()) {
//            split[i] = tok.nextToken();
//            //System.out.println("split: "+split[i]);
//            i++;
//        }
//        return split;
//    }
//    public static void main(String[] args) {
//       TriTree tree=null;
//        String cur;
//        BufferedReader in = new BufferedReader(
//                new InputStreamReader(System.in));
//        try {
//            while ((cur=in.readLine())!=null) {
//                cur=cur.trim();
//                if (cur.equals(".")) {
//                  System.exit(0);  
//                } else if (cur.equals("*")) {
//                    tree.dump(".");
//                } else if (cur.startsWith("?")) {
//                    StringTokenizer tok = new StringTokenizer(cur, " \t");
//                    if (tok.countTokens()==2) {
//                        tok.nextToken();
//                        TriTree t = tree.search(splitString(tok.nextToken()));
//                        System.out.println("Found: "+(t!=null?((String)t.key):"null"));
//                    } else { System.out.println("Bad arguments to find"); }
//                } else if (cur.length()>0) {
//                    // split add to tree
//                    tree=tree.insert(tree, splitString(cur), 0);
//                }
//            }
//        } catch (IOException ioe) {
//        }
//    }
}
