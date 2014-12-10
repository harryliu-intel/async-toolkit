/*
 * INTEL TOP SECRET
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.tree;

import java.util.List;
import java.util.ArrayList;
/**
 * Represents a Forest Class where the rootNode could be
 * more than one. It is essentially a list of Trees.
 */

public class Forest<T> {
    private List<Tree<T>> rootElements;
         
    /**
     * Default ctor.
     */
    public Forest() {
        super();
    }

    /**
     * Return the root Trees of the super tree.
     * @return the root trees.
     */
    public List<Tree<T>> getRootTrees() {
        if (this.rootElements == null) {
            return new ArrayList<Tree<T>>();
        }
        return this.rootElements;
    }
    
    /**
     * Return the root Nodes of the super tree.
     * @return the root elements.
     */
    public List<Node<T>> getRootNodes() {
        List<Node<T>> retList = new ArrayList<Node<T>>();
        for (Tree<T> e : getRootTrees()) {
            retList.add(e.getRootElement());
        }
        return retList;
    }

    /**
     * Returns the number of roots of this Forest<T>.
     * @return the size of rootElements.
     */
    public int getNumberOfRoots() {
        if (rootElements == null) {
            return 0;
        }
        return rootElements.size();
    }

    /**
     * Adds a root to the list of roots for this Forest<T>. The addition of
     * the first child will create a new List<Tree<T>>.
     * @param root a Tree<T> object to set.
     */
    public void addRoot(Tree<T>  root) {
        if (rootElements == null) {
            rootElements = new ArrayList<Tree<T>>();
        }
        rootElements.add(root);
    }
     
    /**
     * Inserts a Tree<T> at the specified position in the rootElements list. Will
     * throw an ArrayIndexOutOfBoundsException if the index does not exist.
     * @param index the position to insert at.
     * @param root the Tree<T> object to insert.
     * @throws IndexOutOfBoundsException if thrown.
     */
    public void insertRootAt(int index, Tree<T> root) throws IndexOutOfBoundsException {
        if (index == getNumberOfRoots()) {
            // this is really an append
            addRoot(root);
            return;
        } else {
            rootElements.get(index); //just to throw the exception, and stop here
            rootElements.add(index, root);
        }
    }
     
    /**
     * Remove the Tree<T> element at index index of the List<Tree<T>>.
     * @param index the index of the element to delete.
     * @throws IndexOutOfBoundsException if thrown.
     */
    public void removeRootAt(int index) throws IndexOutOfBoundsException {
        rootElements.remove(index);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        int i = 0;
        for (Tree<T> e : getRootTrees()) {
            sb.append("\nTree: "+i);
            sb.append(e.toString());
            i++;
        }
        return sb.toString();
    }
}
