/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

abstract class BlockCommon implements BlockInterface {
    // Map from String to List of blocks
    protected Map blockMap = new HashMap();

    public abstract String getType();
    public abstract BlockInterface merge(BlockInterface o);
    public abstract BlockInterface replace(BlockInterface o);

    /**
     * Replaces this' blockMap with o's.
     **/
    public void copyInternals(final BlockInterface o) {
        final BlockCommon parent = (BlockCommon) o;
        this.blockMap = parent.blockMap;
    }

    /**
     * Compares the blockMaps.  Inherits all of the subblocks which
     * have no equivalent in this block; refines the others.
     **/
    public void refineFrom(final BlockInterface o) {
        final BlockCommon parent = (BlockCommon) o;
        for (final Iterator i = parent.blockMap.entrySet().iterator();
             i.hasNext();
             ) {
            final Map.Entry parentBlock = (Map.Entry) i.next();
            final List blockList = (List) blockMap.get(parentBlock.getKey());
            if (blockList != null && !blockList.isEmpty()) {
                final BlockInterface block = (BlockInterface) blockList.get(0);
                if (block != null) {
                    final List parentList = (List) parentBlock.getValue();
                    if (!parentList.isEmpty()) {
                        block.refineFrom((BlockInterface) parentList.get(0));
                    }
                    continue;
                }
            }

            // Block from parent doesn't exist in this BlockCommon.
            final BlockIterator bi = iterator((String) parentBlock.getKey());
            final List parentList = (List) parentBlock.getValue();
            for (final Iterator j = parentList.iterator(); j.hasNext(); ) {
                final BlockInterface block = (BlockInterface) j.next();
                if (block instanceof Cloneable) {
                    try {
                        bi.add((BlockInterface) block.clone());
                    } catch (CloneNotSupportedException e) {
                        throw new AssertionError(e);
                    }
                } else {
                    bi.add(block);
                }
            }
        }
    }

    public BlockIterator iterator(String type) {
        List list = (List) blockMap.get(type);
        if (list == null) {
            list = new ArrayList();
            blockMap.put(type, list);
        }
        final ListIterator iterator = list.listIterator();
        return new BlockIterator() {
            public void add(BlockInterface block) {
                iterator.add(block);
            }
            public boolean hasNext() {
                return iterator.hasNext();
            }
            public boolean hasPrevious() {
                return iterator.hasPrevious();
            }
            /** @throws NoSuchElementException */
            public BlockInterface next() {
                return (BlockInterface) iterator.next();
            }
            /** @throws NoSuchElementException */
            public BlockInterface previous() {
                return (BlockInterface) iterator.previous();
            }
            public void remove() {
                iterator.remove();
            }
            public void set(BlockInterface block) {
                iterator.set(block);
            }
            public void merge(BlockInterface block) {
                if (hasNext()) {
                    BlockInterface merge = next();
                    set(merge.merge(block));
                } else {
                    add(block);
                }
            }
        };
    }

    public String toString() {
        return blockMap.toString();
    }

    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
}
