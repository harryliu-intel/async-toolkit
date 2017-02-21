/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

import java.lang.String;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.LinkedList;

import com.avlsi.cell.CellInterface;

import com.avlsi.file.common.HierName;

import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.BinaryPredicate;
import com.avlsi.util.functions.UnaryPredicate;


public class ChildrenFirstCellInterfaceIterator
implements Iterator<CellInterface> {

    private static class StackFrame {
        private final Iterator<Pair<HierName,CellInterface>> mSubcellPairsIter;
        private final BinaryPredicate<List<CellInterface>,CellInterface> mFilter;
        private final ArrayList<CellInterface> mPath;

        public StackFrame( final ArrayList<CellInterface> path,
                           final BinaryPredicate<List<CellInterface>,CellInterface> filter ) {
            mPath = path;
            mFilter = filter;
            mSubcellPairsIter =
                new FilteringIterator<Pair<HierName,CellInterface>>(
                    getCell().getSubcellPairs(),
                    new UnaryPredicate<Pair<HierName,CellInterface>>() {
                        public boolean evaluate(
                            final Pair<HierName,CellInterface> p ) {
                            return filter.evaluate( mPath, p.getSecond() );
                        }
                    });
        }

        public boolean hasNextSubcell(  ) {
            return mSubcellPairsIter.hasNext();
        }

        public StackFrame nextSubcellMaster() {
            final ArrayList<CellInterface> nextPath = new ArrayList<>(mPath);
            nextPath.add( mSubcellPairsIter.next().getSecond() );
            return new StackFrame( nextPath, mFilter );
        }

        public CellInterface getCell() {
            return mPath.get( mPath.size() - 1 );
        }
    }

    private final LinkedList<StackFrame> mStack;

    private final Set<String> mVisitedSet;
    
    private CellInterface mNextCell;

    private final BinaryPredicate<List<CellInterface>,CellInterface> mFilterPredicate;
    
    public ChildrenFirstCellInterfaceIterator( final CellInterface root ) {
        this( root, (a, b) -> true );
    }

    public ChildrenFirstCellInterfaceIterator(
            final CellInterface root,
            final BinaryPredicate<List<CellInterface>,CellInterface> filter ) {
        mStack = new LinkedList<StackFrame>();
        mVisitedSet = new HashSet<String>();
        mNextCell = null;
        mFilterPredicate = filter;
        mStack.addFirst(
                new StackFrame(
                    new ArrayList<>( Collections.singleton( root ) ),
                    mFilterPredicate ) );
    }

    public boolean hasNext() {
        if ( mNextCell == null ) {
            while ( ( mStack.size() != 0 ) && ( mNextCell == null ) ) {
                final StackFrame currFrame = mStack.getFirst();
                if ( currFrame.hasNextSubcell() ) {
                    final StackFrame nextSubcellMaster = currFrame.nextSubcellMaster();
                    final String subcellMasterName =
                        nextSubcellMaster.getCell().getFullyQualifiedType();
                    
                    if ( ! ( mVisitedSet.contains( subcellMasterName ) ) ) {
                        mStack.addFirst( nextSubcellMaster );
                    }
                }
                else {
                    final CellInterface currCell = currFrame.getCell();
                    mNextCell = currCell;
                    mVisitedSet.add( mNextCell.getFullyQualifiedType());
                    mStack.removeFirst();
                }
            }
        }
        return mNextCell != null;
    }

    public CellInterface next() throws NoSuchElementException {
        if ( hasNext() ) {
            final CellInterface ret = mNextCell;
            mNextCell = null;
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}
