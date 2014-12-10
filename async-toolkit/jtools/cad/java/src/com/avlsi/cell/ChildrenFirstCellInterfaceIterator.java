/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

import java.lang.String;

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
import com.avlsi.util.functions.UnaryPredicate;


public class ChildrenFirstCellInterfaceIterator
implements Iterator<CellInterface> {

    private static class StackFrame {
        private final Iterator mSubcellPairsIter;
        private final CellInterface mCell;

        public StackFrame( final CellInterface cell ,
                           final UnaryPredicate<CellInterface> filter ) {
            mCell = cell;
            mSubcellPairsIter =
                new FilteringIterator(
                    mCell.getSubcellPairs(),
                    new UnaryPredicate<Pair<HierName,CellInterface>>() {
                        public boolean evaluate(
                            final Pair<HierName,CellInterface> p ) {
                            return filter.evaluate( p.getSecond() );
                        }
                    });
        }

        public boolean hasNextSubcell(  ) {
            return mSubcellPairsIter.hasNext();
        }

        public CellInterface nextSubcellMaster() {
            final Pair instancePair = ( Pair ) mSubcellPairsIter.next();
            final CellInterface master = ( CellInterface ) instancePair.getSecond();
            return master;
        }

        public CellInterface getCell() {
            return mCell;
        }
    }

    private final LinkedList<StackFrame> mStack;

    private final Set<String> mVisitedSet;
    
    private CellInterface mNextCell;

    private final UnaryPredicate<CellInterface> mFilterPredicate;
    
    public ChildrenFirstCellInterfaceIterator( final CellInterface root ) {
        this( root, new UnaryPredicate.Constant<CellInterface>( true ) );
    }

    public ChildrenFirstCellInterfaceIterator(
            final CellInterface root,
            final UnaryPredicate<CellInterface> filter ) {
        mStack = new LinkedList<StackFrame>();
        mVisitedSet = new HashSet<String>();
        mNextCell = null;
        mFilterPredicate = filter;
        mStack.addFirst( new StackFrame( root, mFilterPredicate ) );
    }

    public boolean hasNext() {
        if ( mNextCell == null ) {
            while ( ( mStack.size() != 0 ) && ( mNextCell == null ) ) {
                final StackFrame currFrame = mStack.getFirst();
                if ( currFrame.hasNextSubcell() ) {
                    final CellInterface nextSubcellMaster = currFrame.nextSubcellMaster();
                    final String subcellMasterName = nextSubcellMaster.getFullyQualifiedType();
                    
                    if ( ! ( mVisitedSet.contains( subcellMasterName ) ) ) {
                        mStack.addFirst( new StackFrame( nextSubcellMaster,
                                                         mFilterPredicate ) );
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
