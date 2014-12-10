/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;

import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.debug.Debug;

/**
 * Interface to an directives block.
 **/

public class DirectiveBlock extends BlockCommon implements DirectiveInterface,
                                                           Cloneable {
    DirectiveInterface directives;

    public DirectiveBlock(DirectiveInterface directives) {
        assert directives != null;
        this.directives = directives;
    }

    public String getType() {
        return BlockInterface.DIRECTIVE;
    }

    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);

        final DirectiveBlock parent = (DirectiveBlock) o;
        directives = new MergeDirective(parent, this.directives);
    }

    public BlockInterface merge(BlockInterface o) {
        if (o instanceof DirectiveBlock) {
            return new MergeDirective(this, (DirectiveBlock) o);
        }
        Debug.assertTrue(false, "DirectiveBlock does not support merge");
        return null;
    }

    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "DirectiveBlock does not support replace");
        return null;
    }


    public Object getDefaultValue(String key, String memberType) throws UnknownDirectiveException {
        return directives.getDefaultValue(key, memberType);
    }

    public Map getValues(String key, String memberType) throws UnknownDirectiveException {
        return directives.getValues(key, memberType);
    }

    public Object lookup(String key) throws UnknownDirectiveException {
        return directives.lookup(key);
    }

    public boolean containsDirective(String key) throws UnknownDirectiveException {
        return directives.containsDirective(key);
    }

    public Object lookup(String key, String memberType, Object parameter) throws UnknownDirectiveException {
        return directives.lookup(key, memberType, parameter);
    }

    public boolean isKey(String key) {
        return directives.isKey(key);
    }

    public boolean isKey(String key, String memberType) {
        return directives.isKey(key, memberType);
    }

    public String toString() {
        return directives + super.toString();
    }

    public Iterator noparamEntryIterator() {
        return directives.noparamEntryIterator();
    }

    public Iterator paramEntryIterator() {
        return directives.paramEntryIterator();
    }  
}
