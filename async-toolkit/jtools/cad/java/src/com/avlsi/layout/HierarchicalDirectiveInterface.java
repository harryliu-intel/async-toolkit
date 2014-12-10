/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.layout;

import java.util.Map;

import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.fast.BlockInterface;
import com.avlsi.util.container.Triplet;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.cell.CellInterface;


/**
 In cell top, looks for the highest level directive for node a.b.c
 i.e. looks first in top for the directive on node a.b.c, then in a for the directive on node b.c, and so on
      if doesn't find anything it returns the default value
**/

public class HierarchicalDirectiveInterface {

    CellInterface topCell;
    public HierarchicalDirectiveInterface(CellInterface topCell) {
        this.topCell = topCell;
    }
   
    public Object getDirectiveHierarchicalWithBackup(final String directive, 
                                                     final String type, 
                                                     final HierName node, 
                                                     final String backupDirective,
                                                     final String backupType, 
                                                     final Object backupKey) {
        Triplet[] triplet = DirectiveTable.lookupParameterizedDirective(BlockInterface.CELL, directive);
        Object defval = triplet[0].getThird();
        Object value = getDirectiveHierarchical( directive, type, node );
        if( value.equals(defval) ) {                        
            return getDirectiveHierarchical(backupDirective, backupType, backupKey, node );
        }
        else
            return value;        
    }

    public Object getDirectiveHierarchical(final String directive, final String type, final HierName node) {
        HierName name =  node;
        Map map;
        Object value = null;
        CellInterface cell = topCell;          
        while( value == null && name != null ) {          
            map = DirectiveUtils.getTopLevelDirective(cell, directive, type);
            value = map.get( name );
            cell = cell.getSubcell( name.head() );
            name = name.tail();
        }
        if(value == null ) {
            Triplet[] triplet = DirectiveTable.lookupParameterizedDirective(BlockInterface.CELL, directive);
            Object defval = triplet[0].getThird();
            return defval;
        }
        return value;
    }

    public Object getDirectiveHierarchical(final String directive, final String type, final Object key, final HierName node) {
        HierName name =  node;
        Map map;
        Object value = null;
        CellInterface cell = topCell;      
        while( value == null && name != null ) {                      
            map = DirectiveUtils.getTopLevelDirective(cell, directive, type);
            value = map.get( key );
            cell = cell.getSubcell( name.head() );
            name = name.tail();
        }
        if(value == null) {
            Triplet[] triplet = DirectiveTable.lookupParameterizedDirective(BlockInterface.CELL, directive);
            Object defval = triplet[0].getThird();
            return defval;
        }
        else
            return value;
    }
}
