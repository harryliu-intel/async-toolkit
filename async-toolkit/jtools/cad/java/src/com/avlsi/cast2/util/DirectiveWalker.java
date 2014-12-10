/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.util;

import java.util.Map;
import java.util.TreeMap;
import java.util.SortedMap;
import java.util.Iterator;
import java.io.IOException;
import java.util.Comparator;

import com.avlsi.fast.DirectiveBlock;
import com.avlsi.util.container.Pair;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.cast2.directive.UnknownDirectiveException;

import com.avlsi.util.debug.Debug;


public class DirectiveWalker {

    private final DirectiveActionInterface action;

    public DirectiveWalker(DirectiveActionInterface action) {
        this.action = action;
    }

    public void walk(BlockInterface cellBlock, String blockType) throws UnknownDirectiveException, IOException {
        BlockIterator iter = cellBlock.iterator( blockType );
        if(iter.hasNext()) {    
            BlockInterface block = iter.next();
            walk(block);
        }
    }
            
    public void walk(BlockInterface block) throws UnknownDirectiveException, IOException  {
        walk(block, DirectiveUtils.getDirectiveBlock(block) );
    }

    private static Comparator directiveComparator = 
        new Comparator() {
            public int compare(Object a, Object b) {
                return a.toString().compareTo(b.toString());
            }
        };

    public void walk(BlockInterface block, DirectiveBlock db) throws UnknownDirectiveException, IOException {
        if( db == null )
            return;
        action.doBlockInterface(block);

        SortedMap noParamMap = new TreeMap(directiveComparator);
        for(Iterator i=db.noparamEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            noParamMap.put(entry.getKey(),entry.getValue());
        }
        for(Iterator i=noParamMap.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            Object value = entry.getValue();	
            String type = (String) DirectiveTable.lookupDirective(block.getType(), key).getFirst();
            action.doUnParameterizedDirective(block, db, key, value, type);
        }

        SortedMap paramMapMap = new TreeMap(directiveComparator);
        for(Iterator i=db.paramEntryIterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            paramMapMap.put(entry.getKey(),entry.getValue());
        }
        for(Iterator i=paramMapMap.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            Pair pair = (Pair) entry.getKey();
            String paramKey = (String) pair.getFirst();
            String paramType = (String) pair.getSecond();
            SortedMap paramMap = new TreeMap(directiveComparator);
            paramMap.putAll((Map)entry.getValue());  
            String valueType = (String) 
                (DirectiveTable.lookupParameterizedDirective(block.getType(), paramKey))[0].getSecond();
            action.doParameterizedDirectiveType(block, db, paramKey, paramType, valueType); 
            for(Iterator j=paramMap.entrySet().iterator(); j.hasNext(); ) {
                Map.Entry paramEntry = (Map.Entry) j.next();
                Object param = paramEntry.getKey();
                Map valueMap = db.getValues(paramKey, paramType);
              
                Object value  = valueMap.get(param);
                action.doParameterizedDirectiveValue(block, db, paramKey, param, value, paramType, valueType); 
            }
        }
    }
}
                

