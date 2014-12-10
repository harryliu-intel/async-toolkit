package com.avlsi.layout;

import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.TreeMap;
import java.util.SortedMap;
import java.util.Map;
import java.util.Iterator;
import java.util.LinkedList;
import com.avlsi.util.container.Pair;

public class MapUtils {

    public static Collection getKeyValuePairs (Map map) {
	LinkedList pairs = new LinkedList();
	for(Iterator i=map.keySet().iterator(); i.hasNext(); ) {
	    Object key = i.next();
	    pairs.add( new Pair(key, map.get(key) ) );
	}	
	return pairs;
    }

    public static SortedMap getImmutableValueSortedMap(final Map map, final Comparator comp) {
	SortedMap newMap = new TreeMap(new Comparator() {
		public int compare(Object a, Object b) {
		    //never equals - put every one
		    int diff = comp.compare( map.get(a), map.get(b) );
		    if(diff == 0)
			return 1;
		    else
			return diff;
		}
	    } );

	newMap.putAll(map);	
	return Collections.unmodifiableSortedMap( newMap );
    }

}
