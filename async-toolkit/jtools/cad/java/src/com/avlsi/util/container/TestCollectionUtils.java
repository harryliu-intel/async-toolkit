/*
 *      TestCollectionUtils.java - test cases for CollectionUtils
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.util.container;

import com.avlsi.test.AbstractTestCase;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import com.avlsi.test.TestFailedError;

/**
 * Test cases for CollectionUtils
 *
 * @author Patrick Pelletier
 */

public class TestCollectionUtils extends AbstractTestCase {
    /**
     * Verifies that the map contains the expected entries,
     * and then returns true if the map is modifiable or false if
     * it is not.
     */
    private boolean testMap(Map map) throws TestFailedError {
        // test mappings
        assertTrue(map.get("one") instanceof Integer);
        assertTrue(map.get("two") instanceof Integer);
        assertTrue(map.get("one and a half") instanceof Double);
        assertTrue(map.get("string") instanceof String);

        // test iteration order
        Iterator i = map.keySet().iterator();
        assertTrue("one".equals(i.next()));
        assertTrue("two".equals(i.next()));
        assertTrue("one and a half".equals(i.next()));
        assertTrue("string".equals(i.next()));
        assertTrue(i.hasNext() == false);

        // check whether modifiable
        try {
            map.put(new Integer(0xCE0), new Integer(0xB0B));
        } catch (UnsupportedOperationException e) {
            return false;
        }

        return true;
    }

    public void test() throws TestFailedError, Throwable {
        // Test toIntArray
        int[] foo1 = CollectionUtils.toIntArray(new ArrayList());
        assertTrue(foo1.length == 0);

        Collection mystuff = new LinkedList();
        mystuff.add(new Integer(0x4ffe874));
        mystuff.add(new Long(0x123456789abcdefL));
        mystuff.add(new Double(-1));

        int[] foo2 = CollectionUtils.toIntArray(mystuff);
        assertTrue(foo2.length == 3);
        assertTrue(foo2[0] == 0x4ffe874);
        assertTrue(foo2[1] == 0x89abcdef);
        assertTrue(foo2[2] == -1);

        // Test asList
        List bar1 = CollectionUtils.asList(foo1);
        assertTrue(bar1.size() == 0);

        List bar2 = CollectionUtils.asList(foo2);
        assertTrue(bar2.size() == 3);
        assertTrue(((Integer)bar2.get(0)).intValue() == 0x4ffe874);
        assertTrue(((Integer)bar2.get(1)).intValue() == 0x89abcdef);
        assertTrue(((Integer)bar2.get(2)).intValue() == -1);

        // Test mapify and constMapify
        Object[] array = new Object[] {
            "one", new Integer(1),
            "two", new Integer(2),
            "one and a half", new Double(1.5),
            "string", "Hello, World!"
        };

        assertTrue(testMap(CollectionUtils.mapify(array)) == true);
        assertTrue(testMap(CollectionUtils.constMapify(array)) == false);

        // test makeHierarchicalMap
        Map quest = CollectionUtils.mapify(new Object[] {
            "foo", "bar",
            "2002.January.1", "New Year's Day",
            "2002.January.21", "Martin Luther King Day",
            "2002.February.2", "Groundhog Day",
            "2002.February.12", "Chinese New Year (Ren-Wu)",
            "2002.February.13", "Ash Wednesday",
            "2002.February.14", "Valentine's Day",
            "2002.February.18", "President's Day",
            "have.a.nice.day", new Boolean(true)
        });

        Map result = CollectionUtils.makeHierarchicalMap(quest, '.');
        assertTrue(result.size() == 3);
        assertTrue(result.get("foo").equals("bar"));
        assertTrue(((Map)result.get("2002")).size() == 2);
        assertTrue(((Map)((Map)result.get("2002")).get("February")).size() == 5);
        assertTrue(((Map)((Map)result.get("2002")).get("January")).get("21").equals("Martin Luther King Day"));

        boolean gotException = false;
        Map bad = CollectionUtils.mapify(array);
        try {
            CollectionUtils.makeHierarchicalMap(bad, ' ');
        } catch (IllegalArgumentException e) {
            gotException = e.getMessage().equals("one and a half");
        }
        assertTrue(gotException);
    }

    public static final String _version =
        "$Id$";
}
