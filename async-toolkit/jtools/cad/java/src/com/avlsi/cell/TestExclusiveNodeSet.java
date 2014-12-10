/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cell;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.test.AbstractTestCase;

/**
 * This is a description of the class
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class TestExclusiveNodeSet extends AbstractTestCase {
    public void test() throws Exception {
        final HierName a = HierName.makeHierName("a");
        final HierName b = HierName.makeHierName("b");
        final HierName c = HierName.makeHierName("c");
        final HierName fooA = HierName.makeHierName("foo.a", '.');
        final HierName fooB = HierName.makeHierName("foo.b", '.');

        final Set ab = new HashSet();
        ab.add(a);
        ab.add(b);

        final Set bc = new HashSet();
        bc.add(b);
        bc.add(c);

        final Set abc = new HashSet();
        abc.add(a);
        abc.add(b);
        abc.add(c);

        final Set fooSet = new HashSet();
        fooSet.add(fooA);
        fooSet.add(fooB);

        final ExclusiveNodeSet ens1 =
            new ExclusiveNodeSet(ExclusiveNodeSet.HI, ab);
        assertTrue(ens1.areExclusive(ab));
        assertTrue(!ens1.areExclusive(Collections.singleton(a)));
        assertTrue(!ens1.areExclusive(Collections.singleton(b)));

        final ExclusiveNodeSet ens2 =
            new ExclusiveNodeSet(ExclusiveNodeSet.HI, abc);
        assertTrue(ens2.areExclusive(ab));
        assertTrue(ens2.areExclusive(bc));
        assertTrue(!ens2.areExclusive(Collections.singleton(a)));
        assertTrue(!ens2.areExclusive(Collections.singleton(b)));
        assertTrue(!ens2.areExclusive(Collections.singleton(c)));

        final ExclusiveNodeSet ens3 =
            new ExclusiveNodeSet(ExclusiveNodeSet.HI, fooSet);
        assertTrue(ens3.areExclusive(fooSet));
        assertTrue(!ens3.areExclusive(Collections.singleton(fooA)));
        assertTrue(!ens3.areExclusive(Collections.singleton(fooB)));
    }

    public static void main(String[] args) {
        AbstractTestCase.testOne(new TestExclusiveNodeSet());
    }
}
