// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.difflayout;

import java.util.Collection;
import java.awt.Graphics2D;
import java.util.Iterator;
import java.awt.Polygon;
import java.util.TreeSet;

public class PolygonUtils {
    public static Collection samePolygons(Collection a, Collection b) {
        TreeSet s = new TreeSet();
        s.addAll(a);
        s.retainAll(b);
        return s;
    }

    public static Collection differentPolygons(Collection a, Collection b) {
        TreeSet s = new TreeSet();
        s.addAll(a);
        s.removeAll(b);
        TreeSet t = new TreeSet();
        t.addAll(b);
        t.removeAll(a);
        s.addAll(t);
        return s;
    }

    public static void drawPolygons(Collection c, Graphics2D g) {
        for (Iterator it = c.iterator(); it.hasNext(); ) {
            Polygon p = (Polygon)it.next();
            g.fill(p);
        }
    }
}
