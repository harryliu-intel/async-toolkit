// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.difflayout;

import java.awt.Polygon;

public class LayeredPolygon extends Polygon implements Comparable {
    public final int layer;

    public LayeredPolygon(int layer) {
        this.layer = layer;
    }

    public int compareTo(Object o) {
        LayeredPolygon p = (LayeredPolygon) o;
        if (layer != p.layer)
            return layer - p.layer;
        if (npoints != p.npoints)
            return npoints - p.npoints;
        for (int i = 0; i < npoints; i++) {
            if (xpoints[i] != p.xpoints[i])
                return xpoints[i] - p.xpoints[i];
            if (ypoints[i] != p.ypoints[i])
                return ypoints[i] - p.ypoints[i];
        }
        return 0;
    }

    public boolean equals(Object o) {
        return (compareTo(o) == 0);
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < npoints; i++) {
            if (i > 0)
                buf.append(' ');
            buf.append('(');
            buf.append(xpoints[i]);
            buf.append(", ");
            buf.append(ypoints[i]);
            buf.append(')');
        }
        return buf.toString();
    }
}
