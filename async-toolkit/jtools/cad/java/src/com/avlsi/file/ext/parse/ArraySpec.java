/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.ext.parse;

import com.avlsi.util.functions.UnaryAction;

/**
 * This class represents the array specification abbreviation syntax
 * in .ext files.  This occurs in two places
 * <ul>
 *     <li> use lines: use[xlo,xhi,xsep_centimicron][ylo,yhi,ysep_centimicron]
 *     <li> merge lines: cell[xlo:xhi,ylo:yhi]/node
 * </ul>
 *
 * The class contains the following information:
 * <ul>
 *     <li> before: the string before the array spec opening bracket
 *     <li> after: the string before the array spec closing bracket
 *     <li> xlo: the min x index
 *     <li> xhi: the max x index
 *     <li> ylo: the min y index
 *     <li> yhi: the max y index
 * </ul>
 * The constructor is passed all these data, and accessors are provided for
 * them.  There is one additional method: {@link #genName} 
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ArraySpec {
    private final String before;
    private final String after;
    private final int xlo, xhi, ylo, yhi;

    /**
     * True if the given coord should be treated as an array.
     **/
    private final boolean xIsArray, yIsArray;

    /**
     * Class constructor.
     * 
     * @throws IllegalArgumentException  
     **/
    public ArraySpec(final String before,
                     final String after,
                     int xlo, int xhi, final boolean xIsArray,
                     int ylo, int yhi, final boolean yIsArray) {
        if (xlo > xhi) {
            // swap them
            int t = xhi;
            xhi = xlo;
            xlo = t;
        }

        if (ylo > yhi) {
            // swap them
            int t = yhi;
            yhi = ylo;
            ylo = t;
        }

        if (!xIsArray && (xlo != xhi))
            throw new IllegalArgumentException(
                    "You said x wasn't arrayed, but it is.");

        if (!yIsArray && (ylo != yhi))
            throw new IllegalArgumentException(
                    "You said y wasn't arrayed, but it is.");

        this.before = before;
        this.after = after;
        this.xlo = xlo;
        this.xhi = xhi;
        this.xIsArray = xIsArray;
        this.ylo = ylo;
        this.yhi = yhi;
        this.yIsArray = yIsArray;
    }

    /**
     * Generates the (x,y)-th name.  Examples:
     * new ArraySpec("foo", "bar", 1, 7, 3, 5).genName(2, 4)
     *   => foo[4][2]bar
     * new ArraySpec("foo", "bar", 1, 7, 3, 3).genName(2, 3)
     *   => foo[2]bar
     * new ArraySpec("foo", "bar", 1, 1, 3, 5).genName(1, 4)
     *   => foo[4]bar
     * new ArraySpec("foo", "bar", 1, 1, 3, 3).genName(1, 3)
     *   => foobar
     * new ArraySpec("foo", "bar", 1, 1, 3, 5).genName(1, -1)
     *   => throws IllegalArgumentException
     *
     * @throws IllegalArgumentException
     **/
    public String genName(final int x, final int y) {
        if (x < xlo)
            throw new IllegalArgumentException("x < xlo: " + x + "<" + xlo);
        else if (x > xhi)
            throw new IllegalArgumentException("x > xhi: " + x + ">" + xhi);
        else if (y < ylo)
            throw new IllegalArgumentException("y < ylo: " + y + "<" + ylo);
        else if (y > yhi)
            throw new IllegalArgumentException("y > yhi: " + y + ">" + yhi);

        final String xs = !xIsArray ? "" : "[" + x + "]";
        final String ys = !yIsArray ? "" : "[" + y + "]";

        return before + ys + xs + after;
    }

    public void mapNames(final UnaryAction f) {
        for (int ix = xlo; ix <= xhi; ++ix)
            for (int iy = ylo; iy <= yhi; ++iy)
                f.execute(genName(ix, iy));
    }

    public String getBeforeString() {
        return before;
    }

    public String getAfterString() {
        return after;
    }

    public boolean getXIsArray() {
        return xIsArray;
    }

    public int getXLo() {
        return xlo;
    }

    public int getXHi() {
        return xhi;
    }

    public int getXSize() {
        return xhi - xlo + 1;
    }

    public boolean getYIsArray() {
        return yIsArray;
    }

    public int getYLo() {
        return ylo;
    }

    public int getYHi() {
        return yhi;
    }

    public int getYSize() {
        return yhi - ylo + 1;
    }

    /**
     * Return a string representation for debugging only.
     **/
    public String toString() {
        return before + "["
            + xlo + ":" + xhi + ":" + xIsArray + ","
            + ylo + ":" + yhi + ":" + yIsArray + "]"
            + after;
    }
}
