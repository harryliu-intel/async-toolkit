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

package com.avlsi.util.math;

import com.avlsi.util.exception.AssertionFailure;

/**
 * Class representing common math operations.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/

public final class Math {
    /**
     * DO NOT USE, this is a static class.
     * @deprecated Do not use, this is a static class.
     **/
    Math() {
        throw new AssertionFailure("Math self-instantiation");
    }

    /**
     * put 32 bits of x into low 32 bits of result without sign extending
     **/
    public static long Int2UL(int x)
    {
        return (long)x & 0xFFFFFFFFL;
    }

    /**
     * put 32 bits of lo into low bits of result, 32 bits of hi into 
     *  high bits of result (no sign extension)
     **/
    public static long IntInt2UL(int lo, int hi)
    {
        return ((long)lo & 0xFFFFFFFFL) | ((long)hi << 32);
    }

    /**
     * bottom 32 bits of long x
     **/
    public static int UL_lo(long x)
    {
        return (int)(x & 0xFFFFFFFFL);
    }

    /** 
     * top 32 bits of long x
     **/
    public static int UL_hi(long x)
    {
        return (int)(x>>32);
    }

    /** 
     * x rotated (i.e. bits shifted off replaced on other end) left by a bits.
     **/
    public static int ROL(int x, int a)
    {
        long lx = Int2UL(x);
        lx += lx<<32;
        return UL_hi(lx<<a);
    }

    /** 
     * x rotated (i.e. bits shifted off replaced on other end) right by a bits.
     **/
    public static int ROR(int x, int a)
    {
        long lx = Int2UL(x);
        lx += lx<<32;
        return UL_lo(lx>>a); 
    }

    /**
     * low 8 bits, extract unsigned (C) char encoded in int.
     **/
    public static int XUCH(int x)
    {
        return x&0xFF;
    }

    /**
     * low 8 bits, extract signed (C) char encoded in int.
     **/
    public static int XCH(int x)
    {
        return (x<<24)>>24;
    }

    /**
     * low 16 bits, extract unsigned short encoded in int.
     **/
    public static int XUSH(int x)
    {
        return x&0xFFFF;
    }

    /**
     * low 16 bits, extract signed short encoded in int.
     **/
    public static int XSH(int x)
    {
        return (x<<16)>>16;
    }

    /**
     * A "maximum" on two integers that handles wrapping.
     *
     * i.e. 2^30 is less than -2^30
     **/
    public static int wrappingMax(int x, int y) {
        if ((x - y) > 0) {
            return x;
        } else {
            return y;
        }
    }

    /**
     * Is x a power of 2?
     *
     * 0 is a power of 2.
     **/
    public static boolean isPowerOf2(int x) {
        return (x & (x-1)) == 0;
    }
}

