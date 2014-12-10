/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.ilg;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.LinkedList;
import java.util.Hashtable;

import com.avlsi.tools.ilg.ILG;
import com.avlsi.tools.ilg.Edge;
import com.avlsi.tools.ilg.Poly;
import com.avlsi.tools.ilg.Layer;
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/** Class to hold many utility functions for the ILG classes. **/
public class Util {
	/** Calculate the length of a vertical or horizontal edge. **/
    public static int LineLength(Point a, Point b) {
        return Math.max( Math.abs(a.x-b.x), Math.abs(a.y-b.y));
    }
    /** Returns a direction for the given line segment. **/
    public static int GetLineDir(Point a, Point b) {
        if (b.x>a.x) { return 0; } // R
        if (b.y>a.y) { return 1; } // U
        if (b.x<a.x) { return 2; } // L
        if (b.y<a.y) { return 3; } // D
        return 0; // BAD if we get here
    }
    /** Takes 3 points and assumes they are vertical or horizontal,
      * returns true if they are counter-clockwise. **/
    public static boolean IsCCW (int dira, int dirb) {
		int dif=(4+ReverseDirection(dira)-dirb)%4;
        return (dif==3);
    }
	/** Returns the opposite direction from the given direction. **/
    public static int ReverseDirection(int d) { return ((d+2)%4); }
	/** Returns the inside direction for an edge with the given direction. **/
    public static int GetInsideDir(int d) { return ((d+1)%4); }
	/** Returns the outside direction for an edge with the given direction. **/
    public static int GetOutsideDir(int d) { return ((d+3)%4); }
    /** Returns true if the given direction is horizontal. **/
    public static boolean DirIsHorizontal(int d) { return (d==0) || (d==2);}
	/** Returns true if the input 1D ranges overlap.  **/
	public static boolean SpansOverlap(int a0, int a1, int b0, int b1) {
		int lo0=Math.min(a0, a1), hi0=Math.max(a0, a1);
		int lo1=Math.min(b0, b1), hi1=Math.max(b0, b1);
		if (hi1<=lo0) { return false; }
		if (hi0<=lo1) { return false; }
		return true;
	}
	/** Reads a point from the input stream in lisp-like format. **/
	public static Point ReadPoint(StreamTokenizer st) {
		//(X Y)
		if (!ReadSymbol(st, "(")) { return null; }
		Point p = new Point();
		p.x=(int)ReadNumber(st);
		p.y=(int)ReadNumber(st);
		ReadSymbol(st, ")");
		return p;
	}
	/** Returns true if a given token is next on the input stream. **/
	public static boolean ReadSymbol(StreamTokenizer st, String sym) {
		int cur;
		try {
			cur=st.nextToken();
		} catch (IOException ex) {}
		//System.out.println("Read Token: "+st.sval+" expected: "+sym);
		if (st.sval.startsWith(sym)) {
            if (st.sval.length()>sym.length()) {
    			st.sval=st.sval.substring(sym.length());
			    st.pushBack();
            }
			return true;
        }
        st.pushBack();
        return false;
	}
	/** Reads a string from the input stream. **/
	public static String ReadString(StreamTokenizer st) {
		try {
			int cur=st.nextToken();
		} catch (IOException ex) {}
		//System.out.println("Read string: "+st.sval);
		return st.sval;
	}
	/** Reads a floating point number from the input stream. **/
	public static double ReadNumber(StreamTokenizer st) {
        double val =0.0;
		try {
			int cur=st.nextToken();
			val = Double.parseDouble(st.sval);
		} catch (Exception ex) {
            if (st.sval==null) {
                val=st.nval;
            }
            //System.out.println("Read number exception ");
            //ex.printStackTrace();
        }
		//System.out.println("Read number: "+val+" ("+st.sval+")");
        return val;
		//System.out.println("Read number: "+st.nval);
		//return st.nval;
	}
}
