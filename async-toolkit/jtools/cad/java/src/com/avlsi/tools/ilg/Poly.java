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

import com.avlsi.tools.ilg.Util;
import com.avlsi.tools.ilg.ILG;
import com.avlsi.tools.ilg.Edge;
import com.avlsi.tools.ilg.Layer;
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/**
 * A polygon class, optimized to deal with rectangular polygons
 * (i.e all angles are multiples of 90 degrees).
 * Polygons are encoded counter clockwise, so a clockwise one is considered a hole.
 * NOTE: Some operations will malfunction if the polygons are not rectangular.
 **/
public class Poly {
	/** The vertices of the polygon. Last point may be repeated. **/
	public Vector points=new Vector();
	/** Edge records. May or may not be filled out. **/
	public Vector edges=new Vector();

	public Poly() {}
	/** Constructor to make a rectange with given origin and size. **/
	public Poly(Point at, int size) {
		Insert(0, new Point(at.x-size, at.y-size), null);
		Insert(1, new Point(at.x+size, at.y-size), null);
		Insert(2, new Point(at.x+size, at.y+size), null);
		Insert(3, new Point(at.x-size, at.y+size), null);
		CreateEdges();
	}
	/** Copy constructor. **/
	public Poly(Poly p) {
		for (int i=0; i<p.Size(); i++) {
		   points.addElement(new Point(p.Get(i)));
		}
		CreateEdges();
	}
	/** Number of vertices **/
	public int Size() { return points.size(); }
	/** Get the vertex at the specified index. **/
	public Point Get(int i) { return (Point)points.elementAt(i); }
	/** Set the vertex at the specified index. **/
	public void Set(int i, Point p) { points.setElementAt(p, i); }
	/** Remove the vertex at the specified index. **/
	public void Delete(int index) {
		points.removeElementAt(index);
		edges.removeElementAt(index);
		int pSz=Size();
		if (pSz>0) {
			for (int i=index; i<pSz; i++) {
				GetEdge(i).index--;
			}
			GetEdge(index%pSz).UpdateDir();
			GetEdge((index-1+pSz)%pSz).UpdateDir();
		}
	}
	/** Insert a vertex at the specified index. **/
	public void Insert(int index, Point p, Edge e) {
		if (e!=null) {
			int pSz=Size();
			for (int i=index; i<pSz; i++) {
				GetEdge(i).index++;
			}
			edges.insertElementAt(e, index);
			e.p=this; e.index=index;
		}
		points.insertElementAt(p, index);
	}
	/** Reverse the ordering of the points.
	    Turns a hole into a polygon or vice versa. **/
	public void Reverse() {
		int sz=Size(), half=sz/2, j;
		Point p;
		Edge e;
		for (int i=0; i<half; i++) {
			j=sz-(i+1);
			// swap around points,
			p=Get(i);
			points.setElementAt(Get(j), i);
			points.setElementAt(p, j);
			if (edges.size()>0) {
				// swap edges and reverse their dir...
				e=GetEdge(i);
				edges.setElementAt(GetEdge(j), i);
				edges.setElementAt(e, j);
				e.index=j; e.dir=Util.ReverseDirection(e.dir);
				e=GetEdge(i);
				e.index=i; e.dir=Util.ReverseDirection(e.dir);
			}
		}
	}
	/** Get the edge at the specified vertex. **/
	public Edge GetEdge(int i) { return (Edge)edges.elementAt(i); }
	/** Read the polygon from a stream in lisp-like format. **/
	public Poly Read(StreamTokenizer st) {
		//(Point, Point,...)
		if (!Util.ReadSymbol(st, "(")) { return null; }
		Poly p = new Poly();
		while (true) {
			Point cur=Util.ReadPoint(st);
			if (cur==null) { break; }
			p.points.addElement(cur);
		}
		Util.ReadSymbol(st, ")");
		return p;
	}
	/** Write the polygon to a stream in lisp-like format. **/
	public void Dump (PrintStream out) {
		out.print("(");
		for (int i=0; i<Size(); i++) {
			Point p = Get(i);
			out.print("("+p.x+" "+p.y+")");
		}
		out.println(")");
	}
	/** Fill out the edges array from the points array. **/
	public void CreateEdges() {
		edges.removeAllElements();
		for (int i=0; i<Size(); i++) {
			edges.addElement(new Edge(this, i));
		}
	}
	/** Removes co-linear or co-incident points. Requires edge array. **/
	public boolean DeleteIfDegenerate() {
		boolean cur, did=false;
		for (int i=0; i<edges.size(); i++) {
			cur=GetEdge(i).DeleteIfDegenerate();
			did=cur||did;
		}
		return did;
	}
    /** Returns directed area. 
        Holes have negative area in the calculation. **/
	public long Area() {
		int last = Size()-1;
		if (last<0) { return 0; }
		long area=0;
		int miny=Get(last).y;
		for (int i=last; i>0; i--) {
			miny=Math.min(miny, Get(i).y);
		}
		Point p0=Get(0), p1=null;
		for (int i=last; i>0; i--) {
			p1=Get(i);
			area+=(p1.x-p0.x)*((long)(p0.y-miny));
			p0=p1;
		}
		p1=Get(0); //p0=Get(0);
		if (p0!=p1) {
			area+=(p1.x-p0.x)*((long)(p0.y-miny));
		}
		return area;
	}

	/** Point in polygon test. Works on non-rectangular polys and with holes. **/
	//From http://www.ecse.rpi.edu/Homepages/wrf/geom/pnpoly.html
	// Based on Simulation Of Simplicity and Jordan Curve Theorem
	//int ptInPoly(int npol, float *xp, float *yp, float x, float y) {
	//  int i, j, c = 0;
	//  for (i = 0, j = npol-1; i<npol; j = i++) {
	//    if (
	//        (
	//         ((yp[i]<=y) && (y<yp[j])) ||
	//         ((yp[j]<=y) && (y<yp[i]))
	//        ) &&
	//        (x-xp[i] <
	//          (xp[j]-xp[i])*(y-yp[i]) /
	//          (yp[j]-yp[i])
	//        )
	//       )
	//      c = !c;
	//  }
	//  return c;
	//}
	boolean PointInside(Point p) {
		int len = Size();
		boolean in=false;
		for (int i=0, j=len-1; i<len; j=i++) {
			int x=p.x, y=p.y, xi=Get(i).x, xj=Get(j).x, yi=Get(i).y, yj=Get(j).y;
			int yji=yj-yi, xji=xj-xi, x_i=x-xi, y_i=y-yi;
			if (((yi<=y && y<yj) || (yj<=y && y<yi)) &&
				(((yji>0) && (x_i*yji<xji*y_i)) || ((yji<0) && (x_i*yji>xji*y_i)))) {
				in=!in;
			}
		}
		return in;
	}
	/** Inserts a hole (reversed polygon) into this poly.
	    The hole must lie STRICTLY within this poly. **/
	public void MergeHole(Poly hole) {
		int i, x, y, x0, x1, y0, hi, hx, hy, pi=0, py = Integer.MAX_VALUE;
		Point p;
		boolean aligned = false;
		// find top edge of hole
		hi=0; hx=hole.Get(0).x; hy=hole.Get(0).y;
		for (i=hole.Size()-1; i>=0; i--) {
			p=hole.Get(i);
			if (p.y>hy) {hi=i; hy=p.y; hx=p.x;}
		}
		//   find closest edge above the hole
		for (i=Size()-1; i>=0; i--) {
			Edge e=GetEdge(i);
			x0=e.GetX(); x1=e.GetMaxX();
			y0=e.GetY();
			if (Util.DirIsHorizontal(e.dir)) {
//System.out.println("Span ("+x0+","+x1+")@"+y0+" vs "+hx+","+hy+"   "+py);
				if ((x0<=hx) && (x1>=hx) && (y0>hy) && (y0<py)) {
//System.out.println("matched");
					pi=e.index; py=y0;
					aligned=((x0==hx) || (x1==hx));
				}
			}
		}
		Poly cp = new Poly(this);
try {
		//   make double edge up to it, and insert points
		if (!aligned) {
			// skip one point if one already directly above
			Insert(++pi, new Point(hx, py), null);
		} else if (hx==Get(pi+1).x) {
			++pi;
			if (pi>Size()) { pi=0; }
		}
		for(i=hi; i<hole.Size(); i++) {
			Insert(++pi, hole.Get(i), null);
		}
		for(i=0; i<=hi; i++) {
			Insert(++pi, hole.Get(i), null);
		}
		Insert(++pi, new Point(hx, py), null);
		CreateEdges();
} catch (Exception ex) {
System.out.println("BOGUS MERGE");
Dump(System.out); cp.Dump(System.out); hole.Dump(System.out);
}
	}
}
