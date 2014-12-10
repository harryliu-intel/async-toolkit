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
import com.avlsi.tools.ilg.Poly;
import com.avlsi.tools.ilg.Layer;
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/** A convienience class to treat a Poly as a list of edges.
    Completely depends on the data in the Poly class. **/
public class Edge {
	/** The poly we came from. **/
	public Poly p;
	/** Vertex index and direction for the edge **/
	public int index, dir;
	public Edge(Poly poly, int i) {
		p = poly;
		index=i;
		dir=CalcDir();
	}
	/** Print debugging info. **/
	public void Dump() {
		boolean hor=Util.DirIsHorizontal(dir);
		Point p=P0();
		System.out.print((hor?"Horiz":"Vert ")+" edge ");
		System.out.print("("+p.x+" "+p.y+") ");
		p=P1();
		System.out.print("("+p.x+" "+p.y+") ");
		System.out.println("length "+Length());
	}
	/** Returns initial point. **/
	public Point P0() { return p.Get(index); }
	/** Returns final point. **/
	public Point P1() { return p.Get((index+1) % p.Size()); }
	/** Returns the next edge in the poly with wrapping. **/
	public Edge NextEdge() { return p.GetEdge((index+1) % p.Size()); }
	/** Returns the previous edge in the poly with wrapping. **/
	public Edge PrevEdge() { return p.GetEdge((index-1+p.Size()) % p.Size()); }
	/** The positive (or 0) length of the line. **/
	public int Length() { return Util.LineLength(P0(), P1()); }
	/** Returns the direction of the line **/
	public int CalcDir() { return Util.GetLineDir(P0(), P1()); }
	/** Updates the direction of the line **/
	public boolean UpdateDir() {
		int oldDir=dir;
		dir=CalcDir();
		return dir!=oldDir;
	}
	//public boolean UpdateNeighbors() {
	//    boolean cur, did = false;
	//    int pSz=p.Size();
	//    cur=p.GetEdge((index-1+pSz) % pSz).UpdateDir();
	//    did=cur||did;
	//    cur=p.GetEdge((index+1+pSz) % pSz).UpdateDir();
	//    did=cur||did;
	//    return did;
	//}
	/** The minimum x value on the edge. **/
	public int GetX() { return Math.min(P0().x, P1().x); }
	/** The minimum y value on the edge. **/
	public int GetY() { return Math.min(P0().y, P1().y); }
	/** The maximum x value on the edge. **/
	public int GetMaxX() { return Math.max(P0().x, P1().x); }
	/** The maximum y value on the edge. **/
	public int GetMaxY() { return Math.max(P0().y, P1().y); }
	/** The distance of the edge from the parallel axis. **/
	public int GetIndexAxis() { return Util.DirIsHorizontal(dir) ? GetY() : GetX(); }
	/** The minimum distance of the edge from the perpendicular axis. **/
	public int GetMin() { return Util.DirIsHorizontal(dir) ? GetX() : GetY(); }
	/** The maximum distance of the edge from the perpendicular axis. **/
	public int GetMax() { return Util.DirIsHorizontal(dir) ? GetMaxX() : GetMaxY(); }
	/** The midpoint of the line **/
	public int GetMid() { return (GetMin()+GetMax())/2; }
	/** If two edges overlap. **/
	public boolean Spans(Edge e) {
		if (Util.DirIsHorizontal(dir)==Util.DirIsHorizontal(e.dir)) {
			if (Util.DirIsHorizontal(dir)) {
				return Util.SpansOverlap(P0().x, P1().x, e.P0().x, e.P1().x);
			} else {
				return Util.SpansOverlap(P0().y, P1().y, e.P0().y, e.P1().y);
			}
		} else { System.out.println("Checking span on orthogonal edges."); }

		return false;
	}
	/** Cuts an edge along its axis at point d. Returns new edge. **/
	public Edge SplitAt(int d) {
		boolean hor=Util.DirIsHorizontal(dir);
		Edge ne = this;
		if (hor) {
			if ((d!=GetX()) && (d!=GetMaxX())) {
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(d, GetY()), ne);
			}
		} else {
			if ((d!=GetY()) && (d!=GetMaxY())) {
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(GetX(), d), ne);
			}
		}
		return ne;
	}
	/** Inserts a completely new edge, so that prev or next can grow
	  * and still leave orthogonal edges. **/
	public Edge DoubleSplitAt(int d) {
		boolean hor=Util.DirIsHorizontal(dir);
		Edge ne = this;
		if (hor) {
			if ((d!=GetX()) && (d!=GetMaxX())) {
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(d, GetY()), ne);
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(d, GetY()), ne);
			}
		} else {
			if ((d!=GetY()) && (d!=GetMaxY())) {
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(GetX(), d), ne);
				ne = new Edge(p, index);
				p.Insert(index+1, new Point(GetX(), d), ne);
			}
		}
		return ne;
	}
	/** Cuts an edge so it can flow around another edge. Returns the cut edges. **/
	Vector SplitOnBarrier(Edge bar) {
		boolean hor=Util.DirIsHorizontal(dir);
		Vector ret = new Vector();
		int e0, e1, b0, b1;
		Edge ne;
		if (hor) {
			e0=GetX(); e1=GetMaxX();
			b0=bar.GetX(); b1=bar.GetMaxX();
		} else {
			e0=GetY(); e1=GetMaxY();
			b0=bar.GetY(); b1=bar.GetMaxY();
		}
		if (b1<e1) {
//System.out.println("  Split HI "+b1+(hor?" Hor":" Vert"));
			DoubleSplitAt(b1);
			ne = NextEdge();
			ret.add(ne);
			ret.add(ne.NextEdge());
		}
		if (b0>e0) {
//System.out.println("  Split LO "+b0+(hor?" Hor":" Vert"));
			DoubleSplitAt(b0);
			ne = NextEdge();
			ret.add(ne);
			ret.add(ne.NextEdge());
		}
		return ret;
	}
	/** Grows an edge in a given distance. Dist should be perpendicular to the edge. **/
	public boolean Grow(int dist) {
		int x=0, y=0;
		if (dir==0) { y=-dist; }
		if (dir==1) { x=dist;  }
		if (dir==2) { y=dist;  }
		if (dir==3) { x=-dist; }
		Point cp=P0();
		cp.x+=x; cp.y+=y;
		cp=P1();
		cp.x+=x; cp.y+=y;
		return true;
	}
	/** Removes this edge and returns true, if it is coincident or colinear with a neighbor. **/
	public boolean DeleteIfDegenerate() {
		// delete any inline points/edges..
		// i.e. zero length or neighboring edge angle is 0 or 180
		int pSz=p.Size();
		Edge prev=p.GetEdge((index-1+pSz) % pSz);
		prev.UpdateDir();
		UpdateDir();
		int prevDir=prev.dir;
		if ((Length()==0) || (Util.DirIsHorizontal(dir)==Util.DirIsHorizontal(prevDir))){
//System.out.println("Degenerate edge deleted");
			dir=-1;
			p.Delete(index);
			return true;
		}
		return false;
	}
}

