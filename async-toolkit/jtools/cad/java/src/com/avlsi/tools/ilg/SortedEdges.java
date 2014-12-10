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
import com.avlsi.tools.ilg.Poly;
import com.avlsi.tools.ilg.Layer;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/** Speeds up some polygonal operations by holding a sorted list of edges, either
  * all horizontal or all vertical, sorted by distance from the parallel axis. **/
public class SortedEdges {
	/** The sorted list of edges. **/
	public Vector edges = new Vector();
	/** The layer that the edges came from. **/
	public Layer layer;
	/** Do we hold vertical or horizontal edges. **/
	public boolean horiz;
	/** Constructor, takes input layer and whether to hold horizontal or vertical edges. **/
	public SortedEdges(Layer l, boolean h) { layer=l; horiz=h; }
	/** Gets a sorted edge by index. **/
	public Edge Get(int i) { return (Edge)edges.elementAt(i); }
	/** The count of sorted edges. **/
	public int Size() { return edges.size(); }
	/** Adds an edge in sorted order. **/
	public void Add(Edge e) {
		int d = GetIndexAxis(e);
		int index=GetIndex(d);
		if (index<Size() && d>GetIndexAxis(Get(index))) { index++; }
		edges.insertElementAt(e, index);
	}
	/** Remove the given edge. **/
	public void Delete(Edge e) { edges.removeElement(e); }
	/** Remove an edge by index. **/
	public void Delete(int i) { edges.removeElementAt(i); }
	/** Returns the index (via binary search) that an edge should be at, based on its IndexAxis. **/
	public int GetIndex(int d) {
		int s=0, e=Size()-1, m, dif;
		while (s<e) {
			m=(s+e)/2;
			dif = d-GetIndexAxis(Get(m));
			if (dif<0) {
				e=m-1;
			} else if (dif>0) {
				s=m+1;
			} else { break; }
		}
		m=(s+e)/2;
		return m;
	}
	/** Empty the edge list. **/
	public void Clear() { edges.removeAllElements(); }
	/** Removes edges that have been marked degenerate. **/
	public void RemoveDegenerates() {
		for (int e=Size()-1; e>=0; e--) {
			Edge cur = Get(e);
			if (cur.dir<0) {
				Delete(e);
			}
		}
	}
	/** Removes any perpendicular edges and puts them in the opposite edge list. **/
	public void MoveOrthogonalEdges() {
		for (int e=Size()-1; e>=0; e--) {
			Edge cur = Get(e);
			if (horiz!=Util.DirIsHorizontal(cur.dir)) {
				Delete(e);
				layer.InsertEdge(cur);
			}
		}
	}
	/** Searches in direction dir away from the given edge (up to distance max)
	  * for other edges that the given edge would impact,
	  * returns the closest edge distance or max if none are found. **/
	public int GetNearestEdgeDist(Edge e, int dir, int max) {
		Edge near = GetNearestEdge( e, dir, max);
		if (near==null) { return max; }
		return Math.abs(GetIndexAxis(e)-GetIndexAxis(near));
	}
	/** Searches in direction dir away from the given edge (up to distance max)
	  * for other edges that the given edge would impact, returns the closest index. **/
	public int GetNearestEdgeIndex(Edge e, int dir, int max) {
		Edge ret=null, cur;
		int d, l=Size(), m, dif;
		if (l<1) { return -1; }
		d=GetIndexAxis(e);
		m=GetIndex(d);
		if (dir<2) {
			// be sure to start at minimum index..
			while ((m>0) && (d>=GetIndexAxis(Get(m)))) { m--; }
			// step up
			while (m<l) {
				cur=Get(m);
				dif=GetIndexAxis(cur)-d;
				if (dif>=max) { return -1; }
				if (e!=cur && dif>=0 && e.Spans(cur)) { return m; }
				m++;
			}
		} else {
			// be sure to start at maximum index..
			while ((m<(l-1)) && (d<=GetIndexAxis(Get(m)))) { m++; }
			// step down
			while (m>=0) {
				cur=Get(m);
				dif=d-GetIndexAxis(cur);
				if (dif>=max) { return -1; }
				if (e!=cur && dif>=0 && e.Spans(cur)) { return m; }
				m--;
			}
		}
		return -1;
	}
	/** Searches in direction dir away from the given edge (up to distance max)
	  * for other edges that the given edge would impact, returns the closest edge. **/
	public Edge GetNearestEdge(Edge e, int dir, int max) {
		int index = GetNearestEdgeIndex(e, dir, max);
		if (index<0) { return null; }
		return Get(index);
	}
	/** See Edge.GetIndexAxis. **/
	public int GetIndexAxis(Edge e) { return horiz ? e.GetY() : e.GetX(); }
	/** Splits any abutting edges. Prepares polys for merge. **/
	public boolean Split() {
		Edge edges[] = GetAbuts(true);
		if ((edges!=null) && (edges.length>1)) {
			SplitPolys(edges[0], edges[1]);
			return true;
		}
		return false;
	}
	/** Merges any abutting edges, and their associated polygons. **/
	public boolean Merge() {
		Edge edges[] = GetAbuts(false);
		if ((edges!=null) && (edges.length>1)) {
			//System.out.println("Merging polygons: ");
			//edges[0].p.Dump(System.out);
			//System.out.println("And: ");
			//edges[1].p.Dump(System.out);
			StitchPolys(edges[0], edges[1]);
			//System.out.println("for "+layer.Size()+" on layer: "+layer.name);
			return true;
		}
		return false;
	}
	/** Returns a list of abutting edges. **/
	public Edge[] GetAbuts(boolean samePoly) {
		int start=0, d, dt, len = Size();
		if (len>0) {
			dt=d=GetIndexAxis(Get(0));
			for (int i=0; i<len; i++) {
				dt=GetIndexAxis(Get(i));
				if (d!=dt) {
					if ((i-1)>start) {
						Edge edges[] = CheckAbuts(start, i, samePoly);
						if ((edges!=null) && (edges.length>1)) { return edges; }
					}
					d=dt; start=i;
				}
			}
			if (start<(len-1) && (dt==GetIndexAxis(Get(start)))) {
				return CheckAbuts(start, len, samePoly);
			}
		}
		return null;
	}
	/** Checks within a range for abutting edges. **/
	public Edge[] CheckAbuts(int start, int end, boolean samePoly) {
		Edge e1, e2;
		for (int i=start; i<end; i++) {
			e1=Get(i);
			for (int j=i+1; j<end; j++) {
				e2=Get(j);
				if (((e1.p==e2.p)==samePoly) && e1.Spans(e2)) {
					//if (e1==e2) { System.out.println("Bogus!!!"); }
					Edge edges[] = {e1, e2};
					return edges;
				}
			}
		}
		return null;
	}
	/** Merges two polygons at the given edges. NOTE: The edges must abut! **/
	public void StitchPolys(Edge e1, Edge e2) {
		//System.out.println("Abutting edges :"); e1.Dump(); e2.Dump();
		Poly src=e2.p, dst=e1.p;
		int lo;
		boolean reverse = e1.dir!=Util.ReverseDirection(e2.dir);
		Edge ne1, ne2;
		if (horiz) { lo = Math.max(e1.GetX(), e2.GetX()); }
		else { lo = Math.max(e1.GetY(), e2.GetY()); }
		e1=SplitPolyAt(e1, lo);
		e2=SplitPolyAt(e2, lo);
		int ssz=src.Size(), dsz=dst.Size();
		layer.shapes.removeElement(src);
		int at=e1.index, from=e2.index;
		if (reverse) {
			//System.out.println("Reverse Edge Stitching...."+lo+" "+
			//  (horiz?(e1.P0().x):(e1.P0().y))+" "+(horiz?(e2.P0().x):(e2.P0().y)));
			for (int i=0; i<ssz; i++) {
				dst.Insert(at+i, src.Get(from), src.GetEdge(from));
				from--;
				if (from<0) { from=ssz-1; }
			}
		} else {
			for (int i=0; i<ssz; i++) {
				dst.Insert(at+i, src.Get(from), src.GetEdge(from));
				from=(from+1)%ssz;
			}
		}
		//System.out.println("Resulting polygon: "); dst.Dump(System.out);
		boolean did=true;
		while (did) { did=dst.DeleteIfDegenerate(); }
		layer.hEdges.RemoveDegenerates();
		layer.vEdges.RemoveDegenerates();
		layer.MoveOrthogonalEdges();
	}
	/** Splits an edge in two at a given point (along its long axis). **/
	public Edge SplitPolyAt(Edge e, int at) {
		Edge e1=e.SplitAt(at);
		if (e1!=e) { layer.InsertEdge(e1); }
		if (horiz && (at!=e.P0().x)) {
			Edge ne=e.NextEdge();
			if (at==ne.P0().x) { e=ne; }
			else { e=e.PrevEdge(); }
		}
		else if ((!horiz) && (at!=e.P0().y)) {
			Edge ne=e.NextEdge();
			if (at==ne.P0().y) { e=ne; }
			else { e=e.PrevEdge(); }
		}
		return e;
	}
	/** Splits two edges of two polygons to create flush abutting edges. **/
	public void SplitPolys(Edge e1, Edge e2) {
		//System.out.println("Splitting on Abutting edges :"); e1.Dump(); e2.Dump();
		Poly src = e1.p;
		int lo;
		if (horiz) { lo = Math.max(e1.GetX(), e2.GetX()); }
		else { lo = Math.max(e1.GetY(), e2.GetY()); }
		e1=SplitPolyAt(e1, lo);
		e2=SplitPolyAt(e2, lo);
		Poly newPoly = new Poly();
		int min = Math.min(e1.index, e2.index);
		int max = Math.max(e1.index, e2.index);
		for (int i=min; i<max; i++) {
			newPoly.Insert(i-min, src.Get(min), src.GetEdge(min));
			src.Delete(min);
		}
		for (int i=0; i<newPoly.Size(); i++) {
			newPoly.GetEdge(i).UpdateDir();
		}
		layer.shapes.addElement(newPoly);
		boolean did=true;
		while (did) { did=src.DeleteIfDegenerate(); }
		did=true;
		while (did) { did=newPoly.DeleteIfDegenerate(); }
		layer.hEdges.RemoveDegenerates();
		layer.vEdges.RemoveDegenerates();
		//layer.MoveOrthogonalEdges();
	}
}
