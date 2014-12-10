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
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;


/** Represents a chip process layer, stored as a list of polygons. **/
public class Layer {
	/** The name... "poly", "nwell", etc. **/
	public String name;
	/** The layer's location (depth) in the list of layers. **/
	public int index;
	/** Does this layer grow or is it a boundary layer. **/
	public boolean fixed = true;
	/** Extra information, if this layer represents plug positions for another layer. **/
	public WellPlugData wellData = null;
	/** List of possible or chosent plug positions. **/
	public Vector plugEntries = null;
	/** The list of Polys that define this layer. **/
	public Vector shapes=new Vector();
	/** Sorted (on x) list of vertical edges to speed up some operations. **/
	public SortedEdges vEdges=new SortedEdges(this, false);
	/** Sorted (on y) list of horizontal edges to speed up some operations. **/
	public SortedEdges hEdges=new SortedEdges(this, true);


	/** The number of shapes in the layer. **/
	public int Size() { return shapes.size(); }
	/** Returns the Poly at position i in the shapes list. **/
	public Poly Get(int i) { return (Poly)shapes.elementAt(i); }
	/** Removes all shapes from the layer. **/
	public void Clear() {
		shapes.clear();
		vEdges.Clear();
		hEdges.Clear();
	}
	/** Imports the shapes from another layer. **/
	public void CopyPolys(Layer l) {
		for (int p=0; p<l.Size(); p++) {
			Poly poly=new Poly(l.Get(p));
			shapes.addElement(poly);
			AddEdges(poly);
		}
	}
	/** Adds a shape to this layer. **/
	public void Add(Poly p) {
		shapes.addElement(p);
		Update();
	}
	/** Reads the layer from a stream in lisp-like format **/
	public Layer Read(StreamTokenizer st) {
		// ("name", grow {t/nil}, Poly, Poly..)
		if (!Util.ReadSymbol(st, "(")) { return null; }
		Layer layer = new Layer();
		layer.name=Util.ReadString(st);
		String symb = Util.ReadString(st);
		layer.fixed=!(symb.equals("t"));
		if (symb.equals("(")) {
			layer.wellData = new WellPlugData();
			layer.wellData.ReadExtraData(st);
			Util.ReadSymbol(st, ")");
		}
//System.out.println("On layer "+name+" Extra Data is "+wellData);
		Util.ReadSymbol(st, "(");
		Poly dummy = new Poly();
		while (true) {
			Poly cur = dummy.Read(st);
			if (cur==null) { break; }
			if (cur.Area()<0) {
//System.out.println("reversed polygon on layer "+layer.name+" size "+cur.Size());
				cur.Reverse();
			}
			layer.shapes.addElement(cur);
		}
		Util.ReadSymbol(st, ")");
		Util.ReadSymbol(st, ")");
		layer.CreateEdges();
		return layer;
	}
	/** Dumps the layer to a stream in lisp-like format. **/
	public void Dump (PrintStream out) {
		out.println("(\""+name+"\" "+(fixed ? "nil":"t")+" (");
		for (int i=0; i<shapes.size(); i++) {
			out.print("  ");
			((Poly)shapes.elementAt(i)).Dump(out);
		}
		out.println("))");
		if (plugEntries!=null) {
			Poly poly = new Poly();
			int ind=0;
			out.println("(\"@"+wellData.wellName+"\" "+(fixed ? "nil":"t")+" (");
			for (int i=plugEntries.size()-1; i>=0; i--) {
				PlugEntry pe = (PlugEntry)plugEntries.elementAt(i);
				poly.Insert(ind++, pe.at, null);
				//for (int i=plugEntries.size()-1; i>=0; i--) {
				//	((Poly)pe.polys.elementAt(j)).Dump(out);
				//}
			}
			out.print("  ");
			poly.Dump(out);
			out.println("))");
		}
	}
	/** Dumps debugging info to stdout. **/
	public void DebugDump() {
		System.out.println("Layer "+name);
		System.out.println("Horizontal:");
		for (int i=0; i<hEdges.Size(); i++) {
			hEdges.Get(i).Dump();
		}
		System.out.println("Vertical:");
		for (int i=0; i<vEdges.Size(); i++) {
			vEdges.Get(i).Dump();
		}
	}

	/** Calculates the area covered by all polys in the layer. **/
	public long Area() {
		long area=0;
		for (int i=Size()-1; i>=0; i--) {
			area+=Get(i).Area();
		}
		return area;
	}

	/** Removes degenerate edges and recalculates the sorted edge lists. **/
	public void Update() {
		DeleteIfDegenerate();
		CreateEdges();
	}
	/** Clears and recalculates the sorted edge lists. **/
	public void CreateEdges() {
		vEdges.Clear();
		hEdges.Clear();
		for (int i=0; i<shapes.size(); i++) {
			AddEdges((Poly)shapes.elementAt(i));
		}
	}
	/** Imports a shape. **/
	public void AddEdges(Poly p) {
		p.CreateEdges();
		for (int j=0; j<p.Size(); j++) {
			InsertEdge(p.GetEdge(j));
		}
	}
	/** Puts a single edge into the propper sorted edge list. **/
	public void InsertEdge(Edge e) {
		if (Util.DirIsHorizontal(e.dir)) { hEdges.Add(e); }
		else { vEdges.Add(e); }
	}
	/** Removes and re-inserts a sorted edge who's direction may have changed. **/
	public void UpdateEdge(Edge e) {
		boolean horiz = Util.DirIsHorizontal(e.dir);
		e.UpdateDir();
		if (horiz) { hEdges.Delete(e); }
		else { vEdges.Delete(e); }
		InsertEdge(e);
	}
	/** Extends an edge in the given direction and updates the edge lists. **/
	public void Grow(Edge e, int dist) {
		Poly p=e.p;
		int sz = p.Size();
		//System.out.print("Grew Edge :\n    "); e.Dump();
		e.Grow(dist);
		UpdateEdge(e);
		//System.out.print("To :\n    "); e.Dump();
		UpdateEdge(p.GetEdge((sz+e.index-1)%sz));
		UpdateEdge(p.GetEdge((sz+e.index+1)%sz));
		DeleteIfDegenerate();
	}
	/** Checks all edges, removing degenerate edges and updates the edge lists. **/
	public void DeleteIfDegenerate() {
		for (int p=0; p<Size(); p++) {
			Poly pol=Get(p);
			boolean did=true;
			while (did) {
				did=pol.DeleteIfDegenerate();
			}
		}
		for (int p=Size()-1; p>=0; p--) {
			Poly pol = Get(p);
			if (pol.Size()<3) {
				for (int e=0; e<pol.Size(); e++) {
					pol.GetEdge(e).dir=-1;
				}
				shapes.removeElementAt(p);
			}
		}
		hEdges.RemoveDegenerates();
		vEdges.RemoveDegenerates();
		//CreateEdges();
	}
	/** Updates any edges whose direction have changed. **/
	public void MoveOrthogonalEdges() {
		hEdges.MoveOrthogonalEdges();
		vEdges.MoveOrthogonalEdges();
	}
	/** Merges any abutting edges. **/
	public boolean Merge() {
		boolean h = hEdges.Merge();
		boolean v = vEdges.Merge();
		return h||v;
	}
	/** Splits any abutting edges. **/
	public boolean Split() {
		boolean h = hEdges.Split();
		boolean v = vEdges.Split();
		return h||v;
	}
	/** Calculates min on two Integers, either of which may be null. **/
	public Integer SpecialMin(Integer a, Integer b) {
		if (a==null) { return b; }
		if (b==null || a.intValue()<b.intValue()) { return a; }
		return b;
	}
	/** Calculates the minimum point and distance from some origin (a well plug),
	  * given the minimum point and distance on a nearby edge.
	  * This should only be used on edges that were freshly split. **/
	public Point CalcSpecialDist(boolean horiz, int dist, int at, Edge newEdge) {
		int p, plo, phi, dlo, dhi, newAt;
		if (newEdge.Length()==0) {
			// zero length edges will be perp to the split, if they grow at all
			if (horiz) {
				p=newEdge.GetX(); newAt=newEdge.GetY();
			} else {
				p=newEdge.GetY(); newAt=newEdge.GetX();
			}
			return new Point(dist+Math.abs(at-p), newAt);
		} else {
			if (horiz) {
				plo=newEdge.GetX(); phi=newEdge.GetMaxX();
			} else {
				plo=newEdge.GetY(); phi=newEdge.GetMaxY();
			}
			if (plo<=at && phi>=at) {
				return new Point(dist, at);
			}
			dlo = Math.abs(plo - at); dhi = Math.abs(phi - at);
			if (dlo<dhi) {
				return new Point(dist+dlo, plo);
			} else {
				return new Point(dist+dhi, phi);
			}
		}
	}

	/** Grows an arbitrary rectangular polygon within the confines of this layer.
	  * The Poly grows up to distance dist, and grown indicates how much particular
	  * edges have already grown. NOTE: Input objects are modified by this function.
	  * The objects in grown are points keyed by edge, with the x representing
	  * distance grown, and y representing the closest point on the edges major axis. **/
	public void SquareGrowPoly(Poly p, int dist, LinkedList active, Hashtable grown) {
		Edge e, bar;
		Point di;
		int d, cur, grew;
		while (active.size() > 0) {
			e = (Edge)active.removeFirst();
			if (e.Length()>0) {
//System.out.println("Square Grow iteration (poly size "+p.Size()+")");
				cur = e.GetIndexAxis();
				di = (Point)grown.get(e);
				d = di.x;
				e.UpdateDir();
				int dir = e.dir;
				int out = Util.GetOutsideDir(dir);
				SortedEdges edges = (Util.DirIsHorizontal(dir)? hEdges : vEdges);
				bar = edges.GetNearestEdge(e, out, dist-d);
				if (bar!=null) { grew = Math.abs(cur-bar.GetIndexAxis()); }
				else { grew = Math.max(0, dist-d); }
				if (grew>0) {
					e.Grow(grew);
					Edge prev=e.PrevEdge(), next=e.NextEdge();
					prev.UpdateDir(); next.UpdateDir();
					// add perpendicular edges to grow list
					// (NOTE prev and next edge may not be perp after splits...)
					Point ed = (Point)grown.get(next);
					if (dist>ed.x) { active.addLast(next); }
					ed = (Point)grown.get(prev);
					if (dist>ed.x) { active.addLast(prev); }
				}
				grown.put(e, new Point(d+grew, di.y));
				if (bar!=null) {
					int min = e.GetMin(), max = e.GetMax();
					boolean horiz = Util.DirIsHorizontal(dir);
					Vector splitEdges = e.SplitOnBarrier(bar);
					Point oldDist, newDist = CalcSpecialDist(horiz, d+grew, di.y, e);
					if (splitEdges.size()>0) {
						active.addLast(e);
						if (newDist.x>(d+grew)) { grown.put(e, newDist); }
					}
					for (int j=0; j<splitEdges.size(); j++) {
						Edge newEdge = (Edge)splitEdges.elementAt(j);
						active.addLast(newEdge);
						newEdge.UpdateDir();
						newDist = CalcSpecialDist(horiz, d+grew, di.y, newEdge);
						oldDist = (Point)grown.get(newEdge);
						if (oldDist==null || newDist.x<oldDist.x) {
							grown.put(newEdge, newDist);
						}
					}
				}
			}
		}
	}
	/** Like SquareGrowPoly above, but initializes the grown and active lists
	  * so that all edges ready to grow full dist, and their closest point is
	  * at the midpoint of the edge. **/
	public void SquareGrowPoly(Poly p, int dist) {
		/** list of points (indexed by edge) that give dist from plug (in .x)
		  * and position on the edges axis (in .y) **/
		Hashtable grown = new Hashtable();
		/** List of edges to consider growing **/
		LinkedList active = new LinkedList();
		Edge e;
		for(int i=p.Size()-1; i>=0; i--) {
			e = p.GetEdge(i);
			active.addLast(e);
			grown.put(e, new Point(0, e.GetMid()));
		}
		SquareGrowPoly(p, dist, active, grown);
	}
	/** Calculates (as a rectangular polygon) the coverage of a plug,
	  * assuming that this layer is the well layer. **/
	public Poly SquareGrowFromPoint(Point at, int dist) {
//System.out.println("Square Grow called");
		// allow poly to cross self (so order of growth doesn't cut off edges around loops)
		// perform self-or operation on the poly to clean it up
		Poly p = new Poly(at, 1);
		SquareGrowPoly(p, dist-1);
		// self-OR the poly to remove overlaps caused by small barriers
		if (p!=null) {
			Layer temp = new Layer();
			temp.Add(p);
			temp = (new PolyOperator()).CombineLayers(PolyOperator.OR, temp, temp);
			if (temp.Size()>0) {
				p = temp.Get(0);
			} else {
				// TODO check if this indicates a bug...
				p=null;
			}
		}
		return p;
	}
}
