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
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/** A class that performs boolean operations on pairs of layers. **/
public class PolyOperator {
	/** List of operations **/
	static final int OR=0, AND=1, ANDNOT=2, SEP=3;
	/** List active edges. **/
	SpanList s = new SpanList();
	/** Input and output layers **/
	Layer a, b, layer = new Layer();
	/** List of sorted (on y) edges from both layers. **/
	LinkedList l = new LinkedList();
	/** Table of edges, start point is key, value is end point (possibly a list). **/
	Hashtable h = new Hashtable();

	/** Put the edges from the input layers into the sorted list **/
	public void SortHorizEdges(Layer a, Layer b) {
		SortedEdges ela=a.hEdges, elb=b.hEdges;
		int y, ia=0, ib=0, amax=ela.Size(), bmax=elb.Size();
		Edge cura, curb;
//System.out.println("a:"+amax+" b:"+bmax+"\n");
//System.out.println("Vert: a:"+a.vEdges.Size()+" b:"+b.vEdges.Size()+"\n");
		while (ia<amax || ib<bmax) {
			if (ia<amax) { cura=ela.Get(ia); } else { cura=null; }
			if (ib<bmax) { curb=elb.Get(ib); } else { curb=null; }
			if ((cura!=null) && ((curb==null) || (cura.GetY()<curb.GetY()))) {
				l.add(cura); ia++;
				// record layer number
				l.add(new Integer(0));
//System.out.println("edge ("+cura.P0().x+","+cura.P0().y+" "+cura.P1().x+","+cura.P1().y+") sorted on a "+ia);
			} else if (curb!=null) {
				l.add(curb); ib++;
				l.add(new Integer(1));
//System.out.println("edge ("+curb.P0().x+","+curb.P0().y+" "+curb.P1().x+","+curb.P1().y+") sorted on b "+ib);
			} else { break; }
		}
	}

	/** Insert an edge into the hashtable, creating lists as necessary. **/
	public void TableEdge(Point p0, Point p1) {
//System.out.println("Putting edge "+p0.x+","+p0.y+" to "+p1.x+","+p1.y+" into table");
		Object ob = h.get(p0);
		if (ob==null) {
			h.put(p0, p1);
		} else {
//System.out.println("non-empty edge");
			// more than one vertex here, make a list
			if (ob instanceof Point) {
//System.out.println("creating linked list");
				LinkedList l = new LinkedList();
				l.add(ob);
				ob=l;
			}
			((LinkedList)ob).add(p1);
			h.put(p0, ob);
		}
	}
	/** Remove an edge from the hashtable, dismantling lists as necessary. **/
	public Point UntableEdge(Point cur, Point last) {
		Object next = null;
		if (cur!=null) {
			next = h.get(cur);
			if (next!=null) {
				if (next instanceof Point) {
					h.remove(cur);
				} else {
//System.out.println("Found multi-entry edge");
					Point temp=null;
					LinkedList el = (LinkedList)next;
					if (last!=null) {
//System.out.println("Searching for joint edge");
						int dir = Util.GetInsideDir(Util.GetLineDir(last , cur));
						// more than one vertex, shrink, re-add
						ListIterator iter = el.listIterator(0);
						while (iter.hasNext()) {
							temp = (Point) iter.next();
							if (Util.GetLineDir(cur , temp)==dir) {
								iter.remove();
								break;
							}
							temp=null;
						}
//if (temp==null) { System.out.println("Failed to match edge"); }
					}
					if (temp==null) { next = el.removeLast(); }
					else { next=temp; }
					int sz = el.size();
					if (sz<1) { h.remove(cur); }
					else if (sz<2) { h.put(cur, el.removeLast()); }
				}
			}
		} else {
			// called with nulls to get starting point
			Enumeration e=h.keys();
			if (e.hasMoreElements()) { next=e.nextElement(); }
		}
		return (Point)next;
	}

	/** Output all the edges for a given y value into the hashtable. **/
	public void EmitEdges(int y, int lasty) {
		int x0, y0, x1;
		Point p0, p1;
		Vector spans, on;
		// emit newly-on/off spans as edges (into h, by first coord)
		spans = s.GetNewEdges(lasty);
		on = s.GetOnEdges(lasty);
//System.out.println("y:"+y+" last:"+lasty+" spans  new:"+spans.size()/2+"  on:"+on.size()/2);
		for (int i=spans.size()-2; i>=0; i-=2) {
			p0=(Point)spans.elementAt(i);
			p1=(Point)spans.elementAt(i+1);
			TableEdge(new Point(p0), new Point(p1));
//System.out.println("emitting "+(p0.x<p1.x?"TOP":"BOT")+" "+p0+" ; "+p1+" at "+lasty);
		}
		spans=on;
		for (int i=spans.size()-2; i>=0; i-=2) {
			x0=((Point)spans.elementAt(i)).x;
			y0=((Point)spans.elementAt(i)).y;
			x1=((Point)spans.elementAt(i+1)).x;
			//if ((y0!=y) && (x0>x1))
			if (x0<x1) {
				// emit downward edges for right side of all 'on' spans
				TableEdge(new Point(x1, y0), new Point(x1, y));
//System.out.println("emitting RIGHT "+(new Point(x1, y0))+" ; "+(new Point(x1, y))+" at "+lasty);
				// emit upward edges for left side of all 'on' spans
				TableEdge(new Point(x0, y), new Point(x0, y0));
//System.out.println("emitting LEFT "+(new Point(x0, y))+" ; "+(new Point(x0, y0))+" at "+lasty);
			}
		}
	}
	/** Combine two layers with a boolean operation,
	  * the entry point function that does all the work.
	  * Operators are: 0=OR, 1=AND, 2=ANDNOT, 3=OR without hole recombination **/
	public Layer CombineLayers(int oper, Layer a, Layer b) {
		int y, x0, y0, x1, lasty;
		int lnum=0;
		Edge edge=null;
		Vector spans;
		y=lasty=Integer.MAX_VALUE;
		s.Clear();
		if (oper==3) {
			s.SetOper(0);
		} else {
			s.SetOper(oper);
		}
		// make sure Layers have edge lists filled out
		a.Update(); b.Update();
		SortHorizEdges(a, b);
		Iterator iter = l.iterator();
		if (iter.hasNext()) {
			edge=(Edge)iter.next();
			lnum=((Integer)iter.next()).intValue();
		}
		while (edge!=null) {
			y=edge.GetY();
			EmitEdges(y, lasty);
			while (edge!=null) {
//System.out.println("edge ("+edge.P0().x+","+edge.P0().y+" "+edge.P1().x+","+edge.P1().y+")");
				s.SetSpan(edge, lnum, edge.P0().x<edge.P1().x);
				if (iter.hasNext()) {
					edge=(Edge)iter.next();
					lnum=((Integer)iter.next()).intValue();
				} else { edge=null; }
				if (edge==null || y!=edge.GetY()) { break; }
			}
			s.Update(y);
			lasty=y;
			if (edge==null) { EmitEdges(y, lasty); }
		}
		Layer l = new Layer();
		LinkedList holes = new LinkedList();
		Poly p;
		Point last, cur, old;
		while ((cur=UntableEdge(null, null))!=null) {
//System.out.println("new poly");
			p=new Poly();
			int pi=0;
			last=old=null;
			while (cur!=null) {
//System.out.println("  vertex at "+((Point)cur));
				p.Insert(pi++, (Point)cur, null);
				old=last;
				last = cur;
				cur=UntableEdge(last, old);
			}
			// merge colinear vertical edges
			p.CreateEdges();
			p.DeleteIfDegenerate();
			long area = p.Area();
			if (area<0) {
//System.out.println("added hole");
				if (oper==3) {
					layer.Add(p);
				} else {
					holes.add(p);
				}
			} else if (area>0) {
//System.out.println("added poly");
				layer.Add(p);
			} else {
//System.out.println("zero area polygon???");
			}
		}
		iter = holes.iterator();
		Poly hole;
		while (iter.hasNext()) {
//System.out.println("new hole");
			hole=(Poly)iter.next();
			//have a hole that must be added to some poly
			for (int i=layer.Size()-1; i>=0; i--) {
				p=layer.Get(i);
				if (p.PointInside(hole.Get(0))) {
					p.MergeHole(hole);
					break;
				}
			}
		}
		return layer;
	}
}
