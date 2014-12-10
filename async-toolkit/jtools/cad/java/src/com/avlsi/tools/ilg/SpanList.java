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
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;

/** List of active spans. Used by PolyOperator to perform boolean operations on layers. **/
public class SpanList {
	/** The ordered (in x) list of active spans. **/
	LinkedList spans = new LinkedList();
	/** The operation type: 0=or, 1=and, 2=andNot**/
	int oper = 0;

	/** Removes all active spans. **/
	public void Clear() {
	  spans.clear();
	}
	/** Sets the operation type, should only be done on an empty SpanList. **/
	public void SetOper(int op) {
		if ((op>=0) && (op<=2)) {
			oper=op;
		}
	}
	/** Prints debugging info about the spans. **/
	public void DumpSpans() {
		System.out.println("Span list:");
		ListIterator iter = spans.listIterator();
		while (iter.hasNext()) {
			Span span=(Span)iter.next();
			span.Dump();
		}
	}
	/** Performs consistency checks on the list of spans. **/
	public void Validate() {
		//System.out.println("Validating Span list:");
		ListIterator iter = spans.listIterator();
		Span last = null;
		while (iter.hasNext()) {
			Span span=(Span)iter.next();
			if (!span.Validate()) { span.Dump(); }
			if (last!=null && last.x1>span.x0) {
				System.out.println("Mis-Ordered Spans:");
				span.Dump(); last.Dump();
			}
			last=span;
		}
	}
	/** Processes an edge from a given layer, creating and adjusting spans. **/
	public void SetSpan(Edge e, int layer, boolean state) {
		if ((layer<0) || (layer>1)) { return; } // throw IllegalArg?
		// split the edge list as nec
		int startx = e.P0().x, endx = e.P1().x;
		int y = e.P0().y;
		int xmin=Math.min(startx, endx), xmax=Math.max(startx, endx);
		Span span=null, temp;
		boolean newState;
		ListIterator iter = spans.listIterator();

		if (!iter.hasNext()) {
			// empty, insert this edge
			temp = new Span(xmin, xmax, y, layer, state);
			//temp.UpdateNewState(oper,y);
			spans.add(temp);
//System.out.println("first edge "+temp.x0+" "+temp.x1);
			return;
		}
		while (iter.hasNext() && (xmin<xmax)) {
			span = (Span)iter.next();
//System.out.println("iterating");
			if (xmin<span.x0) {
				// go before current, insert a new edge
				temp = new Span(xmin, Math.min(xmax, span.x0), y, layer, state);
				//temp.UpdateState(oper,y);
				iter.set(temp); // iter will crash if set is not first
				iter.add(span);
				iter.previous();
				xmin=Math.min(xmax, span.x0);
//System.out.println("pre-edge insert "+xmin+" "+span.x0);
			} else if (xmin<span.x1) {
				if (xmin>span.x0) {
					// cut from beginning of span
					temp = span.Split(xmin);
					iter.add(temp);
					iter.previous();
//System.out.println("pre-edge cut "+span.x0+","+temp.x1+" at "+xmin);
				} else {
//System.out.println("modifying span "+span.x0+","+span.x1);
					if (xmax<span.x1) {
						// cut from end of span
						temp=span.Split(xmax);
						iter.add(temp);
						iter.previous();
//System.out.println("post-edge cut at "+temp.x0);
					}
					span.SetState(layer, state);
					//span.UpdateState(oper,y);
					xmin=span.x1;
				}
			}
		}
		if (xmin<xmax) {
			temp = new Span(xmin, xmax, y, layer, state);
			//temp.UpdateNewState(oper,y);
			iter.add(temp);
//System.out.println("post-edge insert");
		}
	}
	/** Returns list of points (in pairs), right to left is top, reversed is bottom.
	    NOTE This function has side effects! **/
	public Vector GetEdges(int y, boolean off, boolean old) {
//DumpSpans();
Validate();
		Vector edges = new Vector();
		ListIterator iter = spans.listIterator();
		Span span;
		int prev=0, len;
		while (iter.hasNext()) {
			span=(Span)iter.next();
			if (old || span.IsNew(y)) {// i.e. span changed on last pass
				Point a=new Point(span.x0, y), b=new Point(span.x1, y);
				len=edges.size();
				if (span.GetState()) {
					if (prev==2 && (((Point)edges.elementAt(len-1)).x==a.x)) {
						// merge off edges with differing src layers
						edges.set(len-1, b);
//System.out.println("Merged overlap spans at points "+edges.elementAt(len-2)+" "+edges.elementAt(len-1));
					} else {
						edges.add(a); edges.add(b);
					}
					prev=2;
				} else if (off) {
					if (prev==1 && (((Point)edges.elementAt(len-2)).x==a.x)) {
						// merge on edges with differing src layers
						edges.set(len-2, b);
//System.out.println("Merged overlap spans at points "+edges.elementAt(len-1)+" "+edges.elementAt(len-2));
					} else {
						edges.add(b); edges.add(a);
					}
					prev=1;
				}
			}
		}
		Merge();
		Clean(); // FIXME should we do this here??
		return edges;
	}
	/** Returns list of points (in pairs), left to right is top, reversed is bottom.
	  * NOTE That this function has side effects, and should only be called at
	  * the completion of processing for a given y value **/
	public Vector GetNewEdges(int y) { return GetEdges(y, true, false); }
	/** Returns list of points (in pairs), left to right is top, reversed is bottom.
	  * NOTE That this function has side effects, and should only be called at
	  * the completion of processing for a given y value **/
	public Vector GetOnEdges(int y) { return GetEdges(y, false, true); }
	/** Recalculates the state of all spans
	  * NOTE This must be done at the end of a given y value, or double edges
	  * may mess up the calculation of "new" or "on" edges. **/
	public void Update(int y) {
		ListIterator iter = spans.listIterator();
		Span span=null;
		while (iter.hasNext()) {
			span=(Span)iter.next();
			span.UpdateState(oper,y);
		}
	}
	/** Joins any edges with equivalent states.
	  * NOTE Should only be done after harvesting "new" edges. **/
	public void Merge() {
		ListIterator iter = spans.listIterator();
		Span last=null, cur;
		while (iter.hasNext()) {
			cur=(Span)iter.next();
			if (last!=null && last.Merge(cur)) {
//System.out.println("MERGING:"); last.Dump(); cur.Dump();
				iter.remove();
				continue;
			}
			last=cur;
		}
	}
	/** Removes any edges that are completely off in both layers.
	  * NOTE Should only be done after harvesting "new" edges. **/
	public void Clean() {
		ListIterator iter = spans.listIterator();
		Span cur;
		while (iter.hasNext()) {
			cur=(Span)iter.next();
			if (cur.layer[0]==0 && cur.layer[1]==0) {
				// delete completely off edges
				iter.remove();
				continue;
			}
		}
	}
}
