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
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;


/** A span is a horizontal strip that records the state of two layers
 *  and calculates the output layer. Used by SpanList.
 **/
public class Span {
	int x0=0, x1=0, y=0;// x0 and x1 are sorted span, y is last 'time' changed
	int layer[]={0, 0, 0}; // on/off data about each layer
	boolean wasOn=false; // used to skip superfluous AND-NOT edges
	public Span() {}
	public Span(int xa, int xb, int ya, int l, boolean state) {
		x0=xa; x1=xb; y=ya;
		SetState(l, state);
	}
	public Span(Span s) {
		x0=s.x0; x1=s.x1; y=s.y;
		layer[0]=s.layer[0]; layer[1]=s.layer[1]; layer[2]=s.layer[2];
		wasOn=s.wasOn;
	}
	/** Prints debugging data. **/
	public void Dump() {
		System.out.println(
			"    span "+x0+" ... "+x1+" @ "+y+
			" : {"+layer[0]+", "+layer[1]+", "+layer[2]+"} "//+"flips:"+flips
		);
	}
	/** Breaks a Span at an intermediate point, and returns a new span for the right part.
	    at MUST lie between x0 and x1. **/
	public Span Split(int at) {
		Span s = new Span(this);
		x1=at; s.x0=at;
		s.wasOn=wasOn;
		return s;
	}
	/** If two spans touch and have equivalent states, merges them and returns true. **/
	public boolean Merge(Span s) {
		if ((x1==s.x0) && (layer[0]==s.layer[0]) && (layer[1]==s.layer[1])) {
			x1=s.x1;
			y=Math.max(y, s.y);
			wasOn=wasOn||s.wasOn;
			return true;
		}
		return false;
	}
	/** Returns true if span is self consistent. **/
	public boolean Validate() {
		return (x0<x1) &&
		  (layer[0]>=0) && (layer[1]>=0) && (layer[2]>=0);
	}
	/** Increments or decrements the state for a given layer. **/
	public boolean SetState(int lyr, boolean val) {
		if (val) { layer[lyr]++; }
		else { layer[lyr]--; }
		return GetState();
	}
	/** Returns the last calculated state for the span **/
	public boolean GetState() { return layer[2]>0; }
	/** Returns true if the span just changed and was ever on. **/
	public boolean IsNew(int yval) { return (y==yval) && wasOn;	}
	/** Returns reverse order for closing edges. **/
	public int GetStartX() { return GetState() ? x0 : x1; }
	/** Returns reverse order for closing edges. **/
	public int GetEndX() { return GetState() ? x1 : x0; }
	/** Returns y value at which span was last changed. **/
	public int GetLastY() { return y; }
	/** Returns whether the span state changed or not. **/
	public boolean UpdateNewState(int oper, int yval) {
		boolean ret = UpdateState(oper, yval);
		return ret;
	}
	/** Calculates new state. Operators are: 0=or, 1=and, 2=andNot. **/
	public boolean UpdateState(int oper, int yval) {
		boolean ret;
		int state=0;
		if (oper==0) {
			state = (layer[0]>0 || layer[1]>0) ? 1:0;
		} else if (oper==1) {
			state= (layer[0]>0 && layer[1]>0) ? 1:0;
		} else if (oper==2) {
			state= (layer[0]>0 && layer[1]==0) ? 1:0;
		}
		ret = (state!=layer[2]);
		layer[2]=state;
		if (state==1) { wasOn=true; }
		if (ret) {
			y=yval;
		}
		return ret;
	}
}
