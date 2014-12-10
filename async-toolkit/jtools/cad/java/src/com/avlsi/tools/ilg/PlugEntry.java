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
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.WellPlugData;

/** Data to represent a plug and the well area that it covers. **/
public class PlugEntry implements Comparable {
	/** Numeric area covered. **/
	long area;
	/** Actual position of the Plug. **/
	Point at;
	/** The polygons within the well that the plug covers.
	    Overlap with other plugs may be removed. **/
	Vector polys = new Vector();

	public PlugEntry(Point p, Poly pol) {
		at=p; polys.add(pol);
		RecalcArea();
	}
	/** Recalculates the area covered. **/
	public long RecalcArea() {
		area=0;
		for (int i=polys.size()-1; i>=0; i--) {
			area+=((Poly)polys.elementAt(i)).Area();
		}
		return area;
	}
	/** Function to let us sort the entry on area (and other more arbitrary data). **/
	public int compareTo(Object o) {
		int dif=0;
		if (o instanceof PlugEntry) {
			PlugEntry ope = (PlugEntry)o;
			// TODO check sign
			if      (ope.area>area) { dif=-1; }
			else if (ope.area<area) { dif= 1; }
			else if (ope.at.x>at.x) { dif=-1; }
			else if (ope.at.x<at.x) { dif= 1; }
			else if (ope.at.y>at.y) { dif=-1; }
			else if (ope.at.y<at.y) { dif= 1; }
		//} else if (o==null) {
		//	return -1;
		} else {
			throw new ClassCastException();
		}
		return dif;
	}
	/** Dumps coverage polygons to the output layer.  **/
	public void ExportPolys(Layer l) {
		for (int i=polys.size()-1; i>=0; i--) {
			l.Add((Poly)polys.elementAt(i));
		}
	}
	/** Clears the coverage polygons and adds new ones from the input layer. **/
	public void ImportPolys(Layer l) {
		polys.clear();
		for (int i=l.Size()-1; i>=0; i--) {
			polys.add(l.Get(i));
		}
		RecalcArea();
	}
}
