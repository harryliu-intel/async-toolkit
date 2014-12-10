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
import com.avlsi.tools.ilg.PlugEntry;

/** A class to hold plug related data for a given Layer. **/
public class WellPlugData {
	/** The name of the well layer this layer plugs. **/
	String wellName=null;
	/** The distance that a given plug covers (plugs). **/
	int covDist=0;
	/** The size of a plug, placement origin, and increment from the origin. **/
	Point size, origin, spacing;
	/** Reads the data from a stream in lisp-like format. **/
	public void ReadExtraData (StreamTokenizer st) {
//System.out.println("Reading Extra Data");
		// the well-layer name to be plugged
		wellName = Util.ReadString(st);
		//  coverage distance
		covDist = (int)Util.ReadNumber(st);
		Util.ReadSymbol(st, "(");
		// size of the plugs
		size = new Point((int)Util.ReadNumber(st), (int)Util.ReadNumber(st));
		Util.ReadSymbol(st, ")");
		Util.ReadSymbol(st, "(");
		// spacing of the plugs
		origin = new Point((int)Util.ReadNumber(st), (int)Util.ReadNumber(st));
		Util.ReadSymbol(st, ")");
		Util.ReadSymbol(st, "(");
		// origin of plugs
		spacing = new Point((int)Util.ReadNumber(st), (int)Util.ReadNumber(st));
		Util.ReadSymbol(st, ")");
	}
}
