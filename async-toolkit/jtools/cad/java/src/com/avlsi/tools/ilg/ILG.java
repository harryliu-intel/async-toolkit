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
import com.avlsi.tools.ilg.Edge;
import com.avlsi.tools.ilg.Poly;
import com.avlsi.tools.ilg.Layer;
import com.avlsi.tools.ilg.SortedEdges;
import com.avlsi.tools.ilg.Span;
import com.avlsi.tools.ilg.SpanList;
import com.avlsi.tools.ilg.PolyOperator;
import com.avlsi.tools.ilg.PlugEntry;
import com.avlsi.tools.ilg.WellPlugData;
import com.avlsi.tools.ilg.GA;

/** The Entry point for Implied Layer Generation and Well Plugging.
  * contains both GUI and CLI code. **/
public class ILG {
	/** List of layers. **/
	Vector layers = new Vector();
	/** The main window (or null in CLI mode). **/
	ILGWindow window;

	/** Reads a list of layers from a stream in lisp-like format. **/
	public void ReadAllShapes(Reader in) {
		// (Layer, Layer, ...)
		StreamTokenizer st = new StreamTokenizer(in);
		st.wordChars('(', '(');
		st.wordChars(')', ')');
		st.parseNumbers();
		Util.ReadSymbol(st, "'");
		Util.ReadSymbol(st, "(");
		Layer dummy = new Layer();
		while (true) {
            Layer cur = dummy.Read(st);
            if (cur==null) { break; }
            layers.addElement(cur);
		}
        st.pushBack();
		Util.ReadSymbol(st, ")");
	}

	/** Looks up a layer by its name. **/
	public Layer GetLayerByName(String name) {
		if (name==null) { return null; }
		for (int i=layers.size()-1; i>=0; i--) {
			Layer l = (Layer)layers.elementAt(i);
			if ((l.name!=null) && l.name.equals(name)) { return l; }
		}
		return null;
	}

	/** Grows and edge until it hits another (if possible).
	  * Returns true if any work is done. **/
	public boolean GrowEdge(Edge e, int max, Layer layer) {
		int dir=Util.GetOutsideDir(e.dir);
		//int dir=GetInsideDir(e.dir);
		int dist = max;
		if (Util.DirIsHorizontal(e.dir)) {
			for (int l=0; l<layers.size(); l++) {
				Layer curLayer = (Layer)layers.elementAt(l);
				dist=curLayer.hEdges.GetNearestEdgeDist(e, dir, dist);
				if (dist==0) { break; }
			}
		} else {
			for (int l=0; l<layers.size(); l++) {
				Layer curLayer = (Layer)layers.elementAt(l);
				dist=curLayer.vEdges.GetNearestEdgeDist(e, dir, dist);
				if (dist==0) { break; }
			}
		}
		if (dist>0) {
            if (dist>max) { System.out.println("Bogus dist "+dist+" !"); dist=max; }
            layer.Grow(e, dist);
            //UpdateLayer(layer.index);
			return true;
		}
		return false;
	}

	/** Cleans degenerate edges and regenerates sorted edge data. **/
	public void UpdateLayer(int l) {
		Layer layer=(Layer)layers.elementAt(l);
        layer.Update();
	}

	/** Merges polygons (if possible). Returns true if any work is done. **/
	public boolean MergeLayer(int l) {
		Layer layer=(Layer)layers.elementAt(l);
		return layer.Merge();
	}

	/** Splits polygons (if possible). Returns true if any work is done. **/
	public boolean SplitLayer(int l) {
		Layer layer=(Layer)layers.elementAt(l);
		return layer.Split();
	}

	/** Grows indentations in polygons out (if possible). Returns true if any work is done. **/
	public boolean GrowNotches(int layer) {
		boolean cur, did=false;
        Layer curLayer = (Layer)layers.elementAt(layer);
        //System.out.println("Growing Notches on: "+curLayer.name);
        if (!curLayer.fixed) {
            for (int p=0; p<curLayer.shapes.size(); p++) {
                Poly curPoly = (Poly)curLayer.shapes.elementAt(p);
                int pSz = curPoly.Size();
                for (int e=0; e<pSz; e++) {
                    Edge e0=curPoly.GetEdge(e);
                    Edge e1=curPoly.GetEdge((e+1)%pSz);
                    // expand both...
                    if (Util.IsCCW(e0.dir, e1.dir)) {
                        cur = GrowEdge(e0, Math.abs(e1.Length()), curLayer);
                        did=did || cur;
                        if (!cur) {
                            cur = GrowEdge(e1, Math.abs(e0.Length()), curLayer);
                            did=did || cur;
                        }
                        if (cur) { break; }
                        //if (cur) { UpdateLayer(layer); return true; }
                    }
                }
            }
        }
		//if (did) {
		//	UpdateLayer(layer);
		//}
		return did;
	}
	/** Grows polygons out horizontally or vertically (if possible). Returns true if any work is done. **/
	public boolean GrowOnAxis(int layer, boolean horiz) {
        boolean cur, did=false;
        Layer curLayer = (Layer)layers.elementAt(layer);
        if (!curLayer.fixed) {
            for (int p=0; p<curLayer.shapes.size(); p++) {
                Poly curPoly = (Poly)curLayer.shapes.elementAt(p);
                int pSz = curPoly.Size();
                for (int e=0; e<pSz; e++) {
                    Edge edge=curPoly.GetEdge(e);
                    if (horiz==Util.DirIsHorizontal(edge.dir)) {
                        if (GrowEdge(edge, 6, curLayer)) {
	                        did=true;
	                        break;
						}
                    }
                }
            }
        }
        //System.out.println("GrowOnAxis :"+did);
        return did;
	}
	/** Forces a repaint and waits for user input. Not used. **/
    public void RepaintAndWait() {
        if (window!=null) {
            window.repaint();
            window.WaitForUser();
        }
    }
	/** Fills holes left by any of the growth layers (if possible). Returns true if any work is done. **/
    public boolean FillHoles() {
		boolean growable = false;
		for (int l=layers.size()-1; l>=0; l--) {
			growable = growable || Growable(l);
if (Growable(l)) { System.out.println("Growable Layer "+l); }
		}
		if (!growable) { return false; }
        Layer holes = new Layer();
        Poly base;
        holes.name="holes";
        holes.index = layers.size();
        layers.addElement(holes);
        for (int l=1; l<holes.index; l++) {
            holes.CopyPolys((Layer)layers.elementAt(l));
        }
        while (MergeLayer(holes.index)) {
        }
        for (int i=0; i<holes.Size(); i++) {
            holes.Get(i).Reverse();
        }
        UpdateLayer(holes.index);
        Poly prPoly = ((Layer)layers.elementAt(0)).Get(0);
        base = new Poly(prPoly);
        holes.shapes.addElement(base);
        holes.AddEdges(base);
        while (MergeLayer(holes.index)) {}
        UpdateLayer(holes.index);
        while (holes.Split()) {}
        for (int i=holes.Size()-1; i>=0; i--) {
            Poly p = holes.Get(i);
            if (p.Size()<3) {
                holes.shapes.removeElementAt(i);
                continue;
            }
System.out.println("Filling hole ");
//p.Dump(System.out);
            Edge e = p.GetEdge(0), e2, near;
            //e.Dump();
            int len=e.Length(), len2;
            // find longest edge
            for (int j=1; j<p.Size(); j++) {
                e2=p.GetEdge(j); len2=e2.Length();
                if (len<len2) {
                    e=e2; len=len2;
                }
            }
            // find neighbor to that edge
            int cur, minDist=Integer.MAX_VALUE, minLayer=1;
            int outside = Util.GetOutsideDir(e.dir);
            for (int l=1; l<holes.index; l++) {
                Layer curLayer = (Layer)layers.elementAt(l);
                if (Util.DirIsHorizontal(e.dir)) {
                    cur = curLayer.hEdges.GetNearestEdgeDist(e, outside, minDist);
                } else {
                    cur = curLayer.vEdges.GetNearestEdgeDist(e, outside, minDist);
                }
                if (cur<minDist) {
                    minDist=cur; minLayer=l;
                    //System.out.println(curLayer.name+" at "+minDist);
                }
            }
            // fill with neighbor type
            Layer dstLayer = (Layer)layers.elementAt(minLayer);
            dstLayer.shapes.addElement(new Poly(p));
            UpdateLayer(minLayer);
        }
        layers.removeElementAt(holes.index);
        return holes.Size()>0;
    }
    /** Checks if a given layer should be grown. **/
    public boolean Growable(int l) {
		return !((Layer)layers.elementAt(l)).fixed;
	}
	/** GrowNotches, grow sides, merge, fill holes, lather rinse, repeat...
	  * The main algorithm that turns messy well polygons into nice big rectangles
	  * (mostly). **/
	public void GrowAll() {
		int len=layers.size();
		boolean growable = false;
		for(int i=1; i<len; i++) {
			growable = growable || Growable(i);
		}
		if (!growable) { return; }
		boolean did = true, cur;
        while (did) {
			for(int i=1; i<len; i++) {
				UpdateLayer(i);
			}
            did=false;
            System.out.println("GrowNotches");
            for(int i=1; i<len; i++) {
				if (Growable(i)) {
					cur=GrowNotches(i);
					if (cur) { System.out.println("  did "+i); }
					did=cur||did;
				}
            }
            if (!did) {
                System.out.println("GrowAxis Horiz");
                for(int i=1; i<len; i++) {
					if (Growable(i)) {
						cur=GrowOnAxis(i, true);
						if (cur) { System.out.println("  did "+i); }
						did=cur||did;
					}
				}
            }
            if (!did) {
                System.out.println("GrowAxis Vert");
                for(int i=1; i<len; i++) {
					if (Growable(i)) {
						cur=GrowOnAxis(i, false);
						if (cur) { System.out.println("  did "+i); }
						did=cur||did;
					}
				}
            }
            if (!did) {
                System.out.println("MergeLayer");
                for(int i=1; i<len; i++) {
					if (Growable(i)) {
						cur=MergeLayer(i);
						if (cur) { System.out.println("  did "+i); }
						if (cur) { UpdateLayer(i); }
						did=cur||did;
					}
				}
            }
            if (!did) {
                System.out.println("FillHoles");
                did=FillHoles();
                if (did) { System.out.println("  did."); }
            }
        }
    }

	/** Given a plug layer, calculates everywhere a plug could go. **/
	public Vector GetAllPlugPoints(Layer l) {
System.out.println("  Getting Plug Points");
		Vector points = new Vector();
		Poly ply;
		Point pt, min=null, max=null;
		for (int i=l.Size()-1; i>=0; i--) {
			ply = l.Get(i);
			for (int j=ply.Size()-1; j>=0; j--) {
				pt=ply.Get(j);
				if (min==null) {
					min=new Point(pt);
					max=new Point(pt);
				} else {
					if (pt.x<min.x) { min.x=pt.x; }
					if (pt.y<min.y) { min.y=pt.y; }
					if (pt.x>max.x) { max.x=pt.x; }
					if (pt.y>max.y) { max.y=pt.y; }
				}
			}
//System.out.println("  range is "+min+" to "+max);
			Point tmp, orig = l.wellData.origin, spc = l.wellData.spacing;
			min.x = ((int)((orig.x-min.x)/spc.x))*spc.x-orig.x;
			min.y = ((int)((orig.y-min.y)/spc.y))*spc.y-orig.y;
			for (int x=min.x; x<=max.x; x+=spc.x) {
				for (int y=min.y; y<=max.y; y+=spc.y) {
					tmp = new Point(x, y);
					if (ply.PointInside(tmp)) {
//System.out.println("  plug at "+tmp);
						points.add(tmp);
					}
				}
			}
			min=null; max=null;
		}
System.out.println("  Done Getting Plug Points ("+points.size()+")");
		return points;
	}

	public class GeneInterface implements GA.GeneEvaluator {
		//public Layer plugLayer = null;
		public Vector plugEntries=null;
		public long wellArea = 0;
		public int wellpolys = 0;
		public long plugArea = 0;
		int minPlugs = 0;

		public float GetFitness(GA.Gene g) {
			Layer l = new Layer();
			PlugEntry pe;
			long plugArea=0, covArea=0;
			int numPlugs = g.chrom.length;
			for (int i=numPlugs-1; i>=0; i--) {
//System.out.print(" "+g.chrom[i]);
				pe = (PlugEntry)plugEntries.elementAt(g.chrom[i]);
				plugArea+=pe.area;
				for (int j=pe.polys.size()-1; j>=0; j--) {
					l.Add((Poly)pe.polys.elementAt(j));
				}
			}
			l = (new PolyOperator()).CombineLayers(PolyOperator.OR, l, l);
			covArea = l.Area();
			float fitness = (float)((covArea/(float)wellArea)*(.9+.1*minPlugs/(1+numPlugs)));
//System.out.println("Fitness: "+fitness+" covArea: "+covArea+" wellArea: "+wellArea+" numPlugs: "+numPlugs);
			return fitness;
		}

		public Vector Run() {
			GA ga = new GA();
			minPlugs=Math.max(1, (int)(.5+wellArea/plugArea));
			minPlugs=Math.max(minPlugs, wellpolys);
System.out.println("GI.RUN minPlugs: "+minPlugs+", wellArea: "+wellArea+", plugArea: "+plugArea);
			//ga.mutationRate=.01; ga.crossoverRate=.75; ga.elitism=.33;
			ga.evaluator = this;
			//.... do stuff
			ga.Populate(minPlugs, plugEntries.size());
			for (int i=0; i<170; i++) {
if ((i%10)==0) { System.out.println("GI.STEP : Best Fitness is "+ga.GetBest().fitness);}
				ga.Step();
			}
			GA.Gene best = ga.GetBest();
			// avoid garbage collection cycle
			ga.evaluator = this;
			Vector results = new Vector();
			int numPlugs = best.chrom.length;
			for (int i=numPlugs-1; i>=0; i--) {
				results.add(plugEntries.elementAt(best.chrom[i]));
			}
			return results;
		}
	}

	/** Chooses semi-optimal set of plugs given the plug layer. **/
	public void ProcessPlugLayer(Layer l, boolean linear) {
		if (l.wellData!=null) {
			Vector points = GetAllPlugEntries(l);
			// scale down by square root of 2 to compensate for square growth
			int dist = (int)(l.wellData.covDist/1.4142135623731);
			int spacing = Math.max(l.wellData.size.x, l.wellData.size.y);
			Vector plugs = null;
			if (linear) {
				plugs = CalcPlugsOpt(points, spacing);
			} else {
				GeneInterface gi = new GeneInterface();
				Layer well = GetLayerByName(l.wellData.wellName);
				gi.wellArea = well.Area();
				gi.plugArea = 4*dist*dist;
				gi.plugEntries = points;
				gi.wellpolys = well.Size();
				plugs = gi.Run();

			}
			l.plugEntries = plugs;
		}
	}

	public Vector GetAllPlugEntries(Layer l) {
		Vector entries = null;
		if (l.wellData!=null) {
			entries = new Vector();
			Layer well = GetLayerByName(l.wellData.wellName);
			Layer plugLyr = (new PolyOperator()).CombineLayers(PolyOperator.AND, well, l);
			plugLyr.wellData = l.wellData;
			Vector points = GetAllPlugPoints(plugLyr);
			// scale down by square root of 2 to compensate for square growth
			int dist = (int)(l.wellData.covDist/1.4142135623731);
			well = (new PolyOperator()).CombineLayers(PolyOperator.SEP, well, well);
System.out.println("  Growing Plugs");
			Point at;
			for(int i=0; i<points.size(); i++) {
				at=(Point)points.elementAt(i);
				Poly p = well.SquareGrowFromPoint(at, dist);
				if (p!=null) {
					entries.add(new PlugEntry(at, p));
				}
			}
System.out.println("  Done Growing Plugs");
		}
		return entries;
	}
	/** Given a list of plug points, chooses a semi-optimal set.
	  * First the coverage is calculated for all,
	  * then the highest area one is chosed, and overlaps are removed.
	  * Lather, rinse, repeat...
	  * returns a list of PlugEntries. **/
	public Vector CalcPlugs(Vector plugEntries, int spacing) {
		// split all holes from polys and add as seperate polys
		// copy l to temp layer (to handle FAKE (double) edges)
		TreeSet entries = new TreeSet(plugEntries);
		TreeSet tempEntries = new TreeSet();
		Vector results = new Vector();
		int area = 0;
		Point at;
		PlugEntry hi = null, cur;
		Layer hiLyr = new Layer(), tmpLyr = new Layer();
		while (!entries.isEmpty()) {
			tempEntries.clear();
			// chose highest area
			hi = (PlugEntry)entries.last();
			area+=hi.area;
//System.out.println("  Picked plug sized "+hi.area+" at "+hi.at);
			results.add(hi);
			entries.remove(hi);
			tmpLyr.Clear(); hiLyr.Clear();
			hi.ExportPolys(hiLyr);
			//hiLyr = (new PolyOperator()).CombineLayers(PolyOperator.OR, hiLyr, hiLyr);
			Iterator iter = entries.iterator();
			while (iter.hasNext()) {
				// andNot with other polys and re-calc area
				cur = (PlugEntry)iter.next();
				cur.ExportPolys(tmpLyr);
				tmpLyr = (new PolyOperator()).CombineLayers(PolyOperator.ANDNOT, tmpLyr, hiLyr);
				cur.ImportPolys(tmpLyr);
				tmpLyr.Clear();
				int pickDist = Math.max(Math.abs(hi.at.x-cur.at.x), Math.abs(hi.at.y-cur.at.y));
				if ((cur.area>0) && (pickDist>spacing)) {
					tempEntries.add(cur);
//System.out.println("Re-Adding Point at "+cur.at.x+","+cur.at.y+" area: "+cur.area);
				}
			}
			// force a re-shuffling now that the areas have completely changed
			entries.clear();
			entries.addAll(tempEntries);
		}
		// should we return unplugged well polys at some point?
System.out.println("  finished plugging "+results.size()+" plugs used covering area "+area);
		return results;
	}
	/** Given a list of plug points, chooses a semi-optimal set.
	  * First the coverage is calculated for all,
	  * then the highest area one is chosed, and overlaps are lazily removed.
	  * Lather, rinse, repeat...
	  * returns a list of PlugEntries. **/
	public Vector CalcPlugsOpt(Vector plugEntries, int spacing) {
		// split all holes from polys and add as seperate polys
		// copy l to temp layer (to handle FAKE (double) edges)
		SortableVector entries = new SortableVector(plugEntries);
		SortableVector outdated = new SortableVector();
		Vector results = new Vector();
		int i, area = 0, elast;
		Point at;
		PlugEntry hi = null, cur;
		Layer covLyr = new Layer(), hiLyr = new Layer(), tmpLyr = new Layer();
		while (!entries.isEmpty()) {
			entries.Sort();
			elast = entries.size()-1;
			// chose highest area
			hi = (PlugEntry)entries.elementAt(elast);
//System.out.println("  Picked plug sized "+hi.area+" at "+hi.at);
			results.add(hi);
			entries.remove(elast);
			area+=hi.area;
			tmpLyr.Clear();
			hi.ExportPolys(tmpLyr);
			hiLyr = (new PolyOperator()).CombineLayers(PolyOperator.OR, tmpLyr, hiLyr);
			outdated.addAll(entries); entries.clear();
			// remove entries that violate spacing..
			for (i=outdated.size()-1; i>=0; i--) {
				cur = (PlugEntry)outdated.elementAt(i);
				int pickDist = Math.max(Math.abs(hi.at.x-cur.at.x), Math.abs(hi.at.y-cur.at.y));
				if (pickDist<spacing) { outdated.remove(i); }
			}
			outdated.Sort();
			long bestArea=0;
			for (i=outdated.size()-1; i>=0; i--) {
				cur = (PlugEntry)outdated.elementAt(i);
				if (bestArea>cur.area) {
					// sorted, so no remaining entries can be better
					break;
				}
				outdated.remove(i);
				cur.ExportPolys(tmpLyr);
				tmpLyr = (new PolyOperator()).CombineLayers(PolyOperator.ANDNOT, tmpLyr, hiLyr);
				cur.ImportPolys(tmpLyr);
				tmpLyr.Clear();
				if (cur.area>0) {
					if (bestArea<cur.area) { bestArea=cur.area; }
					entries.add(cur);
//System.out.println("Re-Adding Point at "+cur.at.x+","+cur.at.y+" area: "+cur.area);
				}

			}
		}
		// should we return unplugged well polys at some point?
System.out.println("  finished plugging "+results.size()+" plugs used covering area "+area);
		return results;
	}

	/** Iterates thru layers looking for plug layers, and calulates which
	  * plugs should be used for them. **/
	public void ChoosePlugs() {
System.out.println("Chosing plugs");
		// start display hack
		Layer outLayer = new Layer();
		if (window!=null) {	layers.set(layers.size()-2, outLayer); }
		// end display hack
		for (int i=layers.size()-1; i>=0; i--) {
			Layer l = (Layer)layers.elementAt(i);
			if (l.wellData!=null) {
System.out.println("Found Plug layer "+l.name+" for "+l.wellData.wellName);
				ProcessPlugLayer(l, true);
				// export Polys
				if ((window!=null) && (l.plugEntries!=null)) {
					for (int j=0; j<l.plugEntries.size(); j++) {
						PlugEntry pe = (PlugEntry)l.plugEntries.elementAt(j);
						for (int k=pe.polys.size()-1; k>=0; k--) {
							outLayer.Add((Poly)pe.polys.elementAt(k));
						}
					}
					window.repaint();
				}
System.out.println("Done with layer "+l.wellData.wellName+" ("+(l.plugEntries!=null?l.plugEntries.size():0)+" plugs used)");
			}
		}
		if (window!=null) {	window.repaint(); }
System.out.println("Done Chosing plugs");
	}

	/** Class that scales Polys and turns them into a format used by the
	  * gui polygon drawing routines. **/
    public class MultiPoly {
		int x[], y[], l;
		public MultiPoly(Poly p) {
			l=p.Size(); x=new int[l]; y=new int[l];
			for (int i=0; i<l; i++) {
				Point pt=p.Get(i);
				x[i]=pt.x; y[i]=pt.y;
			}
		}
		public MultiPoly(Poly p, float xoff, float yoff, float xmag, float ymag, int xsz, int ysz) {
			l=p.Size(); x=new int[l]; y=new int[l];
			for (int i=0; i<l; i++) {
				Point pt=p.Get(i);
				x[i]=(int)((pt.x-xoff)*xmag)+xsz/2;
				y[i]=(int)((pt.y-yoff)*ymag)+ysz/2;
			}
		}
	}

	/** Dumps all layers to a stream in lixp-like format. **/
    public void Dump (PrintStream out) {
        out.println("(");
        for (int i=0; i<layers.size(); i++) {
            ((Layer)layers.elementAt(i)).Dump(out);
        }
        out.println(")");
    }
	/** Prints debugging info to stdout about all layers. **/
    public void DebugDump() {
        //((Layer)layers.elementAt(1)).DebugDump();
        //((Layer)layers.elementAt(2)).DebugDump();
        for (int i=0; i<layers.size(); i++) {
			((Layer)layers.elementAt(i)).DebugDump();
		}
    }

	/** The main gui class that displays the layers as polygons,
	  * allows drawing, scaling, and scrolling, and ILG/Plug operations. **/
	public class ILGCanvas extends Canvas {
		float xsz=700, ysz=500,
		      xoff=0, yoff=0,
		      xmag=(float).4, ymag=(float)-.4;
		/** Temporary storage for the start of a mouse drag. **/
        Point dragStart=new Point(-1,-1);
        /** Current mouse position. **/
        Point mousePos=new Point(0,0);
        /** Whether dragging scales or draws rectangles. **/
        boolean drawMode=false;
        /** Whether rectangles are filled and alpha **/
        boolean fillMode = false;
        /** Various user source and destination layers. **/
        int src1=0, src2=1, dst=2, cur=2;
        /** Layer drawing colors. **/
		Color colors[]={Color.magenta, Color.green, new Color(192, 96, 16),
			Color.red, Color.yellow, Color.black, Color.orange};

		Poly hilite = null;

		public ILGCanvas(int x, int y) {
			super();
			xsz = x; ysz=y;
			repaint();
			addKeyListener(new KeyListener() {
				public void keyReleased(KeyEvent e) {}
				public void keyTyped(KeyEvent e) {}
				public void keyPressed(KeyEvent e) {
					int lyr = layers.size()-1;
					switch (e.getKeyCode()) {
						// ILG/Plug top level operations
						case KeyEvent.VK_E : ChoosePlugs(); break;
						case KeyEvent.VK_I : GrowAll(); break;

                        case KeyEvent.VK_D : DebugDump(); break;
						case KeyEvent.VK_P : Dump(System.out); break;

						// Boolean layer, and drawing/erase operations
						case KeyEvent.VK_A : CombineLayers(PolyOperator.AND,    src1, src2, dst); break;
						case KeyEvent.VK_U : CombineLayers(PolyOperator.SEP,    src1, src2, dst); break;
						case KeyEvent.VK_N : CombineLayers(PolyOperator.ANDNOT, src1, src2, dst); break;
						case KeyEvent.VK_O : CombineLayers(PolyOperator.OR,     src1, src2, dst); break;
						case KeyEvent.VK_C : ClearLayer(cur); break;
						case KeyEvent.VK_R : drawMode=!drawMode; break;
						case KeyEvent.VK_T : fillMode=!fillMode; break;

						// ILG/Plug low level operations
						case KeyEvent.VK_F : GrowNotches(cur); break;
                        case KeyEvent.VK_H : GrowOnAxis(cur, true); break;
                        case KeyEvent.VK_V : GrowOnAxis(cur, false); break;
                        case KeyEvent.VK_G : FillHoles(); break;
                        case KeyEvent.VK_M : MergeLayer(cur); break;
                        case KeyEvent.VK_S : SplitLayer(cur); break;
						case KeyEvent.VK_W : GrowPlug(); break;

						// input/output layer selection
                        case KeyEvent.VK_0 : cur=0; break;
                        case KeyEvent.VK_1 : cur=1; break;
                        case KeyEvent.VK_2 : cur=2; break;
                        case KeyEvent.VK_3 : cur=3; break;
                        case KeyEvent.VK_4 : cur=4; break;
                        case KeyEvent.VK_5 : cur=5; break;
                        case KeyEvent.VK_6 : cur=6; break;
                        case KeyEvent.VK_7 : cur=7; break;
                        case KeyEvent.VK_8 : cur=8; break;
                        case KeyEvent.VK_9 : cur=9; break;
                        case KeyEvent.VK_COMMA  : src1=cur; break;
                        case KeyEvent.VK_PERIOD : src2=cur; break;
                        case KeyEvent.VK_SLASH  : dst=cur;  break;

						// scaling/scrolling operations
						case KeyEvent.VK_DOWN :
						 	Move((float) 0.0,(float)-1.0); break;
						case KeyEvent.VK_UP :
							Move((float) 0.0,(float) 1.0); break;
						case KeyEvent.VK_LEFT :
							Move((float) 1.0,(float) 0.0); break;
						case KeyEvent.VK_RIGHT :
							Move((float)-1.0,(float) 0.0); break;
						case KeyEvent.VK_PAGE_UP :
							Zoom((float)1.3,(float) 1.3); break;
						case KeyEvent.VK_PAGE_DOWN :
							Zoom((float)0.75,(float) 0.75); break;
						case KeyEvent.VK_HOME :
							Reset(); break;

						case KeyEvent.VK_SPACE :
                            window.waitingForUser = false; break;
						case KeyEvent.VK_ESCAPE :
							System.exit(0);
					}
					UpdateStatusBar();
					repaint();
				}
            });
			//enableEvents();
            addMouseListener( new MouseListener () {
                public void mouseClicked (MouseEvent e) {
                }
                public void mouseEntered (MouseEvent e) {
                }
                public void mouseExited (MouseEvent e) {
                }
                /** Set the starting drag point. **/
                public void mousePressed (MouseEvent e) {
                    dragStart=e.getPoint();
                    //System.out.println("pressed: "+e.getPoint());
                }
                /** Set scaling/scrolling, or draw a rectangle. **/
                public void mouseReleased (MouseEvent e) {
                    int w=getSize().width, h=getSize().height;
                    Point end=e.getPoint();
                    Point dbs, dbe;
                    dbs=userToDB(dragStart);
                    dbe=userToDB(end);
                    //System.out.println("released: "+e.getPoint());
                    if ((dragStart.x!=-1 || dragStart.y!=-1) &&
                        (dragStart.x!=end.x) && (dragStart.y!=end.y) &&
                        (end.x>=0) && (end.x<=w) && (end.y>=0) && (end.y<=h)) {
                        float x0, x1, y0, y1, mag;
                        x0=Math.min((int)dbs.x, (int)dbe.x);
                        x1=Math.max((int)dbs.x, (int)dbe.x);
                        y0=Math.min((int)dbs.y, (int)dbe.y);
                        y1=Math.max((int)dbs.y, (int)dbe.y);
                        if (!drawMode) {
							xoff=-(x1+x0)/2; yoff=-(y1+y0)/2;
							mag = Math.max( (x1-x0)/w, (y1-y0)/h );
							ymag=-mag; xmag=mag;
							repaint();
							System.out.println("Mag: "+xmag+","+ymag+"  Off: "+xoff+","+yoff);
						} else {
							Layer dlayer = (Layer)layers.elementAt(cur);
							// add rectangle
							Poly poly = new Poly();
							poly.Insert(0, new Point((int)x0, (int)y0), null);
							poly.Insert(1, new Point((int)x1, (int)y0), null);
							poly.Insert(2, new Point((int)x1, (int)y1), null);
							poly.Insert(3, new Point((int)x0, (int)y1), null);
							//poly.Insert(4, new Point((int)x0, (int)y0), null);
							dlayer.Add(poly);
							repaint();
							System.out.println("rect: "+x0+","+y0+"  Off: "+x1+","+y1);
						}
                    }
                }
            });
            addMouseMotionListener( new MouseMotionListener () {
                public void mouseDragged (MouseEvent e) {
                }
                public void mouseMoved (MouseEvent e) {
                    SetMousePos(e.getPoint());
                }
            });
		}
		/** Force our size. **/
		public Dimension getMinimumSize() {
			return new Dimension((int)xsz, (int)ysz);
		}
		public Dimension getPreferredSize() {
			//if (window.getSize()!=null) {
			//  return window.getSize();
		  	//} else  {
			  return new Dimension((int)xsz, (int)ysz);
		    //}
		}
		public void ClearLayer(int l) {
			Layer lyr = (Layer)layers.elementAt(l);
			lyr.Clear();
		}
		/** Perform a boolean operation on layers. **/
		public void CombineLayers(int oper, int a, int b, int out) {
			PolyOperator pOper = new PolyOperator();
			Layer la = (Layer)layers.elementAt(a);
			Layer lb = (Layer)layers.elementAt(b);
			Layer lOut=pOper.CombineLayers(oper, la, lb);
			lOut.name=((Layer)layers.elementAt(out)).name;
			layers.set(out, lOut);
		}
		/** Draw the coverage polygon at the current mouse position. **/
		public void GrowPlug() {
			Layer inLayer = (Layer)layers.elementAt(src1);
			Layer outLayer = new Layer();
			layers.set(dst, outLayer);
			Poly p = inLayer.SquareGrowFromPoint(userToDB(mousePos), 212);
			outLayer.Add(p);
			repaint();
		}
		/** Convert lambda to screen coordinates. **/
        public Point dbToUser(Point pt) {
            int w=getSize().width, h=getSize().height;
            return new Point(
				(int)((pt.x-xoff)*xmag)+w/2,
				(int)((pt.y-yoff)*ymag)+h/2
            );
        }
        /** Convert screen coordinates to lambda. **/
        public Point userToDB(Point pt) {
            int w=getSize().width, h=getSize().height;
            return new Point(
				(int)((pt.x-(w/2))/xmag+xoff),
				(int)((pt.y-(h/2))/ymag+yoff)
            );
        }
        public void SetMousePos(Point pt) {
            mousePos = pt;
            UpdateStatusBar();
            HiliteSelection();
        }
        public void HiliteSelection() {
			Poly oldHilite = hilite;
			hilite = null;
			Point pt = userToDB(mousePos);
			Layer layer = (Layer)layers.elementAt(cur);
			for (int i=layer.Size()-1; i>=0; i--) {
				Poly pol = layer.Get(i);
				if (pol.PointInside(pt)) { hilite=pol; }
			}
			if (oldHilite!=hilite) { repaint(); }
		}
        /** Prints current input/output layers, mouse position, and draw mode. **/
        public void UpdateStatusBar() {
            Point upt = userToDB(mousePos);
			HiliteSelection();
			String hi = new String();
			if (hilite!=null) {
				hi="   Poly.. size "+hilite.Size()+" area "+hilite.Area();
			}
			window.statusBar.setText(
			  "Layers "+cur+" {"+src1+", "+src2+", "+dst+"} "+
			  (drawMode?"draw ":"scale ")+
			  "At: ("+upt.x+","+upt.y+")"+hi
			);
			Color color = colors[cur];
			//window.statusBar.setBackground(color);
			//color=new Color(255-color.getRed(), 255-color.getGreen(), 255-color.getBlue());
			window.statusBar.setForeground(color);
		}
		/** Reset scaling/scrolling. **/
		public void Reset() {
			xoff=0; yoff=0;
			xmag=(float).4; ymag=(float)-.4;
            UpdateStatusBar();
			repaint();
		}
		/** Set scrolling. **/
		public void Move(float x, float y) {
			xoff=xoff-x*(xsz/xmag)*(float).125;
			yoff=yoff-y*(ysz/ymag)*(float).125;
            UpdateStatusBar();
			repaint();
		}
		/** Set scaling. **/
		public void Zoom(float x, float y) {
			xmag=xmag*x;
			ymag=ymag*y;
            UpdateStatusBar();
			repaint();
		}

		public void PaintPoly(Graphics g, Poly p, Color color, boolean fill) {
            int w=getSize().width, h=getSize().height;
			g.setColor(color);
			MultiPoly mp=new MultiPoly(p, xoff, yoff, xmag, ymag, w, h);
			if (fill) { g.fillPolygon(mp.x, mp.y, mp.l); }
			else { g.drawPolygon(mp.x, mp.y, mp.l); }
		}

		public void PaintLayer(Graphics g, Layer layer, Color color, boolean fill) {
            int w=getSize().width, h=getSize().height;
			g.setColor(color);
			for (int p=0; p<layer.Size(); p++) {
				MultiPoly mp=new MultiPoly(layer.Get(p), xoff, yoff, xmag, ymag, w, h);
				if (fill) { g.fillPolygon(mp.x, mp.y, mp.l); }
				else { g.drawPolygon(mp.x, mp.y, mp.l); }
			}
		}
		public void paint(Graphics g) {
            int w=getSize().width, h=getSize().height;
			g.setColor(Color.gray);
			g.clearRect(0, 0, w, h);
			if (fillMode) {
				for (int l=0; l<layers.size(); l++) {
					Color color = colors[l];
					color=new Color(color.getRed(), color.getGreen(), color.getBlue(), 128);
					g.setColor(color);
					Layer layer=(Layer)layers.elementAt(l);
					PaintLayer(g, layer, color, true);
				}
			}
			for (int l=0; l<layers.size(); l++) {
				Color color = colors[l];
				g.setColor(color);
				Layer layer=(Layer)layers.elementAt(l);
				PaintLayer(g, layer, color, false);
			}
			if (hilite!=null) {
				PaintPoly(g, hilite, colors[cur], true);
			}
		}
	}
	public class ILGWindow extends Frame {
            int xSz = 700, ySz = 500;
            Panel panel = new Panel();
            ILGCanvas canvas;

            private class MyTextArea extends TextArea {
            
                public MyTextArea() throws UnsupportedOperationException {
                
                    super( "At (0, 0)", 1, 50, TextArea.SCROLLBARS_NONE );
                }

                public Dimension getPreferredSize() {
                    return new Dimension((int)xSz, (int)80);
                }
                public Dimension getMaxnimumSize() {
                    return new Dimension((int)Integer.MAX_VALUE, (int)80);
                }

            }

            TextArea statusBar = new MyTextArea();
            boolean waitingForUser = false;
            public ILGWindow() {
                super("ILG");
                canvas = new ILGCanvas(xSz, ySz);
                //statusBar.setColumns(50);
                //statusBar.setRows(50);
                statusBar.setEditable(false);
                panel.add(canvas);
                panel.add(statusBar);
                add(panel);
                setSize(xSz, ySz+80);
                show();
                repaint();
                addWindowListener(new WindowListener() {
                        public void windowOpened(WindowEvent e){}
                        public void windowClosing(WindowEvent e){
                            System.exit(0);
                        }
                        public void windowClosed(WindowEvent e){
                            System.exit(0);
                        }
                        public void windowIconified(WindowEvent e){}
                        public void windowDeiconified(WindowEvent e){}
                        public void windowActivated(WindowEvent e){}
                        public void windowDeactivated(WindowEvent e){}
                    });
                //enableEvents();
                //statusBar.addKeyListener(canvas.getKeyListener());
                statusBar.addKeyListener(new KeyListener() {
                        public void keyReleased(KeyEvent e) { canvas.dispatchEvent(e); }
                        public void keyTyped(KeyEvent e) { canvas.dispatchEvent(e); }
                        public void keyPressed(KeyEvent e) { canvas.dispatchEvent(e); }
                    });
                repaint();
            }
            /** Force our size **/
            public Dimension getMinimumSize() {
                return new Dimension(xSz, ySz);
            }
            public Dimension getPreferredSize() {
                return new Dimension(xSz, ySz);
            }
            public void RequestUpdate() { canvas.repaint(); }
            /** Method to pause for user, not used. **/
            public void WaitForUser() {
                waitingForUser = true;
                while (waitingForUser) {
                    try { Thread.sleep(100); } catch (Exception e) {}
                }
            }
	}
    public void MakeWindow() {
        window = new ILGWindow();
    }
    /** Print command line option info. **/
    public static void PrintUsage() {
        System.out.println("ILG [-gui] infile [outfile]");
        System.out.println("   Grows complementary layers to fill a space");
        System.out.println("   infile is a list of polygonalized layers in psuedo-Lisp form,");
        System.out.println("   a boundary layer, followed by two complementary layers ");
        System.out.println("      to fill the boundary space with.");
        System.out.println("   -gui starts in interactive mode and only dumps to stdout");
    }
    public static void main(String[] args) {
        String inName=null, outName=null;
        boolean gui = false;
        for (int i=0; i<args.length; i++) {
            if ((!gui) && args[i].equals("-gui")) {
                gui=true;
            } else if (inName==null) {
                inName = args[i];
            } else if (outName==null){
                outName = args[i];
            } else {
                System.out.println("ERROR: Extra argument: "+args[i]);
                PrintUsage();
                System.exit(1);
            }
        }
        if ((inName==null) || ((!gui) && (outName==null))) {
            System.out.println("ERROR: Insufficient arguments");
            PrintUsage();
            System.exit(1);
        }
		ILG ilg = new ILG();
        if (gui) {
			ilg.MakeWindow();
			ilg.window.setTitle("ILG  --  "+inName);
        }
		try {
			System.out.println("Reading "+inName);
			FileReader in = new FileReader(inName);
			ilg.ReadAllShapes(in);
			if (gui) {
				Layer combo=new Layer(), user=new Layer();
				combo.name="Combo"; user.name="User";
				ilg.layers.addElement(combo); // qui user combination layer
				ilg.layers.addElement(user); // gui drawing layer
				ilg.window.repaint();
			}
            else {
                System.out.println("Growing shapes in "+inName);
			    ilg.GrowAll();
			    ilg.ChoosePlugs();
			    System.out.println("Writing "+outName);
                PrintStream out = new
                    PrintStream(new FileOutputStream(outName), true);
                ilg.Dump(out);
            }
		} catch (Exception e) {
			e.printStackTrace();
            System.exit(1);
		}
	}
}





