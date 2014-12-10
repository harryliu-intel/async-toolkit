/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.ilg;

import java.util.*;
import java.util.Random;

import com.avlsi.tools.ilg.SortableVector;

public class GA {
	public interface GeneEvaluator {
		float GetFitness(Gene g);
	}
	public static Random rand = new Random();

	// popSize must be a multiple of 2 greater than 2
	public int popSize=50;
	public float mutationRate=(float).03, crossoverRate=(float).67, elitism=(float).33;
	public SortableVector population = new SortableVector();
	// NOTE: THESE SHOULD GET SET
	int geneSize=20, geneMaxIndex=1000;
	GeneEvaluator evaluator = null;

	public void Clear() { population.clear(); }
	public void Populate(int _geneSize, int _geneMaxIndex) {
		population.clear();
		population.ensureCapacity(popSize);
		geneSize=_geneSize; geneMaxIndex=_geneMaxIndex;
		int delta = geneSize*2;
System.out.println("Creating population");
System.out.println("POPULATE pop: "+popSize+", geneSz: "+geneSize+", delta: "+delta);
		for (int i=popSize-1; i>=0; i--) {
			Gene g = new Gene(geneSize+rand.nextInt(delta), geneMaxIndex);
			float fitness = g.Fitness();
//System.out.println("Gene ("+(popSize-i)+"): "+fitness);
			population.add(g);
		}
		population.Sort();
System.out.println("Done Creating population ("+population.size()+" of "+popSize+" entries)");
	}

	// do one generation...
	public void Step() {
		SortableVector v = population;
		population = new SortableVector();
		int len = v.size();
		int delta=(int)(len*elitism), start=len-delta;
//System.out.println("STEP pop: "+popSize+", len: "+len+", start: "+start+", delta: "+delta);
		int dup=0;
		// extreme eletism. always save 2 best genes...
		population.add(v.elementAt(len-1));
		population.add(v.elementAt(len-2));
		for (int i=popSize-3; i>=0; i-=2) {
//System.out.print(" "+i);
			Gene ga = new Gene((Gene)v.elementAt(start+rand.nextInt(delta)));
			Gene gb = new Gene((Gene)v.elementAt(start+rand.nextInt(delta)));
			if (rand.nextFloat()<=crossoverRate) {
				ga.Cross(gb);
			}
			if (rand.nextFloat()<=mutationRate) { ga.Mutate(geneMaxIndex); }
			if (rand.nextFloat()<=mutationRate) { gb.Mutate(geneMaxIndex); }
			// recalculate fitness
			ga.Fitness(); gb.Fitness();
			if (!population.add(ga)) { dup++; }
			if (!population.add(gb)) { dup++; }
		}
		population.Sort();
//System.out.println("Best Fitness is "+GetBest().fitness);
//System.out.println("Done with Step ("+population.size()+"-"+dup+" of "+popSize+" entries)");
	}

	public Gene GetBest() { return (Gene)population.elementAt(population.size()-1); }

	public class Gene implements Comparable {
		public float fitness=0;
		public int chrom[];

		public Gene() {}
		// random creation
		public Gene(int size, int max) {
			chrom = new int[size];
//System.out.print("NEW Gene"+population.size());
			for (int i=size-1; i>=0; i--) {
				chrom[i]=GA.rand.nextInt(max);
//System.out.print(" "+chrom[i]);
			}
//System.out.println();
		}
		public Gene(Gene g) { Copy(g); }
		public float Fitness() {
			fitness = evaluator.GetFitness(this);
			return fitness;
		}
		public void Cross(Gene g) {
			int i, len = chrom.length, lenb = g.chrom.length;
			int site = GA.rand.nextInt(len);
			int siteb = GA.rand.nextInt(lenb);
			int tempa[] = new int[site+lenb-siteb];
			int tempb[] = new int[siteb+len-site];
			for (i=0; i<site; i++) { tempa[i]=chrom[i]; }
			for (i=0; i<siteb; i++) { tempb[i]=g.chrom[i]; }
			for (i=0; i<lenb-siteb; i++) { tempa[i+site]=g.chrom[i+siteb]; }
			for (i=0; i<len-site; i++) { tempb[i+siteb]=chrom[i+site]; }
			chrom=tempa; g.chrom=tempb;
		}
		public void Mutate(int max) {
			// single site mutation
			int site = GA.rand.nextInt(chrom.length);
			chrom[site] = GA.rand.nextInt(max);
		}
		public void Copy(Gene g) {
			int len = g.chrom.length;
			chrom = new int[len];
			for (int i=len-1; i>=0; i--) {
				chrom[i]=g.chrom[i];
			}
		}
		public int compareTo(Object o) {
			int dif=0;
			if (o instanceof Gene) {
				Gene g = (Gene)o;
				int length = chrom.length;
				int glength = g.chrom.length;
				if      (g.fitness>fitness) { dif=-1; }
				else if (g.fitness<fitness) { dif= 1; }
				else if (glength<length)   { dif=-1; }
				else if (glength>length)   { dif= 1; }
				else {
					for (int i=length-1; i>=0; i--) {
						if (g.chrom[i]>chrom[i])       { dif=-1; break; }
						else if (g.chrom[i]<chrom[i])  { dif= 1; break; }
					}
				}
			} else {
				throw new ClassCastException();
			}
			return dif;
		}
	}
}

