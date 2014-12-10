/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.ilg;

import java.util.*;

// NOTE: this class is sorted on demand, not by default!
public class SortableVector extends Vector {

	public SortableVector() {}
	public SortableVector(Collection c) {
		super(c);
	}

	public void Sort() {
		Sort(0, size()-1);
		CleanUp(0, size()-1);
	}
	public void Swap(int a, int b) {
		Object t = elementAt(a);
		set(a, elementAt(b));
		set(b, t);
	}
	public int Compare(int a, int b) {
		return ((Comparable)elementAt(a)).compareTo(elementAt(b));
	}
	public void Sort(int l, int r) {
		int M=4, i, j, k;
		if ((r-l)>M) {
			i=(r+l)/2;
			if (Compare(l,i)>0) Swap(l,i);
			if (Compare(l,r)>0) Swap(l,r);
			if (Compare(i,r)>0) Swap(i,r);
			j=r-1;
			Swap(i,j);
			i=l; k=j;
			for(;;)	{
				while(Compare(++i,k)>0);
				while(Compare(--j,k)>0);
				if (j<i) break;
				Swap(i,j);
			}
			Swap(i,r-1);
			Sort(l,j);	Sort(i+1,r);
		}
	}

	public void CleanUp(int lo, int hi)  { // was insertion sort...
		int i, j;
		for (i=lo+1;i<=hi;i++) {
			Object o = elementAt(i);
			j=i;
			while ((j>lo) && (Compare(j-1,i)>0)) {
				set(j,elementAt(j-1));
				j--;
			}
			set(j, o);
		}
	}
}
