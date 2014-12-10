/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.container;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * Keep some objects lazy-sorted, allowing fast finds and adds.
 * @todo jmr clean mu up sometime
 *
 * @author Andrew Lines
 * @version $Date$
 * @review kiniry 18-23 July 2002
 *
 * @bug kiniry 23 July 2002 - MultiSet is being abused (memory- and
 * performance-wise) probably via the AliasedMap.  The primary culprits
 * are find(), findIndex(), and binarySearch().  E.g., approximately
 * 25% of the runtime of moderate sized PrsToNet jobs is spent in this
 * class.
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1168">Bug#1168</a>
 **/
public class MultiSet extends AbstractCollection 
    implements Collection, Comparable {
  private boolean fully_sorted; // is the MultiSet completely  or lazy-?
  private final Comparator comp; // a comparison class
  private final List elems; // holds the objects of the MultiSet

  /**
   * Constructs an empty multi-set using the element's natural order
   * as the comparator.
   **/
  public MultiSet() {
      this(new NaturalOrderComparator());
  }
 
  /** Create an empty MultiSet with the specified comparison method. */
  public MultiSet(Comparator comp)
    {
    this.comp=comp;
    fully_sorted=true;
    elems=new ArrayList();
    }

  /**
   * Constructs a multi-set containing the elements from the collection,
   * using the element's natural order as the comparator.
   **/
  public MultiSet(final Collection coll) {
      this();
      addAll(coll);
  }

  /**
   * Constructs a multi-set containing the elements from the collection,
   * using the element's natural order as the comparator.
   **/
  public MultiSet(final Collection coll, final Comparator comp) {
      this(comp);
      addAll(coll);
  }


  /**
   * Binary search between [lo..hi), returning the index of an object
   * that compares equal to <code>obj</code> using the Comparator. 
   * Returns -1 if no such element was found.
   *
   * @param lo unknown.
   * @param hi unknown.
   * @param obj unknown.
   * @return index of found object, or -1 if none found.
   *
   * @bug kiniry 23 July 2002 - MultiSet is being abused (memory- and
   * performance-wise) probably via this class.  The primary culprits
   * are find(), findIndex(), and binarySearch().  E.g., approximately
   * 25% of the runtime of moderate sized PrsToNet jobs is spent in this
   * class.
   * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1168">Bug#1168</a>
   **/
  private int binarySearch(int lo, int hi, Object obj, Comparator comp)
    {
    // we should be able to just do this, but it doesn't seem to work:
    // return Collections.binarySearch(elems.subList(lo, hi), obj, comp);
    //
    // ... shouldn't it be:
    // int x = Collections.binarySearch(elems.subList(lo, hi), obj, comp);
    // return x >= 0 ? x + lo : -1;
    while (lo<hi)
      {
      int mid,c;
      mid=(lo+hi)/2;
      c=comp.compare(elems.get(mid),obj);
      if      (c>0) hi=mid;
      else if (c<0) lo=mid+1;
      else          return mid;
      }
    return -1;
    }

  /**
   * Binary search between [lo..hi), returning the index of an object
   * that compares equal to <code>obj</code> using the current
   * Comparator.  Returns -1 if no such element was found.
   **/
  private int binarySearch(int lo, int hi, Object obj)
    {
    return binarySearch(lo, hi, obj, comp);
    }

  /** 
   * Find any object which matches under comparison method, return
   * null if none. 
   *
   * @param obj unknown.
   * @return unknown.
   *
   * @bug kiniry 23 July 2002 - MultiSet is being abused (memory- and
   * performance-wise) probably via this class.  The primary culprits
   * are find(), findIndex(), and binarySearch().  E.g., approximately
   * 25% of the runtime of moderate sized PrsToNet jobs is spent in this
   * class.
   * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1168">Bug#1168</a>
   */
  public Object find(Object obj)
    {
    int i = findIndex(obj);
    if (i>=0) return elems.get(i);
    else return null;
    }

  /** 
   * Find any object which matches under comparison method, return its
   * index or -1. 
   *
   * @param obj unknown.
   * @return unknown.
   *
   * @bug kiniry 23 July 2002 - MultiSet is being abused (memory- and
   * performance-wise) probably via this class.  The primary culprits
   * are find(), findIndex(), and binarySearch().  E.g., approximately
   * 25% of the runtime of moderate sized PrsToNet jobs is spent in this
   * class.
   * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1168">Bug#1168</a>
   */
  public int findIndex(Object obj)
    {
    int i,max=elems.size();
    if (fully_sorted) // binary search through  list
      {
      i=binarySearch(0,max,obj);
      if (i>=0) return i;
      }
    else // search in lazy-sorted list
      {
      int x=max,step;
      for (step=1; step<=max; step*=2) if ((step&max)!=0) // find in all sorted chunks
	{
        int ox=x;
	x-=step;
	i=binarySearch(x,ox,obj);
        if (i>=0) return i;
	}
      }
    return -1;
    }

  /** Enumerate all objects which match under current comparison method. */
  public Iterator findAll(final Object obj)
    {
    return findAll(obj, comp);
    }

  /**
   * Enumerate all objects which match under the specified comparison method.
   * <code>comp</code> must be "compatible" with the current comparison method,
   * or the result will be undefined.  A comparator C1 is compatible with
   * comparator C2 if for any two objects O1 and O2, sgn(C1.comp(O1, O2)) ==
   * sgn(C2.comp(O1, O2)) || C1.comp(O1, O2) == 0.
   **/
  public Iterator findAll(final Object obj, final Comparator comp)
    {
    int i,j,max=elems.size();
    sort(); // easier to do if fully sorted
    i=binarySearch(0,max,obj,comp);
    while ((i>0)&&(comp.compare(elems.get(i-1),obj)==0)) i--; // find first match
    final int start=i;
    return new Iterator()
      {
      private int j=start;

      public boolean hasNext()
        {
        if ((j>=0)&&(j<elems.size())&&(comp.compare(elems.get(j),obj)==0)) return true;
	return false;
	}

      public Object next()
	{
        if (!hasNext()) throw new NoSuchElementException();
        return elems.get(j++);
	}

      public void remove()
	{
        throw new UnsupportedOperationException();
	}
      };
    }

  /** Merge [a..b) and [b..c) into [a..c). */
  private void merge(int a, int b, int c)
    {
    int max=c-a,i0,i1,i2;
    if ((a>=b)||(b>=c)) return; // early out for trivial cases
    final Object[] nv = new Object[max];
    i0=a; i1=b;
    for (i2=0; i2<max; i2++)
      {
      boolean s;
      if      ((i0>=b)&&(i1<c)) s=true;
      else if ((i0<b)&&(i1>=c)) s=false;
      else    s=(comp.compare(elems.get(i1),elems.get(i0))<0);
      if (s) nv[i2] = elems.get(i1++);
      else   nv[i2] = elems.get(i0++);
      }
    for (i2=0; i2<max; i2++) elems.set(i2+a,nv[i2]);
    }

  /** Get nth sorted element. */
  public Object get(int index)
    {
    sort();
    return elems.get(index);
    }

  /** Add a new object to the set then lazy-sort. */
  public boolean add(Object obj)
    {
    int max,step;
    fully_sorted=false;
    elems.add(obj); // add to end of the vector
    max=elems.size();      // get new size of vector
    for (step=1; (step&max)==0; step*=2) // merge newly complete powers of 2
      merge(max-2*step,max-step,max);
    return true;
    }

  /** Remove all matching elements from the MultiSet. */
  public boolean remove(final Object obj)
    {
    int i,max=elems.size();
    sort(); // easier to do if fully sorted
    i=binarySearch(0,max,obj); // find any match
    if (i<0) return false; // no matches found
    while ((i>0)&&(comp.compare(elems.get(i-1),obj)==0)) i--; // find first match
    while ((i<elems.size())&&(comp.compare(elems.get(i),obj)==0)) elems.remove(i);
    return true; // one or more matches removed
    }

  /** Fully sort the set by current comparison method. */
  public void sort()
    {
    int max,step,x;
    if (fully_sorted) return;
    fully_sorted=true;
    x=max=elems.size();
    for (step=1; step<=max; step*=2) if ((step&max)!=0) // merge incomplete powers of 2
      {
      int ox=x;
      x-=step;
      merge(x,ox,max);
      }
    }

  /** Lexicographically compare this set to another (implements Comparator). */
  public static class MultiSetComparator implements Comparator
    {
    public static int compare(MultiSet a, MultiSet b)
      {
      a.sort();
      b.sort();
      int c,amax=a.elems.size(),bmax=b.elems.size();
      if (!a.comp.equals(b.comp))
          throw new InconsistentlySortedException();
      a.sort(); b.sort(); // must be fully sorted
      for (int i=0; (i<amax)&&(i<bmax); i++)
	{
	c=a.comp.compare(a.elems.get(i),b.elems.get(i));
	if (c<0) return -1;
	if (c>0) return  1;
	}
      if (amax<bmax) return -1;
      if (amax>bmax) return  1;
      return 0;
      }
    public int compare(Object A, Object B)
      {
      MultiSet a=(MultiSet)A, b=(MultiSet)B;
      return MultiSetComparator.compare(a, b);
      }
    }

  /** Lexicographically compare this set to another (implements Comparable) */
  public int compareTo (Object B) {
    return MultiSetComparator.compare(this, (MultiSet) B);
  }

  /****************** Simple but convenient methods *******************/

  public int size() {
      return elems.size();
  }

  /** Add a Collection of objects. */
  public boolean addAll(final Collection c)
    {
    for (final Iterator i = c.iterator(); i.hasNext(); )
        add(i.next());
    return true;
    }

  /** Add a new object only if it wasn't already in the MultiSet. */
  public void addIfUnique(Object obj)
    {
    if (find(obj)==null) add(obj);
    }

  /** Add a Collection of new objects if they are unique. */
  public void addAllIfUnique(final Collection c)
    {
    for (final Iterator i = c.iterator(); i.hasNext(); )
        addIfUnique(i.next());
    }

  /** Check if this MultiSet is a subset or equal of another set.
   *  NOTE: Uses comparator of the other set.
   *  NOTE: An empty set is considered a subset of any other set.
   */
  public boolean isSubset(MultiSet s)
    {
    for (int i=0; i<elems.size(); i++)
      if (s.find(elems.get(i))==null) return false;
    return true;
    }

  /** Return an enumeration of all sorted objects in the MultiSet. */
  public Iterator iterator()
    {
    sort();
    return elems.iterator();
    }

  /** Return a sorted list **/
  public List list()
    {
    sort();
    return elems;
    }

  /** Debugging toString of all objects in the MultiSet. */
  public String toString()
    {
    sort();
    return elems.toString();
    }

  /** Exception given when comparing MultiSet's with inconsistent Comparators. */
  public static class InconsistentlySortedException extends RuntimeException
    {
    }

  }
