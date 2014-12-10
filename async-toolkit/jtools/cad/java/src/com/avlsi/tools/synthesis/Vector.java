package com.avlsi.tools.synthesis;

import java.util.ArrayList;

public class Vector {
	
	ArrayList<Double> d;

	public Vector(ArrayList<Double> d) {
		super();
		assert(d!=null);
		this.d = new ArrayList<Double>();
		for (int i=0; i<d.size(); i++) {
			this.d.add(d.get(i).doubleValue());
		}
	}
	
	public Vector(int n, Double v) {
		super();
		this.d = new ArrayList<Double>();
		for (int i=0; i<n; i++) {
			d.add(v.doubleValue());
		}
	}
	
	public Vector() {
		super();
		this.d = new ArrayList<Double>();
	}
	
	public void add(Double v) {
		d.add(v);
	}
	
	public Double get(int index) {
		return d.get(index);
	}
	
	public String getElementAsString(int index) {
	    return String.format("%6.3f", this.get(index));
	}
	
	public Double getMin() {		
		Double m = Double.POSITIVE_INFINITY;
		if (d==null) { return m; }
		for (Double v : d) {
			if (v.compareTo(m) < 0) { m = v.doubleValue(); }
		}
		return m;
	}
	
	public Double getMax() {		
		Double m = Double.NEGATIVE_INFINITY;
		if (d==null) { return m; }
		for (Double v : d) {
			if (v.compareTo(m) > 0) { m = v.doubleValue(); }
		}
		return m;
	}
	
	public int size() {
		return d.size();
	}
	
	public static Vector minCopy(Vector a, Vector b) {
		assert(a.size() == b.size());
		Vector m = new Vector(a.size(), new Double(0));
		for (int i=0; i<a.size(); i++) {
			if (a.d.get(i).compareTo(b.d.get(i)) < 0) {
				m.d.set(i, a.d.get(i).doubleValue());
			} else {
				m.d.set(i, b.d.get(i).doubleValue());
			}
		}
		return m;
	}
	
	public static Vector maxCopy(Vector a, Vector b) {
		assert(a.size() == b.size());
		Vector m = new Vector(a.size(), new Double(0));
		for (int i=0; i<a.size(); i++) {
			if (a.d.get(i).compareTo(b.d.get(i)) > 0) {
				m.d.set(i, a.d.get(i).doubleValue());
			} else {
				m.d.set(i, b.d.get(i).doubleValue());
			}
		}
		return m;
	}
	
	
	/**
	 * overwrite contents of array with b's contents if b is smaller
	 * @param b
	 */
	public void minMerge(Vector b) {
		assert(d.size() == b.size());
		for (int i=0; i<b.size(); i++) {
			if (d.get(i).compareTo(b.d.get(i)) <= 0) {
				
			} else {
				d.set(i, b.d.get(i).doubleValue());
			}
		}
	}
	
	/**
	 * stores the value of min(v,e) back into element e
	 * 
	 * @param v
	 */
	public void minWith(Double v) {
		for (int i=0; i<d.size(); i++) {
			if (d.get(i).compareTo(v) >= 0) {
				d.set(i, v.doubleValue());
			}
		}
	}
	
	/**
	 * stores the value of max(v,e) back into element e
	 * 
	 * @param v
	 */
	public void maxWith(Double v) {
		for (int i=0; i<d.size(); i++) {
			if (d.get(i).compareTo(v) <= 0) {
				d.set(i, v.doubleValue());
			}
		}
	}
	
	/**
	 * 
	 * @return
	 * quoted, comma seperated list
	 * "0.001,0.002,0.003,...,n"
	 */
	public String emit() {
		String s = "\"";
		for (int i=0; i<d.size(); i++) {
			//don't print -0, make all zeros positive
			if (d.get(i).equals(new Double(0))) { d.set(i, new Double(0)); } 
			s += String.format("%4.3f", d.get(i));
			if (i+1!=d.size()) { s+=","; }
		}
		s += "\"";
		return s;
	}
	
	

}
