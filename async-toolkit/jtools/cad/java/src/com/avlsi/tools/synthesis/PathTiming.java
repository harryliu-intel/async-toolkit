/*
 * Copyright 2011 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.util.ArrayList;

public class PathTiming {
	
	//strings direct from .timing file	
	
    public final String from;		//pin name
    public final String from_dir;   //+ or -
    public final String to;    	    //pin name
    public final String to_dir;	    //+ or -
    public final String delay;		//2 dim string, separated by "" and ,
    public final String slew;		//2 dim string, separated by "" and ,
    public final String power;      //2 dim string, separated by "" and , specifying the power
    
    //the following are parsed from the above
    
    //outer array dimension is input slew
    //inner array dimension is output cap
    
    public final ArrayList<ArrayList<Double>> delayTiming; //parsed values of delay
    public final ArrayList<ArrayList<Double>> slewTiming;  //parsed values of slew
    public final ArrayList<ArrayList<Double>> power2d; // parsed values of power 
    public final Double nominalInputSlew;
    public final ArrayList<Double> delayTiming1d;
    public final ArrayList<Double> slewTiming1d;
    public final ArrayList<Double> power1d;
    
    /** whether can this be modified after constructed **/
    final boolean mutable;
    
    /** 
     * Constructor using token array matching line format from input timing file 
     *
     */
    public PathTiming(String [] toks, CellTiming cellTiming) {
    	super();
        //toks 0 -path 1- A[1].1+ 2 - X.0+ 3- delay 2 d 4 -slew 2d 
    	this.from     = toks[1].substring(0,toks[1].length()-1);
        this.from_dir = toks[1].substring(toks[1].length()-1,toks[1].length());
        this.to       = toks[2].substring(0,toks[2].length()-1);
        this.to_dir   = toks[2].substring(toks[2].length()-1,toks[2].length());
        this.delay 	  = toks[3];
        this.slew     = toks[4];
        if(toks.length > 5)
            this.power = toks[5];
        else
            this.power = null;
        //System.out.println ("\nThe delay here is :"+this.delay);
        //System.out.println ("\nThe power here is :"+this.power);
        this.delayTiming = this.parsePathTimingString(delay);
		this.slewTiming = this.parsePathTimingString(slew);
        if(this.power != null) this.power2d = this.parsePathTimingString(power);     
        else this.power2d = null;
   		this.mutable = false;
		this.nominalInputSlew = cellTiming.realLibNominalInputSlew;
		this.delayTiming1d = this.interpolateValues(nominalInputSlew, cellTiming, delayTiming);
		this.slewTiming1d = this.interpolateValues(nominalInputSlew, cellTiming, slewTiming);
        if(this.power2d !=null)
            this.power1d = this.interpolateValues(nominalInputSlew, cellTiming, power2d);
        else
            this.power1d = null;
            
    }
    

	public PathTiming(
			String to,
			String toDir,
			ArrayList<ArrayList<Double>> delayTiming,
			ArrayList<ArrayList<Double>> slewTiming,
            ArrayList<ArrayList<Double>> power2d,
			CellTiming cellTiming) {
		from = null;
		from_dir = null;
		this.to = to;
		this.to_dir = toDir;
		delay = null;
		slew = null;
        power =null;
		this.nominalInputSlew = cellTiming.realLibNominalInputSlew;
        
				
		//make a value copy so original doesn't get modified
		this.delayTiming = new ArrayList<ArrayList<Double>>();
		for (int i=0; i<delayTiming.size(); i++) {
			ArrayList<Double> l = new ArrayList<Double>();
			for (int j=0; j<delayTiming.get(i).size(); j++) {
				l.add(delayTiming.get(i).get(j).doubleValue());
			}
			this.delayTiming.add(l);
		}
		
		//make a value copy so original doesn't get modified
		this.slewTiming = new ArrayList<ArrayList<Double>>();
		for (int i=0; i<slewTiming.size(); i++) {
			ArrayList<Double> l = new ArrayList<Double>();
			for (int j=0; j<slewTiming.get(i).size(); j++) {
				l.add(slewTiming.get(i).get(j).doubleValue());
			}
			this.slewTiming.add(l);
		}

        //make a value copy so original doesn't get modified
        if(power2d == null) this.power2d = null;
        else{
		    this.power2d = new ArrayList<ArrayList<Double>>();
	        for (int i=0; i<power2d.size(); i++) {
		        ArrayList<Double> l = new ArrayList<Double>();
			    for (int j=0; j<power2d.get(i).size(); j++) {
				l.add(power2d.get(i).get(j).doubleValue());
			    }
			    this.power2d.add(l);
		    }
        }

		
		this.delayTiming1d = this.interpolateValues(nominalInputSlew, cellTiming, this.delayTiming);
        this.slewTiming1d = this.interpolateValues(nominalInputSlew, cellTiming, this.slewTiming);
	    if(this.power2d !=null ) this.power1d = this.interpolateValues(nominalInputSlew, cellTiming, this.power2d);
        else this.power1d = null;
		
	
		this.mutable = true;
	}
    
    public void setMaxTiming(PathTiming b) {
    	
    	assert(mutable);
    	
    	ArrayList<ArrayList<Double>> d = maxTiming(this.delayTiming, b.delayTiming);
    	ArrayList<ArrayList<Double>> s = maxTiming(this.slewTiming, b.slewTiming);

        ArrayList<ArrayList<Double>> p ;
        if(this.power2d==null || b.power2d == null) p =null;
        else p= maxTiming(this.power2d,b.power2d);

    	//XXX should we max then interpolate or is it commutable?
    	ArrayList<Double> d1 = maxTiming1d(this.delayTiming1d, b.delayTiming1d);
    	ArrayList<Double> s1 = maxTiming1d(this.slewTiming1d, b.slewTiming1d);


        ArrayList<Double> p1; 
        if(this.power1d==null || b.power1d == null) p1 =null;
        else p1 = maxTiming1d(this.power1d, b.power1d);



    	
    	for (int i=0; i<this.delayTiming.size(); i++) {
    		for (int j=0; j<this.delayTiming.get(i).size(); j++) {
    			this.delayTiming.get(i).set(j, d.get(i).get(j));
    		}
    	}
    	for (int i=0; i<this.slewTiming.size(); i++) {
    		for (int j=0; j<this.slewTiming.get(i).size(); j++) {
    			this.slewTiming.get(i).set(j, s.get(i).get(j));
    		}
    	}
        if (p != null ){
            for (int i=0; i<this.power2d.size(); i++) {
        	    for (int j=0; j<this.power2d.get(i).size(); j++) {
    			    this.power2d.get(i).set(j, p.get(i).get(j));
    		    }
    	    }
        }


    	for (int j=0; j<this.delayTiming1d.size(); j++) {
    	    this.delayTiming1d.set(j, d1.get(j));
    	}
    	for (int j=0; j<this.slewTiming1d.size(); j++) {
            this.slewTiming1d.set(j, s1.get(j));
        }
        if ( p1 != null ){
             for (int j=0; j<this.power1d.size(); j++) {
                this.power1d.set(j, p1.get(j));
            }
        }

    }

    
	/**
	 * 
	 * @param s string containing timing info, skew or delay, expecting a 2 dimensional array "1,2,3,4,5,6,7","...","..."
	 * each string corresponds to an input skew value
	 * each element of the inner string corresponds to the output capacitance
	 * @return
	 */
	private ArrayList<ArrayList<Double>> parsePathTimingString(final String s) {
				
		ArrayList<ArrayList<Double>> timingList = new ArrayList<ArrayList<Double>>();
		
		String x = new String(s);
		while(x.length()>0) {
			//System.out.println("x:"+x);
			
			assert(x.substring(0, 1).equals("\""));
			x = x.substring(1, x.length());
			
			//parse out first string of outer dimension
			int i = x.indexOf("\"");
			String y = x.substring(0, i);
			//System.out.println("y:"+y);
			
			//remainder of outer dimension after parsing y
			x = x.substring(i+1, x.length()); 
			//strip "," if present
			if (x.length() > 0) {
				if (x.substring(0, 1).equals(",")) { x = x.substring(1, x.length()); }
			}
			//System.out.println("next rem: " + x);
			
			//parse the inner dimension
			ArrayList<Double> inner = CellTiming.parseCommaSeperatedString(y);
			timingList.add(inner);
		}
		
		return timingList;

	}
	
	public static final ArrayList<Double> maxTiming1d(
	    ArrayList<Double> a,
	    ArrayList<Double> b) {
	    
        ArrayList<Double> r = new ArrayList<Double>();
        
        for (int j=0; j < a.size(); j++) {
            Double aa = a.get(j);
            Double bb = b.get(j);
            Double m;
            if (aa.compareTo(bb) > 0) {
                m = aa;
            } else {
                m = bb;
            }
            r.add(m);
            
        }
        
        return r;
	}
	/**
	 * @param a
	 * @param b
	 * @return
	 * max of a and b
	 */
	public static final ArrayList<ArrayList<Double>> maxTiming(
			ArrayList<ArrayList<Double>> a,
			ArrayList<ArrayList<Double>> b
			) {
		
		ArrayList<ArrayList<Double>> r = new ArrayList<ArrayList<Double>>();
		
		assert(a.size() == b.size());
				
		for (int i = 0; i < a.size(); i++) {
			ArrayList<Double> aa = a.get(i);
			ArrayList<Double> bb = b.get(i);
			assert(aa.size() == bb.size());
			
			ArrayList<Double> rr = new ArrayList<Double>();
			
			for (int j=0; j < aa.size(); j++) {
				Double aaa = aa.get(j);
				Double bbb = bb.get(j);
				Double m;
				if (aaa.compareTo(bbb) > 0) {
					m = aaa;
				} else {
					m = bbb;
				}
				rr.add(m);
				//System.out.println("aaa="+aaa+" bbb="+bbb+ " m="+m);
			}
		
			r.add(rr);
		}
		
		
		return r;
	}

	public static final String emitTimingValues (ArrayList<ArrayList<Double>> t) {
		
		String s = new String();
		
		for (int j=0; j<t.size(); j++) {
			
			ArrayList<Double> a = t.get(j);
			
			s += "\""; //opening quote
			for (int i=0; i<a.size(); i++ ) {
				Double d = a.get(i);
				
				s += String.format("%4.3f", d);
				
				if (i+1 != a.size()) {
					s += ",";
				}
				
			}
			s += "\""; //closing quote
			
			if (j+1 != t.size()) {
				s += ",";
			}
		}
		
		return s;
	}
	
	public String emitTimingValues1d (ArrayList<Double> a) {
	    String s = new String();

	    s += "\""; //opening quote
	    for (int i=0; i<a.size(); i++ ) {
	        Double d = a.get(i);

	        s += String.format("%4.3f", d);

	        if (i+1 != a.size()) {
	            s += ",";
	        }

	    }
	    s += "\""; //closing quote

	    return s;
	}

	public String emitDelayTimingValues1d() {
	    return this.emitTimingValues1d(this.delayTiming1d);
	}

	public String emitSlewTimingValues1d() {
	    return this.emitTimingValues1d(this.slewTiming1d);
	}

    public String emitPowerValues1d() {
        if(this.power1d == null) return null;
 	    else return this.emitTimingValues1d(this.power1d);
	}


	/**
	 * 
	 * @param inputSlew interpolation point
	 * @param values either the delay 2d array or slew 2d array
	 * @return
	 * output values as interpolated at the specified inputSlew point
	 * not interpolation done on the output cap points because those stay
	 * constant
	 */
	public ArrayList<Double> interpolateValues(
	    Double inputSlew, CellTiming cellTiming,
	    ArrayList<ArrayList<Double>> values) {
	    	    
        //if the inputSlewsCnt = 0, then its a 1D table.
        //Hence return the 0th list from values
        if(cellTiming.inputSlewsCnt == 0) {
            return values.get(0);
        }

	    ArrayList<Double> r = new ArrayList<Double>();
	    //get array of input slew points from cellTiming

	    //figure out upper and lower index
	    int upperIdx = cellTiming.inputSlewValues.size()-1;
	    int lowerIdx = 0;
	    
	    Double upper = cellTiming.inputSlewValues.get(upperIdx);
	    Double lower = cellTiming.inputSlewValues.get(lowerIdx);
	    
	    assert(inputSlew <= upper) : "desired slew out of upper bound";
	    assert(inputSlew >= lower) : "desired slew out of lower bound";
	    
	    //find lower and upper value
	    for (int i=0; i<cellTiming.inputSlewValues.size()-1; i++) {
	        
	        //System.out.println("i="+i+" lower="+lower+" upper="+upper);
	        //System.out.println("test upper="+cellTiming.inputSlewValues.get(upperIdx-1));
	        if (inputSlew <= cellTiming.inputSlewValues.get(upperIdx-1)) { 
	            upperIdx--;
	            upper = cellTiming.inputSlewValues.get(upperIdx);
	        }
	        
	        if (cellTiming.inputSlewValues.get(lowerIdx+1) <= inputSlew) {
	            lowerIdx++;
	            lower = cellTiming.inputSlewValues.get(lowerIdx);
	        }
	        
	    }
	    assert(upperIdx - lowerIdx <= 1);
	    
	    //System.out.println("lower="+lower+" upper="+upper+" input="+inputSlew);
	    //System.out.println("lowerIdx="+lowerIdx+" upperIdx="+upperIdx);
	    
	    //interpolate values
	    ArrayList<Double> upperValues = values.get(upperIdx);
	    ArrayList<Double> lowerValues = values.get(lowerIdx);
	    Double delta = upper - lower;

	    for (int capIdx=0; capIdx<cellTiming.outputCapsCnt; capIdx++) {
	        Double v;
	        Double upperV = upperValues.get(capIdx);
	        Double lowerV = lowerValues.get(capIdx);
	        //System.out.println("uV="+ upperV+" lV="+lowerV);
	        if (delta==0) {
	            v = new Double (upperV);
	        } else {
	            v = new Double (((upper - inputSlew)*lowerV + (inputSlew - lower)*upperV)/delta);
	        }
	        r.add(v); 
	    }
	    return r;
	}


	public ArrayList<ArrayList<Double>> getDelayTiming() {
		return delayTiming;
	}
	

	public ArrayList<ArrayList<Double>> getSlewTiming() {
		return slewTiming;
	}
     
    public ArrayList<ArrayList<Double>> getPower2d() {
        return power2d;
    }


	/**
     * @return
     * max delay time corresponding to table entry for nominal
     * input slew
     */
    public Double getMaxDelay() {
        ArrayList<Double> l = this.delayTiming1d;
        Double m = new Double(0);
        for (Double d : l) {
            if (d.compareTo(m)>0) {
                m = d.doubleValue();
            }
        }
        return m;
    }
    
    /**
     * 
     * @return
     * max delay time corresponding to table entry for nominal
     * input slew
     */
    public Double getMaxSlew() {
        ArrayList<Double> l = this.slewTiming1d;
        Double m = new Double(0);
        for (Double d : l) {
            if (d.compareTo(m)>0) {
                m = d.doubleValue();
            }
        }
        return m;
    }
}
