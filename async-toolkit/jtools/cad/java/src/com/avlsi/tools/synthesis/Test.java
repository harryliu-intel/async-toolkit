package com.avlsi.tools.synthesis;

import java.util.ArrayList;


public class Test {
	
	ArrayList<ArrayList<Double>> delayList = null; 

	//delay or slew string
	static final String delay = "\"0.077,0.087,0.104,0.133,0.187,0.29,0.494\",\"0.094,0.104,0.121,0.15,0.204,0.308,0.511\",\"0.116,0.127,0.145,0.176,0.231,0.335,0.539\"";
	static final String slew  = "\"0.077,0.087,0.104,0.133,0.187,0.29,0.494\",\"0.094,0.104,0.121,0.15,0.204,0.308,0.511\",\"0.116,0.127,0.145,0.176,0.231,0.335,0.539\"";
	
	
	static final String delay2 = "\"0.999,0.087,0.999,0.133,0.187,0.29,0.494\",\"0.094,0.104,0.121,0.15,0.204,0.308,0.511\",\"0.116,0.127,0.145,0.176,0.231,0.335,0.539\"";
	static final String slew2  = "\"0.077,0.08,0.104,0.133,0.187,0.29,0.494\",\"0.094,0.104,0.121,0.15,0.204,0.308,0.511\",\"0.116,0.127,0.145,0.176,0.231,0.335,0.539\"";
	/**
	 * 
	 * @param s string containing timing info, skew or delay, expecting a 2 dimensional array "1,2,3,4,5,6,7","...","..."
	 * each string corresponds to an input skew value
	 * each element of the inner string corresponds to the output capacitance
	 * @return
	 */
	public static ArrayList<ArrayList<Double>> parsePathTimingString(final String s) {
				
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
			ArrayList<Double> inner = new ArrayList<Double>();
			while(y.length()>0) {
				int j = y.indexOf(",");
				String v;
				if (j == -1) { 
					v = y; 
					y = "";			
				}
				else { 
					v = y.substring(0, j);
					y = y.substring(j+1, y.length());
				}
				
				//System.out.println("v="+v);
				//System.out.println("y="+y);
				
				Double d = Double.valueOf(v);
				inner.add(d);
				
			}
			timingList.add(inner);
		}
		
		return timingList;

	}
	

	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
//		//parse the delay/skew string into a 2-dimensional array
//		String x = delay;
//
//		ArrayList<ArrayList<Double>> timingList = parsePathTimingString(x);
//		
//		System.out.println("original string x="+x);
//		
//		for ( ArrayList<Double> a : timingList ) {
//			for (int i=0; i<a.size(); i++ ) {
//				Double d = a.get(i);
//				
//				System.out.printf("%4.3f", d);
//				
//				if (i+1 != a.size()) {
//					System.out.print(",");
//				}
//				
//			}
//			System.out.println();
//		}
		
		
		//PathTiming pt = new PathTiming("from", "+", "to", "+", delay, slew);
		//PathTiming pt2 = new PathTiming("from", "+", "to", "+", delay2, slew2);
		//ArrayList<ArrayList<Double>> r = PathTiming.maxTiming(pt.getDelayTiming(), pt2.getDelayTiming());
		//System.out.println("size r="+r.size());
		//System.out.println("max:\n" + PathTiming.emitTimingValues(r));
		
		ArrayList<ArrayList<Double>> delay, slew;
		delay = new ArrayList<ArrayList<Double>>();
		slew = new ArrayList<ArrayList<Double>>();
		double offset = 1;
		for (int j=0; j<3; j++) {
		    offset*=10.0;
    		ArrayList<Double> d1 = new ArrayList<Double>();
    		ArrayList<Double> s1 = new ArrayList<Double>();
    		for (int i=0; i<7; i++) {
    		    //System.out.println("j="+j+" i="+i+" v="+i*offset);
    		    d1.add(new Double(i*offset));
    		    s1.add(new Double(i*offset));
    		}
    		delay.add(d1);
    		slew.add(s1);
		}
		
		
		CellTiming ct = new CellTiming(
		    "/home/user/zloh/hw/cast/synthesis/timing/qdi",
		    "synthesis.qdi.logic2.LOGIC2_1.2",
		    true,
		    false);
		
		PathTiming pt = new PathTiming("to", "+", delay, slew, ct);
		
		ArrayList<Double> interp = pt.interpolateValues(0.017, ct, delay);
		for (Double v: interp) {
		    System.out.println("v="+v);
		}
	}
	
	


}
