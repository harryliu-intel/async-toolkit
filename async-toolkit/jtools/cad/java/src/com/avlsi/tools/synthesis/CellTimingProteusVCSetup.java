package com.avlsi.tools.synthesis;

import java.util.ArrayList;
import java.util.List;

public class CellTimingProteusVCSetup {
	
	CellTiming ct;
	
	/**
	 * Compute the setup timing constraints
	 * @param ct
	 */
	public CellTimingProteusVCSetup(
			String input,
			CellTiming ct) {
		this.ct = ct;
		
		//foreach output reachable by this input
    	//compute the timing slack for this input
    	//will need to do this for both
    	//rise_constraint (inputs with from_dir="-")
    	//and fall_constraint (inputs with from_dir="+")
   	
    	//list of all paths leading from this input
    	List<PathTiming> pts = ct.inputToPathTimingListMap.get(input);
    	//separate input paths by rising or falling
    	List<PathTiming> risingPaths = new ArrayList<PathTiming>();
    	List<PathTiming> fallingPaths = new ArrayList<PathTiming>();
    	if (pts != null) {
    		for (PathTiming p : pts) {
    			//s += "/* path "+p.from+" to "+p.to+" */\n";
    			if (p.from_dir.equals("+")) {
    				risingPaths.add(p);
    			} else if (p.from_dir.equals("-")) {
    				fallingPaths.add(p);                				
    			} else {
    				assert(false);
    			}
    		}
    	}
    	
    	//find max setup time for all paths from same input pin
    	//(negative values indicate existence of timing margins, so taking max to combine)
    	Vector maxSetupTimeRising;
    	Vector maxSetupTimeFalling;
    	
    	//Step A (output delay correction factor)
    	if (risingPaths.size() > 0) {
    		//s += "/* listing rising paths */\n";                		
    		maxSetupTimeRising  = new Vector(ct.inputSlewsCnt, Double.NEGATIVE_INFINITY);
    		//for rise_constraints
    		for (PathTiming p : risingPaths) {
    			maxSetupTimeRising = computeSetupConstraintsStepA(p, maxSetupTimeRising);
    		}
    		
    	} else {
    		maxSetupTimeRising = new Vector(ct.inputSlewsCnt, new Double(0));
    	}
    	//s += "/* max setup time rising = "+maxSetupTimeRising.emit()+" */\n";
    	
    	
//    	//Step B (output slew correction factor)
//    	if (risingPaths.size() > 0) {
//    		//TODO
//    		//for each timing arc, evaluate the output slew for the VC model
//    		for (PathTiming p : risingPaths) {
//    			//for each input slew & output cap evaluate the output slew of VC model
//    			for (int inputSlewIndex = 0; inputSlewIndex < ct.N_INPUT_SLEW; inputSlewIndex++) {
//    				
//    				//compute setup constraint correction factors
//    				//if output slew of VC model is slower then the timing model
//    				//additional timing needed if output slew of VC model is faster 
//    				Double fastSlewDelay;
//    				
//    				for (int outputCapIndex = 0; outputCapIndex < ct.N_OUTPUT_CAP; outputCapIndex++) {
//    					//VC model's output slew is independent of input slew
//    					//VC model's output delay is dependent on input slew
//    					
//    					
//    					TimingBlock outputTimingBlock = ct.outputTimingBlockMap.get(p.to);
//    					assert(outputTimingBlock!=null);
//    					//which output sense to use?
//    					String outputSense = p.to_dir;
//    					assert(outputSense.equals("+") | outputSense.equals("-"));
//    					
//    					//get output timing block data
//    					Double vcOutputDelay = outputSense.equals("+") ?
//    						outputTimingBlock.rise_timing.delayTiming.get(ct.realLibNominalInputSlewIndex).get(outputCapIndex) :
//    						outputTimingBlock.fall_timing.delayTiming.get(ct.realLibNominalInputSlewIndex).get(outputCapIndex); 
//    					Double vcOutputSlew = outputSense.equals("+") ?
//    						outputTimingBlock.rise_timing.slewTiming.get(ct.realLibNominalInputSlewIndex).get(outputCapIndex) :
//    						outputTimingBlock.fall_timing.slewTiming.get(ct.realLibNominalInputSlewIndex).get(outputCapIndex);
//    					
//    					
//    					//setup time delay was computed by step A
//    					Double stepASetupTimeRising = maxSetupTimeRising.get(inputSlewIndex);
//    					
//    					//subtract setup time from output delay to get actual delay of vc model
//    					Double effectiveOutputDelay = vcOutputDelay - stepASetupTimeRising;
//    					
//    					
//    					Double actualOutputDelay = p.delayTiming.get(inputSlewIndex).get(outputCapIndex);
//    					Double actualOutputSlew  = p.slewTiming.get(inputSlewIndex).get(outputCapIndex);
//    					
//    					//compute slew as delay, convert from .33 .66 to 0 to 100% levels
//    					//assuming linear transition
//    					Double vcOutputTransitionDelay = (100.0 / (ct.slewUpperThreshold  - ct.slewLowerThreshold)) * vcOutputSlew;
//    					Double outputTransitionDelay = (100.0 / (ct.slewUpperThreshold - ct.slewLowerThreshold)) * actualOutputSlew;
//    					
//    					if (vcOutputTransitionDelay > outputTransitionDelay) {
//    						//VC output slew is slower than actual,
//    						//may be able to save some of the timing slack
//    					}
//    					
//    					String s = String.format("/* VC  [S=%d,C=%d] output slew %5.3f delay %5.3f (effective) delay %5.3f (VC->out) setup %5.3f */", 
//    							inputSlewIndex,
//    							outputCapIndex,
//    							vcOutputSlew,
//    							effectiveOutputDelay,
//    							vcOutputDelay,
//    							stepASetupTimeRising
//    							);
//    					
//    					
//    					
//    					//compare to timing arc
//    					
//    					String s2 = String.format("/* path[S=%d,C=%d] output slew %5.3f delay %5.3f */",
//    							inputSlewIndex,
//    							outputCapIndex,
//    							actualOutputSlew,
//    							actualOutputDelay);
//    					
//    					//DEBUG
//    					//System.out.printf("/* path %8s %8s rising */\n", p.from, p.to);
//    					//System.out.println(s);
//    					//System.out.println(s2);
//    				}
//    			}
//    		}
//    	}
    	
    	//repeat above for falling paths
    	if (fallingPaths.size() > 0) {
    		maxSetupTimeFalling = new Vector(ct.inputSlewsCnt, Double.NEGATIVE_INFINITY);
    		//s += "/* listing falling paths */\n";
    		for (PathTiming p : fallingPaths) {
    			maxSetupTimeFalling = computeSetupConstraintsStepA(p, maxSetupTimeFalling);
    		}
    		
    	} else {
    		maxSetupTimeFalling = new Vector(ct.inputSlewsCnt, new Double(0));
    	}
    	//s += "/* max setup time falling = "+maxSetupTimeFalling.emit()+" */\n";
		this.setupRising = maxSetupTimeRising;
		this.setupFalling = maxSetupTimeFalling;
	}
	
	
	/** contains setup rising times */
	final Vector setupRising;
	/** contains setup falling times */
	final Vector setupFalling;
		
	
    /**
     * for lib_vc task
     * compute setup delay constraints for the timing arc specified by p
     * @param p
     * @param maxSetupTime tracking setup time for all p from same input
     * @param s printout
     * @return
     */
    private Vector computeSetupConstraintsStepA(PathTiming p, Vector maxSetupTime) {
		// p is an rising input to output timing arc
    	String s = "";
		//figure out for this output
		//what is the max delay/skew over all inputs and for the given 
		//realLibNominalInputSlewIndex going to this output
		//can get this from the max timing block that we have
		//already computed
		//use the realLibNominalInputSlewIndex to pick a particular
		//set of delay/slew timing arc values to compute the output delay/slew
		TimingBlock maxTimingBlock = ct.outputTimingBlockMap.get(p.to);
		assert(maxTimingBlock!=null);
		//which output sense to use?
		String outputSense = p.to_dir;
		assert(outputSense.equals("+") | outputSense.equals("-"));
		
		//get the max delay/skew from this output
		//max over all output caps
		Double outputMaxDelay = outputSense.equals("+") ?
			maxTimingBlock.rise_timing.getMaxDelay() :
			maxTimingBlock.fall_timing.getMaxDelay(); 
		Double outputMaxSlew = outputSense.equals("+") ?
			maxTimingBlock.rise_timing.getMaxSlew() :
			maxTimingBlock.fall_timing.getMaxSlew();
		
		s += String.format("/* path from %s to %s */\n", p.from, p.to);
		s += String.format("/* outputMaxDelay=%f */\n", outputMaxDelay);
		s += String.format("/* outputMaxSlew=%f */\n", outputMaxSlew);
		

		//vector will store one entry per input slew corresponding to max output
		//delay for that input slew
		Vector pathMaxDelayOverOutputCaps = new Vector();
		
		//setup time per input slew
		Vector pathSetupTime = new Vector();
		
		
		//for each input slew
		for (int i0 = 0; i0 < p.getDelayTiming().size(); i0++) {
			//delay times for different output cap
			Vector delayByOutputCap = new Vector(p.getDelayTiming().get(i0));
			//get the worst case delay to get rid of output cap dependence
			Double maxDelayOverOutputCaps = delayByOutputCap.getMax();
			
			pathMaxDelayOverOutputCaps.add(maxDelayOverOutputCaps);
			s += String.format("/* path delay [input slew=%d] (maxed over output caps) = %f */\n", i0, maxDelayOverOutputCaps);
			
			Double setupTimingSlack = outputMaxDelay - maxDelayOverOutputCaps;
			pathSetupTime.add(-setupTimingSlack);
			
			s += String.format("/* setup timing slack [input slew=%d] = %f */\n", i0, setupTimingSlack);
		}
		s += "/* setup delay */\n";
		s += "/* setup time:  "+pathSetupTime.emit() +" */\n";
		
		//DEBUG
		//System.out.println(s);
		
		//combine to get max setup time over all instances
		return Vector.maxCopy(maxSetupTime, pathSetupTime);

    }
    
	

	public Vector getSetupRising() {
		return setupRising;
	}
	
	public Vector getSetupFalling() {
		return setupFalling;
	}
}
