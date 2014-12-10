package com.avlsi.tools.synthesis;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import com.avlsi.util.container.SortingIterator;

public class TimingBlock {

	final String input;
	final String output;
		
	String rise_sense;
	String fall_sense;
	
	String rise;
	String fall;

    String risePower;
    String fallPower;
	
	//only used for virtual CK -> out delay/slew
	//1 dimensional table indexed by output load
	String rise_1d;
	String fall_1d;
    String risePower_1d;
    String fallPower_1d;
	
	
	String proteus_budget_rise;
	String proteus_budget_fall;
	
	PathTiming rise_timing;
	PathTiming fall_timing;
	
	final CellTiming cellTiming;
	
	boolean isVirtualClockPresent;


	public TimingBlock(String input, String output, List<PathTiming> ps,
			CellTiming cellTiming,boolean image,String fwdLatency,String slackerString) {
		super();
		this.input = input;
		this.output = output;
		this.cellTiming = cellTiming;
		
		for (PathTiming p: ps) {
			assert p.from.equals(input);
			assert p.to.equals(output);

			String sense = p.from_dir.equals(p.to_dir) ?
					"positive_unate" : "negative_unate";
			String cell_dir = p.to_dir.equals("+") ?
					"cell_rise" : "cell_fall";
			String dir_slew = p.to_dir.equals("+") ?
					"rise_transition" : "fall_transition";
            String power_dir = p.to_dir.equals("+") ?
                    "rise_power" :"fall_power";

			String proteus_budget = "";
			int nok=0;
			int nbad=0;
			Boolean ok = true;
			String missing = "";
			String has = "";
			for (Iterator si = new SortingIterator(cellTiming.slacktaus.keySet().iterator(), new FloatComparator()); si.hasNext(); ) {
				String tau = (String)si.next();
				String key = tau+" "+output+p.to_dir;
				if (cellTiming.slacks.containsKey(key)) {
					Float slack = new Float((String) cellTiming.slacks.get(key));
					Float firstdly = new Float((String)p.delay.split("[,\" ]")[cellTiming.slackloadindex+1]);
					String total = String.format(Locale.US,"%.4f", slack+firstdly);
					if (proteus_budget.length()>0)
						proteus_budget += ",";
					proteus_budget += String.format(Locale.US,"%.4f",slack+firstdly);
					nok++;
					has += " "+tau;
				}
				else {
					nbad++;
					missing += " "+tau;
				}
			}
			if (nok > 0 && nbad > 0) {
				System.err.println("WARNING: incomplete data for "+output+p.to_dir+" missing tau"+missing+", has"+has);
				ok = false;
			}
			if (p.to_dir.equals("+")) {
				rise_sense = sense;
				if (proteus_budget.length()>0 && ok)
					proteus_budget_rise = "      proteus_budget_riseDelay : \""+proteus_budget+"\";\n";
                if(cellTiming.macroLib && image){
                    double total_delay=0;
                    String unitdelay;
                    //The delay is 0 if its an acknowledgement ping
                    if(output.endsWith(".e")) unitdelay= cellTiming.getUnitDelaySlew(cellTiming.inputSlewsCnt,cellTiming.outputCapsCnt,0);
                    else{
                        if(slackerString !=null) total_delay = 1 * Double.parseDouble(fwdLatency)
                            *(Double.parseDouble(slackerString)- cellTiming.earliestInput);
                        else  {
                            total_delay = 1 * (1- cellTiming.earliestInput) * Double.parseDouble(fwdLatency); 
                            System.err.println("WARNING: Using default slacker time 1 for output pin "+output);
                        }
                        // 2*tau*(slacker_time(output_pint)-earliest_Arriving_pin's slackerTime)
                        unitdelay= cellTiming.getUnitDelaySlew(cellTiming.inputSlewsCnt,cellTiming.outputCapsCnt,2*total_delay);
                    }
                    rise = cellTiming.getDelaySlew(cell_dir,dir_slew,unitdelay,unitdelay);
                } 
                else rise = cellTiming.getDelaySlew(cell_dir,dir_slew,p.delay,p.slew);
                if(!cellTiming.macroLib)
                    risePower = cellTiming.getPower(power_dir,p.power);
				rise_timing = p;
			} else {
				fall_sense = sense;
				if (proteus_budget.length()>0 && ok)
					proteus_budget_fall = "      proteus_budget_fallDelay : \""+proteus_budget+"\";\n";
                if(cellTiming.macroLib && image){
                    double total_delay=0;
                    String unitdelay;
                    if(output.endsWith(".e")) unitdelay= cellTiming.getUnitDelaySlew(cellTiming.inputSlewsCnt,cellTiming.outputCapsCnt,0);
                    else{
                        if(slackerString !=null) total_delay = 1 * Double.parseDouble(fwdLatency) 
                            *( Double.parseDouble(slackerString)- cellTiming.earliestInput);
                        else  {
                             total_delay = 1 * (1- cellTiming.earliestInput) * Double.parseDouble(fwdLatency);
                             System.err.println("WARNING: Using default slacker time 1 for output pin "+output);
                        }
                        // 2*tau*(slacker_time(output_pint)-earliest_Arriving_pin's slackerTime)
                        unitdelay = cellTiming.getUnitDelaySlew(cellTiming.inputSlewsCnt,cellTiming.outputCapsCnt,2*total_delay);
                    }
                    fall = cellTiming.getDelaySlew(cell_dir,dir_slew,unitdelay,unitdelay);

                } 
                else fall = cellTiming.getDelaySlew(cell_dir,dir_slew,p.delay,p.slew);
                if(!cellTiming.macroLib)
                    fallPower = cellTiming.getPower(power_dir,p.power);
				fall_timing = p;
			}
		}
		
		isVirtualClockPresent = cellTiming.isSetupCheckInput(input);

	}
	
	/**
	 * for representing virtual CK to output timing blocks
	 * @param output
	 * @param riseTiming
	 * @param fallTiming
	 * @param cellTiming
	 */
	public TimingBlock(
			String output,
			PathTiming riseTiming,
			PathTiming fallTiming,
			CellTiming cellTiming) {
		this.input = null; //average over all inputs
		
		this.output = output;
		this.isVirtualClockPresent = true;
		this.cellTiming = cellTiming;
		this.rise_timing = riseTiming;
		this.fall_timing = fallTiming;
		
		this.rise =
			cellTiming.getDelaySlew("cell_rise", "rise_transition", 
					PathTiming.emitTimingValues(riseTiming.getDelayTiming()),
					PathTiming.emitTimingValues(riseTiming.getSlewTiming()));
		
		this.fall =
			cellTiming.getDelaySlew("cell_fall", "fall_transition", 
					PathTiming.emitTimingValues(fallTiming.getDelayTiming()),
					PathTiming.emitTimingValues(fallTiming.getSlewTiming()));
				        
		this.rise_1d = 
		    cellTiming.getDelaySlewCK("cell_rise", "rise_transition", 
		        riseTiming.emitDelayTimingValues1d(),
		        riseTiming.emitSlewTimingValues1d());

		this.fall_1d = 
		    cellTiming.getDelaySlewCK("cell_fall", "fall_transition",
		        fallTiming.emitDelayTimingValues1d(),
		        fallTiming.emitSlewTimingValues1d());

        // Power blocks 
        if(riseTiming.getPower2d() != null)
      	    this.risePower = cellTiming.getPower("rise_power", 
                                PathTiming.emitTimingValues(riseTiming.getPower2d()));
        else
            this.risePower = null;

		
        if(fallTiming.getPower2d() != null)
     	    this.fallPower =
     	    cellTiming.getPower("fall_power", 
                                PathTiming.emitTimingValues(fallTiming.getPower2d()));
        else
            this.fallPower = null;
				        
		this.risePower_1d = 
		    cellTiming.getPowerCK("rise_power",  
		        riseTiming.emitPowerValues1d());

		this.fallPower_1d = 
		    cellTiming.getPowerCK("fall_power", 
		        fallTiming.emitPowerValues1d());
	                
	
	}
	
	public PathTiming getValueOfRiseTiming(CellTiming cellTiming) {
		return new PathTiming(this.rise_timing.to,
			this.rise_timing.to_dir,
			this.rise_timing.getDelayTiming(),
			this.rise_timing.getSlewTiming(),
            this.rise_timing.getPower2d(),
			cellTiming);
	}
	
	public PathTiming getValueOfFallTiming(CellTiming cellTiming) {
		return new PathTiming(this.fall_timing.to, 
			this.fall_timing.to_dir,
			this.fall_timing.getDelayTiming(),
			this.fall_timing.getSlewTiming(),
            this.fall_timing.getPower2d(),
			cellTiming);
	}
	

	
	/**
	 * emit timing block string for .lib file
	 * @param s
	 * @return
	 */
	public String toString() {
		String s = "";
				
        // handle virtual clock or real timing arcs
        if (this.isVirtualClockPresent) {
            if  (rise!=null || fall!=null) {
            	//s += "/* emitTiming case A */\n";
                // only report CK->output timing when either rise or fall exist
                s += "    timing() {\n";
                s += "      related_pin : \"CK\";\n";
                s += "      timing_sense : non_unate;\n";
                s += "      timing_type : rising_edge;\n";
                if (proteus_budget_rise != null)
                    s += proteus_budget_rise;
                if (proteus_budget_fall != null)
                    s += proteus_budget_fall;
                if (rise!=null) {
                    if (cellTiming.macroLib) {
                        s += rise;
                    } else {
                        s += rise_1d;
                    }
                }
                if (fall!=null) {
                    if (cellTiming.macroLib) {
                        s += fall;
                    } else {
                        s += fall_1d;
                    }
                }
                s += "    }\n";
            }
            if(risePower!=null || fallPower!=null){
                s += "    internal_power() {\n";
                // .lib standard only allows related_pin when using
                // 2D or 3D tables (wkoven)
                if (cellTiming.macroLib) {
                    s += "      related_pin : \"CK\";\n";
                }
                if (risePower!=null) {
                    if (cellTiming.macroLib) {
                        s += risePower;
                    } else {
                        s += risePower_1d;
                    }
                }
                if (fallPower!=null) {
                    if (cellTiming.macroLib) {
                        s += fallPower;
                    } else {
                        s += fallPower_1d;
                    }
                }
                s += "    }\n";

            }
        }
        else if (rise!=null && fall!=null && rise_sense.equals(fall_sense)) {
        	//s += "/* emitTiming case B */\n";
            s += "    timing() {\n";
            s += "      related_pin : \"" + input + "\";\n";
            s += "      timing_sense : " + rise_sense + ";\n";
            s += "      timing_type : combinational;\n";
            if (proteus_budget_rise != null)
                s += proteus_budget_rise;
            if (proteus_budget_fall != null)
                s += proteus_budget_fall;
            s += rise;
            s += fall;
            s += "    }\n";
   
            if(risePower!=null || fallPower!=null){
                s += "    internal_power() {\n";
                s += "      related_pin : \"" + input + "\";\n";
                if (risePower!=null)    s += risePower;
                if (fallPower!=null)    s += fallPower;
                s += "    }\n";

            }


        } else {
            if (rise!=null && fall==null) {
            	//s += "/* emitTiming case C */\n";
                s += "    timing() {\n";
                s += "      related_pin : \"" + input + "\";\n";
                s += "      timing_sense : " + rise_sense + ";\n";
                s += "      timing_type : combinational_rise;\n";
                if (proteus_budget_rise != null)
                    s += proteus_budget_rise;
                s += rise;
                s += "    }\n";
                if(risePower!=null || fallPower!=null){
                    s += "    internal_power() {\n";
                    s += "      related_pin : \"" + input + "\";\n";
                    if (risePower!=null)    s += risePower;
                    if (fallPower!=null)    s += fallPower;
                    s += "    }\n";
                }

            }
            else if (fall!=null && rise==null) {
            	//s += "/* emitTiming case D */\n";
                s += "    timing() {\n";
                s += "      related_pin : \"" + input + "\";\n";
                s += "      timing_sense : " + fall_sense + ";\n";
                s += "      timing_type : combinational_fall;\n";
                if (proteus_budget_fall != null)
                    s += proteus_budget_fall;
                s += fall;
                s += "    }\n";
                if(risePower!=null || fallPower!=null){
                    s += "    internal_power() {\n";
                    s += "      related_pin : \"" + input + "\";\n";
                    if (risePower!=null)    s += risePower;
                                    
                    if (fallPower!=null)    s += fallPower;
                    s += "    }\n";

                }

            }
            else if (fall!=null && rise!=null) {
                String edge = "rising_edge";
                if (rise_sense.equals("negative_unate"))
                    edge = "falling_edge";
                //s += "/* emitTiming case E */\n";
                s += "    timing() {\n";
                s += "      related_pin : \"" + input + "\";\n";
                s += "      timing_sense : non_unate;\n";
                s += "      timing_type : "+edge+";\n";
                if (proteus_budget_rise != null)
                    s += proteus_budget_rise;
                if (proteus_budget_fall != null)
                    s += proteus_budget_fall;
                s += rise;
                s += fall;
                s += "    }\n";
                if(risePower!=null || fallPower!=null){
                    s += "    internal_power() {\n";
                    s += "      related_pin : \"" + input + "\";\n";
                    if (risePower!=null)    s += risePower;
                                    
                    if (fallPower!=null)    s += fallPower;
                    s += "    }\n";

                }

            }
        }
        return s;
	}
	
	private class FloatComparator implements Comparator {
		public int compare(Object o1, Object o2) {
			Float a = new Float((String) o1);
			Float b = new Float((String) o2);
			if (a - b > 0.0)
				return 1;
			if (a - b < 0.0)
				return -1;
			return 0;
		}
	}
}
