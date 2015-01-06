/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.io.*;
import java.util.*;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.util.container.SortingIterator;

/** for generating Liberty ".lib" files **/
public class CellTiming {
	
    final Double slewLowerThreshold = new Double(33.3);
	
    final Double slewUpperThreshold = new Double(66.7);
	
    /** number of indices for clock slew (setup constraints) (TODO: read from timing file) **/
    final int N_CLOCK_SLEW = 5;

    /** 5x7 formated 0's to fill in for dummy delay arcs (TODO: read from timing file) **/
    final String zeroValues = "\"0.0,0.0,0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0,0.0,0.0\"";

    /** 5x5 formated 0's to fill in for setup times (TODO: read from timing file) **/
    final String zeroSetupValues = "\"0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0\",\"0.0,0.0,0.0,0.0,0.0\"";

    /** just the slew values to use with the above zeroSetupValues (TODO: read from timing file) **/
    final String zeroSlewValues = "\"0.003,0.007,0.013,0.027,0.053\"";
    
    /** timing paths in data structure **/
    List<PathTiming> timingPaths = new ArrayList<PathTiming>();
        
    /** map from output -> input -> path(output, input) **/
    final Map<String,Map<String,List<PathTiming>>> outputToInputToPathTimingListMap =
    	new HashMap<String,Map<String,List<PathTiming>>>();
    
    /** map from input -> List of PathTiming, get a list of paths starting
     * from input **/
    final Map<String,List<PathTiming>> inputToPathTimingListMap =
    	new HashMap<String,List<PathTiming>>();

    /** input pins **/
    List<String> inputs = new ArrayList<String>();

    /** Slacker time of the earliest arriving input **/
    double earliestInput = 0;

    /** input caps as String's **/
    List<String> inputsInfo = new ArrayList<String>();

    /** output pins **/
    List<String> outputs = new ArrayList<String>();

    /** output caps as String's **/
    List<String> outputsInfo = new ArrayList<String>();

    /** does timing file exist? **/
    public boolean valid = false;

    /** slacks **/
    public HashMap slacks = new HashMap();
    public HashMap slacktaus = new HashMap();
    
    /** output pin -> timing block **/
    Map<String, TimingBlock> outputTimingBlockMap = 
    	new HashMap<String, TimingBlock>();
    
    /** cell area **/
    double area = 0; 

    /** cell leakage **/
    double leakage = 0;

    /** for future addition to timing file **/
    /** process for timing **/
    String process = "tt";

    /** temperature for timing **/
    String temperature = "125";

    /** voltage for timing **/
    String voltage = "0.9";

    /** input slews **/
    String inputSlews = "";
    int    inputSlewsCnt = 0;
    public ArrayList<Double> inputSlewValues = new ArrayList<Double>();

    /** output caps **/
    String outputCaps = "";
    int    outputCapsCnt = 0;
    public ArrayList<Double> outputCapValues = new ArrayList<Double>();

    /** emit a virtual clock and fake setup check in real library **/
    boolean virtualClocks = true;

    /** generate the proper libs for custom QDI hard macros **/
    boolean macroLib = false;

    /** outputCap index for slack values **/
    int slackloadindex = 0;
    
    //TODO support this in the input arguments
    /**
     * computing CK->out delay/slew times using this values as the 
     * fastest input slew value, setup times will add offset to the output values
     */
    final double realLibNominalInputSlew = 0.003;
    
    //TODO support in input args
    /**
     * default (true) is to use input slew to index the setup constraints
     * however when set to false we can use CK slew to select setup
     * constraints, this mode is used for testing
     *
     */
    final boolean setupConstraintsIndexByInputSlew = true;

    /** parse a timing file **/
    CellTiming(String timingDir, String castName, boolean virtualClocks) {
        this(timingDir, castName, virtualClocks, false);
    }

    CellTiming(String timingDir, String castName,
               boolean virtualClocks, boolean macroLib) {
        this.macroLib = macroLib;
        this.virtualClocks = virtualClocks;
        final File ff = new File(timingDir);
        String fileName;
        slackloadindex=0;
        List<String[]> paths = new ArrayList<String[]>();

        if (ff.isFile())
            fileName = timingDir;
        else
            fileName = timingDir + "/" + castName + ".timing";
        try {
            BufferedReader in  = new BufferedReader(new FileReader(fileName));
            String line = null;
            while ((line = in.readLine()) != null) {
                String [] toks = line.split(" ");
                if (toks[0].equals("area")) area = Double.parseDouble(toks[1]);
                else if (toks[0].equals("cellLeakage")) leakage = Double.parseDouble(toks[1]);
                else if (toks[0].equals("inputSlews")) inputSlews = toks[1];
                else if (toks[0].equals("outputCaps")) outputCaps = toks[1];
                else if (toks[0].equals("output"))  {
                    outputs.add(toks[1]);
                    outputsInfo.add(line);
                }
                else if (toks[0].equals("input"))  {
                    inputs.add(toks[1]);

                    inputsInfo.add(line);
                }
                else if (toks[0].equals("path")) paths.add(toks);
                //Copy the power token in the same path where time tokens have been stored
                else if (toks[0].equals("power")){
                    int flag=0;
                    for (int i =0 ; i<paths.size(); i ++){
                        if(paths.get(i)[1].equals(toks[1]) && paths.get(i)[2].equals(toks[2])){
                            String[] temp = new String[paths.get(i).length+1];
                            System.arraycopy(paths.get(i),0,temp,0,paths.get(i).length);
                            temp[paths.get(i).length] = toks[3];
                            paths.set(i,temp);
                            flag = 1;
                        }

                    }
                    assert(flag==1);
                }
                else if (toks[0].equals("process")) process = toks[1];
                else if (toks[0].equals("temperature")) temperature = toks[1];
                else if (toks[0].equals("voltage")) voltage = toks[1];
                else if (toks[0].equals("slackloadindex")) slackloadindex = Integer.parseInt(toks[1]);
                else if (toks[0].equals("slack")) {
                    if (slacks.containsKey(toks[1]+" "+toks[2])) {
                        Float nslack = new Float((String)toks[3]);
                        Float oslack = new Float((String)slacks.get(toks[1]+" "+toks[2]));
                        if (oslack > nslack) {
                            slacks.put(toks[1]+" "+toks[2], toks[3]);
                        }
                    }
                    else {
                        slacks.put(toks[1]+" "+toks[2], toks[3]);
                    }
                    slacktaus.put(toks[1], 1);
                }
            }
            in.close();
            inputSlewsCnt = 0;
            if (!inputSlews.equals("0.0")) {
                inputSlewValues = parseCommaSeperatedString(inputSlews);
                inputSlewsCnt = inputSlewValues.size();
            }
            outputCapsCnt = 0;
            if (!outputCaps.equals("0.0")) {
                outputCapValues = parseCommaSeperatedString(outputCaps);
                outputCapsCnt = outputCapValues.size();
            }
            for (int i = 0; i < paths.size(); i++) processTimingPaths(paths.get(i));            
            valid = true;

        } catch (IOException e) {
            System.err.println("WARNING: no timing file " + fileName);
        }
    }

    public static ArrayList<Double>  parseCommaSeperatedString(final String s) {
        ArrayList<Double> tempList = new ArrayList<Double>();
        String x = new String(s);

        //Remove \" from the string that might exist
        String y = "";
        for (int i = 0; i < x.length(); i ++) {
            if (x.charAt(i) != '\"') y += x.charAt(i);
        }

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
			tempList.add(d);
		}
        return tempList;
    }

    /** Process Path Timings **/
    void processTimingPaths(String [] path) {
        PathTiming pathTiming = new PathTiming(path, this);
        timingPaths.add(pathTiming);
          
        addPath(pathTiming, outputToInputToPathTimingListMap);
        
        assert(!this.inputToPathTimingListMap.containsKey(pathTiming.from));
        
        if (this.inputToPathTimingListMap.get(pathTiming.from)==null) {
            this.inputToPathTimingListMap.put(pathTiming.from, new ArrayList<PathTiming>());
        }
        
        this.inputToPathTimingListMap.get(pathTiming.from).add(pathTiming);       
    }

    /** get area **/
    double getArea() {
        return area;
    }

    /** get leakage **/
    double getLeakage() {
        return leakage;
    }

    /** get temperature **/
    String getTemperature() {
        return temperature;
    }

    /** get voltage **/
    String getVoltage() {
        return voltage;
    }

    /** get process **/
    String getProcess() {
        return process;
    }

    /** get an input cap **/
    double getInputCap(String node) {
        Iterator i = inputsInfo.iterator();
        while (i.hasNext()) {
            String line = (String) i.next();
            String [] toks = line.split(" ");
            if (toks[0].equals("input") && toks[1].equals(node))
                return Double.parseDouble(toks[2]);
        }
        return 0;
    }

    /** get an output cap **/
    double getOutputCap(String node) {
        Iterator i = outputsInfo.iterator();
        while (i.hasNext()) {
            String line = (String) i.next();
            String [] toks = line.split(" ");
            if (toks[0].equals("output") && toks[1].equals(node))
                return Double.parseDouble(toks[2]);
        }
        return 0;
    }

    /** returns true if a PathTiming matches by pin name and direction **/
    private boolean matchPath(final String from, final String from_dir,
                              final String to, final String to_dir,
                              final PathTiming p) {
        return p.from.equals(from) && p.from_dir.equals(from_dir) &&
               p.to.equals(to) && p.to_dir.equals(to_dir);
    }

    /** get delay matrix **/
    String getDelayMatrix(String from, String from_dir,
                          String to, String to_dir,String fwdLatency) {
         if(fwdLatency !=null){
             double tau = Double.parseDouble(fwdLatency); 
             String delay  = getUnitDelaySlew(inputSlewsCnt,outputCapsCnt,2*tau);
             return delay;

        }
    	for (PathTiming p : timingPaths) {
            if (matchPath(from, from_dir, to, to_dir, p)) {
                return p.delay;
            }
    	}
        return null;
    }

    /** get slew matrix **/
    String getSlewMatrix(String from, String from_dir,
                         String to, String to_dir,String fwdLatency ) {
        if(fwdLatency !=null){
             double tau = Double.parseDouble(fwdLatency); 
             String slew  = getUnitDelaySlew(inputSlewsCnt,outputCapsCnt,2*tau);
             return slew;

        }

    	for (PathTiming p : timingPaths) {
            if (matchPath(from, from_dir, to, to_dir, p)) {
                return p.slew;
            }
    	}
        return null;
    }
   // This function returns a string that contains all values equal to 2*tau
   // For eg : "0.34,0.34,0.34","0.34,0.34,0.34","0.340.34,0.34"  
    String getUnitDelaySlew(int slew_length, int cap_length, double tau){
        StringBuffer delaybuffer = new StringBuffer();
        String delay;
        double delayLatency = tau;
        if(slew_length == 0){
            delaybuffer.append("\"");;
            for (int j=0;j<cap_length;j++){
                delaybuffer.append(delayLatency);
                if(j!=cap_length-1)
                    delaybuffer.append(",");
            }
            delaybuffer.append("\"");
        }

        for(int i =0;i<slew_length;i++){
            delaybuffer.append("\"");;
            for (int j=0;j<cap_length;j++){
                delaybuffer.append(delayLatency);
                if(j!=cap_length-1)
                    delaybuffer.append(",");
            }
            delaybuffer.append("\"");
            if(i!= slew_length-1)
                delaybuffer.append(",");
        }
        delay = delaybuffer.toString();
        return delay;

    }

    
       
    /*** create a dummy clock input ***/
    String clockInput() {
        String s = "";
        s += "  pin(CK) {\n";
        s += "    direction : input;\n";
        s += "    clock : true;\n";
        s += "    capacitance : 0.0;\n";
        s += "  }\n";
        return s;
    }

    /** dummy input pin **/
    String dummyInput(String name, String type, String extra) {
        String s = "";
        s += "  pin(" + name + ") {\n";
        s += "    direction : input;\n";
        s += "    capacitance : 0.0;\n";
        s += "    fanout_load : 1.0;\n";
        if (type!=null) s += "    proteus_channel : \"" + type + "\";\n";
        if (extra!=null) s += extra;
        s += "  }\n";
        return s;
    }

    String dummyInput(String name, String type) {
        return dummyInput(name,type,null);
    }

    String dummyInput(String name) {
        return dummyInput(name,null,null);
    }

    /** dummy output pin **/
    String dummyOutput(String name, String type, String extra) {
        String s = "";
        s += "  pin(" + name + ") {\n";
        s += "    direction : output;\n";
        if (type!=null) s += "    proteus_channel : \"" + type + "\";\n";
        if (extra!=null) s += extra;
        s += "    max_fanout : 3.0;\n";
        s += "  }\n";
        return s;
    }

    String dummyOutput(String name, String type) {
        return dummyOutput(name,type,null);
    }

    String dummyOutput(String name) {
        return dummyOutput(name,null,null);
    }
    
    String setupCheckUsingOutputLoad(String clock, TimingBlock outputTiming) {
    	String s = "";
    	return s;
    }

    /*** create a dummy setup check timing block ***/
    String setupCheck(String clock,String str) {
        if (str ==null){
            str = zeroSetupValues;
        }
        String s = "";
        s +="    timing() {\n";
        s +="      related_pin : \"" + clock + "\";\n";
        s +="      timing_type : setup_rising;\n";
        s +="      rise_constraint(setup_template) {\n";
        s +="        index_1 ("+zeroSlewValues+");\n";
        s +="        index_2 ("+zeroSlewValues+");\n";
        s +="        values (" + str + ");\n";
        s +="      }\n";
        s +="      fall_constraint(setup_template) {\n";
        s +="        index_1 ("+zeroSlewValues+");\n";
        s +="        index_2 ("+zeroSlewValues+");\n";
        s +="        values (" + str + ");\n";
        s +="      }\n";
        s +="    }\n";
        s = s.replaceAll("\"\"", "\"");
        return s;
    }
    
    /**
     * 
     * @param clock
     * @param riseSetupTimes
     * @param fallSetupTimes
     * @return
     * setup check timing block utilizing rise and fall setup times
     */
    String setupCheck(String clock, Vector riseSetupTimes, Vector fallSetupTimes) {
    	
    	//emit setup checks
    	String riseRepeated = "";
    	String fallRepeated = "";
    	String debug = "";
    	if (this.setupConstraintsIndexByInputSlew) {
        	//repeat N times for input slew index (index1)
        	for (int i=0; i<this.inputSlewsCnt; i++) {
        	    riseRepeated += "\"";
        	    fallRepeated += "\"";
        	    for (int j=0; j<this.N_CLOCK_SLEW; j++) {
        	        //repeat N times for the CK slew index (index2) of the setup constraint table
        	        riseRepeated += riseSetupTimes.getElementAsString(i);
        	        fallRepeated += fallSetupTimes.getElementAsString(i);
        	        if (j+1<N_CLOCK_SLEW) { riseRepeated += ", "; fallRepeated += ", "; }
        	    }
        	    riseRepeated += "\"";
        	    fallRepeated += "\"";
        		//riseRepeated += riseSetupTimes;
        		//fallRepeated += fallSetupTimes;
        		if (i+1!=this.inputSlewsCnt) { riseRepeated += ","; fallRepeated += ",";}
        		
        	}
    	} else {
    	    debug += "    \n/* debug mode: setup table indexed by CK slew */\n";
            //repeat N times for input slew index (index1)
            for (int i=0; i<this.inputSlewsCnt; i++) {
                riseRepeated += "\"";
                fallRepeated += "\"";
                for (int j=0; j<this.N_CLOCK_SLEW; j++) {
                    //vary by CK slew
                    riseRepeated += riseSetupTimes.getElementAsString(j);
                    fallRepeated += fallSetupTimes.getElementAsString(j);
                    if (j+1<N_CLOCK_SLEW) { riseRepeated += ", "; fallRepeated += ", "; }
                }
                riseRepeated += "\"";
                fallRepeated += "\"";
                if (i+1!=this.inputSlewsCnt) { riseRepeated += ","; fallRepeated += ",";}
                
            }
    	}
    	String s = debug;
        s +="    timing() {\n";
        s +="      related_pin : \"" + clock + "\";\n";
        s +="      timing_type : setup_rising;\n";
        s +="      rise_constraint(setup_template) {\n";
        s +="        index_1 ("+zeroSlewValues+");\n";
        s +="        index_2 ("+zeroSlewValues+");\n";
        s +="        values (" + riseRepeated + ");\n";
        s +="      }\n";
        s +="      fall_constraint(setup_template) {\n";
        s +="        index_1 ("+zeroSlewValues+");\n";
        s +="        index_2 ("+zeroSlewValues+");\n";
        s +="        values (" + fallRepeated + ");\n";
        s +="      }\n";
        s +="    }\n";
        s = s.replaceAll("\"\"", "\"");
        return s;
    }

    /** test if an input pin should have a fake setup check **/
    boolean isSetupCheckInput(String input) {
        return macroLib ||
            virtualClocks && (input.startsWith("L") ||
                              input.startsWith("E") ||
                              input.startsWith("A") ||
                              input.equals("en") ||
                              input.equals("_pc") ||
                              input.equals("eval") ||
                              input.startsWith("R") ||
                              input.startsWith("X")) &&
            !input.startsWith("LC");
    }
    
    //FIXME this code seems to be deprecated by the header.lib in the
    //synthesis dir, which is concatenated by the make_proteus script
    Map getTemplates(boolean power) {
        final HashMap map = new HashMap();
        Iterator i = inputsInfo.iterator();
        String Template;
        String name;
        
        String var;
        if(power) var = "power";
        else var="delay";

        if(!power){
            //create setup template
            Template = "    variable_1 : constrained_pin_transition;\n"+
                       "    variable_2 : related_pin_transition;\n";
            Template += "    index_1 (\"1001,1002,1003\")\n";
            Template += "    index_2 (\"1001,1002,1003\")\n";
            map.put("setup_template", Template);
        }
        
        //create delay/power template
        name = var+"_template_"+inputSlewsCnt+"x"+outputCapsCnt;
        int ndx = 1;
        Template = "";
        if (inputSlewsCnt > 0) {
            if(power) Template += "    variable_1 : input_transition_time;\n";
            else Template += "    variable_1 : input_net_transition;\n";
            ndx++;
        }
        if (outputCapsCnt > 0) {
            Template += "    variable_"+ndx+" : total_output_net_capacitance;\n";
        }
        ndx = 1;
        if (inputSlewsCnt > 0) {
            Template += "    index_"+ndx+" (\"1000";
            for (int v = 1001; v - 1000 < inputSlewsCnt; v++) {
                Template += ","+v;
            }
            Template += "\");\n";
            ndx++;
        }
        if (outputCapsCnt > 0) {
            Template += "    index_"+ndx+" (\"1000";
            for (int v = 1001; v - 1000 < outputCapsCnt; v++) {
                Template += ","+v;
            }
            Template += "\");\n";
            ndx++;
        }
        Template = Template.replaceAll("\"\"", "\"");
        map.put(name, Template);
        name = var + "_template_only_output_caps_"+outputCapsCnt;
        ndx = 1;
        Template = "";
        if (outputCapsCnt > 0) {
            Template += "    variable_"+ndx+" : total_output_net_capacitance;\n";
        }
        if (outputCapsCnt > 0) {
            Template += "    index_"+ndx+" (\"1000";
            for (int v = 1001; v - 1000 < outputCapsCnt; v++) {
                Template += ","+v;
            }
            Template += "\");\n";
            ndx++;
        }
        Template = Template.replaceAll("\"\"", "\"");
        map.put(name, Template);
        return map;
    }

    String getPropString(final Map<String,String> props) {
        StringBuilder result = new StringBuilder();
        if (props != null) {
            for (Map.Entry<String,String> prop : props.entrySet()) {
                result.append("    ");
                result.append(prop.getKey());
                result.append(" : ");
                result.append(prop.getValue());
                result.append(";\n");
            }
        }
        return result.toString();
    }
    
//TODO deprecate
//    
//    /**
//     * for lib_vc task
//     * compute setup delay constraints for the timing arc specified by p
//     * @param p
//     * @param maxSetupTime tracking setup time for all p from same input
//     * @param s printout
//     * @return
//     */
//    private Vector computeSetupConstraints(PathTiming p, Vector maxSetupTime) {
//		// p is an rising input to output timing arc
//    	String s = "";
//		//figure out for this output
//		//what is the max delay/skew over all inputs and for the given 
//		//realLibNominalInputSlewIndex going to this output
//		//can get this from the max timing block that we have
//		//already computed
//		//use the realLibNominalInputSlewIndex to pick a particular
//		//set of delay/slew timing arc values to compute the output delay/slew
//		TimingBlock maxTimingBlock = outputTimingBlockMap.get(p.to);
//		assert(maxTimingBlock!=null);
//		//which output sense to use?
//		String outputSense = p.to_dir;
//		assert(outputSense.equals("+") | outputSense.equals("-"));
//		//get the max delay/skew from this output
//		//max over all output caps
//		Double outputMaxDelay = outputSense.equals("+") ?
//			maxTimingBlock.rise_timing.getMaxDelay() :
//			maxTimingBlock.fall_timing.getMaxDelay(); 
//		Double outputMaxSlew = outputSense.equals("+") ?
//			maxTimingBlock.rise_timing.getMaxSlew() :
//			maxTimingBlock.fall_timing.getMaxSlew();
//		
//		s += String.format("/* path from %s to %s */\n", p.from, p.to);
//		s += String.format("/* outputMaxDelay=%f */\n", outputMaxDelay);
//		s += String.format("/* outputMaxSlew=%f */\n", outputMaxSlew);
//		
//
//		//vector will store one entry per input slew corresponding to max output
//		//delay for that input slew
//		Vector pathMaxDelayOverOutputCaps = new Vector();
//		
//		//setup time per input slew
//		Vector pathSetupTime = new Vector();
//		
//		
//		//for each input slew
//		for (int i0 = 0; i0 < p.getDelayTiming().size(); i0++) {
//			//delay times for different output cap
//			Vector delayByOutputCap = new Vector(p.getDelayTiming().get(i0));
//			//get the worst case delay to get rid of output cap dependence
//			Double maxDelayOverOutputCaps = delayByOutputCap.getMax();
//			
//			pathMaxDelayOverOutputCaps.add(maxDelayOverOutputCaps);
//			s += String.format("/* path delay [input slew=%d] (maxed over output caps) = %f */\n", i0, maxDelayOverOutputCaps);
//			
//			
//			Double setupTimingSlack = maxDelayOverOutputCaps - outputMaxDelay;
//			if (setupTimingSlack == -0.0) { setupTimingSlack = 0.0; }
//			pathSetupTime.add(setupTimingSlack);
//			
//			s += String.format("/* setup timing slack [input slew=%d] = %f */\n", i0, setupTimingSlack);
//		}
//		s += "/* setup delay */\n";
//		s += "/* setup time:  "+pathSetupTime.emit() +" */\n";
//		
//		//DEBUG
//		//System.out.println(s);
//		
//		//combine to get max setup time over all instances
//		return Vector.maxCopy(maxSetupTime, pathSetupTime);
//
//    }
    
   void setEarliestInput(Map<String,String> pinToSlackerMap) {
       boolean slackFlag = true;
       for (String input : this.inputs) {
           //Check the slacker time of this input pin and modify the earliest arriving input time
           if(pinToSlackerMap.containsKey(input)&& input.matches("^.*\\d$") ){ 
               String slackerString;
               double pin_SlackerTime;
               slackerString = pinToSlackerMap.get(input);
               if(slackerString !=null) {
                   pin_SlackerTime = Double.parseDouble(slackerString);
               }else{
                   pin_SlackerTime = 0;
               }
               if(slackFlag){
                   this.earliestInput = pin_SlackerTime;
                   slackFlag = false;
               }
               else{
                   if (pin_SlackerTime < this.earliestInput){
                      this.earliestInput = pin_SlackerTime;
                   }
               }
               
           }
       }
       //System.out.println("\nThe earliest input is :"+ this.earliestInput);

   }
    /** declare inputs with capacitance using alias map and renamer **/
    String getAllInputs(Map<String,Map<String,String>> pinProps,
                        CDLNameInterface namer,Map<String,String> pinToSlackerMap,
                        boolean image, String fwdLatency) {
        boolean doSetupCheck = false;
        String s = "";
        Iterator i = inputsInfo.iterator();
        while (i.hasNext()) {
            String line = (String) i.next();
            String [] toks = line.split(" ");
            if (toks[0].equals("input")) {
                String input = toks[1];
                try {
                    input = namer.renameNode(toks[1]);
                }
                catch(Exception e) {
                    System.err.println("Error: Illegal pin name translation from "+toks[1]);
                }
                s += "  pin(" + input + ") {\n";
                s += "    direction : input;\n";
                s += "    capacitance : " + toks[2] + ";\n";
                s += getPropString(pinProps.get(input));
                if (isSetupCheckInput(toks[1])) {
                	
                    if(!this.macroLib) {
                	    CellTimingProteusVCSetup vcSetup = new CellTimingProteusVCSetup(input, this);
                	    //emit setup timing block
                	    s += setupCheck("CK", vcSetup.getSetupRising(), vcSetup.getSetupFalling());
                    } else {

                        if (image && input.matches("^.*\\.\\d+$") ){
                            String slackerString=null;
                            Double slackerTime =null;
                            double total_delay;
                            if(pinToSlackerMap.containsKey(input)) slackerString = pinToSlackerMap.get(input);
                            if(slackerString !=null) total_delay = 
                               //the total delay = 2* tau * (earliest arriving input pin's time - slacker time 
                               //of this input pin)
                                (this.earliestInput - Double.parseDouble(slackerString) )
                                    * Double.parseDouble(fwdLatency);
                            else  {
                                total_delay =    (this.earliestInput - 0 )
                                    * Double.parseDouble(fwdLatency); 
                                System.err.println("WARNING: Using the default slacker time 0 for the input"+input);
                            }
                            String unitdelay;
                            unitdelay = getUnitDelaySlew(3,3,2*total_delay);
                            s += setupCheck("CK",unitdelay);
                        }
                        else
                 	        s += setupCheck("CK",zeroSetupValues);
                    }
                	
                    doSetupCheck = true;
                }
                s += "  }\n";
            }
        }
        if (doSetupCheck) s += clockInput();
        return s;
    }


    
    private void addPath(final PathTiming p,
    					 final Map<String,Map<String,List<PathTiming>>> organized) {
        Map<String,List<PathTiming>> inputPath = organized.get(p.to);
        if (inputPath == null) {
            inputPath = new HashMap<String,List<PathTiming>>();
            organized.put(p.to, inputPath);
        }

        List<PathTiming> ps = inputPath.get(p.from);
        if (ps == null) {
            ps = new ArrayList<PathTiming>();
            inputPath.put(p.from, ps);
        }

        ps.add(p);
    }

    /** declare all output pins with timing using alias map and renamer **/
    String getAllOutputs(String trueFunc,
                         Map<String,Map<String,String>> pinProps,
                         CDLNameInterface namer,Map<String,String> pinToSlackerMap, boolean image, String fwdLatency) {

        String s = "";
        for (String cname : outputs) {
            String output = cname;
            String slackerString = null;
            try {
                output = namer.renameNode(cname);
            }
            catch(Exception e) {
                System.err.println("Error: Illegal pin name translation from "+cname);
            }
            s += "  pin(" + output + ") {\n";
            s += "    direction : output;\n";
            s += "    capacitance : 0.0;\n";
            if (trueFunc!=null) s += "    function : \"" + trueFunc + "\";\n";
            s += getPropString(pinProps.get(output));
            
            //look up all inputs feeding this output
            //final Map<String,List<Path>> inputPath = organized.get(cname);
            final Map<String,List<PathTiming>> inputPath2 = outputToInputToPathTimingListMap.get(cname);
            //DEBUG
            //if (inputPath == null) { s += "/* inputPath null for "+cname+" */\n"; }
            
            if (inputPath2 != null) {

                if (macroLib) {
                	//DEBUG
                	//s += "/* macroLib */\n";
                    final List<PathTiming> ps2 = inputPath2.get("CK");
                    if (ps2 != null) {
                    	//s += getTimingBlock("CK",cname,ps);
                        if(pinToSlackerMap.containsKey(output)) slackerString = pinToSlackerMap.get(output);
                    	TimingBlock b = new TimingBlock("CK", cname, ps2, this,image,fwdLatency,slackerString);
                    	s += b;
                    }
                }
                //TODO: Note: if the cell is a macrolib, then the following code will never
                //be executed as all paths for a macrolib start at CK. Verify the same with Harry.

                //blocks with virtual clocks
                ArrayList<TimingBlock> vcTimingBlocks = new ArrayList<TimingBlock>();
                
                for (String input : inputs) {
                    //DEBUG
                    //s += "/* input: "+input+" cname: "+cname+" */\n";
                    //final List<Path> ps = inputPath.get(input);
                    final List<PathTiming> ps2 = inputPath2.get(input);
                    if (ps2 != null) {
                    	//FIXME s += getTimingBlock(input,cname,ps);
                    	
                    	TimingBlock b = new TimingBlock(input, output, ps2, this,image,fwdLatency,null);
                    	if (b.isVirtualClockPresent) {
                    		//don't emit virtual clock -> output timing blocks
                    		//until we combine them, see later code
                    		vcTimingBlocks.add(b);
                    	} else {
                    		//print timing blocks for paths w/o virtual clocks
                    		s += b;
                    	}
                    	
                    }
                }
                if (vcTimingBlocks.size() > 0) {
                	//s += "/* merging the output timing blocks */\n";
                	//keep track of max delay/skew seen so far
                	PathTiming maxRising = null;
                	PathTiming maxFalling = null;

                	// combine the timing blocks with virtual clocks
                	for (TimingBlock b : vcTimingBlocks) {

                		if (b.rise_timing != null) {
                			if (maxRising==null) {
                			    //make a copy
                				maxRising = b.getValueOfRiseTiming(this); 
                			} else {
                				maxRising.setMaxTiming(b.rise_timing);
                			}
                		}
                		if (b.fall_timing != null) {
                			if (maxFalling==null) {
                			    //make a copy
                				maxFalling = b.getValueOfFallTiming(this);
                			} else {
                				maxFalling.setMaxTiming(b.fall_timing);
                			}
                		}

                		
                	}

                	
                	//if either rise or fall timing exists, emit timing block
                	if (maxRising != null || maxFalling != null) {
                		//TimingBlock maxBlock = new TimingBlock(output, maxRiseDelay, maxRiseSlew, maxFallDelay, maxFallSlew, this);
                        //System.out.println("\n max rising || max falling");
                		TimingBlock maxBlock = new TimingBlock(output, maxRising, maxFalling, this);
                		s += maxBlock;
                		
                		assert(!this.outputTimingBlockMap.containsKey(output));
                		this.outputTimingBlockMap.put(output, maxBlock);
                	}
                }
            }

            s += "  }\n";
        }
        return s;
    }


    /** get a delay/slew pair **/
    String getDelaySlew(String cell_dir, String dir_slew, String delay, String slew) {
        String s = "";
        if (delay==null || slew==null ||
            delay.equals("") || slew.equals("") ||
            delay.equals("\"\"") || slew.equals("\"\"")) return s;
        String dlytemplate = "delay_template";
        int ndx = 1;
        if (macroLib)
            dlytemplate = "delay_template_"+inputSlewsCnt+"x"+outputCapsCnt;
        s += "      " + cell_dir + "("+dlytemplate+") {\n";
        if (inputSlewsCnt > 0) {
            s += "        index_"+ndx+" (\"" + inputSlews + "\");\n";
            ndx++;
        }
        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + delay + ");\n";
        s += "      }\n";
        ndx = 1;
        s += "      " + dir_slew + "("+dlytemplate+") {\n";
        if (inputSlewsCnt > 0) {
            s += "        index_"+ndx+" (\"" + inputSlews + "\");\n";
            ndx++;
        }
        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + slew + ");\n";
        s += "      }\n";
        s = s.replaceAll("\"\"", "\"");
        return s;
    }
    
    /** get delay/slew pair for virtual_CK->output timing arcs 
     * 
     * delay and slew string should be 1 dimensional
     * using the nominal CK slew to pick out from the 2d input slew, output cap
     * data 
     **/
    String getDelaySlewCK(String cell_dir, String dir_slew, String delay, String slew) {
        String s = "";
        if (delay==null || slew==null ||
            delay.equals("") || slew.equals("") ||
            delay.equals("\"\"") || slew.equals("\"\"")) return s;
        String dlytemplate = "delay_template_1d";
        assert !macroLib : "macro lib not supported by getDelaySlewCK";
        int ndx = 1;
        
        s += "      " + cell_dir + "("+dlytemplate+") {\n";

        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + delay + ");\n";
        s += "      }\n";
        ndx = 1;
        s += "      " + dir_slew + "("+dlytemplate+") {\n";        
        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + slew + ");\n";
        s += "      }\n";
        s = s.replaceAll("\"\"", "\"");
        return s;        
    }
     /** get a power block **/
    String getPower(String power_dir, String power) {
        String s = "";
        if (power==null ||
            power.equals("")||
            power.equals("\"\"")) return s;
        String powertemplate = "power_template";
        int ndx = 1;
        if (macroLib)
            powertemplate = "power_template_"+inputSlewsCnt+"x"+outputCapsCnt;
        s += "      " + power_dir + "("+powertemplate+") {\n";
        if (inputSlewsCnt > 0) {
            s += "        index_"+ndx+" (\"" + inputSlews + "\");\n";
            ndx++;
        }
        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + power + ");\n";
        s += "      }\n";
        ndx = 1;
  
        s = s.replaceAll("\"\"", "\"");
        return s;
    }
    
    /** get power block for virtual_CK->output timing arcs 
     * 
     * Power should be 1 dimensional
     * using the nominal CK slew to pick out from the 2d input slew, output cap
     * data 
     **/
    String getPowerCK(String power_dir, String power) {
        String s = "";
        if (power==null || 
            power.equals("") ||
            power.equals("\"\"") )return s;
        String powertemplate = "power_template_1d";
        assert !macroLib : "macro lib not supported by getDelaySlewCK";
        int ndx = 1;
        
        s += "      " + power_dir + "("+powertemplate+") {\n";

        if (outputCapsCnt > 0) {
            s += "        index_"+ndx+" (\"" + outputCaps + "\");\n";
            ndx++;
        }
        s += "        values (" + power + ");\n";
        s += "      }\n";
        ndx = 1;
     
        s = s.replaceAll("\"\"", "\"");
        return s;        
    }

}
