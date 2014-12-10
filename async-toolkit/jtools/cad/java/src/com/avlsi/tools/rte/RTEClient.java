/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.rte;

import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.net.ServerSocket;
import java.net.UnknownHostException;
import java.util.StringTokenizer;

import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.tools.dsim.DSim;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;

/**
 *  Client starts off by attempting to communicate with the server, if the 
 *  server is alive it retrives tests from it and then invokes autosimulate 
 *  cell to perform digital/csp/java testing of the passed cell. 
 */
public class RTEClient {

    /**  TO DO: Remove hardcoded port form this interface
     **       :  Kill autosimulate cell and start again, 
     **          this is probably the simplest 
     **       :  way to get around the cosim recalcitrant java processes
     **       :  we need to figure of what to do with DSim and its associated 
     **       :  threads
     **       :  general code clean up.
     **/

    private DSim sim;
    
    /* some global string used between ClientHandler and the RTEClient */
    public static final String GET_TEST_STRING = "Next";
    public static final String NO_MORE_CELLS = "Empty";
    public static final String CELL_PASSED = "Pass";
    public static final String CELL_FAILED = "Fail";
    
    //static help method
    public static void help(){
	RTEClient client = new RTEClient();
	System.err.println(client.getClass().getName() +"\n"
			   + "\t--cast-path=cast_path \n"
			   + "\t--server=server machine \n"
			   + "\t--hostname=hostname \n"
			   + "\t--devide-path=DSim device path\n"
			   + "\t--enable-file-cache=[true|false] enable file cache\n"
                           + "\t--digital-delay=digital_delay \n"
                           + "\t--estimated-delay=estimated_delay \n"
                           + "\t--measured-delay=measured_delay \n"
                           + "\t--dataset=dataset \n"
                           + "\t--cycle-count=cycle_count \n"
                           + "\t--cycle-time=cycle_time \n"
                           + "\t--use-measured-delay=[on|off] \n"
			   + "\t--base-dir=results_dir \n"
                           + "\t--server-limit=number of servers \n"
			   + "\t--walltime=max real time \n"
			   + "\t--port=server port \n"
			   + RTEClient.version());
	return;
    }

    //get version info
    public static String version(){
        return com.avlsi.util.debug.VersionInfo.getVersionString(RTEClient.class);
    }

    /* constructor */
    public RTEClient () { 
	this.sim = DSim.get(); 
	this.sim.setCastVersion("2");
    }

    public DSim getDSim(){return this.sim; }

    public static void main (String[] args) throws Exception {
	
	/*** parse command-line arguments here ***/
	String []path =null;
	String []devicePath = null;
	String basedir=null;
	String server=null;
	String hostname=null;
        boolean enable_file_cache = true;
        String digital_delay = "80";      
        String estimated_delay = "1e-12";
        String dataset = "0";
        String use_measured_delay = "off";
        String measured_delay = "1";
        int port = 8080;
        int cycle_count, cycle_time;
        cycle_count = 0;
        cycle_time = 0;
        
	/**** new command line processing stuff ****/
	CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
	
        CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
	
        CommandLineArgs theArgs = cachedArgs;
	
	CommandLineArgsIterator cmdIter = theArgs.iterator();
	
	while ( cmdIter.hasNext() ) {
	    CommandLineArg curr = cmdIter.next();
	    if((curr.getName()).equals("cast-path")){
		String pathstr = curr.getValue();
		path = RegressionTestEngine.getStringArray(pathstr);
		if(path == null){
		    System.out.println("cast-path not specified");
		    System.exit(0);
		} 
	    }
	    if((curr.getName()).equals("device-path")){
		String dPathstr = curr.getValue();
		devicePath = RegressionTestEngine.getStringArray(dPathstr);
		if(devicePath == null){
		    System.out.println("device-path not specified");
		    System.exit(0);
		} 
	    }
	    if((curr.getName()).equals("base-dir")){
		basedir = curr.getValue();
		if(basedir == null){
		    System.out.println("basedir not specified");
		    System.exit(0);
		}
	    }
	    if((curr.getName()).equals("server")){
		server = curr.getValue();
		if(server == null){
		    System.out.println("server not specified");
		    System.exit(0);
		}
	    }
	    if((curr.getName()).equals("port")){
		port = Integer.parseInt(curr.getValue());
	    }
            if((curr.getName()).equals("digital-delay")){
                digital_delay = curr.getValue();
                if(digital_delay == null){
		    System.out.println("digital_delay arg error");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("estimated-delay")){
                estimated_delay = curr.getValue();
                if(estimated_delay == null){
		    System.out.println("estimated_delay arg error");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("measured-delay")){
                measured_delay = curr.getValue();
                if(measured_delay == null){
		    System.out.println("measured_delay arg error");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("cycle-count")){
                cycle_count = (new Integer(curr.getValue())).intValue();
            }
            if((curr.getName()).equals("cycle-time")){
                cycle_time = (new Integer (curr.getValue())).intValue();
            }
            if((curr.getName()).equals("dataset")){
                dataset = curr.getValue();
                if(dataset == null){
		    System.out.println("timescale arg error");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("use-measured-delay")){
                use_measured_delay = curr.getValue();
                if(use_measured_delay == null){
		    System.out.println("use_measured_delay arg error");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("hostname")){
		hostname = curr.getValue();
	    }
	    if((curr.getName()).equals("help")){
		RTEClient.help();
		System.exit(-1);
	    }
	    if((curr.getName()).equals("enable-file-cache")){
		enable_file_cache = (new Integer(curr.getValue()).intValue() != 0);
		String str = enable_file_cache ? "enabled" : "disabled";
		System.out.println("CAST file caching " +str);
	    }
	    if((curr.getName()).equals("version")){
		System.err.println(RTEClient.version());
		System.exit(-1);
	    }
	    //process other potential args here
	}
	System.out.println("Base Directory: " +basedir);
	
	/** done parsing new command-line stuff ****/
 
	RTEClient rtec = new RTEClient();
	boolean done = false;
	Socket sock;
        try {
            sock = new Socket (server, port);
        }
        catch (Exception e) {
            System.err.println(e.getMessage());
            return;
        }
	
	System.err.println("Client Started on " +hostname);

	//set up input and output streams
	
        ObjectInputStream in = new ObjectInputStream(sock.getInputStream());
	
	ObjectOutputStream out = new ObjectOutputStream(sock.getOutputStream());
	
	out.writeUTF(hostname);
        out.flush();
	
	if(enable_file_cache)rtec.getDSim().enableFileCache();
	while(!done){
	    String s;
	    try {
	    out.writeUTF(GET_TEST_STRING);
            out.flush();
	    s = in.readUTF();
	    }
	    catch (Exception e){
                e.printStackTrace();
		System.out.println("I guess we're done");
		return;
	    }
	    if (s == null) { 
		System.out.println("String is null; Terminating client");
		break;
	    }
	    if(s.equals(NO_MORE_CELLS)){ done = true; }
	    else { 
		System.out.println("RTEClient: (Running JDSIM on) "+s);
		
		StringTokenizer cmd = new StringTokenizer(s);
		if(cmd.countTokens() != 3){
		    System.out.println("Format error");
		    System.exit(-1);
		}
		String moduleName = cmd.nextToken();
		String cellType = cmd.nextToken();
		String hashedFileName = cmd.nextToken();
		
		//System.out.println(moduleName +" :: " +cellType);
                System.out.println("digital_delay: "  +digital_delay
                                   +"estimated_delay: "+estimated_delay
                                   +"measured_delay: "+measured_delay
                                   +"dataset: "+dataset
                                   );
                CastParsingOption opt = new StandardParsingOption(theArgs);
		AutoSimulateCell sc = new AutoSimulateCell(moduleName, 
							   cellType, 
							   hashedFileName, 
                                                           digital_delay,
                                                           estimated_delay,
                                                           measured_delay,
                                                           dataset,
                                                           use_measured_delay,
                                                           10, true,
							   path, devicePath,
							   basedir, 
							   rtec.getDSim(),
                                                           opt);
                if(cycle_count > 0)sc.setDefaultCycleCount(cycle_count);
                if(cycle_time > 0)sc.setDefaultCycleTime(cycle_time);
                SimResults result = sc.simulate();
		out.writeUTF(s);
                out.flush();
		
		out.writeObject(result);
		out.flush();
		if(enable_file_cache)rtec.getDSim().flushFileCache();
	    }
	}
	System.out.println("Regression Testing Done");
	return;
    }
}

